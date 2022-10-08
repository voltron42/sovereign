(ns sovereign.core
  (:require [clojure.set :as set])
  (:import (clojure.lang IFn)))

(defn build-err [value & {:as err-obj}]
  (assoc err-obj :value value))

(defn build [pred errFn]
  (fn [value]
    (try
      (when-not (pred value)
        (errFn value))
      (catch Throwable e
        (build-err value :exception e)))))

(defn aspect-of [validate aspect-fn aspect-label]
  (fn [value]
    (let [error (validate (aspect-fn value))]
      (when error
        (let [error (if (and (map? error) (contains? error :value)) error {:error error})]
          (assoc error :value value :aspect aspect-label))))))

(defn- type-is [type-in-question-fn type-in-question-label]
  (build type-in-question-fn #(build-err % :type-in-question type-in-question-label)))

(def is-vector (type-is vector? :vector))

(def is-list (type-is list? :list))

(def is-set (type-is set? :set))

(def is-map (type-is map? :map))

(def is-number (type-is number? :number))

(def is-int (type-is int? :int))

(def is-string (type-is string? :string))

(def is-boolean (type-is boolean? :boolean))

(def is-keyword (type-is keyword? :keyword))

(def is-symbol (type-is symbol? :symbol))

(defn all-of-verbose [& validators]
  (fn [value]
    (let [errors (reduce
                   (fn [acc [validate index]]
                     (let [error (validate value)]
                       (if error (assoc acc index error) acc)))
                   {}
                   (mapv vector validators (range)))]
      (when-not (empty? errors) errors))))

(defn- fail-first [flag error-label & validators]
  (fn [value]
    (when (->> validators
               (take-while #(= flag (boolean (% value))))
               (count)
               (= (count validators) )
               (= flag))
      (build-err value error-label :failed))))

(defn all-of [& validators]
  (apply fail-first :all-of false validators))

(defn one-of [& validators]
  (apply fail-first :one-of true validators))

(def is-primitive (one-of is-number is-string is-boolean))

(defn every-is [validate]
  (fn [[& value]]
    (let [error (is-vector value)]
      (if error
        error
        (let [errors (reduce
                       (fn [acc [elem index]]
                         (let [error (validate elem)]
                           (if error (assoc acc index error) acc)))
                       {}
                       (mapv vector value (range)))]
          (when-not (empty? errors) errors))))))

(defn build-with-arg [comparer label]
  (fn [arg] (build (partial comparer arg) #(build-err % label arg))))

(def min-is (build-with-arg <= :min))

(def max-is (build-with-arg >= :max))

(def is-greater-than (build-with-arg < :lesser))

(def is-less-than (build-with-arg > :greater))

(def is-step (build-with-arg #(mod %2 %1) :step))

(def pattern-matches (build-with-arg re-matches :pattern))

(def min-count-is (aspect-of (build-with-arg <= :min) count :count))

(def max-count-is (aspect-of (build-with-arg >= :max) count :count))

(def is-count-greater-than (aspect-of (build-with-arg < :lesser) count :count))

(def is-count-less-than (aspect-of (build-with-arg > :greater) count :count))

(def is-count-step (aspect-of (build-with-arg #(mod %2 %1) :step) count :count))

(defn is-enum [& enum-values]
  (let [enum-set (set enum-values)]
    (build enum-set #(build-err % :enum enum-set))))

(defn build-from-validator-map [validator-map & {:keys [init-validators arg-mod] :or {arg-mod identity init-validators []}}]
  (let [init-validators (if (vector? init-validators) init-validators [init-validators])]
    (fn [& {:as args}]
      (->> (set/intersection (set (keys validator-map)) (set (keys args)))
           (reduce #(conj %1 (apply (get validator-map %2) (get args %2))) [])
           (apply all-of-verbose)
           (apply all-of init-validators)))))

(def ^:private validator-maps-by-type
  {:int
   {:min min-is
    :max max-is
    :step is-step
    :lesser is-greater-than
    :greater is-less-than}
   :number
   {:min min-is
    :max max-is
    :lesser is-greater-than
    :greater is-less-than}
   :string
   {:min-length min-count-is
    :max-length max-count-is
    :length-step is-count-step
    :lesser-length is-count-greater-than
    :greater-length is-count-less-than
    :pattern pattern-matches}
   :vector
   {:min-length min-count-is
    :max-length max-count-is
    :length-step is-count-step
    :lesser-length is-count-greater-than
    :greater-length is-count-less-than
    :every every-is}
   })

(def int-of (build-from-validator-map (:int validator-maps-by-type) :init-validators is-int :arg-mod #(assoc % :step (or (:step %) 1))))

(def number-of (build-from-validator-map (:number validator-maps-by-type) :init-validators is-number))

(def string-of (build-from-validator-map (:string validator-maps-by-type) :init-validators is-string))

(def vector-of (build-from-validator-map (:vector validator-maps-by-type) :init-validators is-vector))

(def map-of
  (build-from-validator-map
    {:each-key #(aspect-of (every-is %) keys :keys)
     :each-value #(aspect-of (every-is %) vals :vals)}
    :init-validators is-map))

(defn make-recursive [specFn]
  (let [recurse (atom nil)]
    (reset! recurse (specFn (fn [] #(@recurse %))))
    @recurse))

(def object-of
  (build-from-validator-map
    {:fields (fn [property-validators]
               (let [properties (set (keys property-validators))]
                 (fn [value]
                   (let [errors (reduce
                                  (fn [out key]
                                    (let [error ((get property-validators key) (get value key))]
                                      (if error (assoc out key error) out)))
                                  {}
                                  (set/intersection properties (set (keys value))))]
                     (when-not (empty? errors) errors)))))
     :required (fn [& required]
                 (fn [value]
                   (let [missing (set/difference (set required) (set (keys value)))]
                     (when-not (empty? missing)
                       (build-err value :missing missing :required required)))))}
    :init-validators is-map))

(def ^:private cardinality-flags
  {"" {:optional false :multiple false}
   :? {:optional true :multiple false}
   :* {:optional true :multiple true}
   :* {:optional false :multiple true}})

(defn- seq-of-arg-mapper [[label cardinality validator]]
  (let [[cardinality validator] (if (instance? IFn cardinality)
                                  ["" cardinality]
                                  [cardinality validator])]
    (assoc (get cardinality-flags cardinality) :label label :validator validator)))

(defn- recurse-seq-validator [seq-spec my-vector spec-index vector-index & [error]]
  (if (or (>= spec-index (count seq-spec)) (>= vector-index (count my-vector)) error)
    [spec-index vector-index error]
    (let [spec (get seq-spec spec-index)
          elem (get my-vector vector-index)
          error ((:validator spec) elem)]
      (if error
        (if (:optional spec)
          (recur seq-spec my-vector (inc spec-index) vector-index)
          (recur seq-spec my-vector spec-index vector-index error))
        (recur seq-spec my-vector (if (:multiple spec) spec-index (inc spec-index)) (inc vector-index))))))

(defn seq-of [& args]
  (let [seq-spec (mapv seq-of-arg-mapper args)
        seq-validator (fn [my-vector]
                        (let [[spec-index vector-index error] (recurse-seq-validator seq-spec my-vector 0 0)
                              spec-index (if (and (= (count seq-spec) (dec spec-index)) (= (count my-vector) vector-index) (not error)) (inc spec-index) spec-index)]
                          (if error
                            {:value my-vector
                             :error error
                             :vector-index vector-index
                             :element (get my-vector vector-index)
                             :spec-element (:label (get seq-spec spec-index))}
                            (if (< (inc spec-index) (count seq-spec))
                              (let [missing (->> seq-spec
                                                 (mapv vector (range))
                                                 (filter (fn [[spec index]]
                                                           (and (not (:optional spec)) (> index spec-index))))
                                                 (mapv #(:label (second %))))]
                                (when-not (empty? missing)
                                  {:value my-vector
                                   :missing missing}))
                              (when (< vector-index (count my-vector))
                                {:value my-vector
                                 :unexpected-count (- (count my-vector) vector-index)})))))]
    (all-of is-vector seq-validator)))

(def ^:private spec-map
  {:seq-of identity})

(defn build-from-spec [spec])