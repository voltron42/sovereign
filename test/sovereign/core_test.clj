(ns sovereign.core-test
  (:require [clojure.test :refer :all]
            [sovereign.core :as sovereign]))

(deftest test-hiccup-schema
  (let [validator (sovereign/make-recursive
                    #(sovereign/seq-of
                       [:tag-name sovereign/is-keyword]
                       [:attrs :? (sovereign/map-of
                                    :each-key sovereign/is-keyword
                                    :each-value sovereign/is-primitive)]
                       [:children :* (sovereign/one-of
                                       sovereign/is-string
                                       (%))]))
        ]
    ))