(ns clojure.core.matrix.test-complex
  (:use clojure.test)
  (:require [complex.core :as c]
            [clojure.core.matrix.complex :as cm]
            [clojure.core.matrix :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(deftest test-complex
  (let [c (c/complex-number 1 2)]
    (is (== 1 (cm/real c)))
    (is (== 2 (cm/imag c)))))

(deftest test-complex-array
  (let [ca (cm/complex-array [1 2] [3 4])]
    (is (= [2] (m/shape ca)))
    (is (m/equals [1 2] (cm/real ca)))
    (is (m/equals [3 4] (cm/imag ca)))
    (is (= (c/complex-number 1 3) (m/mget ca 0)))
    (is (= (c/complex-number 2 4) (m/mget ca 1)))))

(deftest test-complex-predicates
  (is (cm/complex? (c/complex-number 1 2)))
  (is (cm/complex? (cm/complex-array 1 2)))
  (is (not (cm/complex? 3.5)))
  (is (not (cm/complex? [1 2 3])))
  (is (cm/complex? (cm/complex-array [1 2 3]))))

()

