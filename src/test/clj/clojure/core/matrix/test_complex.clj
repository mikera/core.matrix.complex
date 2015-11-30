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

(deftest test-complex-equality
  (is (m/e= (c/complex-number 1 2) (cm/complex-array 1 2))))

(deftest test-scaling
  (is (= (c/complex-number 3 6) (m/scale (c/complex-number 1 2) 3)))
  (is (= (c/complex-number 3 6) (m/scale 3 (c/complex-number 1 2))))
  (is (= (c/complex-number 3 6) (m/mul (c/complex-number 1 2) 3)))
  (is (= (c/complex-number 3 6) (m/mul 3 (c/complex-number 1 2))))
  (is (= (c/complex-number 3 6) (m/mul 3 (c/complex-number 1 2))))
  (is (= (c/complex-number -310 70) (m/scale (c/complex-number 1 3) (c/complex-number -10 100))))
  (is (= (m/scale
           (cm/complex-array (m/identity-matrix 2))
           (cm/complex 3 6))))
  (is (m/e= (m/scale
              (cm/complex-array [[1 2] [3 4]] [[5 6] [7 8]]) (cm/complex 0 1))
            (cm/complex-array [[-5.0 -6.0] [-7.0 -8.0]] [[1.0 2.0] [3.0 4.0]]))))

(deftest test-transpose
  (is (m/e= (m/transpose (cm/complex-array (m/identity-matrix 10)))
            (cm/complex-array (m/identity-matrix 10))))
  (is (m/e= (m/transpose (cm/complex-array [[1 2] [3 4]])) (cm/complex-array [[1 3] [2 4]] [[0 0] [0 0]])))
  (is (m/e= (m/transpose (cm/complex-array [[1 2] [3 4]] [[10 2] [1 5]]))
            (cm/complex-array [[1 3] [2 4]] [[10 1] [2 5]]))))

;; TODO: Add test for Hermitian Transpose
;; TODO: Add test for inverse
;; TODO: Add test for trace
;; TODO: Add test for determinant

