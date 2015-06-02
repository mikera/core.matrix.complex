(ns clojure.core.matrix.test-complex
  (:use clojure.test)
  (:require [complex.core :as c]
            [clojure.core.matrix.complex :as cm]
            [clojure.core.matrix :as m]))

(deftest test-complex
  (let [c (c/complex-number 1 2)]
    (is (== 1 (cm/real c)))
    (is (== 2 (cm/imag c)))))

