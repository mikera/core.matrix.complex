(ns clojure.core.matrix.test-compliance
  (:use clojure.test)
  (:require [complex.core :as c]
            [clojure.core.matrix.complex :as cm]
            [clojure.core.matrix :as m])
  (:require [clojure.core.matrix.compliance-tester :as tester]))

(deftest compliance-test
  (tester/test-implementation cm/canonical-complex-array))

(deftest instance-tests
  (tester/instance-test (cm/complex-array [1 2 -3] [4 5 6]))
  (tester/instance-test (cm/complex-array [[1 2] [3 4]] [[5 -6] [7 8]])))