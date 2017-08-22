(defproject core.matrix.complex "0.0.1-SNAPSHOT"
  :description "Complex numerical arrays in Clojure, as an extension to core.matrix"
  :url "https://github.com/mikera/core.matrix.complex"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.mikera/core.matrix "0.60.3"]
                 [complex "0.1.8"]
                 [net.mikera/clojure-utils "0.8.0"]
                 [net.mikera/core.matrix.stats "0.7.0"]
                 [org.clojure/tools.trace "0.7.9"]
                 [org.clojure/math.numeric-tower "0.0.4"]]

  :profiles {:dev {:dependencies [[net.mikera/vectorz-clj "0.47.0"]
                                  [net.mikera/cljunit "0.6.0"]
                                  [net.mikera/core.matrix.testing "0.0.4"]]}}

  :source-paths ["src/main/clj"]
  :test-paths ["src/test/clj"])
