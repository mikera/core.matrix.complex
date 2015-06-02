(ns clojure.core.matrix.complex
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix :as m]
            [complex.core :as c]
            [clojure.core.matrix.implementations :as imp]
            [mikera.cljutils.error :refer :all])
  (:import [org.apache.commons.math3.complex Complex]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(declare real imag complex-array complex complex? canonical-complex-array)

;; ==============================================================
;;
;; The ComplexArray class and protocol implementations
;;
;; Implemented as a wrapper of two numerical arrays representing the real and imaginary parts

(deftype ComplexArray [real imag]
  mp/PImplementation 
	  (implementation-key [m]
	    :complex-array)
	  (meta-info [m]
	      {:doc "Implementation for complex arrays"})
	  (new-vector [m length] (ComplexArray. (m/new-vector length) (m/new-vector length)))
	  (new-matrix [m rows columns] 
      (ComplexArray. (m/new-matrix rows columns) (m/new-matrix rows columns)))
	  (new-matrix-nd [m dims]
	    (ComplexArray. (m/new-array dims) (m/new-array dims)))
	  (construct-matrix [m data]
	    (ComplexArray. (clojure.core.matrix.complex/real data) 
                     (clojure.core.matrix.complex/imag data)))
	  (supports-dimensionality? [m dims]
	    true)
   
   mp/PDimensionInfo
    (dimensionality [m]
      (mp/dimensionality (.real m)))
    (is-vector? [m]
      (mp/is-vector? (.real m)))
    (is-scalar? [m]
      false)
    (get-shape [m]
      (or (mp/get-shape (.real m)) []))
    (dimension-count [m x]
      (mp/dimension-count (.real m) x))
    
   mp/PIndexedAccess
    (get-1d [m x]
      (c/complex-number (double (mp/get-1d (.real m) x))
                        (double (mp/get-1d (.imag m) x))))
    (get-2d [m x y]
      (c/complex-number (double (mp/get-2d (.real m) x y))
                        (double (mp/get-2d (.imag m) x y))))
    (get-nd [m indexes]
      (c/complex-number (double (mp/get-nd (.real m) indexes))
                        (double (mp/get-nd (.imag m) indexes))))
    
    mp/PIndexedSetting
	    (set-1d [m row v]
	      (ComplexArray. (mp/set-1d real row (clojure.core.matrix.complex/real v))
                       (mp/set-1d imag row (clojure.core.matrix.complex/imag v))))
	    (set-2d [m row column v]
	      (ComplexArray. (mp/set-2d real row column (clojure.core.matrix.complex/real v))
                       (mp/set-2d imag row column (clojure.core.matrix.complex/imag v))))
	    (set-nd [m indexes v]
	      (ComplexArray. (mp/set-nd real indexes (clojure.core.matrix.complex/real v))
                       (mp/set-nd imag indexes (clojure.core.matrix.complex/imag v))))
	    (is-mutable? [m]
	      (mp/is-mutable? real))
     
     mp/PIndexedSettingMutable
     (set-1d! [m x v]
      (mp/set-1d! real x (clojure.core.matrix.complex/real v))
       (mp/set-1d! imag x (clojure.core.matrix.complex/imag v)))
     (set-2d! [m x y v]
      (mp/set-2d! real x y (clojure.core.matrix.complex/real v))
      (mp/set-2d! imag x y (clojure.core.matrix.complex/imag v)))
     (set-nd! [m indexes v]
      (mp/set-nd! real indexes (clojure.core.matrix.complex/real v))
      (mp/set-nd! imag indexes (clojure.core.matrix.complex/imag v)))
     
     mp/PTypeInfo
     (element-type [m]
       Complex)
    
     java.lang.Object 
     (toString [m]
       (str "#complex/complex-array [" (clojure.core.matrix.complex/real m) ", " (clojure.core.matrix.complex/imag m) "]"))
  
     clojure.lang.Seqable
     (seq [m]
       (map complex-array (m/slices (.real m)) (m/slices (.imag m))))

   mp/PMatrixAdd
     (matrix-add [m a]
       (ComplexArray. (mp/matrix-add (clojure.core.matrix.complex/real m)
                                     (clojure.core.matrix.complex/real a))
                      (mp/matrix-add (clojure.core.matrix.complex/imag m)
                                     (clojure.core.matrix.complex/imag a))))

   mp/PMatrixProducts
     (inner-product [m a]
       (ComplexArray. (mp/add-scaled (mp/inner-product (clojure.core.matrix.complex/real m)
                                                       (clojure.core.matrix.complex/real a))
                                     (mp/inner-product (clojure.core.matrix.complex/imag m)
                                                       (clojure.core.matrix.complex/imag a))
                                     -1)
                      (mp/matrix-add (mp/inner-product (clojure.core.matrix.complex/real m)
                                                       (clojure.core.matrix.complex/imag a))
                                     (mp/inner-product (clojure.core.matrix.complex/imag m)
                                                       (clojure.core.matrix.complex/real a))))))



;; ========================================================================
;;
;; Protocol implementations for the Complex number type, used as a scalar
;;
;; These are needed to allow the COmplex numbers themselves to participate fully in core.matrix protocols

(extend-protocol mp/PImplementation 
   Complex
	  (implementation-key [m]
	    :complex)
	  (meta-info [m]
	      {:doc "Implementation for complex scalars"})
	  (new-vector [m length] 
      (mp/new-vector canonical-complex-array length))
	  (new-matrix [m rows columns] 
      (mp/new-matrix canonical-complex-array rows columns))
	  (new-matrix-nd [m dims]
	    (mp/new-matrix-nd canonical-complex-array dims))
	  (construct-matrix [m data]
	    (complex data))
	  (supports-dimensionality? [m dims]
	    true))

;; ========================================================================
;;
;; Functions for handling complex arrays

(defn complex
  "Function to coerce a value to a complex scalar or array"
  ([a]
    (cond 
      (complex? a) a
      (number? a) (c/complex-number a)
      (mp/get-shape a) (complex-array a)
      :else (error "Unable to coerce to complex value: " a))))

(defn complex?
  "Predicate to test whether a value is a complex number or array supported by core.matrix.complex"
  ([a]
    (boolean (or (instance? Complex a) (instance? ComplexArray a)))))

(defn complex-array 
  ([real]
    (complex-array real 0.0))
  ([real imag]
    (ComplexArray. real (m/broadcast-coerce real imag))))

(defn real 
  "Get the real part of a complex array"
  ([m]
    (cond 
      (instance? Complex m) (.getReal ^Complex m)
      (instance? ComplexArray m) (.real ^ComplexArray m)
      (number? m) m
      (m/numerical? m) m
      (m/array? m) (m/emap real m)
      :else (error "Unable to get real part of object: " m " with type " (class m)))))

(defn imag 
  "Get the imaginary part of a complex array"
  ([m]
    (cond 
	    (instance? Complex m) (.getImaginary ^Complex m)
	    (instance? ComplexArray m) (.imag ^ComplexArray m)
	    (number? m) 0.0
	    (m/numerical? m) (mp/coerce-param m (mp/broadcast-like m 0.0)) 
	    (m/array? m) (m/emap imag m)
      :else (error "Unable to get imaginary part of object: " m " with type " (class m)))))

;; Print methods for complex types
(defmethod print-method ComplexArray [^Object v ^java.io.Writer w]
  (.write w (.toString v)))

(defmethod print-method Complex [v ^java.io.Writer w]
  (.write w (str "#complex/complex [" (real v) ", " (imag v) "]")))

;; a canoncial object used to register the complex array implementation
(def canonical-complex-array
  (let [a (complex-array 0 1)] 
    (imp/register-implementation a)
    a))

(imp/register-implementation (complex-array 0 1))
(imp/register-implementation (c/complex-number 2 3))

