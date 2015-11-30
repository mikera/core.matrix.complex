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
  (implementation-key [m]                                   ;; in this case, 'm' takes the place of the more typical 'this'
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

  mp/PTranspose
  (transpose [m]
    (ComplexArray. (mp/transpose (clojure.core.matrix.complex/real m))
                   (mp/transpose (clojure.core.matrix.complex/imag m))))

  mp/PMatrixScaling
  (scale [m a]
    (cond
      (number? a) (ComplexArray. (mp/scale (clojure.core.matrix.complex/real m) a)
                                 (mp/scale (clojure.core.matrix.complex/imag m) a))
      (instance? Complex a) (ComplexArray.
                              (mp/matrix-sub
                                (mp/scale (clojure.core.matrix.complex/real m)
                                          (c/real-part a))
                                (mp/scale (clojure.core.matrix.complex/imag m)
                                          (c/imaginary-part a)))
                              (mp/matrix-add
                                (mp/scale (clojure.core.matrix.complex/imag m)
                                          (c/real-part a))
                                (mp/scale (clojure.core.matrix.complex/real m)
                                          (c/imaginary-part a))))
      :else (error "Unable to multiply " (class a) " with a Complex value")))
  (pre-scale [m a]
    (cond
      (number? a) (ComplexArray. (mp/scale (clojure.core.matrix.complex/real m) a)
                                 (mp/scale (clojure.core.matrix.complex/imag m) a))
      :else (error "Unable to multiply " (class a) " with a Complex value")))

  mp/PMatrixOps
  (trace [m]
    (let [real-trace (m/trace (clojure.core.matrix.complex/real m))
          imag-trace (m/trace (clojure.core.matrix.complex/imag m))]
      (complex real-trace imag-trace)))
  (determinant [m])
  (inverse [m]
    (let [A (clojure.core.matrix.complex/real m)
          C (clojure.core.matrix.complex/imag m)
          r0 (m/mul (m/inverse A) C)  ; use the highly optimized real inverse from
          y11 (m/inverse (m/add (m/mul C r0) A))
          y01 (m/mul r0 y11)]
      (complex-array y11 (m/negate y01))))

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

(extend-protocol mp/PMatrixScaling
  Complex
  (scale [m a]
    (cond
      (or (instance? Complex a) (number? a)) (c/* m a)
      (instance? ComplexArray a) (mp/pre-scale a m)
      (m/array? a) (mp/pre-scale (complex-array a) m)
      :else (error "Unable to multiply " (class a) " with a Complex value")))
  (pre-scale [m a]
    (cond
      (or (instance? Complex a) (number? a)) (c/* a m)
      (instance? ComplexArray a) (mp/scale a m)
      (m/array? a) (mp/scale (complex-array a) m)
      :else (error "Unable to multiply " (class a) " with a Complex value")))
  Object
  (scale [m a]
    (mp/element-map m #(* % a)))
  (pre-scale [m a]
    (mp/element-map m (partial * a))))

;; ========================================================================
;;
;; Functions for handling complex arrays

(defn complex
  "Function to coerce a value to a complex scalar or array"
  ([r]
   (cond
     (complex? r) r
     (number? r) (c/complex-number r)
     (mp/get-shape r) (complex-array r)
     :else (error "Unable to coerce to complex value: " r)))
  ([r i]
   (cond
     (and (number? r) (number? i)) (c/complex-number r i)
     :else (error "Unable to coerce to complex value: " r i))))

(defn complex?
  "Predicate to test whether a value is a complex number or array supported by core.matrix.complex"
  ([a]
   (boolean (or (instance? Complex a) (instance? ComplexArray a)))))

(defn complex-array
  "Contruct a complex array from real and imaginary components. Components must be numerical arrays."
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

(defn hermitian-transpose
  "Returns the Hermitian Transpose (complex conjugate transpose) of the matrix"
  [m]
  (m/transpose
    (clojure.core.matrix.complex/complex-array
      (clojure.core.matrix.complex/real m)
      (m/negate (clojure.core.matrix.complex/imag m)))))

;; ======================================================================
;; Print methods for complex types
(defmethod print-method ComplexArray [^Object v ^java.io.Writer w]
  (.write w (.toString v)))

(defmethod print-method Complex [v ^java.io.Writer w]
  (.write w (str "#complex/complex [" (real v) ", " (imag v) "]")))

;; ======================================================================
;; canonical objects used to register the complex array implementation
(def canonical-complex-array
  (let [a (complex-array 0 1)]
    (imp/register-implementation a)
    a))

(def canonical-complex-number
  (let [a (c/complex-number 2 3)]
    (imp/register-implementation a)
    a))

