(ns learn.core
  (:use [clojure.tools.trace])
  (:use [clojure.contrib math]))

;;;;

;will throw error, since no []
;(defn hello (println ".."))
;[] required, even if empty

;;;;

;fn returns fn object, can store it in a var, pass it around
(fn [n] n)
;#<core$eval984$fn__985 learn.core$eval984$fn__985@219a6087>

;;;;

;create fn object and then call it
((fn [n] n) 1)
;1

;fn are just data, here we pass + to (x 1 2)
((fn [x] (x 1 2)) +)
;3

;reader macro for fn, no [] req. use % with numbers
(#(* 2 %1 %2) 10 20)
;400

;;;;

;create hello fn, which calls itself, will result in stackoverflow error
;((fn hello [] (println "Hello World") (hello)))
;Hello World

;;;;

(def hello (fn [] (println "Hello World")))
;#'learn.core/hello

;defn is syntactic sugar for def + fn
(defn hello []
  (println "Hello World"))

(hello)
;Hello World

;;;;

;binding hi to "hello" only inside let fn
(let [hi "hello"] hi)

;;;;

;as of 1.3, needs to be marked dynamic for it to be reassigned with
;binding
(def ^:dynamic my-value 10)

(defn add-to-my-value [n]
  (+ my-value n))

(add-to-my-value 10)
;20

;let has no effect on variables outside its scope
(let [my-value 20]
  (add-to-my-value 10))
;20

;temp reassigns my-value to 20
(binding [my-value 20]
  (add-to-my-value 10))
;30

;;;;

;deftrace same as defn, but shows visual output of f(x) calls

(deftrace factorial [number]
  "recursive factorial"
  (if (= number 1) 1
    (* number (factorial (- number 1)))))

;(factorial 5)

(deftrace factorial-iter [number counter product]
  "iterative factorial"
  (if (= counter 0) product
    (factorial-iter number (- counter 1) (* product counter))))

;(factorial-iter 5 5 1)

;;;;

; do is a way to have multiple clauses
(deftrace fib-iter [a b count]
  "iterative fib"
  (if (= count 0)
    (do
      (println count)
      b)
    (do
      (let [one-up (+ a b)]
        (fib-iter one-up a (- count 1))
        (println one-up)))))

(defn fib [n]
  (fib-iter 0 1 n))

;(fib 7)

;;;;

(deftrace find-exp [base exp result]
  "iterative finding of exponents"
  (if (= exp 0) result
    (find-exp base (- exp 1) (* base result))))

;(find-exp 2 3 1)

;;;;

;vectors are like ruby arrays, but immutable like most data structs
(= [1 2 3] (vector 1 2 3))
;true

;no pair data type, use 2 element vector
(def pair [1 2 3])

(first pair)
;1

(rest pair)
;(2 3)

(first (rest pair))
;2

(defn const [x y]
  "similar to cons in scheme"
  (vector x y))

(const 1 2)
;[1 2]

(const [1] [2])
;[[1] [2]]

;inbuilt fn that appends 1st arg to 2nd arg which is a seq.
(cons 2 [1 2])
;(2 1 2)

;can be vector/seq.
(cons 1 '(2 2))
;(1 2 2)

(defn pair? [x]
  (and (coll? x) (seq x)))

;;;;

(defn greatest-common-denom [big small]
  "find greatest common denominator"
  (let [remainder (rem big small)]
    (if (= remainder 0) small
      (greatest-common-denom small remainder))))

(greatest-common-denom 10 5)
;5

(greatest-common-denom 2230 8)
;2

;;;;

(defn make-rat [x y]
  "make rational number; save as vector"
  ;[x y] works as well, feels weird having arg list and vector
  ;implemented with same syntax
  (vector x y))

(defn numer [r]
  "return numerator of rational number"
  (first r))

(defn denom [r]
  "return denominator of rational number"
  (last r))

(make-rat 1 2)
;[1 2]

(numer (make-rat 1 2))
;1

(denom (make-rat 1 2))
;2

(defn one-half [] (make-rat 1 2))

(one-half)

(defn sign-of [number]
  (if (< number 0) -1
    (+ 1)))

(abs 1)

(defn make-rational [numer denom]
  "takes two integers and save as vector; reduce first; normalize so
  numerator returns negative if numer/denom arg is negative"
  (let [common-divisor 
          (greatest-common-denom (abs numer) (abs denom))
        sign 
          (sign-of denom)]
    [(/ numer (* sign common-divisor))
     (/ denom (* sign common-divisor))]))

(make-rational 20 10)

;if we don't do (abs numer) & (abs denom) we'll get [1, -2]
(make-rational -1 2)
;[-1 2]

(make-rational 1 -2)
;[-1 2]

(defn print-rational [rational]
  "prints numerator/denominator"
  (print (numer rational))
  (print "/")
  (println (denom rational)))

(print-rational (make-rational 2 4))
;1/2

(print-rational (one-half))
;1/2

;;;;

(defn const [x y]
  "implement pair using procedures, not data structures; returns fn"
  (defn dispatch [m]
    (cond (= m 0) x
          (= m 1) y))
  dispatch)

(defn first-const [const] 
  "passes 0 to z fn"
  (const 0))

(defn rest-const [const] 
  "passes 1 to z fn"
  (const 1))

(first-const (const 1 2))

(rest-const (const 1 2))

(defn const [x y]
  "returns f(x) that accepts 1 arg which it will apply with x, y"
  (fn [m] (m x y)))

(defn car [z]
  "applies fn that two two arg and returns 1 to passed in arg"
  (z (fn [p q] p)))

;((fn [z x y] (z x y)) fn [p q] p)

(defn cdr [z]
  (z (fn [p q] q)))

(car (const 1 2))
(cdr (const 1 2))
