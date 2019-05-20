(ns clj-sicp.chapter-01
  (:refer-clojure :exclude [test]))

;; Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
(defn fh [x y] (+ (* x x) (* y y)))

(defn square-of-max-two [a b c]
  (cond
    (>= a b c) (fh a b)
    (>= a c b) (fh a c)
    (>= b c a) (fh b c)
    (>= b a c) (fh b a)
    (>= c a b) (fh c a)
    (>= c b a) (fh c b)))

;; Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(comment
  (define (p) (p))

  (define (test x y)
    (if (= x 0)
      0
      y))
  )

;; Then he evaluates the expression

(comment (test 0 (p)))

;; What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)
(defn p [] (p))

(defn test [x y]
  (if (= x 0)
    0
    y))

(comment
  ;; this will throw StackOverflow error
  (test 0 (p)))

;; Example: Square Roots by Newton’s Method
(defn average [x y]
  (/ (+ x y) 2))

(defn square [x]
  (* x x))

(defn abs [n]
  (max n (- n)))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (println guess)
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

;; Exercise 1.6
;; Alyssa P. Hacker doesn’t see why if needs to be provided as a special form. “Why can’t I just define it as an ordinary function in terms of cond ?” she asks. Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if :
(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))
;; Eva demonstrates the program for Alyssa:

(comment
  > (new-if (= 2 3) 0 5)
  5

  > (new-if (= 1 1) 0 5)
  0)

;; Delighted, Alyssa uses new-if to rewrite the squareroot program:

(comment
  (defn sqrt-iter [guess x]
          (new-if (good-enough? guess x)
                  guess
                  (recur (improve guess x) x))))
;; What happens when Alyssa attempts to use this to compute square roots? Explain.
;; => In Clojure we have unsupported operation of `recur`; becasue due to new `new-if` it will not have tail recursion, but if you put plain `if` then compiler can infer the `recur` call as tail-recursion.

;; Exercise 1.9:
;; Each of the following two procedures defines a method for adding two positive integers in terms of the procedures inc , which increments its argument by 1, and dec , which decrements its argument by 1.

(comment
  (defn + [a b]
  (if (= a 0) b (inc (+ (dec a) b))))
;; => #'clj-sicp.chapter-01/+ [recursion]

(+ 4 5)
(defn + [a b]
  (if (= a 0) b (+ (dec a) (inc b))))
;; => #'clj-sicp.chapter-01/+ [iteration]
)

;; Exercise 1.10: The following procedure computes a mathematical function called Ackermann’s function.

(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1) (A x (- y 1)))))
;; What are the values of the following expressions?
(A 1 10)
;; => 1024
(A 2 4)
;; => 65536
(A 3 3)
;; => 65536

;; Consider the following procedures, where A is the procedure defined above:

;; (def (f n) (A 0 n))
(def f (fn [n] (A 0 n))) ;; -> 2n
;; (def (g n) (A 1 n))
(def g (fn [n] (A 1 n))) ;; -> 2^n
;; (def (h n) (A 2 n))
(def h (fn [n] (A 2 n))) ;; -> (h, n) -> (A 1 (h (- n 1))) -> 2^(h (- n 1))
;;(def (k n) (* 5 n n))
(def k (fn [n] (* 5 n n))) ;; -> 5n^2
;;Give concise mathematical definitions for the functions computed by the procedures f , g , and h for positive integer values of n. For example, (k n) computes 5n2.

;; Ex 1.11
;; A function f is defined by the rule that

;; f(n) = n if n < 3
;;      = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3.

;; Write a procedure that computes f by means of
;; a recursive process
;; an iterative process

(defn f-recur [n]
  (if (< n 3)
    n
    (+ (f-recur (- n 1))
       (* 2 (f-recur (- n 2)))
       (* 3 (f-recur (- n 3))))))
;; TODO
(defn f-iter [n]
  (let [helper (fn [a b c count]
                 (println "a " a  ", b " b ", c " c ", count " count)
                 (if (< count 3)
                   c
                   (recur (+ (+ a b)
                             (* 2 (- n 2))
                             (* 3 (- n 3))) (dec c))))]
    (helper n n)))


;;Exercise 1.41:
;;Define a function double that takes a function of one argument as argument and returns a function that applies the original function twice. For example, if inc is a function that adds 1 to its argument, then (double inc) should be a function that adds 2. What value is returned by

(defn double [f] (fn [x] (f (f x))))

(((double (double double)) inc) 5) ;; => 21

;; Exercise 1.42:
;; Let f and g be two one-argument functions.

;; The composition of f and g, denoted f∘g is defined to be the function

;; x → f(g(x))
;;Define a function compose that implements composition.

;; You should find that
(defn compose [f g] (fn [x] (f (g x))))
((compose square inc) 6)
;; => 49

;; Exercise 1.43:
;; If f is a numerical function and n is a positive integer, then we can form the nth repeated application of f , which is defined to be the function whose value at x is

;; f(f(…(f(x))…))
;; For example, if f is the function x→x+1 then the nth repeated application of f is the function x→x+n

(defn repeated [f n]
  (fn [x]
    (letfn [(h [x c]
              (if (= c 0)
                x
                (recur (f x) (dec c))))]
      (h x n))))

((repeated inc 4) 1)
;; => 5
((repeated square 2) 5)
;; => 625


