#lang simply-scheme

; CS61A homework week 2

; 1 (SICP 1.31(a), 1.32(a), 1.33, 1.40, 1.41, 1.43, 1.46)

;; 1.31(a)
; Helper functions
(define (inc x) (+ x 1))
(define (identity x) x)

(define (product term a next b)
  ; Analogous to (sum), but used to construct products
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial b)
  (product identity 1 inc b))
; (factorial 5) = 120

(define (pi-product a b)
  ; Computes the Wallis product approximation of pi
  (define (pi-term x)
    (/ (* (* 2.0 x) (+ 2.0 (* 2.0 x)))
       (* (+ 1.0 (* 2.0 x)) (+ 1.0 (* 2.0 x)))))
  (define (pi-next x)
    (+ x 1.0))
  (* 4.0 (product pi-term a pi-next b)))
; (pi-product 1 1000000) = 3.141593438981073


;; 1.32(a)
(define (accumulate combiner null-value term a next b)
  ; Generic (accumulate) function to construct (sum)/(product) or
  ; similar functions
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-accumulate a b)
  ; (sum) using (accumulate) function. identity and inc
  ; can be replaced with other values
  (accumulate + 0 identity a inc b))

(define (product-accumulate a b)
  ; (product) using (accumulate) function. identity and inc
  ; can be replaced with other values
  (accumulate * 1 identity a inc b))


;; 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  ; Even more general form of (accumulate) that allows for use of a filter
  (if (> a b)
      null-value
      (if (filter a)
      (combiner (term a)
                (filtered-accumulate combiner null-value term (next a) next b filter))
      (combiner null-value
                (filtered-accumulate combiner null-value term (next a) next b filter)))))

;(a)
(define (prime? n)
  ; prime predicate from Sedgewick's "Algorithms." Needed to test filter!
  (define (F n i) "helper"
    (cond ((< n (* i i)) #t)
          ((zero? (remainder n i)) #f)
          (else
           (F n (+ i 1)))))
  "primality test"
  (cond ((< n 2) #f)
        (else
         (F n 2))))

(define (sum-primes a b)
  ; Sum of squares of primes between a and b
  (define (square x) (* x x))
  (filtered-accumulate + 0 square a inc b prime?))

;(b)
(define (relatively-prime? a b)
  ; determines if a is relatively prime to b
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (modulo a b))))
  (= (gcd a b) 1))

(define (product-relative-primes-up-to-n n)
  ; Product of positive integers less than n that are relatively prime to n
  (define (relatively-prime-to-n? a)
    (relatively-prime? a n))
  (filtered-accumulate * 1 identity 1 inc n relatively-prime-to-n?))

;; 1.40
(define (fixed-point f first-guess)
  ; Approximate fixed-point of a function
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define tolerance 0.00001)

(define (average a b)
  (/ (+ a b) 2))


(define (deriv g)
  ; Approximates derivative of g
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; Actual answer
(define (cubic a b c)
  ; Constructs the cubic x^3 + ax^2 + bx + c
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

; (newtons-method (cubic a b c) 1) now approximates zeros of x^3 + ax^2 + bx + c


;; 1.41
(define (double f)
  ; Takes procedure of one argument as argument and returns a procedure that
  ; applies the original procedure twice
  (lambda (x) (f (f x))))

; (((double (double double)) inc) 5)
; 21

;; 1.43
(define (compose f g)
  ; Creates a composition of two functions
  (lambda (x) (f (g x))))

(define (repeated f n)
  ; Takes a procedure and positive integer n and returns the procedure that
  ; computes the nth repeated application of f
  (if (= n 1)
      (compose f identity)
      (compose (repeated f (- n 1)) f)))

;; 1.46

(define (iterative-improve good-enough? improve)
  (define (improve-guess guess)
    (if (good-enough? guess)
    guess
    (improve-guess (improve guess))))
  improve-guess)

(define (sqrt-iterative-improve x)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- (square guess) x)) 0.001))
   (lambda (guess)
     (average guess (/ x guess))))
  1))
