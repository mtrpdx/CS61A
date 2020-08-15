#lang simply-scheme

; implement new version of good-enough? to watch how `guess' changes
; from one iteration to the next and stop when change is a very small
; fraction of the guess

(define (square x) (* x x))

; new procedure
(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x)
                 x)))

;; old procedure
;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x)
;                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

; new procedure
(define (good-enough? old-guess guess)
  (< (abs (/ (- guess old-guess) guess)) 0.00000000001))

;; old procedure
;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))
