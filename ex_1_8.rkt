#lang simply-scheme

; Implement version of Newton's method for cube roots

(define (square x) (* x x))

(define (cube x) (square (cube x)))

(define (cbrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (cbrt-iter (improve guess x)
                 x)))

; guess = y, x = x (from exercise 1.8 in text)
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? old-guess guess)
  (< (abs (/ (- guess old-guess) guess)) 0.00000000001))

(define (cbrt x)
  (cbrt-iter 1.0 x))
