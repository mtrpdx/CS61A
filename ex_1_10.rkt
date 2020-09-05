#lang simply-scheme

; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (A 1 10) -> 1024
;
; (A 2 4) -> 65536
;
; (A 3 3) -> 65536

; Computes 2n
(define (f n) (A 0 n))

; Computes 2^n
(define (g n) (A 1 n))

; Computes 2^(2^(2^(...)) where the entire previous term is raised as exponent to a 2.
; 2 appears a total of n times
(define (h n) (A 2 n))

; Computes 5n^2
(define (k n) (* 5 n n))

