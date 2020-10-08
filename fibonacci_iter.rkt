#lang simply-scheme

; From SICP 1.2.2
; Computes Fibonacci sequence iteratively; example of linear iteration

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
