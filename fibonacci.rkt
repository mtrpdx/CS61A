#lang simply-scheme

; From SICP 1.2.2
; Computes Fibonacci sequence; example of tree recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
