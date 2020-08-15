#lang simply-scheme


(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;`a-plus-abs-b' takes two arguments, a and b. If b is positive,
; the operator applied to a and b is `+', but if b is negative,
; the operator becomes `-' such that the absolute value of b is used

