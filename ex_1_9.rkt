#lang simply-scheme

; inc and dec increment and decrement the argument, respectively
; first procedure:
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

; The procedure runs as follows (using the substitution model)
; (+ 4 5)
; => (inc (+ (dec 4) 5))
; => (inc (inc (+ (dec 3) 5)))
; => (inc (inc (inc (+ (dec 2) 5))))
; => (inc (inc (inc (inc (+ (dec 1) 5)))))
; => (inc (inc (inc (inc (+ 0 5)))))
; => (inc (inc (inc (inc (5)))))
; => (inc (inc (inc 6)))
; => (inc (inc 7))
; => (inc 8)
; => 9
;
; So, this procedure appears to be linear recursive

; Second procedure:
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

; This procedure runs as follows:
; (+ 4 5)
; => (+ (dec 4) (inc 5))))
; => (+ 3 6)
; => (+ 2 7)
; => (+ 1 8)
; => (+ 0 9)
; => 9
;
; This procedure is linear iterative
