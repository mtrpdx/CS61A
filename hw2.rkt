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
