#lang simply-scheme

; Code from CS61A (Spr. 2011) lecture 3

(define pi 3.141592654)

(define (square-area r) (* r r))

(define (circle-area r) (* pi r r ))

(define (sphere-area r) (* 4 pi r r))

(define (hexagon-area r) (* (sqrt 3) 1.5 r r))

;;;;;;;;;;;;;;;;;;;

(define (area shape r) (* shape r r))

(define square 1)
(define circle pi)
(define sphere (* 4 pi))
(define hexagon (* (sqrt 3) 1.5))

;;;;;;;;;;;;;;;;;

(define (sumsquare a b)
  (if (> a b)
      0
      (+ (* a a) (sumsquare (+ a 1) b)) ))

(define (sumcube a b)
  (if (> a b)
      0
      (+ (* a a a) (sumcube (+ a 1) b)) ))

; gives unboud identifier error
;(define (sum FN a b)
;  (if (> a b)
;      0
;      (+ (FN A) (sum FN (+ a 1) b))))

;;;;;;;;;;;;;;;;

(define (evens nums)
  (cond ((empty? nums) '())
        ((= (remainder (first nums) 2) 0)
         (se (first nums) (evens (bf nums))) )
        (else (evens (bf nums))) ))

(define (ewords sent)
  (cond ((empty? sent) '())
        ((member? 'e (first sent))
          (se (first sent) (ewords (bf sent))) )
        (else (ewords (bf sent))) ))

(define (pronouns sent)
  (cond ((empty? sent) '())
        ((member? (first sent) '(I i me you he she it him her we us they them))
         (se (first sent) (pronouns (bf sent))) )
        (else (pronouns (bf sent))) ))

(define (keep PRED sent)
  (cond ((empty? sent) '())
        ((PRED (first sent))
         (se (first sent) (keep PRED (bf sent))))
        (else (keep PRED (bf sent)))))

(define (eword? wd) (member? 'e wd))
