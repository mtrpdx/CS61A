#lang simply-scheme

; CS61A Homework week 1

; 1. See ex_1_6.rkt

; 2.
(define (squares sent)
  ; Takes sentence of numbers as input, output is sentence of squares of
  ; numbers
  (define (square x) (* x x))
  (if (empty? sent)
      '()
      (sentence
       (square (first sent)) (squares (bf sent)))))


; 3.
(define (switch sent)
  ; Takes sentence as input, output is sentence with "I" or "me" replaced
  ; by "you" and "you" is replaced by "me," except at the beginning of
  ; a sentence, in which case it is replaced by "I").
  (switch-sent sent #true))

(define (switch-sent sent start?)
  (if (empty? sent)
      '()
      (se (switch-wd (first sent) start?) (switch-sent (bf sent) #false))))

(define (switch-wd wd start?)
  (cond ((equal? wd 'i) 'you)
        ((equal? wd 'me) 'you)
        ((and start? (equal? wd 'you)) 'i)
        ((equal? wd 'you) 'me)
        (else wd)))


; 4
(define (ordered? sent-of-nums)
  ; Takes sentence of numbers as input and returns #true if the numbers
  ; are in ascending order and returns #false otherwise
  (if (> 2 (count sent-of-nums))
      #true
      (if (> (first sent-of-nums) (first (bf sent-of-nums)))
          #false
          (ordered? (bf sent-of-nums)))))


; 5
(define (ends-e sent)
  ; Takes a sentence as input, returns sentence with only the words that
  ; end in "e"
  (if (empty? sent)
      '()
      (if (equal? (last (first sent)) 'e)
          (se (first sent) (ends-e (bf sent)))
          (ends-e (bf sent)))))

; 6
(define (my-or a b c)
  (cond ((equal? a #t) #t)
        ((equal? b #t) #t)
        ((equal? c #t) #t)
        (else #f)))

(define (infinite-loop)
  ; Infinite loop to test if (a function is a special or ordinary form)
  (infinite-loop))

; Special form will output 1
;(or 1 (test-and-or))

; Ordinary form will result in infinite loop
;(my-or 1 (test-and-or))
