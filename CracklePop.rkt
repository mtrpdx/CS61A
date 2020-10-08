#lang simply-scheme

;;;;
(define (cracklepop)
  ; Prints the numbers from 1 to 100 inclusive. If a number is
  ; divisible by 3, prints 'crackle. if a number is divisible by 5,
  ; prints 'pop. if a number is divisible by both, prints 'cracklepop
  (define i 1)
  (define n 100)
  (define (loop i)
    (if (> i n)
      '()
      (cond ((and (equal? (remainder i 3) 0) (equal? (remainder i 5) 0))
             (se 'cracklepop (loop (+ i 1))))
            ((equal? (remainder i 3) 0)
             (se 'crackle (loop (+ i 1))))
            ((equal? (remainder i 5) 0)
             (se 'pop (loop (+ i 1))))
            (else (se i (loop (+ i 1)))))))

  (loop i))
