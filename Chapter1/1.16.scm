#lang sicp

; Exercise 1.16
; Design a procedure that evolves an iterative exponentiation process that uses successive
; squaring and uses a logarithmic number of steps, as does fast-expt.

(define (fast-exp b n)
  (define (square x) (* x x))
  (define (even? x)
    (= (remainder x 2) 0))
  (define (iter prod base pow)
    (cond ((= pow 0) prod)
          ((even? pow) (iter prod (square base) (/ pow 2)))
          (else (iter (* prod base) base (- pow 1)))))

    (iter 1 b n))
