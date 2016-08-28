#lang sicp

; Exercise 1.18
; Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process
; for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number
; of steps.

(define (fast-mult-iter a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (define (even? x)
    (= (remainder x 2) 0))
  (define (iter sum a b)
    (cond ((= a 0) sum)
          ((even? a) (iter sum (halve a) (double b)))
          (else (iter (+ sum b) (- a 1) b))))

  (iter 0 a b))
