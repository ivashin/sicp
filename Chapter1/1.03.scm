#lang sicp

; Exercise 1.3
; Define a procedure that takes three numbers as arguments and returns the sum of the squares
; of the two larger numbers.

(define (sum-of-larger-squares x y z)
  (define (square x) (* x x))
  (define (sum-of-squares x y) (+ (square x) (square y)))
  (define (larger x y) (if (> x y) x y))

  (if (> x y)
      (sum-of-squares x (larger y z))
      (sum-of-squares y (larger x z))))
