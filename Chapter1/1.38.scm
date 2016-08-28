#lang sicp

; Exercise 1.38
; In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which
; included a continued fraction expansion for e-2, where e is the base of the natural logarithms.
; In this fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...
; Write a program that uses your cont-frac procedure from Exercise 1.37 to approximate e, based on
; Euler’s expansion.

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) (if (= (remainder i 3) 1)
                           (* (/ (+ i 2) 3) 2)
                           1.0))
           10000)
