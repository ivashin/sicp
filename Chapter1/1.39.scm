#lang sicp

; Exercise 1.39
; Define a procedure (tan-cf x k) that computes an approximation to the tangent function based on
; Lambert’s formula. k specifies the number of terms to compute, as in Exercise 1.37.

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 0))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 0) x (- (* x x))))
             (lambda (i) (+ (* i 2) 1))
             k))