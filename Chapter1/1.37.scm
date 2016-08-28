#lang sicp

; Exercise 1.37
; Suppose that n and d are procedures of one argument (the term index i) that return the Ni and Di of
; the terms of the continued fraction. Define a procedure cont-frac such that evaluating
; (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure
; by approximating 1/φ using
; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;           k)
; for successive values of k. How large must you make k in order to get an approximation that is
; accurate to 4 decimal places?
; If your cont-frac procedure generates a recursive process, write one that generates an iterative
; process. If it generates an iterative process, write one that generates a recursive process.

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 0))

(define (cont-frac-iter n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1) (/ (n k) (+ (d k) result)))))
  (iter 0 0.0))
