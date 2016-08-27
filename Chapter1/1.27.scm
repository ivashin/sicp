#lang sicp

; Exercise 1.27
; Demonstrate that the Carmichael numbers listed in footnote 47 really do fool the Fermat test. That
; is, write a procedure that takes an integer n and tests whether a^n is congruent to a modulo n for
; every a<n, and try your procedure on the given Carmichael numbers.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square x) (* x x))

(define (carmichael n)
  (define (iter a)
    (cond ((= a n) true)
          ((= (expmod a n n) a) (iter (+ a 1)))
          (else false)))
  (iter 1))
