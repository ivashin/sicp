#lang sicp

; Exercise 1.45
; We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point of
; y -> x/y does not converge, and that this can be fixed by average damping. The same method works for
; finding cube roots as fixed points of the average-damped y -> x/y^2. Unfortunately, the process does
; not work for fourth roots -- a single average damp is not enough to make a fixed-point search for y
; x/y3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the
; average damp of y  x/y3) the fixed-point search does converge. Do some experiments to determine how
; many average damps are required to compute nth roots as a fixed-point search based upon repeated
; average damping of y -> x/y^n-1. Use this to implement a simple procedure for computing nth roots
; using fixed-point, average-damp, and the repeated procedure of exercise 1.43. Assume that any
; arithmetic operations you need are available as primitives.

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 1) f)
        ((even? n) (repeated (compose f f) (/ n 2)))
        (else (compose f (repeated f (- n 1))))))

(define (average x y)
  (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (nth-root x n)
  (define (log2 x)
    (/ (log x) (log 2)))
  (define target (lambda (y) (/ x (expt y (- n 1)))))
  (fixed-point ((repeated average-damp (floor (log2 n))) target)
               1.0))