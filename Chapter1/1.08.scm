#lang sicp

; Exercise 1.8
; Newton's method for cube roots is based on the fact that if y is an approximation to the cube root
; of x, then a better approximation is given by the value (x/y^2 + 2y)/3.
; Use this formula to implement a cube-root procedure analogous to the square-root procedure.

(define (cube-root x)
  (define (square a) (* a a))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough? guess prev-guess)
    (< (abs (/ (- guess prev-guess) guess)) 0.0001))
  (define (iter guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (iter (improve guess) guess)))
  (iter 1.0 0.0))
