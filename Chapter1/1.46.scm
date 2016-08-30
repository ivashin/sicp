#lang sicp

; Exercise 1.46
; Several of the numerical methods described in this chapter are instances of an extremely general
; computational strategy known as iterative improvement. Iterative improvement says that, to compute
; something, we start with an initial guess for the answer, test if the guess is good enough, and
; otherwise improve the guess and continue the process using the improved guess as the new guess.
; Write a procedure iterative-improve that takes two procedures as arguments: a method for telling
; whether a guess is good enough and a method for improving a guess. Iterative-improve should return
; as its value a procedure that takes a guess as argument and keeps improving the guess until it is
; good enough.
; Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms
; of iterative-improve.

(define (iterative-improve good-enough? improve)
  (define improve-guess (lambda (x)
    (if (good-enough? x)
        x
        (improve-guess (improve x)))))
  improve-guess)

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define tolerance 0.00001)
(define (sqrt x)
  (let ((good-enough? (lambda (y) (< (abs (- (square y) x)) tolerance)))
        (improve (lambda (y) (average y (/ x y)))))
    ((iterative-improve good-enough? improve) 1.0)))

(define (fixed-point f first-guess)
  (let ((good-enough? (lambda (y) (< (abs (- y (f y))) tolerance)))
        (improve (lambda (y) (f y))))
    ((iterative-improve good-enough? improve) first-guess)))