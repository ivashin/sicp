#lang sicp

; Exercise 1.7
; The good-enough? test used in computing square roots will not be very effective for finding the
; square roots of very small numbers. Also, in real computers, arithmetic operations are almost
; always performed with limited precision. This makes our test inadequate for very large numbers.
; Explain these statements, with examples showing how the test fails for small and large numbers.
; An alternative strategy for implementing good-enough? is to watch how guess changes from one
; iteration to the next and to stop when the change is a very small fraction of the guess.
; Design a square-root procedure that uses this kind of end test.
; Does this work better for small and large numbers?

(define (sqrt x)
  (define (average a b)
    (/ (+ a b) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess prev-guess)
    (< (abs (/ (- guess prev-guess) guess)) 0.0001))
  (define (sqrt-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (sqrt-iter (improve guess) guess)))
  (sqrt-iter 1.0 0.0))
