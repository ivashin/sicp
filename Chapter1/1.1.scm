#lang sicp

; Exercise 1.2
; Translate the following expression into prefix form
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3)))))
   (* 3 (- 6 2) (- 2 7)))

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