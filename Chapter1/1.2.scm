#lang sicp

; Exercise 1.11
; A function f is defined by the rule that f(n) = n if n<3 and
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3.
; Write a procedure that computes f by means of a recursive process.
; Write a procedure that computes f by means of an iterative process.
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1)) (f-recursive (- n 2)) (f-recursive (- n 3)))))

(define (f-iterative n)
  (define (iter f-1 f-2 f-3 count)
    (if (= count 0)
        f-3
        (iter (+ f-1 f-2 f-3) f-1 f-2 (- count 1))))

  (iter 2 1 0 n))


; Exercise 1.12
; The following pattern of numbers is called Pascal's triangle.
; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum
; of the two numbers above it.
; Write a procedure that computes elements of Pascal's triangle by means of a recursive process.
(define (pascal row col)
  (if (or (= col 0) (= row col))
      1
      (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1)))))


; Exercise 1.16
; Design a procedure that evolves an iterative exponentiation process that uses successive
; squaring and uses a logarithmic number of steps, as does fast-expt.
(define (fast-exp b n)
  (define (square x) (* x x))
  (define (even? x)
    (= (remainder x 2) 0))
  (define (iter prod base pow)
    (cond ((= pow 0) prod)
          ((even? pow) (iter prod (square base) (/ pow 2)))
          (else (iter (* prod base) base (- pow 1)))))

    (iter 1 b n))