#lang sicp

; Exercise 1.31
; The sum procedure is only the simplest of a vast number of similar abstractions that can be captured
; as higher-order procedures. Write an analogous procedure called product that returns the product of
; the values of a function at points over a given range. Show how to define factorial in terms of
; product. Also use product to compute approximations to π using the formula
; π/4=(2⋅4⋅4⋅6⋅6⋅8⋅⋯)/(3⋅3⋅5⋅5⋅7⋅7⋅⋯).
; If your product procedure generates a recursive process, write one that generates an iterative
; process. If it generates an iterative process, write one that generates a recursive process.

(define (product f a b next)
  (if (> a b)
      1
      (* (f a) (product f (next a) b next))))

(define (factorial n)
  (define (id x) x)
  (product id 2 n inc))

(define (pi-prod)
  (define (square x) (* x x))
  (define (next a) (+ a 2))
  (define (f a)
    (/ (* a (+ a 2)) (square (+ a 1))))
  (* 4.0
     (product f 2 1000 next)))

(define (prod-iter f a b next)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (f a)))))
  (iter a 1.0))