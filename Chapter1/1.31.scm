#lang sicp

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