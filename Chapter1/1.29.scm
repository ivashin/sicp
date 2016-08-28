#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (next x) (+ x (* 2 h)))
  (* h
     (/ 1.0 3)
     (+ (f a)
        (f b)
        (* 2 (sum f (next a) next (- b h)))
        (* 4 (sum f (+ a h) next b)))))
