#lang sicp

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (acc a)
    (cond ((> a b) null-value)
          ((filter a) (combiner (term a) (acc (next a))))
          (else (acc (next a)))))
  (acc a))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (square x) (* x x))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-of-squared-primes a b)  
  (filtered-accumulate + 0 square a inc b prime?))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prod-of-coprimes n)
  (define (id x) x)
  (define (coprime? x) (= (gcd n x) 1))
  (filtered-accumulate * 1.0 id 2 inc (- n 1) coprime?))