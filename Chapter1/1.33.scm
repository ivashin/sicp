#lang sicp

; Exercise 1.33
; You can obtain an even more general version of accumulate (Exercise 1.32) by introducing the notion
; of a filter on the terms to be combined. That is, combine only those terms derived from values in
; the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes
; the same arguments as accumulate, together with an additional predicate of one argument that
; specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following
; using filtered-accumulate:
; the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime?
; predicate already written)
; the product of all the positive integers less than n that are relatively prime to n (i.e., all
; positive integers i<n such that GCD(i,n)=1).

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