#lang sicp
; We need to import function for generating random numbers
(#%require (only math/base random-natural))


; Exercise 1.24
; Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the Fermat method), and
; test each of the 12 primes you found in that exercise. Since the Fermat test has (log n) growth, how
; would you expect the time to test primes near 1,000,000 to compare with the time needed to test
; primes near 1000? Do your data bear this out? Can you explain any discrepancy you find?

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-natural (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square x) (* x x))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))
      false))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  true)


(define (search-for-primes n)  
  (define (iter n found)
    (if (< found 3)
        (cond ((= (remainder n 2) 0) (iter (+ n 1) found))
              ((timed-prime-test n) (iter (+ n 1) (+ found 1)))
              (else (iter (+ n 1) found)))))
  (iter n 0))
