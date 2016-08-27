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


; Exercise 1.17
; The exponentiation algorithms in this section are based on performing exponentiation by means of
; repeated multiplication. In a similar way, one can perform integer multiplication by means of
; repeated addition. The following multiplication procedure (in which it is assumed that our
; language can only add, not multiply) is analogous to the expt procedure:
;
;(define (* a b)
;  (if (= b 0)
;      0
;      (+ a (* a (- b 1)))))
;
; This algorithm takes a number of steps that is linear in b. Now suppose we include, together with
; addition, operations double, which doubles an integer, and halve, which divides an (even) integer
; by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic
; number of steps.
(define (fast-mult a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (define (even? x)
    (= (remainder x 2) 0))

  (cond ((= a 1) b)
        ((even? a) (fast-mult (halve a) (double b)))
        (else (+ b (fast-mult (- a 1) b)))))


; Exercise 1.18
; Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process
; for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number
; of steps.
(define (fast-mult-iter a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (define (even? x)
    (= (remainder x 2) 0))
  (define (iter sum a b)
    (cond ((= a 0) sum)
          ((even? a) (iter sum (halve a) (double b)))
          (else (iter (+ sum b) (- a 1) b))))

  (iter 0 a b))


; Exercise 1.19
; There is a clever algorithm for computing the Fibonacci numbers in a logarithmic number of steps.
; Recall the transformation of the state variables aa and bb in the fib-iter process of 1.2.2:
; a←a+ba←a+b and b←ab←a. Call this transformation T, and observe that applying T over and over again
; n times, starting with 1 and 0, produces the pair Fib(n+1) and Fib(n). In other words, the Fibonacci
; numbers are produced by applying T^n, the nth power of the transformation TT, starting with the
; pair (1, 0). Now consider T to be the special case of p=0 and q=1 in a family of transformations
; T_pq, where T_pq transforms the pair (a,b) according to a←bq+aq+ap and b←bp+aq. Show that if we
; apply such a transformation TpqTpq twice, the effect is the same as using a single transformation
; T_p′q′ of the same form, and compute p′ and q′ in terms of p and q. This gives us an explicit way
; to square these transformations, and thus we can compute TnTn using successive squaring, as in the
; fast-expt procedure. Put this all together to complete the following procedure, which runs in a
; logarithmic number of steps.
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))      ; compute p'
                   (+ (* 2 p q) (* q q))    ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
