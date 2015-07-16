(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (simp-term k)
    (cond ((or (= 0 k) (= 1 k)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (sum simp-term 0 inc n) (/ h 3)))

(define (even? x)
  (= (remainder x 2) 0))

(define (inc x)
  (+ 1 x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
      (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
      (product term (next a) next b))))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (approximate-pi n)
  (* 4
  (/ (product top-term 1 inc n)
     (product bottom-term 1 inc n))))

(define (top-term x)
  (if (= (remainder x 2) 0)
      (+ 2 x)
      (+ 1 x)))

(define (bottom-term x)
  (if (= (remainder x 2) 0)
    (+ 1 x)
    (+ 2 x)))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
      (accumulate combiner null-value term (next a) next b))))

(define (sum-accumulate term a next b)
  (accumulate + 0 term a next b))

(define (product-accumulate term a next b)
  (accumulate * 1 term a next b))

(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a b) (combiner (term a)
          (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (combiner null-value
          (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define (sum-of-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next a)
  (if (= 2 a)
  3
  (+ 2 a)))

(define (prime? n nil)
  (= n (smallest-divisor n)))

(define (relative-prime-product n)
  (filtered-accumulate relative-prime? * 1 identity 1 inc n))

(define (relative-prime? a b) (= (gcd a b) 1))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
