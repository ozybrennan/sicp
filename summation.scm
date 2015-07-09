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