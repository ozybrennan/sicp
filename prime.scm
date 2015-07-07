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

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (miller-fast-prime? n 3)
    (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes min max)
  (if (= 1 (remainder min 2))
    (find-prime min max)
    (find-prime (+ 1 min) max)))

(define (find-prime min max)
  (if (< min max)
    (recursive-call-prime min max)))

(define (recursive-call-prime min max)
  (timed-prime-test min)
  (find-prime (+ 2 min) max))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (carmichael-test n)
  (iterative-carmichael-test n 0))

(define (iterative-carmichael-test n i)
  (cond ((= i n) true)
        ((= (expmod i n n) i) (iterative-carmichael-test n (+ i 1)))
        (else false)))

(define (expmod-miller base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-miller a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))
