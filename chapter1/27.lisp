(define (odd? n)
  (= (remainder n 2) 1))

(define (square x) (* x x))

(define (slow-exp b ex)
  (if (= ex 0) 1 (* b (slow-exp b (- ex 1)))))

(define (exp b ex slow)
  (cond ((= ex 0) 1)
        (slow (slow-exp b ex))
        ((odd? ex) (* b (exp b (- ex 1) slow)))
        (else (square (exp b (/ ex 2) slow)))))

(define (reprove-prime a n slow)
  (not (= (remainder (exp a n slow) n) a)))

(define (fermat-test n slow)
  (define (inner-fermat a n)
    (cond ((= n a) "pass")
          ((reprove-prime a n slow) "reproved")
          (else (inner-fermat (+ a 1) n))))
  (inner-fermat 1 n))
