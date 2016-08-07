(define (even? n) (= (remainder n 2) 0))

(define (fast-exp-rec b n)
  (cond ((= n 0) 1)
        ((even? n) (fast-exp-rec (* b b) (/ n 2)))
        (else (* b (fast-exp-rec b (- n 1))))))


(define (fast-exp b n)
  (define (fast-exp-iter product exponent base)
    (cond ((= exponent 0) product)
          ((even? exponent) (fast-exp-iter product (/ exponent 2) (* base base)))
          (else (fast-exp-iter (* product base) (- exponent 1) base))))
  (fast-exp-iter 1 n b))
