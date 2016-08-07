(define (special-check computed-exp m)
  (cond ((and (not-eq? computed-exp (- m 1))
              (not-eq? computed-exp 1)
              (= (remainder (square computed-exp) m) 1)) 0)
        (else (remainder (square computed-exp) m))))


(define (expmod-mod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (special-check (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (miller-rabin-test n)
  (define (do-test a n)
    (not (= (expmod-mod a (- n 1) n) 1)))

  (define (loop-test a n)
    (cond ((= n a) "pass")
          ((do-test a n) "reproved")
          (else (loop-test (+ a 1) n))))
  (loop-test 1 n))
