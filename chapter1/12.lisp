(define (pascal line col)
  (cond ((or (> col line) (< col 1) (< line 1)) 0)
        ((or (= col 1) (= col line)) 1)
        (else (+ (pascal (- line 1) col)
                 (pascal (- line 1) (- col 1))))))
