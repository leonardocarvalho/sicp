(define (main-f fn1 fn2 fn3)
  (+ fn1
     (* 2 fn2)
     (* 3 fn3)))

(define (f1 n)
  (if (< n 3)
      n
      (main-f (f1 (- n 1))
              (f1 (- n 2))
              (f1 (- n 3)))))

(define (f2 n)
  (define (f-iter fn1 fn2 fn3 counter)
    (if (= counter 0)
        (main-f fn1 fn2 fn3)
        (f-iter (main-f fn1 fn2 fn3) fn1 fn2 (- counter 1))))

  (if (< n 3)
      n
      (f-iter 2 1 0 (- n 3))))
