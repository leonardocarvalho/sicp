(define (even? n) (= (remainder n 2) 0))
(define (double n) (* 2 n))
(define (halve n)
  (if (even? n)
      (/ n 2)
      -1))


(define (fast-mul-base n m abs-mul)
  (define (negative-mul? m n)
    (or (and (< m 0) (> n 0))
        (and (> m 0) (< n 0))))

  (cond ((or (= n 0) (= m 0)) 0)
        ((negative-mul? m n) (- (abs-mul (abs m) (abs n))))
        (else (abs-mul (abs m) (abs n)))))


(define (fast-mul-rec n m)
  (define (fast-mul-pos n m)
    (cond ((= m 1) n)
          ((even? m) (fast-mul-pos (double n) (halve m)))
          (else (+ n (fast-mul-pos n (- m 1))))))
  (fast-mul-base n m fast-mul-pos))


(define (fast-mul-iter n m)
  (define (fast-mul-pos-iter sum n m)
    (cond ((= m 0) sum)
          ((even? m) (fast-mul-pos-iter sum (double n) (halve m)))
          (else (fast-mul-pos-iter (+ sum n) n (- m 1)))))

  (define (fast-mul-pos n m) (fast-mul-pos-iter 0 n m))

  (fast-mul-base n m fast-mul-pos))
