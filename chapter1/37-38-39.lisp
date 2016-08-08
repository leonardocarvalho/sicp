(define (cont-frac-iter n-gen d-gen k)
  (define (step k acc)
    (if (= k 0)
        acc
        (step (- k 1) (/ (n-gen k) (+ (d-gen k) acc)))))
  (step k 0))

(define (cont-frac-rec n-gen d-gen k)
  (define (step depth)
    (let ((k (- k depth)))
      (let ((n (n-gen k))
            (d (d-gen k)))
        (if (= depth 1)
            (/ n d)
            (/ n (+ d (step (- depth 1))))))))
  (step k))


(define (golden-iter depth)
  (/ 1.0 (cont-frac-iter (lambda (k) 1.0)
                         (lambda (k) 1.0)
                         depth)))


(define (golden-rec depth)
  (/ 1.0 (cont-frac-rec (lambda (k) 1.0)
                        (lambda (k) 1.0)
                        depth)))

(define (e-cf k)
  (define (d-gen k)
    (if (= (remainder k 3) 2)
        (* (/ (+ k 1) 3) 2)
        1))
  (+ 2 (cont-frac-iter (lambda (k) 1.0)
                       d-gen
                       k)))

(define (tan-cf x n)
  (cont-frac-iter (lambda (k)
                    (let ((res (exp x k)))
                      (if (= k 1) res (- res))))
                  (lambda (k) (- (* 2 k) 1))
                  n))

; 11 goes right to 4th decimal place
