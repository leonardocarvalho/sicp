(define (cube x) (exp x 3))

(define (iter-sum a b map next)
  (define (iter a result)
    (if (float-gt? a b)
        result
        (iter (next a) (+ result (map a)))))
  (iter a 0))

(define (sum a b map next)
  (if (float-gt? a b)
      0
      (+ (map a) (sum (next a) b map next))))

(define (simpson-float f a b n)
  (let ((h (/ (- b a) n)))
    (define (next v) (+ v h))
    (define (map v)
      (cond ((or (float-eq? v a) (float-eq? v b)) (f v))
            ((even? (round->exact (/ (- v a) h))) (* 2 (f v)))
            (else (* 4 (f v)))))
    (* (/ h 3) (sum a b map next))))


(define (simpson-int f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k)
      (cond ((or (= k 0) (= k n)) (f (+ a (* h k))))
            ((even? k) (* 2 (f (+ a (* h k)))))
            (else (* 4 (f (+ a (* h k)))))))
    (* (/ h 3.0) (sum 0 n term inc))))
