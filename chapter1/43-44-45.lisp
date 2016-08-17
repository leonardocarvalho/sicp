(define (repeated f n)
  (define (step f n x)
    (if (= n 0)
        x
        (f (step f (- n 1) x))))
  (lambda (x) (step f n x)))

(define (repeated2 f n)
  (if (= n 1)
      f
      (compose f (repeated2 f (- n 1)))))

(define dx 0.00001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (n-fold-smooth f n)
  (lambda (x) (((repeated smooth n) f) x)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (damp-n-times f n)
  ((repeated average-damp n) f))
