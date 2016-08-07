(define (accumulate-rec bottom top combiner null-value term next)
  (if (float-gt? bottom top)
      null-value
      (combiner (term bottom)
                (accumulate-rec (next bottom)
                                top
                                combiner
                                null-value
                                term
                                next))))

(define (accumulate-iter bottom top combiner null-value term next)
  (define (step current acc)
    (if (float-gt? current top)
        acc
        (step (next current) (combiner (term current) acc))))
  (step bottom null-value))


(define (filtered-accumulate bottom top combiner null-value term next filter)
  (accumulate-iter bottom
                   top
                   combiner
                   null-value
                   (lambda (v) (if (filter v) (term v) null-value))
                   next))


(define (sum bottom top func next)
  (accumulate-iter bottom top + 0 func next))

(define (product bottom top func next)
  (accumulate-iter bottom top * 1 func next))

(define (factorial n)
  (product 1 n id inc))

(define (pi-est2 n-terms)
  (define (est-term n)
    (let ((num (* 2.0 (+ 1 (floor (/ n 2)))))
          (den (+ 1 (* 2.0 (floor (/ (+ 1 n) 2))))))
      (/ num den)))
  (* 4 (product 1 n-terms est-term inc)))


(define (pi-est n-terms)
  (/ (* 8 (product 4.0
                   (+ 4.0 n-terms)
                   (lambda (v) (square (/ v (- v 1))))
                   (lambda (x) (+ x 2))))
     (+ 3 n-terms)))

(define (sum-square-of-primes bottom top)
  (filtered-accumulate bottom top + 0 square inc prime?))

(define (product-coprimes-less-than n)
  (filtered-accumulate 1 n * 1 id inc (lambda (m) (= (gcd n m) 1))))
