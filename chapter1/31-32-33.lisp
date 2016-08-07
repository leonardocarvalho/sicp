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

(define (sum bottom top func next)
  (accumulate-rec bottom top + 0 func next))

(define (product bottom top func next)
  (accumulate-rec bottom top * 1 func next))

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
