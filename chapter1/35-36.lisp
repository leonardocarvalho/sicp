; fixed f(x) = x, f(x) = 1 + 1/x => x = 1 + 1/x => x2 - x - 1 = 0 (golden ratio is the positive root of the equation)
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden)
  (fixed-point (lambda (v) (+ 1 (/ 1 v))) 1.0))

(define (xtoxequal1000)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 4))
