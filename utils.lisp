(define (id x) x)
(define (not-eq? a b) (not (= a b)))
(define (abs x) (if (> x 0) x (- x)))
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (not (even? n)))
(define (inc n) (+ n 1))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2.0))

; FLOAT FUNCS
(define DEF-EPS 0.000001)
(define (pick-eps eps) (if (default-object? eps) DEF-EPS eps))

(define (float-eq? a b #!optional eps)
  (< (abs (- b a)) (pick-eps eps)))

(define (float-gt? a b #!optional eps)
  (> (- a b) (pick-eps eps)))

(define (float-lt? a b #!optional eps)
  (> (- b a) (pick-eps eps)))
; END FLOAT

; RATIONAL
(define (make-rat n d)
  (define (simplify rat)
    (let ((g (gcd n d)))
      (cons (/ (numer rat) g) (/ (denom rat) g))))
  (define (norm-signal rat)
    (if (< (* (numer rat) (denom rat)) 0)
        (cons (- (abs (numer rat))) (abs (denom rat)))
        (cons (abs (numer rat)) (abs (denom rat)))))
  (if (= d 0)
      (error "denominator can't be 0")
      (simplify (norm-signal (cons n d)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))
; END RATIONAL

; TEST HELPERS
(define (assert condition message)
  (display message)
  (display ": ")
  (if condition (display "ok") (display "fail"))
  (newline))
; END TEST HELPERS

(define (gcd a b)
  (let ((a (abs a)) (b (abs b)))
    (if (= b 0)
        a
        (gcd b (remainder a b)))))

(define (exp b n)
  (define (fast-exp-iter product exponent base)
    (cond ((= exponent 0) product)
          ((even? exponent) (fast-exp-iter product (/ exponent 2) (* base base)))
          (else (fast-exp-iter (* product base) (- exponent 1) base))))
  (fast-exp-iter 1 n b))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

  (if (> n 1) (= n (smallest-divisor n)) false))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
