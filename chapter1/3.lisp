(define (square x) (* x x))

(define (sum-square-two-greatest x y z)
  (- (+ (square x) (square y) (square z))
     (square (min x y z))))

(define (sum-square-two-greatest2 x y z)
  (cond
   ((and (< x y) (< x z)) (+ (square y) (square z)))
   ((and (< y x) (< y z)) (+ (square x) (square z)))
   ((and (< z y) (< z x)) (+ (square x) (square y)))))
