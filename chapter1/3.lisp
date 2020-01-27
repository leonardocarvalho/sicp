(define (square x) (* x x))

(define (sum-square-two-greatest x y z)
  (- (+ (square x) (square y) (square z))
     (square (min x y z))))

(define (sum-square-two-greatest2 x y z)
  (cond
   ((<= x y z) (+ (square y) (square z)))
   ((<= y x z) (+ (square x) (square z)))
   ((<= z y x) (+ (square x) (square y)))))
