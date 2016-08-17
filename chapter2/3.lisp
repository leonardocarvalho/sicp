; ex 2.2
(define (make-point x y) (cons (* x 1.0) (* y 1.0)))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (equal-point? p1 p2)
  (and (= (x-point p1) (x-point p2))
       (= (y-point p1) (y-point p2))))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p q)
  (if (equal-point? p q)
      (error "same points don't define segment")
      (cons p q)))
(define (start-segment p) (car p))
(define (end-segment p) (cdr p))
(define (print-segment seg)
  (display "(")
  (print-point (start-segment seg))
  (display " -> ")
  (print-point (end-segment seg))
  (display ")"))

; end ex 2.2

(define (vertical-segment? seg)
  (= (x-point (start-segment seg)) (x-point (end-segment seg))))

(define (horizontal-segment? seg)
  (= (y-point (start-segment seg)) (y-point (end-segment seg))))

(define (length-segment seg)
  (sqrt (+
         (square (- (x-point (start-segment seg)) (x-point (end-segment seg))))
         (square (- (y-point (start-segment seg)) (y-point (end-segment seg)))))))

(define (angular-coeficient seg)
  (/ (- (y-point (start-segment seg)) (y-point (end-segment seg)))
     (- (x-point (start-segment seg)) (x-point (end-segment seg)))))

(define (perpendicular-segments? seg1 seg2)
  (cond ((vertical-segment? seg1) (horizontal-segment? seg2))
        ((horizontal-segment? seg1) (vertical-segment? seg2))
        ((vertical-segment? seg2) false)
        (else (= (* (angular-coeficient seg1) (angular-coeficient seg2)) -1))))

(define (possible-rec-base? seg1 seg2)
  (define (same-origin? seg1 seg2)
    (or (equal-point? (start-segment seg1) (start-segment seg2))
        (equal-point? (start-segment seg1) (end-segment seg2))
        (equal-point? (end-segment seg1) (start-segment seg2))
        (equal-point? (end-segment seg1) (end-segment seg2))))
  (and (same-origin? seg1 seg2) (perpendicular-segments? seg1 seg2)))


; rectangle 1

(define (make-rec seg1 seg2)
  (if (possible-rec-base? seg1 seg2) (cons seg1 seg2) (error "Not a rectangle")))

(define (rec-height rec)
  (max (length-segment (car rec)) (length-segment (cdr rec))))

(define (rec-width rec)
  (min (length-segment (car rec)) (length-segment (cdr rec))))

; end rectagle 1

; rectangle 2
;
;(define (make-rec p1 p2 p3 p4)
;  (define (possible-rec-base-points? q1 q2 q3)
;    (possible-rec-base? (make-segment q1 q2) (make-segment q1 q3)))
;  (define (is-rect q1 q2 q3 q4)
;    (and (= (x-point q4) (+ (x-point q3) (- (x-point q2) (x-point q1))))
;         (= (y-point q4) (+ (y-point q3) (- (y-point q2) (y-point q1))))))
;  (cond ((and (possible-rec-base-points? p1 p2 p3) (is-rect p1 p2 p3 p4))
;         (cons p1 (cons p2 (cons p4 p3))))
;        ((and (possible-rec-base-points? p1 p2 p4) (is-rect p1 p2 p4 p3))
;         (cons p1 (cons p2 (cons p3 p4))))
;        ((and (possible-rec-base-points? p1 p4 p3) (is-rect p1 p4 p3 p2))
;         (cons p1 (cons p4 (cons p2 p3))))
;        (else (error "Not a rectangle"))))
;
;(define (rec-height rec)
;  (max (length-segment (make-segment (car rec) (car (cdr rec))))
;       (length-segment (make-segment (car (cdr rec)) (car (cdr (cdr rec)))))))
;
;(define (rec-width rec)
;  (min (length-segment (make-segment (car rec) (car (cdr rec))))
;       (length-segment (make-segment (car (cdr rec)) (car (cdr (cdr rec)))))))
;
; end rectangle 2

(define (perimiter rec)
  (* 2 (+ (rec-height rec) (rec-width rec))))

(define (area rec)
  (* (rec-height rec) (rec-width rec)))

; tests

(define (test-segment)
  (let ((vert (make-segment (make-point 0 0) (make-point 0 4)))
        (hor (make-segment (make-point 0 0) (make-point 3 0)))
        (incl (make-segment (make-point 0 0) (make-point 1 1)))
        (perp (make-segment (make-point 0 0) (make-point 1 -1))))
    (assert (vertical-segment? vert) "vertical is said to be vertical")
    (assert (not (vertical-segment? hor)) "horizontal is said to be not vertical")
    (assert (not (vertical-segment? incl)) "inclined is said to be not vertical")
    (assert (horizontal-segment? hor) "horizontal is said to be horizontal")
    (assert (not (horizontal-segment? vert)) "vertical is said to be not horizontal")
    (assert (not (horizontal-segment? incl)) "inclined is said to be not horizontal")
    (assert (perpendicular-segment? hor vert) "horizontal is perpendicular to vertical")
    (assert (perpendicular-segments? vert hor) "vertical is perpendicular to horizontal")
    (assert (not (perpendicular-segments? vert incl)) "vertical not perpendicular to inclined")
    (assert (perpendicular-segments? incl perp) "inclined perpendicular segments")))


(define (test-rec)
  (let ((rec (make-rec (make-segment (make-point 0 0) (make-point 1 1))
                       (make-segment (make-point 0 0) (make-point 1 -1)))))
    (display "area:")
    (display (area rec))
    (newline)
    (display "perimiter:")
    (display (perimiter rec))))

(define (test-rec2)
  (let ((rec (make-rec (make-point 0 0) (make-point 1 1) (make-point 1 -1) (make-point 2 0))))
    (display "area:")
    (display (area rec))
    (newline)
    (display "perimiter:")
    (display (perimiter rec))))
