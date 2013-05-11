#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))
(define (split x y)
    (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split x y) painter (- n 1))))
          (x painter (y smaller smaller))))))

(define up-split (split below beside))
(define right-split (split beside below))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
    (let ((combine4 (square-of-four identity flip-vert
                                    identity flip-vert)))
      (combine4 painter)))

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1))))
          (let ((top-left (beside up up))
                (bottom-right (below right right))
                (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
    (let ((quarter (corner-split painter n)))
      (let ((half (beside (flip-horiz quarter) quarter)))
        (below (flip-vert half) half))))


(define (segments->painter segment-list)
    (lambda (frame)
      (for-each
       (lambda (segment)
         (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
       segment-list)))

(define (make-segment start end)
    (cons start end))
(define (start-segment segment)
    (car segment))
(define (end-segment segment)
    (cdr segment))
(define a-segment (make-segment (make-vect 1 2) (make-vect 2 3)))


(define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
    (car frame))
(define (edge1-frame frame)
    (cadr frame))
(define (edge2-frame frame)
    (cddar frame))

(define (frame-coord-map frame)
    (lambda (v)
      (add-vect
       (origin-frame frame)
       (add-vect (scale-vect (xcor-vect v)
                             (edge1-frame frame))
                 (scale-vect (ycor-vect v)
                             (edge2-frame frame))))))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1)
                  (xcor-vect v2))
               (+ (ycor-vect v1)
                  (ycor-vect v2))))
(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1)
                  (xcor-vect v2))
               (- (ycor-vect v1)
                  (ycor-vect v2))))

(define (scale-vect s v)
    (make-vect (* s (xcor-vect v))
               (* s *ycor-vect v)))

(define (make-vect x y)
    (cons x y))
(define (xcor-vect v)
    (car v))
(define (ycor-vect v)
    (cdr v))