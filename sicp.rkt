(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (power b n)
  (define (iter a b n)
    (cond ((= n 0) a) 
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (accumulate combiner null-value func a next b)
  (define (iter a count)
    (if (> a b)
        count
        (iter (next a) (combiner count (func a)))))
  (iter a null-value))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (inc x)
  (+ x 1))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (define (next x)
    (if (= x 2)
        3
        (+ x 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (identity x)
  x)

(define (filtered-accumulate filter combiner null-value func a next b)
  (define (iter a count)
    (if (> a b) 
        count
        (if (filter a)
            (iter (next a) (combiner count (func a)))
            (iter (next a) (combiner count null-value)))))
  (iter a null-value))

(define (cont-frac n d k)
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i) 
                 (+ (d i) result))
              (- i 1))))
  (iter 0 k))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter result n)
    (if (= n 0)
        result
        (iter (compose f result) (- n 1))))
  (iter identity n))

(define (iterative-improve improve test)
  (lambda (x)
    (if (test x (improve x))
        (improve x)
        ((iterative-improve improve test) (improve x)))))

(define (close-enough? v1 v2)
  (> tolerance (/ (abs (- v1 v2)) v2)))

(define tolerance 1e-06)

(define (make-point x y) (cons x y)) 
(define (x-point p) (car p)) 
(define (y-point p) (cdr p)) 
(define (print-point p) 
  (newline) 
  (display "(") 
  (display (x-point p)) 
  (display ",") 
  (display (y-point p)) 
  (display ")")) 

;; Segment 
(define (make-segment start-point end-point) 
  (cons start-point end-point)) 
(define (start-segment segment) (car segment)) 
(define (end-segment segment) (cdr segment)) 

(define (rect-area rectangle)
  (* (find-height rectangle) (find-width rectangle)))
(define (rect-perimeter rectangle)
  (+ (* 2 (find-height rectangle)) (* 2 (find-width rectangle)) ))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (sub-interval x y) 
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y))))) 


(define (make-interval a b) (cons a b)) 
(define (upper-bound interval) (max (car interval) (cdr interval))) 
(define (lower-bound interval) (min (car interval) (cdr interval))) 


(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent interval)
  (* 100 (/ (width interval) (center interval))))

(define (make-center-percent center tolerance)
  (make-center-width center (* center (/ tolerance 100)) ))

(define (xor a b)
  (cond ((and a (not b)) #t)
        ((and b (not a)) #t)
        (else #f)))

(define (same-sign? a b)
  (cond ((and (< a 0) (< b 0)) #t)
        ((and (> a 0) (> b 0)) #t)
        ((xor (<= a 0) (<= b 0)) #f)
        (else #t)))

(define (mul-interval1 x y)
  (define (int-positive? interval)
    (cond ((and (>= (upper-bound interval) 0)
                (>= (lower-bound interval) 0))
           1)
          ((and (< (upper-bound interval) 0)
                (< (lower-bound interval) 0))
           -1)
          (else 0)))
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y))
        (sign-x (int-positive? x))
        (sign-y (int-positive? y))
        )
    (cond ((> sign-x 0) (cond ((> sign-y 0) 
                               (make-interval (* a c) (* b d)))
                              ((< sign-y 0) 
                               (make-interval (* b c) (* a d)))
                              (else 
                               (make-interval (* b c) (* b d)))))
          ((< sign-x 0) (cond ((> sign-y 0) 
                               (make-interval (* a d) (* b c)))
                              ((< sign-y 0)
                               (make-interval (* b d) (* a c)))
                              (else  
                               (make-interval (* a d) (* a c))))) 
          (else 
           (cond ((> sign-y 0)
                  (make-interval (* a d) (* b d)))
                 ((< sign-y 0)
                  (make-interval (* b c) (* a c)))
                 (else
                  (make-interval (min (* a d) (* b c)) 
                                 (max (* a c) (* b d))))))))) 

(define (ensure-mult-works aH aL bH bL) 
  (let ((a (make-interval aL aH)) 
        (b (make-interval bL bH))) 
    (if (eql-interval? (mul-interval a b) 
                       (mul-interval1 a b)) 
        true 
        (error "new mult returns different value!"  
               a  
               b  
               (mul-interval a b) 
               (mul-interval1 a b))))) 

(define (eql-interval? a b) 
  (and (= (upper-bound a) (upper-bound b)) 
       (= (lower-bound a) (lower-bound b)))) 

(define (reverse items)
  (define (iter result item)
    (if (null? item)
        result
        (iter (cons (car item) result) (cdr item))))
  (iter '() items))

(define (last-pair item)
  (define (iter result item)
    (if (null? item)
        result
        (iter item (cdr item))))
  (iter item item))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (+ (total-branch (left-branch mobile))
     (total-branch (right-branch mobile))))

(define (total-branch branch)
  (cond ((not (pair? (branch-structure branch))) 
         (branch-structure branch))
        (else (total-weight (branch-structure branch)))))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init sequence) 
  (define nil '()) 
  (if (null? (car sequence)) 
      nil 
      (cons (accumulate op init (map car sequence)) 
            (accumulate-n op init (map cdr sequence))))) 

(define enumerate-tree fringe)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (equal? a b)
  (cond ((not (or (pair? a) (pair? b))) 
         (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b))
                                        (equal? (cdr a) (cdr b))))
        (else #f)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product 
          (make-product (exponent exp) 
                        (make-exponentiation (base exp) 
                                             (make-sum (exponent exp) -1)))
          (deriv (base exp) var))) 
        (else
         (error "unknown expression type -- DERIV" exp))))
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b n)
  (cond ((=number? n 1) b)
        ((=number? n 0) 1)
        ((and (number? b) (number? n)) (power b n))
        (else (list '** b n))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (accumulate make-sum 0 (cddr s)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (accumulate make-product 0 (cddr p)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2) 
  (if (null? set1) 
      set2 
      (union-set (cdr set1) (adjoin-set (car set1) set2)))) 

(define (append! x y)
    (set-cdr! (last-pair x) y)
    x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))