;;
;; Scheme and the Art of Programming
;; ---------------------------------
;;    - George Springer & Daniel P. Friedman
;;    - Chapter 03 Exercises
;;



;;
;; Example: Program 3.4 p. 77
;; --------------------------
;;
(define harmonic-sum
  (lambda (n)
    (cond
      ((zero? n) 0)
      (else (+ (/ 1 n) (harmonic-sum (- n 1)))))))


;;
;; Exercise 3.01 p. 81: sum
;; ------------------------
;; Define a proc. sum that finds the sum of the components of an n-tuple.
;;
(define sum
  (lambda (ls)
    (cond
      ((null? ls) 0)
      (else (+ (car ls) (sum (cdr ls)))))))


;;
;; Exercise 3.02a p. 81: pairwise-sum
;; ----------------------------------
;; 1. Define a proc pairwise-sum that takes two n-tuples of same length
;;    ntpl-1 & ntpl-2
;; 3. Produces a new n-tuple whose components are the sum of ntpl-1 & ntpl-2
;;
(define pairwise-sum
  (lambda (ntpl-1 ntpl-2)
    (cond
      ((or (null? ntpl-1) (null? ntpl-2)) '())
      (else (cons (+ (car ntpl-1) (car ntpl-2)) (pairwise-sum (cdr ntpl-1) (cdr ntpl-2)))))))


;;
;; Exercise 3.02b p. 81: pairwise-product
;; --------------------------------------
;; 1. Define a proc pairwise-sum that takes two n-tuples of same length
;;    ntpl-1 & ntpl-2
;; 3. Produces a new n-tuple whose components are the product of ntpl-1 & ntpl-2
;;
(define pairwise-product
  (lambda (ntpl-1 ntpl-2)
    (cond
      ((or (null? ntpl-1) (null? ntpl-2)) '())
      (else (cons (* (car ntpl-1) (car ntpl-2)) (pairwise-product (cdr ntpl-1) (cdr ntpl-2)))))))


;;
;; Exercise 3.03a p. 82: dot-product
;; ---------------------------------
;; 1. Take two n-tuples of same length
;; 2. Multiply corresponding components & add resulting products
;; 3. Can code directly or use procedures from exercises 3.01 & 3.02
;; 4. Consider tradeoffs from each method
;;
(define dot-product
  (lambda (tuple-1 tuple-2)
    (cond
      ((or (null? tuple-1) (null? tuple-2)) 0)
      (else (+ (* (car tuple-1) (car tuple-2))
               (dot-product (cdr tuple-1) (cdr tuple-2)))))))


;;
;; Exercise 3.03b p. 82: dot-product2
;; ----------------------------------
;; 1. This version uses proc. sum & pairwise-product as helping procedures
;; 2. Advantages -- Easier to read. More compact 
;; 3. Disadvantage -- Should change name of the sum proc. to something like
;;      sum-list-items.
;;      More importantly -- too much unnecessary recursion
;;
(define dot-product2
  (lambda (tuple-1 tuple-2)
    (cond
      ((null? tuple-1) 0)
      (else (sum (pairwise-product tuple-1 tuple-2))))))


;;
;; Exercise 3.04 p. 82: mult-by-n
;; ------------------------------
;; 1. Proc. mult-by-n takes number num & n-tuple ntpl as args
;; 2. Multiply each component of ntpl by num
;;
(define mult-by-n
  (lambda (num ntpl)
    (cond
      ((null? ntpl) '())
      (else (cons (* num (car ntpl))
                  (mult-by-n num (cdr ntpl)))))))


;;
;; Exercise 3.05 p. 82: index     ;;;;;;;;;;;;;;;;;;;;;;;;; Not Finished ;;;;;;;;;;;;;
;; --------------------------
;; 1. Proc. index takes an item & list of items ls
;; 2. Returns index of item in ls (the zero-based location of item in ls)
;; 3. If item not in the list, return -1
;;
(define index
  (lambda (item ls)
    (cond
      ((null? ls) -1)
      ((equal? item (car ls)) 0)
      (else + 1 (index item (cdr ls))))))



#|
    (if (null? ls) -1
        (if (equal? item (car ls)) 0
            (+ 1 (index item (cdr ls)))))))
|#



;;
;; Exercise 3.06 p. 82: make-list
;; ------------------------------
;; 1. The proc. make-list takes as args
;;    a. a nonnegative int : num
;;    b. an item : a
;; 2. It returns a list of num elements, each of which is a
;;
(define make-list
  (lambda (num a)
    (cond
      ((<= num 0) '())
      (else (cons a (make-list (- num 1) a))))))
     
;; all-same? --> helper for Exercise 3.06 test
(define all-same?
  (lambda (ls)
    (if (or (null? ls) (null? (cdr ls))) #t
            (if (and (pair? ls) (equal? (car ls) (cadr ls))) (all-same? (cdr ls))
                #f))))


;;
;; Exercise 3.07 p. 83: count-background
;; -------------------------------------
;; 1. This proc. takes
;;          a. an item : a
;;          b. a list of items : ls
;; 2. It returns the number of items in ls not equal? to a
;;
(define count-background
  (lambda (a ls)
    (cond
      ((null? ls) 0)
      ((equal? a (car ls)) (+ 0 (count-background a (cdr ls))))
      (else (+ 1 (count-background a (cdr ls)))))))


;;
;; Exercise 3.08 p. 83: list-front
;; -------------------------------
;; 1. This proc. takes
;;       a. a list : ls
;;       b. nonnegative integer: num
;; 2. It returns the first num top-level items in ls
;; 3. If num is larger than the number of top-level items in ls,
;;    an error is signaled
;;
(define list-front
  (lambda (ls num)
    (cond
      ((= num 0) '())
      ((< (length ls) num) (error "Error: legth of "  ls  "is less than " num"."))
      (else (cons (car ls) (list-front (cdr ls) (- num 1)))))))


;;
;; Exercise 3.09 p. 83: wrapa
;; --------------------------
;; 1. Define a proc. wrapa that takes as arguments an item 'a' and
;;    a nonnegative integer 'num' and wraps 'num' sets of parentheses
;;    around the item 'a'.
;;
(define wrapa
  (lambda (a num)
    (cond
      ((= num 0) a)
      (else (cons (wrapa a (- num 1)) '())))))


;;
;; Exercise 3.10 p. 83: multiple?
;; ------------------------------
;; 1. Define a predicate multiple? that takes two integers
;;    m & n and returns #t if m is an integer multiple of n
;;    (Hint: Use remainder)
;;
(define multiple?
  (lambda (m n)
    (cond
      ((zero? m) #t)
      ((zero? n) #f)
      (else (zero? (remainder m n))))))


;;
;; Exercise 3.11 p. 84: sum-of-odds
;; --------------------------------
;; 1. It can be shown that the sum of the first n odd numbers
;;    is equal to n^2. For Example:
;;
;;            1 + 3 + 5 + 7 = 16 = 4^2
;;
;; 2. Write a proc. sum-of-odds that sums the first n odd integers.
;; 3. Test the proc. by evaluating it for all values of n from 1 to 10
;;    to see that each is the perfect square of the number of terms
;;
(define sum-of-odds
  (lambda (n)
    (cond
      ((or (zero? n) (negative? n) (> n 10))
       (display "Out of bounds. Pick a number between 1 - 10"))
      ((= n 1) + 1)
      (else (+ n (- n 1) (sum-of-odds(- n 1)))))))


;;
;; Exercise 3.12 p. 84: n-tuple->integer
;; -------------------------------------
;; 1. Define a proc. n-tuple->integer that converts a nonempty
;;    n-tuple of digits into the number having those digits.
;;
;; Example: (n-tuple->integer '(3 1 4 6)) ---> 3146
;;
(define n-tuple->integer
  (lambda (ls)
    (cond
      ((null? ls) "This is an empty list")
      ((and (pair? ls) (null? (cdr ls))) (+ (car ls)))
      (else (+ (* (expt 10 (- (length ls) 1)) (car ls))
               (n-tuple->integer (cdr ls)))))))



;;
;; Exercise 3.13 p. 84: paper exercise
;; -----------------------------------
;;



;;;;;;;;;; Helper Procedures for Exercises 3.14 - 3.18 ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Example 3.8 p. 85: rzero?
;; -------------------------
;;
(define rzero?
  (lambda (rtl)
    (zero? (num rtl))))



;;
;; Example 3.9 p. 86: r+
;; ---------------------
;;
(define r+
  (lambda (x y)
    (make-ratl
     (+ (* (numr x) (denr y)) (* (numr y) (denr x)))
     (* (denr x) (denr y)))))



;;
;; Example 3.10 p. 86: r*
;; ----------------------
;;
(define r*
  (lambda (x y)
    (make-ratl
     (* (numr x) (numr y))
     (* (denr x) (denr y)))))



;;
;; Example 3.11 p. 86: r-
;; ----------------------
;;
(define r-
  (lambda (x y)
    (- (* (numr x) (denr y)) (* (numr y) (denr x)))
    (* (denr x) (denry))))



;;
;; Example 3.12 p. 87: rinvert
;; ---------------------------
;;
(define rinvert
  (lambda (rtl)
    (if (rzero? rtl)
        (error "rinvert: Cannot invert " rtl)
        (make-ratl (denr rtl) (numr rtl)))))



;;
;; Example 3.13 p. 87: r/
;; ----------------------
;;
(define r/
  (lambda (x y)
    (r* x (rinvert y))))



;;
;; Example 3.14 p. 87: r=
;; ----------------------
;;
(define (x y)
  (lambda (x y)
    (= (* (numr x) (denr y)) (* (numr y) (denr x)))))



;;
;; Example 3.15 p. 87: rpositive?
;; ------------------------------
;;
;; This has been replaced with Exercise 3.15 p. 92: same-sign?
#|
(define rpositive?
  (lambda (rtl)
    (or (and (positive? (numr rtl)) (positive? (denr rtl)))
        (and (negative? (numr rtl)) (negative? (denr rtl))))))
|#


;;
;; Example 3.16 p. 88: r>
;; ----------------------
;;
(define r>
  (lambda (x y)
    (rpositive? (r- x y))))



;;
;; Example 3.17 p. 88: max
;; -----------------------
;;
#|
(define max
  (lambda (x y)
    (if (> x y)
        x
        y)))
|#


;;
;; Example 3.18 p. 88: rmax
;; ------------------------
;;
(define rmax
  (lambda (x y)
    (if (r> x y)
        x
        y)))



;;
;; Example 3.19 p. 89: extreme-value
;; ---------------------------------
;;
(define extreme-value
  (lambda (pred x y)
    (if (pred x y)
        x
        y)))



;;
;; Example 3.20 p. 90: rprint
;; --------------------------
;;
(define rprint
  (lambda (rtl)
    (display (numr rtl) "/" (denr rtl))))



;;
;; Example 3.21 p. 91: numr, denr, make-ratl
;; -----------------------------------------
;;
(define numr
  (lambda (rtl)
    (car rtl)))

(define denr
  (lambda (rtl)
    (cadr rtl)))

(define make-ratl
  (lambda (int1 int2)
    (if (zero? int2)
        (error "make-ratl: The denominator cannot be zero.")
        (list int1 int2))))

;;;;;;;;;; End Helper Procedures for exercises 3.14 - 3.18 ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Exercise 3.14 p. 92: rminus
;; ---------------------------
;; Define a proc. rminus that takes a rational number as it argument,
;; and returns the negative of that number
;; Think of rtl as '(num denom) for now
;;
(define rminus
  (lambda (rtl)
    (if (rpositive? rtl)
        (make-ratl (-(car rtl)) (cadr rtl)))))
        
       

;;
;; Exercise 3.15 p. 92: same-sign?
;; -------------------------------
;; Consider the definition for rpositive?
;;
;; (define rpositive?
;;   (lambda (rtl)
;;     (same-sign? (numr trl) (denr rtl))))
;;
;; Define same-sign? so that rpositive? is correct
;;
(define rpositive?
  (lambda (rtl)
     (same-sign? (numr rtl) (denr rtl))))

(define same-sign?
  (lambda (num denom)
    (cond
      ((or (and (positive? num) (positive? denom))
           (and (negative? num) (negative? denom))))
      (else #f))))
     


;;
;; Exercise 3.16 p. 92: rabs
;; -------------------------
;; Define a proc. rabs that takes a rational number and
;; returns its absolute value
;;
(define rabs
  (lambda (rtl)
    (cond
      ((and (positive? (numr rtl)) (positive? (denr rtl))) rtl)
      (else (make-ratl (abs (numr rtl)) (abs (denr rtl)))))))
     


;;
;; Exercise 3.17 p. 93: make-ratl-reduced
;; ------------------------------
;; Write the definition of the proc. make-ratl so that
;; (make-ratl a b) is a list (p q) in which p/q = a/b
;; and p/q is reduced to lowest terms (so that 1 is the greatest
;; common divisor of p and q) and q is positive.
;; Use the built in gcd
;;
(define make-ratl-reduced
(lambda (int1 int2)
  (cond
    ((zero? int2) "make-ratl-reduced: The denominator cannot be zero.")
    (else (list (/ int1 (gcd int1 int2)) (/ int2 (gcd int1 int2)))))))



#|            
    (if (zero? int2)
        (error "make-ratl: The denominator cannot be zero.")
        (list int1 int2))))
|#








































































