;;
;; Scheme and the Art of Programming
;; ---------------------------------
;;    - George Springer & Daniel P. Friedman
;;    - Chapter 02 Exercises
;;




;;
;; Exercise 2.01 p. 39: second
;; ---------------------------
;; 1. Define a proc. called second that takes a list
;; 2. Return second item of list
;; 3. Assume list contains at least three items
;;

(define second
  (lambda (mylist)
    (car (cdr mylist))))



;;
;; Exercise 2.02 p. 39: third
;; --------------------------
;; 1. Define a proc. called third that takes a list
;; 2. Return the third item in the list
;; 3. Assume the list contains at least three items
;;
(define third
  (lambda (ls)
    (car (cdr (cdr ls)))))



;;
;; Exercise 2.03 p. 39: firsts-of-both
;; -----------------------------------
;; 1. Run this procedure on examples in book
;;
(define firsts-of-both
  (lambda (list-1 list-2)
    (make-list-of-two (car list-1) (car list-2))))

;; helper
(define make-list-of-two
  (lambda (item-one item-two)
    (cons item-one (make-list-of-one item-two))))
    
;; helper
(define make-list-of-one
  (lambda (item)
    (cons item '())))



;;
;; Exercise 2.04 p. 39: juggle
;; ---------------------------
;; 1. Define a proc. juggle that rotates a three-element list
;; 2. juggle returns a list that is a rearrangement of the input list so
;;    first element becomes second,
;;    second element becomes third, and 
;;    third element becomes first
;; 3. Test proc. on (juggle '(jump quick spot)) --> (spot jump quick)
;;                  (juggle '(dog bites man)) --> (man dog bites)
;;
(define juggle
  (lambda (ls)
    (cons (caddr ls) (cons (car ls) (cons (cadr ls) '())))))
    


;;
;; Exercise 2.05 p. 40: switch
;; ---------------------------
;; 1. Define a proc. switch that interchanges the 1st & 3rd elements
;;    of a three-element list
;; 2. Test your procedure on examples from exer. 2.04
;;
(define switch
  (lambda (ls)
    (cons (caddr ls) (cons (cadr ls) (cons (car ls) '())))))



;;
;; Exercise 2.06 - 2.09 p. 45: paper exercises
;; -------------------------------------------
;;
(define s-or-n-list?
  (lambda (ls)
    (and (pair? ls)
         (or (symbol? (car ls))
             (number? (car ls))))))



;;
;; Exercise 2.10 p. 53: last-item
;; ------------------------------
;; Use if expressions to rewrite definitions
;; last-item, member?, and remove-1st
;;

(define last-item
  (lambda (ls)
    (cond
      ((null? ls) (display "This is an empty list"))
      ((null? (cdr ls)) (car ls))
      (else (last-item (cdr ls))))))

(define last-item-if
  (lambda (ls)
    (if (null? ls) (display "This is an empty list")
        (if (null? (cdr ls)) (car ls)
            (last-item-if (cdr ls))))))



(define member?
  (lambda (item ls)
    (cond
     ((null? ls) #f)
      (else (or (equal? (car ls) item)
                (member? item (cdr ls)))))))
  
(define member?-if
  (lambda (item ls)
    (if (null? ls) #f
        (or (equal? item (car ls)) (member? item (cdr ls))))))
   



(define remove-1st
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? item (car ls)) (cdr ls))
      (else (cons (car ls) (remove-1st item (cdr ls)))))))

(define remove-1st-if
  (lambda (item ls)
    (if (null? ls) '()
        (if (equal? item (car ls)) (cdr ls)
            (cons (car ls) (remove-1st-if item (cdr ls)))))))
  


;;
;; Exercise 2.11 p.54: member?-rewrite
;; -----------------------------------
;; Rewrite so  that each of the two subexpressions of the or expression
;; is handled in a separate cond clause
;;
(define member?
  (lambda (item ls)
    (cond
      ((null? ls) #f)
      ((equal? item (car ls)) #t)
      (else (member? item (cdr ls))))))



;;
;; Exercise 2.12 p.54: mystery
;; ---------------------------
;; What does this do?
;;
(define mystery
  (lambda (ls)
    (if (null? (cddr ls)) (cons (car ls) '())
        (cons (car ls) (mystery (cdr ls))))))



;;
;; Exercise 2.13 p. 54: subst-1st
;; ------------------------------
;; 1. Takes 3 parameters -- item new, item old, & ls
;; 2. Looks for the first top level occurrence of item old in ls
;;    & replaces it with item new.
;;
(define subst-1st
  (lambda (old new ls)
    (cond
      ((null? ls) '())
      ((equal? old (car ls)) (cons new (cdr ls)))
      (else (cons (car ls) (subst-1st old new (cdr ls)))))))



;;
;; Exercise 2.14 p.55: insert-right-1st
;; ------------------------------------
;; Inserts a new item to right of first occurence of the old item
;;
(define insert-right-1st
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? old (car ls)) (cons old (cons new (cdr ls))))
      (else (cons (car ls) (insert-right-1st new old (cdr ls)))))))



;;
;; Exercise 2.15 p. 55: list-of-first-items
;; ----------------------------------------
;; Takes as its argument a list composed of nonempty lists of items.
;; Its value is a list composed of the first top-level item in each
;; sublist.
;;
(define list-of-first-items
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((symbol? (car ls)) (cons (car ls) (list-of-first-items (cdr ls))))
      (else (cons (caar ls) (list-of-first-items (cdr ls)))))))



;;
;; Exercise 2.16 p. 56: replace
;; ----------------------------
;; Define a proc. replace that replaces each top level item in a list of items
;; by a given item new-item.
;;
(define replace
  (lambda (item ls)
    (cond
      ((null? ls) '())
      (else (cons item (replace item (cdr ls)))))))
  


;;
;; Exercise 2.17 p. 56: remove-2nd
;; -------------------------------
;; Define a proc. remove-2nd that removes the second occurrence of a
;; given item a from a list of items ls.
;; You may use proc. remove-1st in defining remove-2nd
;;
(define remove-2nd
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? item (car ls)) (cons (car ls) (remove-1st item (cdr ls))))
      (else (cons (car ls) (remove-2nd item (cdr ls)))))))

#| helper reference
(define remove-1st
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? item (car ls)) (cdr ls))
      (else (cons (car ls) (remove-1st item (cdr ls)))))))
|#


           
;;
;; Exercise 2.18 p. 56: remove-last
;; --------------------------------
;; Define a proc. remove-last that removes the last top-level occurrence
;; of a given element item in a list ls
;;
;; (remove-last 'a '(b a n a n a s))
;;
;;     --> (b a n a n s)
;;
(define remove-last
  (lambda (item ls)
    (cond
      ((null? ls) '())
       ;;((and (equal? item (car ls)) (null? (cdr ls))) ls)
     
      
      (else (cons (car ls) (remove-last item (cdr ls)))))))
   

;;
;; Exercise 2.19 p. 56: sandwich-1st
;; ---------------------------------
;; Define a proc sandwich-1st that takes two items, a & b, and list ls.
;; It replaces the 1st occurrence of two succesive b's in ls with b a b
;;
(define sandwich-1st
  (lambda (a b ls)
    (cond
      ((null? ls) '())
      ((and (pair? ls) (null? (cdr ls))) ls)
      ((and (equal? b (car ls)) (equal? b (cadr ls)))
       (cons b (cons a (cdr ls))))
      (else (cons (car ls) (sandwich-1st a b (cdr ls)))))))
      
                            
      
;;
;; Exercise 2.20 p. 57: list-of-symbols?
;; -------------------------------------
;; 1. Define a proc. list-of-symbols? that tests whether the top-level items
;;    in a given list ls are symbols.
;; 2. Write the definitions in three ways
;;      a. Using cond
;;      b. Using if
;;      c. Using and & or
;;
(define list-of-symbols01?
  (lambda (ls)
    (cond
      ((null? ls) #t)
      ((symbol? (car ls)) (list-of-symbols01? (cdr ls)))
      (else #f))))

(define list-of-symbols02?
  (lambda (ls)
    (if (null? ls) #t
        (if (symbol? (car ls)) (list-of-symbols02? (cdr ls))
            #f))))

(define list-of-symbols03?
  (lambda (ls)
    (or (null? ls)
        (and (symbol? (car ls)) (list-of-symbols03? (cdr ls))))))


;;
;; Exercise 2.21 p. 57: all-same?
;; ------------------------------
;; Define a proc all-same? that takes a list ls, and 
;; tests whether all top-level elements of ls are the same
;;
(define all-same?
  (lambda (ls)
    (if (or (null? ls) (null? (cdr ls)))
        #t
        (if (equal? (car ls) (cadr ls))
            (all-same? (cdr ls))
            #f))))









