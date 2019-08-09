;;
;; Scheme and the Art of Programming
;; ---------------------------------
;;    - George Springer & Daniel P. Friedman
;;    - Chapter 04 Exercises
;;




      
;;
;; Exercise 4.1 p. 100: insert-left
;; --------------------------------
;; Define a proc. insert-left with parameters new, old, ls
;; that builds a list obtained by inserting the item new
;; to the left of each top-level occurrence of the item
;; old in the the list ls.
;;
(define insert-left
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? old (car ls))
       (cons new (cons old (insert-left new old (cdr ls)))))
      (else
       (cons (car ls) (insert-left new old (cdr ls)))))))



;;
;; Exercise 4.2 p. 100: insert-right
;; ---------------------------------
;; Define a proc. insert-right with the parameters new, old, and ls
;; that builds a list obtained by inserting the item new to the right
;; of each top-level occurrence of the item old in the list ls.
;;
(define insert-right
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? old (car ls))
       (cons old (cons new (insert-right new old (cdr ls)))))
      (else
       (cons (car ls) (insert-right new old (cdr ls)))))))



;;
;; Exercise 4.3 p. 100: subst
;; --------------------------
;; Define a proc subst with parameters new, old, and ls that
;; builds a list obtained by replacing each top-level occurrence of
;; the item old in the list ls by the item new.
;;
(define subst
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? old (car ls))
       (cons new (subst new old (cdr ls))))
      (else (cons (car ls) (subst new old (cdr ls)))))))



;;
;; Exercise 4.4 p. 101: deepen-1
;; -----------------------------
;; Define a proc. deepen-1 with parameter ls that wraps
;; a pair of parentheses around each top-level item in ls
;;
(define deepen-1
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else (cons (cons (car ls) '()) (deepen-1 (cdr ls)))))))



;;
;; Exercise 4.5 p. 107: subst-all
;; ------------------------------
;; Define a proc. subst-all with call structure (subst-all new old ls)
;; that replaces each occurrence of the item old in a list ls with
;; the item new.
;; (subst-all 'z 'a '(a (b (a c)) (a (d a))))
;;
;;          --> (z (b (z c)) (z (d z)))
;;
(define subst-all
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? old (car ls))
       (cons new (subst-all new old (cdr ls))))
      ((rpair? (car ls))
       (cons (subst-all new old (car ls)) (subst-all new old (cdr ls))))
      (else
       (cons (car ls) (subst-all new old (cdr ls)))))))



;;
;; Exercise 4.6 p. 108: insert-left-all
;; ------------------------------------
;; Define a proc. insert-left-all with call structure
;; (insert-left-all new old ls) that inserts the item new
;; to the left of each occurrence of the item old in the
;; list ls.
;;
(define insert-left-all
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? old (car ls))
       (cons new (cons old (insert-left-all new old (cdr ls)))))
      ((pair? (car ls))
       (cons (insert-left-all new old (car ls)) (insert-left-all new old (cdr ls))))
      (else
       (cons (car ls) (insert-left-all new old (cdr ls)))))))



;;
;; Exercise 4.7 p. 108: sum-all
;; ----------------------------
;; Define a proc. sum-all that finds the sum of
;; the numbers in a list that may contain nested sublists
;; of numbers.
;;
(define sum-all
  (lambda (ls)
    (cond
      ((null? ls) 0)
      ((pair? (car ls)) (+ (sum-all (car ls)) (sum-all (cdr ls))))
      (else (+ (car ls) (sum-all (cdr ls)))))))


  
;;
;; Exercise 4.8 p. 114: count-parens-all
;; -------------------------------------
;; Write the definition of a proc. count-parens-all that takes a list
;; as its argument and counts the number of opening and closing parentheses
;; in the list
;;
(define count-parens-all
  (lambda (ls)
    (cond
      ((null? ls) 2)
      ((symbol? (car ls)) (count-parens-all (cdr ls)))
      ((equal? (car ls) '()) (+ 2 (count-parens-all (cdr ls))))
      (else (+ (count-parens-all (car ls)) (count-parens-all (cdr ls)))))))



;;
;; Exercise 4.9 p. 114: count-background-all
;; ----------------------------------------
;; Define a proc. count background-all that takes
;; as its arguments item and a list ls and returns
;; the number of items in ls that are not the same as item.
;; Use the appropriate sameness predicate for the data shown
;; in the examples
;;
;; example:
;; (count-background-all 'a '((a) b (c a) d)) --> 3
;;
(define count-background-all
  (lambda (item ls)
    (cond
      ((null? ls) 0)
      ((symbol? (car ls))
       (if (equal? item (car ls)) (count-background-all item (cdr ls))
           (+ 1 (count-background-all item (cdr ls)))))
      (else (+ (count-background-all item (car ls)) (count-background-all item (cdr ls)))))))
      
     
     
;;
;; Exercise 4.10 p. 115: leftmost
;; ------------------------------
;; Define a proc. leftmost that takes a nonempty list as its argument
;; and returns the leftmost atomic item in the list
;;
;; example:
;; (leftmost '((a b) (c (d e)))) --> a
;;
(define leftmost
  (lambda (ls)
    (cond
      ((pair? (car ls)) (leftmost (car ls)))
      (else (car ls)))))



;;
;; Exercise 4.11 p. 115: rightmost
;; -------------------------------
;; Define a proc. rightmost that takes a nonempty list as its argument
;; and returns the rightmost atomic item in the list
;;
;; example:
;; (rightmost '((a b) (d (c d (f (g h) i) m n) u) v)) --> v
;;
(define rightmost
  (lambda (ls)
   (cond
     ((null? (cdr ls))
      (if (not (pair? (car ls)))
          (car ls)
          (rightmost (car ls))))
     (else (rightmost (cdr ls))))))
    


;;
;; Exercise 4.12 p. 120:
;; ---------------------
;; Enter the proc. fact into the computer and compute (fact n)
;; for n = 10, 20, 30, 40, 50, and 100. You will have an opportunity
;; to observe how the implementation of Scheme you are using displays large numbers
;;
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))



;;
;; Exercise 4.13 p. 120:
;; ---------------------
;; What happens when you invoke (fact 3.5)
;;
;; Answer:
;; There is no terminating condition
;; The function bypases zero (3.5, 2.5, 1.5, 0.5, -0.5, -1.5, etc...
;;



;;
;; Exercise 4.14 p. 120: harmonic-sum-it
;; -------------------------------------
;; Define an interactive proc. harmonic-sum-it that sums
;; the first n terms of the harmonic series
;;
;;     1/1 + 1/2 + 1/3 + 1/4 + 1/5 + ...
;;
;; Test the proc. by summing the harmonic series for 10 terms,
;; 100 terms, 1000 terms, & 10,000 terms
;;
;; It can be shown that
;;
;; 1/2 + 1/3 + ... + 1/n <= log n <= 1/1 + 1/2 + 1/3 + ... + 1 / (n -1)
;;
;; where log n is the natural logarithm of n. Using the Scheme proc. log,
;; verify this inequality for the values of the sums computed above.
;;
(define harmonic-sum-it
  (lambda (n)
    (cond
      ((<= n 0) "n must be greater than zero.")
      ((= 1 n) 1)
      (else (+ (/ 1 n) (harmonic-sum-it (- n 1)))))))
  


;;
;; Exercise 4.15 p. 127:
;; ---------------------
;; Rewrite the recursive version of the proc. fib with the line
;;
;;     (writeln "n = " n)
;;
;; inserted just below the line (lambda (n). Then compute (fib 4) and
;; compare the results with the tree in Fig. 4.21. Also compute (fib 5)
;; and (fib 6) and observe how the number of recursive calls to fib increases
;;
(define fib
  (lambda (n)
    ;;(display "n = " ) ;; neither writeln, write, nor display worked in R5RS or Pretty Big
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))



;;
;; Exercise 4.16 p. 128
;; --------------------
;; Rewrite the iterative version of the proc. fib-it with the line
;;
;;     (writeln "n = " n ", acc1 = " acc1 ", acc2 = " acc2)
;;
;; inserted just below the line
;;
;;     (lambda (n acc1 acc2)
;;
;; Compute (fib-it 4 0 1) and compare the output with the output for
;; (fib 4) in the preceding exercise. Do the same for (fib-it 5 0 1)
;; and (fib-it 6 0 1)
;;
;;
;; See comment regarding (display) for previous function 4.15
;;



;;
;; Exercise 4.17 p. 128: calls-fib, adds-fib
;; -----------------------------------------
;; Write the definitions of the procs calls-fib and adds-fib
;; discussed in this section (4.6). Test your proc. on the values
;; given in the Table 4.22. Also evaluate each of these procs. for
;; larger values of n to get an idea of their rates of growth
;;
;; Ignore this exercise
;;



;;
;; Exercise 4.18 p. 128: length-it
;; -------------------------------
;; Write an iterative version length-it of the proc. length
;; that computes the length of a list.
;;
;; Ignore this exercise
;;



;;
;; Exercise 4.19 p. 128: mk-asc-list-of-ints, mk-desc-list-of-ints
;; ---------------------------------------------------------------
;; Write an iterative proc. mk-asc-list-of-ints that, for any integer n,
;; produces a list of the integers from 1 to n in ascending order.
;; Then write an iterative proc. mk-desc-list-of-ints that, for any
;; integer n, produces a list of integers from n to 1 in descending order
;;
;; Ignore this exercise
;;



;;
;; Exercise 4.20 p. 128: occurs, occurs-it
;; ---------------------------------------
;; Define both recursive and iterative versions of a procedure occurs
;; that counts the nubmer of times an item occurs at the top level in a list.
;; Call the iterative version occurs-it. Test the proc. by counting how many
;; times the item a occurs at top level in each of the following lists:
;;
;; (a b a c a d)     (b c a (b a) c a)     (b (c d))    
;;
;; Ignore this exercise
;;













































