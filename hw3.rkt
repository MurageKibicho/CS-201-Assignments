#lang racket
(require racket/trace)

; Name: Murage Kibicho
; Email address: murage.kibicho@yale.ed

; ** problem 0 ** (1 point)
; Modify the following procedure to return the number of
; hours you spent on this assignment (including reading):

(define hours
  (lambda () 20))

; CS 201a HW #3  DUE Sunday, Oct 11, 2020 at 11:59 pm, 
; electronically, using the submit command.  

; The topics of this assignment are
; Table lookup, Boolean functions, expressions, 
; environments, satisfiability, substituting gate types


; We define a table as a list of entries, where 
; each entry is a list of two items: the key and the value
; Examples of tables

(define table1 '((a 1) (large 5) (zot 3)))
(define table2 '((2 prime) (3 prime) (2 even)))
(define table3 '((20 2) (10 1) (40 3) (20 2)))

; ** problem 1 ** (11 points)
; Write a procedure (lookup item table)
; that returns a list consisting of two elements:
; the first and second elements are both #f if no
; entry in the table has a key equal to item;
; otherwise, the first element is #t and the second
; element is the value paired with the given key
; (use the earliest entry in the table if several
; have the given key.)

; Examples
; (lookup 'a table1) => '(#t 1)
; (lookup 'zot table1) => '(#t 3)
; (lookup 'fir table1) => '(#f #f)
; (lookup 2 table2) => '(#t prime)
; (lookup 20 table3) => '(#t 2)
; (lookup 80 table3) => '(#f #f)

(define lookup
  (lambda (item table)
    (cond
      ((null? table) (list #f #f))
      ((equal? item (first (first table))) (cons #t (rest (first table))))
      (else (lookup item (rest table)) )
      )
    ))





; ** problem 2 ** (11 points)
; Write a predicate (unique-keys? table)
; that takes a table and returns #t if no two entries
; in the table have the same key

; Examples
; (unique-keys? '()) => #t
; (unique-keys? table1) => #t
; (unique-keys? table2) => #f
; (unique-keys? table3) => #f

(define first-member?
  (lambda (item ls)
    (cond
      ((null? ls) #f)
      ((equal? item (first (first ls))) #t)
      (else (first-member? item (rest ls))))))

(define unique-keys?
  (lambda (table)
    (cond
      ((null? table) #t)
      ((first-member? (first (first table)) (rest table)) #f)
      (else (unique-keys? (rest table)))
      ))
    )


; ** problem 3 ** (11 points)
; Write 3 procedures (bnot x), (bor x y), (band x y)
; that compute Boolean not, or, and
; Examples
; (bnot 0) => 1
; (bnot 1) => 0

; (bor 0 1) => 1
; (bor 1 1) => 1
; (band 0 1) => 0
; (band 1 1) => 1

(define bor
  (lambda (x y)
    (if (= x 1) 1 y)))

(define band
  (lambda (x y)
    (if (= x 0) 0 y)))

(define bnot
  (lambda (x)
    (if (= x 1) 0 1)))


; We define a representation of Boolean expressions as follows.
; 0 and 1 represent the constants 0 and 1
; Racket symbols represent variables
; the list '(- exp) represents the Boolean "not" of exp
; the list '(+ exp1 exp2) represents the Boolean "or" of exp1 and exp2
; the list '(* exp1 exp2) represents the Boolean "and" of exp1 and exp2

; Some examples of Boolean expressions:

(define ex0 '(- x))
(define ex1 '(+ x y))
(define ex2 '(* x y))
(define ex3 '(* x (+ y z)))
(define ex4 '(+ x (- x)))
(define ex5 '(* (+ x 0) (+ x 1)))


; ** problem 4 ** (11 points)
; Write a procedure (get-type exp)
; that takes a Boolean expression as defined above
; and returns its type as one of the symbols:
;   constant, variable, not, or, and
; Note that the type is determined by the top-level
; operation in case the expression is not a constant or variable.

; Examples
; (get-type 0) => 'constant
; (get-type 'x) => 'variable
; (get-type ex0) => 'not
; (get-type ex1) => 'or
; (get-type ex2) => 'and
; (get-type ex3) => 'and
; (get-type ex4) => 'or


(define get-type
  (lambda (exp)
    (cond
      ((number? exp) 'constant)
      ((symbol? exp) 'variable)
      ((equal? (first exp) '-) 'not)
      ((equal? (first exp) '+) 'or)
      ((equal? (first exp) '*) 'and)
      )))



; We define an environment as a table containing entries
; whose keys are variables and values are Boolean constants
; For example

(define environ1 '((x 0) (y 1)))
(define environ2 '((x 1) (y 0) (z 1)))

; ** problem 5 ** (11 points)
; Write a procedure (eval-in-env exp env)
; that takes a Boolean expression exp
; and an environment env
; represented as described above
; and returns 0 or 1 giving the value of the expression in the
; environment
; Hint: use the recursive structure of Boolean expressions

; Examples
; (eval-in-env 1 environ1) => 1
; (eval-in-env 'x environ1) => 0
; (eval-in-env 'x environ2) => 1
; (eval-in-env ex1 environ1) => 1
; (eval-in-env ex2 environ2) => 0
; (eval-in-env ex1 environ2) => 1
; (eval-in-env ex3 environ2) => 1
; (eval-in-env '(* 1 0) '()) => 0


(define eval-in-env
  (lambda (exp env)
    (cond
    ((equal? (get-type exp) 'variable) (list-ref (lookup exp env) 1))
    ((equal? (get-type exp) 'constant) exp)
    ((equal? (get-type exp) 'or) (bor (eval-in-env (list-ref exp 1) env) (eval-in-env (list-ref exp 2) env)))
    ((equal? (get-type exp) 'and) (band (eval-in-env (list-ref exp 1) env) (eval-in-env (list-ref exp 2) env)))
    ((equal? (get-type exp) 'not) (bnot (eval-in-env (list-ref exp 1) env)))
    ) 
    ))




; We represent a substitution as a table
; each entry has a variable for a key
; and a Boolean expression for a value
; The following substitution represents
; replacing x by (* r (+ s t)) and y by (* r s)

(define subst1 (list '(x (* r (+ s t))) '(y (* r s))))


; ** problem 6 ** (11 points)
; Write a procedure  (substitute exp subst)
; that takes a boolean expression exp and a substitution subst,
; and for each variable that occurs as a key in subst,
; replaces every occurrence of that variable in exp by
; the corresponding expression from subst.

; Examples
; (substitute '(+ x y) '((x 1))) => (+ 1 y) list (+ subst(x) sub(y))
; (substitute '(+ x (* x y)) '((x a) (y 0))) => (+ a (* a 0))
; (substitute '(* x (- y)) subst1) => (* (* r (+ s t)) (- (* r s)))
; (substitute ex4 (list '(x (- y)))) => (+ (- y) (- (- y)))
; (substitute '(* x (+ y y)) '((x (- y)) (y 0))) => (* (- y) (+ 0 0))


(define substitute
  (lambda (exp subst)
    (cond
      ((null? subst) exp)
      ((equal? (get-type exp) 'constant) exp)
      ((equal? (get-type exp) 'variable) (if (first (lookup exp subst)) (second (lookup exp subst)) exp))
      ((equal? (get-type exp) 'or) (list '+ (substitute (second exp) subst) (substitute (third exp) subst)))
      ((equal? (get-type exp) 'and) (list '* (substitute (second exp) subst) (substitute (third exp) subst)))
      ((equal? (get-type exp) 'not) (list '- (substitute (second exp) subst)))
      )))



; ** problem 7 ** (11 points)
; Write a procedure (all-variables exp) that takes a Boolean
; expression exp and makes a list containing all the variables
; that occur in exp.  The list should not contain duplicates,
; and should have the variables in the order of their
; first appearance in exp (scanning left to right.)

; Examples
; (all-variables '(* x (+ (- y) x))) => (x y)
; (all-variables '(+ 0 1)) => ()
; (all-variables '(+ (+ x (- x)) (* x (- x)))) => (x)
; (all-variables '(+ (+ z y) (+ x y))) => (z y x)

(define remove-helper
  (lambda (lst lst2)
    (cond
      ((null? lst) lst2)
      ((member (first lst) lst2) (remove-helper (rest lst) lst2))
      (else (remove-helper (rest lst) (append lst2 (list (first lst)))))))
      )
(remove-helper '(1 2 3 4 5 6 7 8 9) '())

(define all-variables
  (lambda (exp)
    (cond
      ((null? exp) exp)
      ((equal? (get-type exp) 'constant) '())
      ((equal? (get-type exp) 'variable) (list exp))
      ((equal? (get-type exp) 'and) (remove-helper (append (all-variables (second exp)) (all-variables (third exp))) '()))
      ((equal? (get-type exp) 'or)  (remove-helper (append (all-variables (second exp)) (all-variables (third exp))) '()))
      ((equal? (get-type exp) 'not) (remove-helper (all-variables (second exp)) '()))
      )))



; ** problem 8 ** (11 points)
; Write a procedure (satisfiable exp)
; that tests whether the Boolean expression exp is satisfiable
; or not.  An exp is satisfiable if 
; there exists an environment env assigning 
; a Boolean value to each of its variables such that 
; (eval-in-env exp env) is equal to 1.
; If exp is not satisfiable, your procedure should return #f.
; If exp is satisfiable, your procedure should return some environment
; env such that (eval-in-env exp env) is equal to 1.

; Examples
; (satisfiable 0) => #f
; (satisfiable 1) => '()
; (satisfiable ex0) => '((x 0))
; (satisfiable ex1) => '((x 0) (y 1))
; (satisfiable ex2) => '((x 1) (y 1))
; (satisfiable ex3) => '((x 1) (y 0) (z 1))
; (satisfiable ex4) => '((x 0))
; (satisfiable '(* x (- x))) => #f

; Your procedure may find a different environment,
; as long as it causes the expression to evaluate to 1.

; Here's a large expression to test that took several seconds on a MacBook Pro:
(define large-exp
  '(* x20 (* x19 (* x18 (* x17 (* x16 (* x15 (* x14 (* x13 (* x12 (* x11 (* x10  (* x9 (* x8 (* x7 (* x6 (* x5 (* x4 (* x3 (+ x2 x1))))))))))))))))))))

(define combine
  (lambda (item lst)
    (map (lambda (element)
           (cons item element))
           lst
           )))

(define all-possible
  (lambda (var-list)
    (cond
      ((null? var-list) '(()))
      (else (append (combine (list (first var-list) 0) (all-possible (rest var-list)))
                    (combine (list (first var-list) 1) (all-possible (rest var-list)))
                    ))
      )))


(define create-env
  (lambda (exp)
      (all-possible (all-variables exp))
    ))

(define find-matches
  (lambda (exp environ)
    (cond
      ((null? environ) #f)
      ((equal? (eval-in-env exp (first environ)) 1) (first environ))
      (else (find-matches exp (rest environ))))
    ))


(define satisfiable
 (lambda (exp)
   (find-matches exp (create-env exp))
      ))

;(satisfiable '(* x (- x)))
;(satisfiable ex0) 
;(satisfiable ex1)

; ** problem 9 ** (11 points)
; Let (! exp) represent the Boolean function
; of two variables that is computed by a NAND gate.
; Write a procedure convert-bnand to take any expression
; written with *, + and - and convert it to an
; equivalent expression with ! only.

; Examples
; (convert-bnand ex0) => (! x x)
; (convert-bnand ex1) => (! (! x x) (! y y))
; (convert-bnand ex2) =>(! (! x y) (! x y))
; (convert-bnand ex3) => (! (! x (! (! y y) (! z z))) (! x (! (! y y) (! z z))))
; (convert-bnand ex4) => (! (! x x) (! (! x x) (! x x)))
;
; Note from these examples, the expressions do not have to be the simplest form possible
;

(define convert-bnand
  (lambda (exp)
    (cond
      ((null? exp) exp)
      ((equal? (get-type exp) 'constant) exp)
      ((equal? (get-type exp) 'variable) exp)
      ((equal? (get-type exp) 'and) (list '! (list '! (convert-bnand (second exp)) (convert-bnand (third exp)))
                                                   (list '! (convert-bnand (second exp)) (convert-bnand(third exp)))))
      ((equal? (get-type exp) 'or) (list '! (list '! (convert-bnand (second exp)) (convert-bnand (second exp)))
                                                   (list '! (convert-bnand (third exp)) (convert-bnand(third exp)))))
      ((equal? (get-type exp) 'not) (list '! (convert-bnand (second exp)) (convert-bnand (second exp))))
      )
    ))




