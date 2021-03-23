#lang racket
(require racket/trace)

; This file may be loaded into Dr. Racket.  
; Lines beginning with semicolons are Racket comments.

; Name: Murage Kibicho
; Email address: murage.kibicho@yale.edu

; CS 201a HW #1  
; this assignment includes 7 problems that require code (probs 1,2,5,6,7,8,9),
; and two that are to be written manually (probs 3 and 4)
; all are DUE Monday, Sept 21, 11:59pm 

; DO NOT use a #define inside a procedure
; ONLY USE Racket syntax introduced in class 

; If you are asked to write a procedure, you may write auxiliary
; procedures -- for each of your auxiliary procedures, 
; please include a comment explaining what it does.

; On inputs other than those specified,
; your procedures need not do anything reasonable.

; Be sure to test out your homework with the autograder before submitting:
; /c/cs201/bin/autograde --test 1


; ** problem 0 ** (1 easy point) 
; How many hours did you spend on this assignment, including reading? Replace
; the N in the define below with your answer

(define hours 20)


; ** problem 1 ** (11 points)
; Write a procedure (digit->name n))
; that takes as argument an integer n
; from 0 through 9
; and returns a symbol giving the
; name of that number (in English).
; For any other number, it should return
; the symbol ?

; Examples
; (digit->name 0) => 'zero
; (digit->name 3) => 'three
; (digit->name 7) => 'seven
; (digit->name 13) => '?

(define digit->name
  (lambda (n)
    (cond ((equal? n 9) 'nine)
          ((equal? n 8) 'eight)
          ((equal? n 7) 'seven)
          ((equal? n 6) 'six)
          ((equal? n 5) 'five)
          ((equal? n 4) 'four)
          ((equal? n 3) 'three)
          ((equal? n 2) 'two)
          ((equal? n 1) 'one)
          ((equal? n 0) 'zero)
          (else '?)
          )))

; ** problem 2 ** (11 points)
; Write a procedure (sum-of-digits n)
; that returns the sum of the decimal digits of 
; the nonnegative integer n.
; If n is negative, return #f

; Examples
; (sum-of-digits 0) => 0
; (sum-of-digits 457) => 16
; (sum-of-digits 88012) => 19
; (sum-of-digits -5) => #f

;Procedure to find least
;significant digit
(define last-digit
  (lambda (n)
    (remainder n 10)
    )
  )

;Procedure to find the next
;least significant digit


(define sum-of-digits
  (lambda (n)
    (cond
      ((< n 0) #f)
      ((< n 10) n)
      (else (+ (last-digit n)(sum-of-digits (quotient (- n (last-digit n)) 10)))
            ))))


; ** problem 3 ** (11 points)
; Draw the internal list structure (box-and-pointer diagram)
; corresponding to the value of the following expression.
; Submit an image of your response as prob3.pdf or prob3.jpg

; (list 0 1 (cons 2 3) (list '(4 5) (list 6)))



; ** problem 4 ** (11 points)
; For the following two procedures, describe
; what each computes, and how it does so.

; Submit a file with your response as prob4.txt

(define puzzle1
  (lambda (x)
    ((if (< x 0)
	 +
	 -)
     5 x)))


(define puzzle2
  (lambda (a b c)
    (let ((a b) (b a)) (- a b))))


; ** problem 5 ** (11 points)
; Write a procedure (remove itm lst)
; that takes an item itm and a list lst
; and returns a list equal to lst with
; every top-level occurrence of itm removed.
; Please use the equal? procedure to test
; equality of values.

; Examples
; (remove 1 '(1 2 3)) => '(2 3)
; (remove 1 '((2 1) (4 5) (7 6))) => '((2 1) (4 5) (7 6))
; (remove 'a '(f a f a b e b a)) => '(f f b e b)
; (remove '(2 1) '((3 4) (2 1) (1 2))) => '((3 4) (1 2))

(define member?
  (lambda (item ls)
    (cond
      ((null? ls) #f)
      ((equal? item (first ls)) #t)
      (else (member? item (rest ls))))))

(define remove
  (lambda (itm ls)
  (cond
    ((not (member? itm ls)) ls)
    ((not (equal? itm (first ls)) ) (cons (first ls) (remove itm (rest ls))))
    (else (remove itm (rest ls)))
    )))



; ** problem 6 ** (11 points)
; Write a procedure (prefix? lst1 lst2)
; that takes two lists lst1 and lst2
; and returns #t if lst1 can be obtained
; by dropping zero or more elements from
; the end of lst2.  Otherwise, #f is returned.

; Use prefix? to write a procedure
; (sublist? lst1 lst2)
; that returns #t if lst1 is a contiguous
; top-level sublist of lst2, and #f otherwise.


; Examples
; (prefix? '(a b) '(a b c)) => #t
; (prefix? '(b c) '(a b c)) => #f
; (prefix? '() '()) => #t
; (prefix? '() '(1 2 3)) => #t

; (sublist? '(a b) '(a b c)) => #t
; (sublist? '(b c) '(a b c d)) => #t
; (sublist? '(a c) '(a b c d)) => #f
; (sublist? '() '(1 2 3)) => #t
; (sublist? '(a b d) '(a b c a b d e)) => #t

(define prefix?
  (lambda (lst1 lst2)
    (cond
      ((null? lst1) #t)
      ((not (equal? (first lst1) (first lst2))) #f)
      ((null? (rest lst1)) #t)
      ((not (equal? (first (rest lst1)) (first (rest lst2)))) #f)
      ((prefix? (rest lst1) (rest lst2)))
      (else (prefix? (rest lst1) (rest lst2)))
      )))


;At each point ask if it is a prefix
(define sublist?
  (lambda (lst1 lst2)
    (cond
      ((null? lst1) #t)
      ((equal? lst1 lst2) #t)
      ((null? lst2) #f)
      ((prefix? lst1 lst2) #t)
      (else (sublist? lst1 (rest lst2)))
      )
    ))



; ** problem 7 (11 points)
; Write a procedure (split lst i)
; that takes a list lst and a nonnegative 
; integer i (between 0 and the length of
; lst) and returns a list of two lists,
; consisting of the first i elements
; of lst, and the rest of the elements of lst.

; Examples
; (split '(a b a c) 0) => '(() (a b a c))
; (split '(a b a c) 2) => '((a b) (a c))
; (split '(a b a c) 4) => '((a b a c) ())
; (split '(a b a (b a)) 3) => '((a b a) ((b a)))

;Helper function
;Builds a list using the append function
(define build
  (lambda (lst built-lst i)
    (cond
      ((null? lst) built-lst)
      ((equal? i 0) (list built-lst lst))
      (else (build (rest lst) (append built-lst (list (first lst))) (- i 1)))
      )
         ))

(define split
  (lambda (lst i)
    (cond
      ((equal? i 0) (list '() lst))
      ((equal? i (length lst)) (list lst '()))
      (else (build lst '() i))
      )
    ))
(split '(1 2) 1)

; ** problem 8 (11 points)
; Write a procedure (repl exp1 exp2 exp3)
; that replaces every occurrence of expression exp1
; by expression exp2 in expression exp3.

; Examples
; (repl 'a 'b '(f a f a b e b a)) => '(f b f b b e b b)
; (repl 'a 'b '((f a) (f (a)))) => '((f b) (f (b)))
; (repl 'x '(first x) '(rest (rest x))) => '(rest (rest (first x)))
; (repl 'z 'a '(f a a b e b)) => '(f a a b e b)
; (repl '(f a) 'h '((f a) ((f a)) (((f a))))) => '(h (h) ((h)))

;Procedure to find if exp1 is in exp3


(define repl
  (lambda (exp1 exp2 exp3)
    (cond
          ((null? exp3) '())
          ((equal? exp1 exp3) exp2)
          ((not (list? exp3)) exp3)
          ((equal? exp1 (first exp3)) (cons exp2 (repl exp1 exp2 (rest exp3))))
          (else (cons (repl exp1 exp2 (first exp3)) (repl exp1 exp2 (rest exp3)))))))


; ** problem 9 (11 points)
; Write a procedure (find-in exp2 exp1)
; that returns (if possible) an expression
; consisting of the procedures
; cons, first, and rest
; and the variable sss, such that if exp2
; is substituted for sss and evaluated,
; the result is exp1.
; If this is not possible, return #f.

; Your procedure should not output the
; procedure list or any representation
; of an empty list (null, '())

; Be sure to check that your implementation
; passes all public test cases under the
; autograder.

; Examples
;( NOTE: If you find an alternate expression containing first, cons, rest and sss that evaluates
; correctly, your solution will be graded as correct)

; The results look complicated -- use deep recursion and helper functions rather
; than explicitly building complicated expressions directly in your code.
; Do not "hard wire" your code (i.e. don't just use conditional statements to check
;  the public examples and return the given response.) Hard wired responses will be given a zero
;  for the entire problem!!!!

; (find-in 'a 'a) => 'sss
; (find-in 'b 'a) => #f
; (find-in '(b c) 'a ) => #f
; (find-in '(a b c) 'a ) => '(first sss)
; (find-in '(a b c) '(b c) ) => '(rest sss)
; (find-in '(a b c) '(a c) ) => '(cons (first sss) (rest (rest sss)))
; (find-in '(a b c) '(c b) ) =>
;     '(cons (first (rest (rest sss))) (cons (first (rest sss)) (rest (rest (rest sss)))))
; (find-in '(c (b c) a b) '(b c) ) => (first (rest sss))
; (find-in '(s q l (a (b c) d) q) '((b c) d)) => (rest (first (rest (rest (rest sss)))))
; (find-in '(s '(((b)) a) (g h)) '( (b h) a s)) =>
;  '(cons
;   (cons (first (first (first (first (rest (first (rest sss))))))) (rest (first (rest (rest sss)))))
;   (cons (first (rest (first (rest (first (rest sss)))))) (cons (first sss) (rest (first (first (first (rest (first (rest sss))))))))))
; (find-in '(x a (x y)) '(z a (b))) => #f


(define find-in
  (lambda (exp2 exp1)
    (cond
      ((equal? exp2 exp1) 'sss)
      ((null? exp2) #f)
      ((not(list? exp2)) #f)
      ((find-in (first exp2) exp1) (repl 'sss '(first sss) (find-in (first exp2) exp1)))
      ((find-in (rest exp2) exp1) (repl 'sss '(rest sss) (find-in (rest exp2) exp1)))
      ((not (list? exp1)) #f)
      ((null? exp1) #f)      
      ((and (find-in exp2 (first exp1)) (find-in exp2 (rest exp1))) (list 'cons (find-in exp2 (first exp1)) (find-in exp2 (rest exp1))))
      (else #f)
      )
    ))


          
