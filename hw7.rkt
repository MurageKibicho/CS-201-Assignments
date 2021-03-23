#lang racket
(require racket/trace)

(provide
 isort
 msort
 qsort
 key<=?
 random-list
 lookup
 depth
 update
 update-table-tree
 binary-table-tree
 lookup-table-tree
 )

; ********************************************************************
; Name: Murage Kibicho
; Email address: murage.kibicho@yale.edu
; ********************************************************************

; CS 201a HW #7  DUE 5:00 pm Thursday, Dec 10, 2020
; end of Reading Period using the submit command.

; Here is a library  and some code that will allow you to
; time the execution of procedures.


; include library and procedure definitions for timings
(#%require srfi/19)

(define make-timer
  (lambda ()
    (let ((start-time (current-time)))
      (lambda ()
	(let ((elapsed (time-difference (current-time) start-time)))
	  (+ (exact->inexact (time-second elapsed))
	     (/ (exact->inexact (time-nanosecond elapsed))
		(* 1.0e3 (time-resolution)))))))))
; example of using make-timer to
; create a procedure for timing a sort of time testsort

(define time-sorter
  (lambda (sorter lst compare?)
    (let ((t1 (make-timer)))
      (sorter lst compare?)
      (t1))))



; Unless the problem specifies otherwise:
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.
; * Your procedures need *not* fail gracefully if their
; inputs do not satisfy the assumptions of the problem.
; * You may assume that the input is given as specified,
; you do not need to include tests in your code to verify
; that the input is valid.
; * Please use the specified names (and numbers of arguments)
; for the required procedures in a problem; this is to
; facilitate automatic testing.
; * Do NOT use MUTATORS (ie. set!)
; * Do NOT use "define" in the body of a procedure definition
; * Do NOT use take, drop, split-at procedures in your solutions.
; Use the procedures we have been using in class and on other
; assignments (i.e. if you can't find a procedure in class notes
; or another assignment, don't use it.)
; * You may and will need to use the predicates string<=? and string>=? as arguments for testing some of
; the sorting of unordered lists.

; ************************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 20)

; ************************************************************************
; ** problem 1 ** (16 points)
; Write two procedures for insertion sort:
; (insert item lst compare?)
; (isort lst compare?)
; where lst is a list and compare? is a procedure to
; compare two list elements and return either #t or #f.
; Insertion sort was described in lecture, and a version
; of the code for insert is in the pdf notes. You need to generalize
; that code for a general predicate compare? .


; (insert item lst compare?) returns a list equal to lst
; with item inserted in the correct order according to compare?,
; assuming that lst is already correctly ordered by compare?
; (isort lst compare?) insertion sorts lst using insert and
; compare? and returns the correctly sorted list.

; Examples:
; (insert "alonzo" '("algorithm" "program") string<=?) => '("algorithm" "alonzo" "program")
; (insert 7 '(2 3 5 11 13 17) <=) => '(2 3 5 7 11 13 17)
; (isort '(1 3 5 7 2 4 6 8) >=) => '(8 7 6 5 4 3 2 1)
; (isort '(1 3 5 7 2 4 6 8) <=) => '(1 2 3 4 5 6 7 8)
; ************************************************************************

(define insert-string
  (lambda (item lst compare?)
    (cond
      ((null? lst) (list item))
      ((compare? item (first lst)) (cons item lst))
      (else (cons (first lst) (insert-string item (rest lst) compare?))))))


(define insert
  (lambda (item lst compare?)
    (cond
      ((null? lst) (list item))
      (else (insert-string item lst compare?))
      )))

(define isort-helper
  (lambda (lst compare? blt-lst)
    (cond
      ((null? lst) blt-lst)
      (else (isort-helper (rest lst) compare? (insert (first lst) blt-lst compare?)))
      )))



(define isort
  (lambda (lst compare?)
    (isort-helper lst compare? '())))





; ************************************************************************
; ** problem 2 ** (16 points)
; Write two procedures for merge sort:
; (merge lst1 lst2 compare?)
; (msort lst compare?)
; where lst, lst1 and lst2 are lists and
; compare? is a procedure that compares two list
; elements and returns either #t or #f.
; Merge sort was described in lecture, and code
; can be found the pdf notes. Again you need to
; generalize for the predicate compare? .

; (merge lst1 lst2 compare?)
; returns a list containing all the elements of lst1 and lst2
; correctly ordered according to compare?,
; assuming that lst1 and lst2 are each correctly ordered according
; to compare?
; (msort lst compare?) merge sorts lst using merge and compare? and
; returns the correctly sorted list.

; Examples:
; (merge '(1 14 55 124) '(2 3 150 155) <=) => '(1 2 3 14 55 124 150 155)
; (merge '(124 55 14 1) '(155 150 3 2) >=) => '(155 150 124 55 14 3 2 1)
; (merge '("that" "is" "best") '("where" "were" "they") string>=?) => '("where" "were" "they" "that" "is" "best")
; (merge '("what" "is" "correct") '("where" "was" "it") string>=?) => '("where" "what" "was" "it" "is" "correct")
; (msort '(14 15 2 99 33 100 16) >=) => '(100 99 33 16 15 14 2)
; (msort '(14 15 2 99 33 100 16) <=) => '(2 14 15 16 33 99 100)
; (msort '("is" "best" "where" "they" "that" "were") string<=?) => '("best" "is" "that" "they" "were" "where")
; ************************************************************************

(define merge-helper
  (lambda (lst1 lst2 compare?)
    (cond
      ((null? lst1) lst2)
      ((null? lst2) lst1)
      ((compare? (first lst1) (first lst2)) (cons (first lst1) (merge-helper (rest lst1) lst2 compare?)))
      (else (cons (first lst2) (merge-helper lst1 (rest lst2) compare?))))))


      

(define merge
  (lambda (lst1 lst2 compare?)
    (merge-helper lst1 lst2 compare?)
      ))
;(merge-less '(14 15 2 99) '(33 100 16) )

(define split-helper
  (lambda (lst blt-lst)
    (cond
      ((null? lst) blt-lst)
      (else (append blt-lst (list (list (first lst))) (split-helper (rest lst) blt-lst)))
      )))
;(split-helper '(14 15 2 33 99 100 16) '()) => '((14) (15) (2) (33) (99) (100) (16))

(define msort-helper-one
  (lambda (lst blt-lst compare?)
    (cond
      ((null? lst) blt-lst)
      ((< (length lst) 2) (cons lst '()))
      (else (append blt-lst (cons (merge (first (split-helper lst '())) (second (split-helper lst '())) compare?) (msort-helper-one (rest (rest lst)) blt-lst compare?))))
      )))

;(msort-helper-one'("is" "best" "where" "they" "that" "were") '() string<=?) => '(("best" "is") ("they" "where") ("that" "were"))

(define msort-helper-two
  (lambda (lst blt-lst compare?)
    (cond
      ((null? lst) blt-lst)
      ((< (length lst) 2) (cons lst '()))
      (else (append blt-lst (cons (merge (first lst) (second lst) compare?) (msort-helper-two (rest (rest lst)) blt-lst compare?))))
     ; (else (append blt-lst (append (merge (first (msort-helper-one lst '() compare?)) (second (msort-helper-one lst '() compare?)) compare?))))
      )))
;(trace msort-helper-two)
;(msort-helper-two '((14 15) (2 99) (33 100) (16)) '() <=) => '((2 14 15 99) (16 33 100))
;(msort-helper-two (msort-helper-one '(14 15 2 99 33 100 16) '() <=) '() <=)
;(msort-helper '((14 15) (2 99) (33 100)) '() <=)

(define msort-helper-three
  (lambda (lst compare?)
    (cond
      ((list? (first (second lst))) (merge (first lst) (first (second lst)) compare?))
      (else (merge (first lst) (second lst) compare?))
      )))

(define msort
  (lambda (lst compare?)
    (msort-helper-three (msort-helper-two (msort-helper-one lst '() compare?) '() compare?) compare?)
    ))
;(trace merge)
;(trace merge-less)
;(msort '(14 15 2 99 33 100 16 17) <=)
;(trace merge-less-string)
;(msort '("is" "best" "where" "they" "that" "were") string<=?)



; ************************************************************************
; *** problem 3 ** 12 pts
; ************************************************************************

; Here is a procedure for quicksort, from 
; http://blog.matthewrathbone.com/
;
(define pHelper (lambda (all chk l m)
                  (cond ((null? all) (cons l (cons chk (cons m '()))))
                        (else
                        (let ((x (first all)))
                          (if (<= x chk) 
                              (pHelper (rest all) chk (cons x l) m)
                              (pHelper (rest all) chk l (cons x m))))))))

(define partition (lambda (l)
                      (pHelper (rest l) (first l) '() '())))



(define quicksort (lambda (l)
                    (cond ((null? l) l)
                          (else
                          (let ((lx (partition l)))
                            (append (quicksort (first lx)) (cons (first (rest lx)) (quicksort (first (rest (rest lx)))))))))))


; Modify the code for quicksort to write a procedure qsort that takes
; a comparison function compare? as a parameter.

; Examples:
; (qsort '("is" "best" "where" "they" "that" "were") string<=?) => '("best" "is" "that" "they" "were" "where")
; (qsort '(9 21 43 6 7 435) >=) => '(435 43 21 9 7 6)
; ************************************************************************

;(quicksort '("is" "best" "where" "they" "that" "were"))
(define pHelper-string (lambda (all chk l m)
                  (cond ((null? all) (cons l (cons chk (cons m '()))))
                        (else
                        (let ((x (first all)))
                          (if (string<=? x chk) 
                              (pHelper-string (rest all) chk (cons x l) m)
                              (pHelper-string (rest all) chk l (cons x m))))))))

(define partition-string (lambda (l)
                      (pHelper-string (rest l) (first l) '() '())))

(define quicksort-string (lambda (l)
                    (cond ((null? l) l)
                          (else
                          (let ((lx (partition-string l)))
                            (append (quicksort-string (first lx)) (cons (first (rest lx)) (quicksort-string (first (rest (rest lx)))))))))))

(define pHelper-greater (lambda (all chk l m)
                  (cond ((null? all) (cons l (cons chk (cons m '()))))
                        (else
                        (let ((x (first all)))
                          (if (>= x chk) 
                              (pHelper-greater (rest all) chk (cons x l) m)
                              (pHelper-greater (rest all) chk l (cons x m))))))))

(define partition-greater (lambda (l)
                      (pHelper-greater (rest l) (first l) '() '())))



(define quicksort-greater (lambda (l)
                    (cond ((null? l) l)
                          (else
                          (let ((lx (partition-greater l)))
                            (append (quicksort-greater (first lx)) (cons (first (rest lx)) (quicksort-greater (first (rest (rest lx)))))))))))


(define pHelper-greater-string (lambda (all chk l m compare?)
                  (cond ((null? all) (cons l (cons chk (cons m '()))))
                        (else
                        (let ((x (first all)))
                          (if (compare? x chk) 
                              (pHelper-greater-string (rest all) chk (cons x l) m compare?)
                              (pHelper-greater-string (rest all) chk l (cons x m) compare?)))))))

(define partition-greater-string (lambda (l compare?)
                      (pHelper-greater-string (rest l) (first l) '() '() compare?)))



(define quicksort-greater-string (lambda (l compare?)
                    (cond ((null? l) l)
                          (else
                          (let ((lx (partition-greater-string l compare?)))
                            (append (quicksort-greater-string (first lx) compare?) (cons (first (rest lx)) (quicksort-greater-string (first (rest (rest lx))) compare?))))))))

(define pHelper-key (lambda (all chk l m)
                  (cond ((null? all) (cons l (cons chk (cons m '()))))
                        (else
                        (let ((x (first all)))
                          (if (<= (first x) (first chk)) 
                              (pHelper-key (rest all) chk (cons x l) m)
                              (pHelper-key (rest all) chk l (cons x m))))))))

(define partition-key (lambda (l)
                      (pHelper-key (rest l) (first l) '() '())))



(define key<=? (lambda (l)
                    (cond ((null? l) l)
                          (else
                          (let ((lx (partition-key l)))
                            (append (key<=? (first lx)) (cons (first (rest lx)) (key<=? (first (rest (rest lx)))))))))))

(define qsort
  (lambda (l compare?)
    (cond
      ((equal? compare? key<=?) (key<=? l))
      (else (quicksort-greater-string l compare?))
      )))
;(qsort '("is" "best" "where" "they" "that" "were") string>=?)
;(qsort '(9 21 43 6 7 435) <=)




; ************************************************************************
; Consider sorting a table of values given by a list of two items (key value) 
; to put the list in order of ascending key value. The keys will all be integers.
; Write a procedure (key<=? lst1 lst2) that will produce the following:
;
; (qsort '((234 "no") (3 "aaa") (324 "foo") (9 "cat") (5 "dog")) key<=?)
;    => '((3 "aaa") (5 "dog") (9 "cat") (234 "no") (324 "foo"))
; (qsort '((234 234) (37 2345) (23 99900) (1 444) (3 2466) (9 2341)) key<=?)
;    => '((1 444) (3 2466) (9 2341) (23 99900) (37 2345) (234 234))
; ************************************************************************
;(trace pHelper)





;(qsort '((234 "no") (3 "aaa") (324 "foo") (9 "cat") (5 "dog")) key<=?)
;(qsort '((234 234) (37 2345) (23 99900) (1 444) (3 2466) (9 2341)) key<=?)







; ************************************************************************
; ** problem 4 ** (11 points)
; Create a procedure (random-list) to generate a list of n random numbers,
; each of which ranges from [0,n-1]
; We will use this to generate lots of lists to use in timing tests of
; our sorting procedures.

; Examples:
; Since random numbers are generated, your results will vary
; note that the same number may (but does not have to) appear several times in the list

;(random-list 10) => '(7 2 1 7 0 3 8 7 2 8)
;(random-list 10) => '(4 0 7 4 5 4 2 7 1 6)
;(random-list 20) => '(7 11 5 19 17 4 15 16 7 15 12 4 3 6 19 3 10 9 18 9)
; ************************************************************************
;(random 1)

(define random-list-helper
  (lambda (n index blt-lst)
    (cond
      ((zero? n) blt-lst)
      (else (random-list-helper (- n 1) index (append blt-lst (list (random index)))))
      )))

(define random-list
  (lambda (n)
    (random-list-helper n n '())))
;(random-list 10)








; ************************************************************************
; ** problem 5 ** (11 points)
; Use the procedure random-list to create some test lists of integers.
; Use these test lists to create a table comparing timings for isort, msort and
; qsort. Test how the timing varies as the length of list varies;
; Test whether the comparison operator (i.e. <=, <, >= or > ) makes
; difference in your results. 
; For your results show:
;      Your table
;      a conclusion about the relative efficiency of the sort methods
;      a conclusion about whether the comparison operator mattered
; Your results should be summarized as either a text file or a pdf file
; named timings.txt or timings.pdf file

; some examples with times from my Mac:
; (define sample-test (random-list 5000))
; (time-sorter isort sample-test <=) => 0.716
; (time-sorter msort sample-test <=) => 0.019
; (time-sorter qsort sample-test <=) => 0.005
; (time-sorter isort sample-test >=) => 0.427
; (time-sorter msort sample-test >=) => 0.006
; (time-sorter qsort sample-test >=) => 0.017

; (define sample2-test (random-list 50000))
; (time-sorter qsort sample2-test <=) => 0.122

; some examples with times from an old machine in the Zoo
; (define another-test (random-list 100000))
; (time-sorter msort another-test >=) => 0.217
; (time-sorter isort another-test >=) => 209.125
; (time-sorter qsort another-test >=) => 0.172
; ************************************************************************

;(time-sorter msort '( 5 8 7))

;(define sample-test (random-list 6000))
;(time-sorter isort sample-test <=) => 2.093
;(time-sorter msort sample-test <=) => 5.276
;(time-sorter qsort sample-test <=) => 0.014
;(time-sorter isort sample-test >=) => 1.878
;(time-sorter msort sample-test >=) => 2.84
;(time-sorter qsort sample-test >=) => 0.021
;(time-sorter isort sample-test <) => 1.834
;(time-sorter msort sample-test <) => 7.254
;(time-sorter qsort sample-test <) => 0.011
;(time-sorter isort sample-test >) => 1.925
;(time-sorter msort sample-test >) => 2.495
;(time-sorter qsort sample-test >) => 0.013

;(define sample-test2 (random-list 10000))
;(time-sorter isort sample-test2 <=) =? 3.937
;(time-sorter msort sample-test2 <=) => 11.192
;(time-sorter qsort sample-test2 <=) => 0.032
;(time-sorter isort sample-test2 >=) => 4.642
;(time-sorter msort sample-test2 >=) => 11.281
;(time-sorter qsort sample-test2 >=) => 0.02
;(time-sorter isort sample-test2 <) => 4.796 
;(time-sorter msort sample-test2 <) => 11.235
;(time-sorter qsort sample-test2 <) => 0.03
;(time-sorter isort sample-test2 >) => 4.552
;(time-sorter msort sample-test2 >) => 10.85
;(time-sorter qsort sample-test2 >) => 0.021









; ************************************************************************
; ** problem 6 ** (11 points)
; We define a Racket data structure for binary search trees
; using lists as follows.
; In this problem and problem 7, we will consider lists of integers only.
; The empty binary search tree is the empty list ()
; A nonempty binary search tree is a list consisting of
; a number (the root), a binary search tree (the left subtree)
; and another binary search tree (the right subtree).
; All the numbers appearing in the left subtree are less than
; the root, and the root is less than or equal to all the numbers appearing
; in the right subtree.


; For example, the following binary search tree can be drawn
;              7
;            /   \
;           5     9

(define tree1 '(7 (5 () ()) (9 () ())))

; Make use of these selectors for a binary search tree.

(define root car)
(define left-subtree cadr)
(define right-subtree caddr)


;(root (right-subtree tree1))

; Write a procedure
; (lookup n tree)
; to look up a number n in the given binary search tree.
; It should return #t if found, #f if not found.
; Please use the selectors defined above.

; Examples:
; (lookup 7 tree1) => #t
; (lookup 6 tree1) => #f
; (lookup 5 tree1) => #t
; (lookup 9 tree1) => #t
; (lookup 11 tree1) => #f
; ************************************************************************

(define lookup
  (lambda (n tree)
    (cond
      ((equal? n (root (left-subtree tree))) #t)
      ((equal? n (root (right-subtree tree))) #t)
      ((equal? n (root tree)) #t)
      (else #f)
      )))








; ************************************************************************
; ** problem 7 ** (11 points)
; Write two procedures
; (depth tree)
; (update tree n)
; where n is a number and tree is a binary search tree

; (depth tree) should return the maximum number of comparisons
; required to look up a number in tree -- that is, the length
; of the longest search path in tree.
; Note: the depth of an empty tree is 0.

; (update tree n) should return a binary search tree equal to tree
; with n inserted in the correct position. You may assume that
; n is not already in the tree.

; Examples:
;(update tree1 6) => '(7 (5 () (6 () ())) (9 () ()))
 ;(update (update tree1 8) 11) => '(7 (5 () ()) (9 (8 () ()) (11 () ())))
; (depth tree1) => 2
; (depth '()) => 0
; (depth (update tree1 6)) => 3
; ************************************************************************
(define depth-helper
  (lambda (tree )
    (cond
      ((null? tree) 0)
      (else (+ 1 (if (> (depth-helper (right-subtree tree)) (depth-helper (left-subtree tree)))
                        (depth-helper (right-subtree tree)) (depth-helper (left-subtree tree)))
                 ))
      )))

(define depth
  (lambda (tree)
    (cond
      ((null? tree) 0)
      (else (depth-helper tree))
      )))


(define update-helper
  (lambda (tree n)
    (cond
      ((null? tree) (list n '() '()))
      ((> n (root tree)) (list (root tree) (left-subtree tree) (update-helper (right-subtree tree) n)))
      (else (list (root tree) (update-helper (left-subtree tree) n) (right-subtree tree)))
      )))

(define update
  (lambda (tree n)
    (update-helper tree n)
    ))


; ************************************************************************
; ** problem 8 ** (11 points)
; 
; We expand the use of binary trees to looking up values in tables.
; Write a procedure (binary-table-tree table compare-key?) that takes a look-up table
; in the form of a list of (key value) and produces a binary tree of key-values
; by recursively calling a procedure (update-table-tree table-tree table-item compare-key?)
; where (compare-key? key1 key2) returns #t if the table-item with key1 should appear
; before the item with key2 in the sorted list.

; Examples:
; (update-table-tree '((7 "y") () ()) '(5 "z") <=) => '((7 "y") ((5 "z") () ()) ())
; (update-table-tree  '(("icelandic" "íkorni") (("dutch" "eekhoorn") ()()) ()) '("finnish" "orava") string<=?)
;'(("icelandic" "íkorni") (("dutch" "eekhoorn") () (("finnish" "orava") () ())) ())
;'(("icelandic" "íkorni") (("dutch" "eekhoorn") () (("finnish" "orava") () ())) ())

(define test-table '((9 "a") (3 "b") (5 "c") (4 "q") (1 "z") (7 "y") ))
(define translate-table '(("bulgarian" " kateritsa") ("estonian" "orav")
                          ("finnish" "orava")("french" "ecureuil") ("welsh" "gwiwer")
                          ("german" "eichhoernchen") ("italian" "scoiattolo")
                          ("lithuanian" "vovere") ("portuguese" "esquilo") ("romanian" "veverita")
                          ("slovak" "vevericka") ("swedish" "ekorre") ("polish" "wiewiorka")
                          ("dutch" "eekhoorn") ("norwegian" "ekorn") ("irish" "iora rua")
                          ("icelandic" "íkorni")))

; The binary tree you build should have the last element in the list as the root.
; Here are examples with you should get for the test tables.


; (binary-table-tree translate-table string<=?)
;  => '(("icelandic" "íkorni") (("dutch" "eekhoorn") (("bulgarian" " kateritsa") () ()) (("german" "eichhoernchen") (("french" "ecureuil") (("finnish" "orava") (("estonian" "orav") () ()) ()) ()) ())) (("irish" "iora rua") () (("norwegian" "ekorn") (("lithuanian" "vovere") (("italian" "scoiattolo") () ()) ()) (("polish" "wiewiorka") () (("swedish" "ekorre") (("slovak" "vevericka") (("romanian" "veverita") (("portuguese" "esquilo") () ()) ()) ()) (("welsh" "gwiwer") () ()))))))
; ************************************************************************
;(first '(("icelandic" "íkorni") (("dutch" "eekhoorn") ()()) ()))

(define compare-less
  (lambda (tree n compare-key?)
    (cond
      ((null? tree) (list n '() '()))
      ((compare-key? (first n) (root (root tree))) (list (root tree) (compare-less (left-subtree tree) n compare-key?) (right-subtree tree))
       )
      (else (list (root tree) (left-subtree tree) (compare-less (right-subtree tree) n compare-key?)))
      )))

(define update-table-tree
  (lambda (table-tree table-item compare-key?)
    (compare-less table-tree table-item compare-key?)
      ))

;(update-table-tree '((7 "y") () ()) '(5 "z") >=)
;(trace compare-less)
;(update-table-tree  '(("icelandic" "íkorni") (("dutch" "eekhoorn") ()()) ()) '("finnish" "orava") string<=?)

;(update-table-tree '() '(5 "z") <=)
(define binary-helper
  (lambda (table compare-key? blt-tree)
    (cond
      ((null? table) blt-tree)
      (else  (binary-helper (rest table) compare-key? (update-table-tree blt-tree (first table) compare-key?)))
      )))

(define binary-table-tree
  (lambda (table compare-key?)
    (binary-helper (reverse table) compare-key? '())
      ))
;(binary-table-tree translate-table string<=?)








; ************************************************************************
; Write a procedure (lookup-table-tree key tree compare-key?) that returns the value 
; associated with key in the table if it exists, and returns #f otherwise

; Examples:
 (define test-tree (binary-table-tree test-table <=))
; (lookup-table-tree 9 test-tree <=) => "a"
; (lookup-table-tree 15 test-tree <=)=> #f

(define test-translation (binary-table-tree translate-table string<=?))
; (lookup-table-tree "welsh" test-translation string<=?) => "gwiwer"
; (lookup-table-tree "romanian" test-translation string<=?) => "veverita"
; (lookup-table-tree "english" test-translation string<=?) => #f
; ************************************************************************

(define lookup-helper
  (lambda (tree n compare-key?)
    (cond
      ((null? tree) #f)
      ((equal? n (root (root tree))) (second (root tree)))
      ((compare-key? n (root (root tree))) (lookup-helper (left-subtree tree) n compare-key?) )
      (else (lookup-helper (right-subtree tree) n compare-key?))
      )))


(define lookup-table-tree
  (lambda (key tree compare-key?)
    (lookup-helper tree key compare-key?)))

;(lookup-table-tree 9 test-tree <=)
;(lookup-table-tree 15 test-tree <=)
;(lookup-table-tree "welsh" test-translation string<=?)
;(lookup-table-tree "romanian" test-translation string<=?)
;(lookup-table-tree "english" test-translation string<=?)









; ************************************************************************
;       _______. _______  _______    ____    ____  ______    __    __     __  .__   __.    ___     ___    ___    __     __   __
;      /       ||   ____||   ____|   \   \  /   / /  __  \  |  |  |  |   |  | |  \ |  |   |__ \   / _ \  |__ \  /_ |   |  | |  | 
;     |   (----`|  |__   |  |__       \   \/   / |  |  |  | |  |  |  |   |  | |   \|  |      ) | | | | |    ) |  | |   |  | |  | 
;      \   \    |   __|  |   __|       \_    _/  |  |  |  | |  |  |  |   |  | |  . `  |     / /  | | | |   / /   | |   |  | |  | 
;  .----)   |   |  |____ |  |____        |  |    |  `--'  | |  `--'  |   |  | |  |\   |    / /_  | |_| |  / /_   | |   |__| |__|
;  |_______/    |_______||_______|       |__|     \______/   \______/    |__| |__| \__|   |____|  \___/  |____|  |_|   (__) (__)


; ************************************************************************
; *** END OF HW #7; END OF HWs; END OF CPSC 201a, 2020!!!!!!!!!***********
; ************************************************************************
