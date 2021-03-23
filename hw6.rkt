#lang racket
(require racket/trace)

; ********************************************************************
; Name: Murage Kibicho
; Email address: murage.kibicho@yale.edu
; ********************************************************************

; CS 201a HW #6  DUE Wednesday, Nov 18 at 11:59 pm
; electronically, using the submit command.  
; Do *not* email your homework -- lateness penalties (5 points per day)
; will accrue until you successfully submit it using your Zoo account.

; Topics: strings, regular expressions, finite-state
; acceptors, and context free grammars

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
; Use "The Racket Language" NOT R5RS.
; DO NOT use mutators. Only use syntax introduced in lectures or hw assignment files.

; ********************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment (including reading):

(define hours 20)

; ********************************************************************
; Representation of finite strings
; ********************************************************************
; Rather than use Racket strings to represent strings, which
; would limit the set of symbols to ASCII characters, we
; define our own representation of Finite Strings as follows.

; A Finite String of symbols is represented by a list of Racket symbols.
; Thus, (a b a b b a) represents a Finite String with six symbols,
; the first, third and sixth being a's and the rest b's.
; We'll also consider (the dog barked at the other dog) 
; as a Finite String of seven symbols.
; The empty Finite String is denoted by the empty list ().
; Note that (append str1 str2) concatenates two
; Finite Strings str1 and str2.
; 
; ********************************************************************
; ** problem 1 ** (11 points)
; Define a procedure (product lst1 lst2)
; that takes two lists lst1 and lst2 of Finite Strings
; and returns a list of all the strings that
; can be obtained by concatenating a string
; from lst1 with a string from lst2.
; The returned list should contain no duplicates.

; Examples
; (product '() '((a) (a b))) => '()
; (product '(()) '((a) (a b))) => '((a) (a b))
; (product '((a) (a b)) '((c d))) => '((a c d) (a b c d))
; (product '((a) (a a)) '((a) (a a))) => '((a a) (a a a) (a a a a))
; (product '((a b) (b c)) '((a b) (d e))) =>
;    '((a b a b) (a b d e) (b c a b) (b c d e))
; (product '(()) '(())) => '(())
; (product '((the)) '((small dog) (large cat))) =>
;     '((the small dog) (the large cat))
; (product '((small) (large)) '((dog) (cat))) =>
;     '((small dog) (small cat) (large dog) (large cat))


;(define combiner
 ; (lambda (lst1 lst2)
  ;  (cond
   ;   ((null? lst1) lst2)
    ;  (else (append (map (lambda (element) (cons (first lst1) element)) (combiner (rest lst1) lst2)) (combiner (rest lst1) lst2)))
     ; )))

(define combiner
  (lambda (lst1 lst2)
    (cond
      ((null? lst1) '())
      (else (append (map (lambda (element) (append (first lst1) element)) lst2) (combiner (rest lst1) lst2)))
      )))
;(combiner '((a b) (b c)) '((a b) (d e))) => '((a b a b) (a b d e) (b c a b) (b c d e))
;(combiner  '((a) (a a)) '((a) (a a))) => '((a a) (a a a) (a a a) (a a a a))
(define copy-remove
  (lambda (lst1)
    (cond
    ((null? (rest lst1)) lst1)
    ((not (equal? (list-ref lst1 0) (list-ref lst1 1))) (cons (first lst1) (copy-remove (rest lst1))))
    (else (cons (first lst1) (copy-remove (rest (rest lst1)))))
    )))
;(trace copy-remove)
;(copy-remove '((a a) (a a a) (a a a) (a a a a))) => '((a a) (a a a) (a a a a))

(define product
  (lambda (lst1 lst2)
    (cond
      ((null? lst1) '())
      ((equal? lst1 '(())) lst2)
      (else (copy-remove (combiner lst1 lst2)))
      )))





; ********************************************************************
; Representation of Regular Expressions
; ********************************************************************
; Regular Expressions will be represented as follows.
; 1) A Finite String (defined above) is a regular expression; for these
; purposes, the string may not contain the symbols !, +, or *.
; 2) The concatenation of two or more expressions is represented by
; a list starting with the symbol ! and containing the expressions.
; 3) The union of two or more expressions is represented
; by a list starting with + and containing the expressions.
; 4) The Kleene star of one expression is represented by a list
; starting with * and containing the expression to be starred.

; Some examples of regular expressions in this representation:

(define exp0 '())
(define exp1 '(a a b a b))
(define exp2 '(+ (a b) (b)))
(define exp3 '(! (* (b)) (a) (* (+ (b) (! (a) (* (b)) (a))))))
(define exp4 '(! (alas) (+ (and) (or)) (alack)))
(define exp5 '(! (the sleeping) (+ (dog) (cat) (beast))))
(define exp6 '(* (+ (a) (b))))
(define exp7 '(! (a) (* (+ (very) (quite))) (long) (+ (story) (tale))))

; ********************************************************************
; ** problem 2 ** (11 points)
; Write predicate and selector procedures for regular expressions exp.
; You may assume that the input is Regular Expression defined as above.

; (re-string? exp) should return #t iff exp is a Finite String
; (re-concatenation? exp) should return #t iff exp is a concatenation
; (re-union? exp) should return #t iff exp is a union
; (re-star? exp) should return #t iff exp is a Kleene star expression
; otherwise, #f should be returned by these functions

; If exp is a concatenation then
; (op exp) should return the symbol ! and (args exp) should return the list
; of its arguments
; If exp is a union then
; (op exp) should return the symbol + and (args exp) should return the list
; of its arguments
; If exp is a Kleene star expression, then
; (op exp) should return the symbol * and (arg exp) should return its argument

; After this problem, use these procedures to test the type and
; refer to the parts of regular expressions.

; Examples
; (re-string? exp0) => #t
; (re-string? exp1) => #t
; (re-string? exp2) => #f
; (re-string? exp4) => #f
; (re-concatenation? exp0) => #f
; (re-concatenation? exp1) => #f
; (re-concatenation? exp2) => #f
; (re-concatenation? exp3) => #t
; (re-concatenation? exp6) => #f
; (re-union? exp0) => #f
; (re-union? exp1) => #f
; (re-union? exp2) => #t
; (re-union? exp3) => #f
; (re-star? exp0) => #f
; (re-star? exp1) => #f
; (re-star? exp2) => #f
; (re-star? exp6) => #t
; (op exp2) => '+
; (op exp3) => '!
; (op exp6) => '*
; (args exp2) => '((a b) (b))
; (args exp3) => '((* (b)) (a) (* (+ (b) (! (a) (* (b)) (a)))))
; (arg exp6) => '(+ (a) (b))



(define re-string?
  (lambda (exp)
    (cond
      ((null? exp) #t)
      ((not (or (equal? '! (first exp)) (equal? '* (first exp)) (equal? '+ (first exp)) (equal? '* (first exp)))) (re-string? (rest exp)))
      (else #f)
      )))

(define re-concatenation?
  (lambda (exp)
    (cond
      ((null? exp) #f)
      ((equal? '! (first exp)) #t)
      (else #f)
      )))

(define re-union?
  (lambda (exp)
    (cond
      ((null? exp) #f)
      ((equal? '+ (first exp)) #t)
      (else #f)
      )))

(define re-star?
  (lambda (exp)
    (cond
      ((null? exp) #f)
      ((equal? '* (first exp)) #t)
      (else #f)
      )))

(define op
  (lambda (exp)
    (first exp)))

(define args
  (lambda (exp)
    (rest exp)))

(define arg
  (lambda (exp)
    (first (rest exp))))





; ********************************************************************
; ** problem 3 ** (11 points)
; Write two procedure (pick lst) and (pick-string exp)
; as follows.

; (pick lst) takes a list lst and
; returns a uniformly chosen random element from lst.
; If lst is the null list, your procedure should return #f.

; Recall Racket procedure
; (random n) returns a randomly chosen
; integer between 0 and n-1 inclusive.

; (pick-string exp) takes a regular expression exp
; and returns a randomly chosen Finite String in the
; language of exp.  Just how you do this is up to you,
; but your procedure should be capable of generating
; any Finite String in the language, and none that aren't.

; You will want to use recursion on the structure of the
; expression for pick-string.

; Examples (Your mileage may vary!):
; (pick '(a b c)) => 'a
; (pick '(a b c)) => 'c
; (pick '(b)) => 'b
; (pick '(a a a b b b c c)) => 'b
; (pick '(5 7 9)) => 5
; (pick '(5 7 9)) => 9
; (pick '()) => #f

;(define exp0 '())
;(define exp1 '(a a b a b))
;(define exp2 '(+ (a b) (b)))
;(define exp3 '(! (* (b)) (a) (* (+ (b) (! (a) (* (b)) (a))))))
;(define exp4 '(! (alas) (+ (and) (or)) (alack)))
;(define exp5 '(! (the sleeping) (+ (dog) (cat) (beast))))
;(define exp6 '(* (+ (a) (b))))
;(define exp7 '(! (a) (* (+ (very) (quite))) (long) (+ (story) (tale))))
(define exp9 '(! (alas) (+ (and) (or)) (+ (back) (board)) (alack)))
; (pick-string exp0) => '()
; (pick-string exp1) => '(a a b a b)
; (pick-string exp2) => '(b)
; (pick-string exp2) => '(a b)
; (pick-string exp3) => '(b a b a a)
; (pick-string exp3) => '(b b b b a b b b a b b b b b a)
; (pick-string exp3) => '(a)
; (pick-string exp4) => '(alas or alack)
; (pick-string exp4) => '(alas and alack)
; (pick-string exp5) => '(the sleeping dog)
; (pick-string exp5) => '(the sleeping cat)
; (pick-string exp6) => '(b a b a a a)
; (pick-string exp6) => '(a a a b)
; (pick-string exp6) => '(b b)
; (pick-string exp7) => '(a quite quite very very very long tale)
; (pick-string exp7) => '(a long story)


(define pick
 (lambda (lst)
   (cond
     ((null? lst) #f)
     (else (list-ref lst (random (length lst))))
     )))


(define pick-string
  (lambda (exp)
    (cond
      ((null? exp) '())
      ((list? (first exp)) (append (pick-string (first exp)) (pick-string (rest exp))))
      ((re-string? exp) exp)
      ((re-union? exp) (pick-string (list-ref (args exp) (random (length (args exp))))))
      ((re-concatenation? exp) (and (pick-string (first (rest exp))) (pick-string (rest exp))))
      ((re-star? exp) (cond
                        ((> (random 10) 6) '())
                        (else (append (pick-string (arg exp)) (pick-string exp)))
                        ))
     ; ((re-star? exp) exp)
      (else (apply append (map (pick-string (args exp)))))
      ))) 

;(pick-string exp2)





; ********************************************************************
; ** problem 4 ** (11 points)
; Write a procedure (reverse-exp exp) 
; to take a regular expression exp
; (in the representation above) and return another
; regular expression exp1 such that L(exp1) contains
; exactly the reverses of all the strings in L(exp).
; (Therefore the reverse of a regular language is regular.)

; Examples (Your mileage may vary!):
; (reverse-exp exp0) => '()
; (reverse-exp exp1) => '(b a b a a)
; (pick-string (reverse-exp exp2)) => '(b a)
; (pick-string (reverse-exp exp3)) => '(a b b b b b b b b b b a a)
; (pick-string (reverse-exp exp3)) => '(a)
; (pick-string (reverse-exp exp4)) => '(alack and alas)
; (pick-string (reverse-exp exp4)) => '(alack or alas)
; (pick-string (reverse-exp exp5)) => '(dog sleeping the)
; (pick-string (reverse-exp exp6)) => '(a a a b b)
; (pick-string (reverse-exp exp6)) => '()
; (pick-string (reverse-exp exp7)) => '(tale long a)
; (pick-string (reverse-exp exp7)) => '(story long quite very quite a)


(define reverse-exp-helper
  (lambda (exp)
    (cond
      ((null? exp) '())
      ((list? (first exp)) (list (reverse (first exp)) (reverse-exp-helper (rest exp))))
      (else (append (reverse-exp-helper (rest exp)) (list (first exp ))))
      )))


;(trace reverse-exp-helper)
;(reverse-exp exp2)
;(reverse-exp-helper '((a b) b)) => '((b a) (b))
(define reverse-exp
  (lambda (exp)
    (cond
      ((null? exp) '())
      ((list? (first exp)) (append (list (reverse-exp (first exp))) (reverse-exp (rest exp))))
      ((re-string? exp) (reverse-exp-helper exp))
      ((re-union? exp) (append (list '+) (reverse-exp (rest exp))))
      ((re-concatenation? exp) (append (list '!) (reverse-exp (reverse (rest exp)))))
      ((re-star? exp) (append (list '*) (reverse-exp (reverse (rest exp)))))
      )))



; ********************************************************************
; Representation of finite state acceptors
; ********************************************************************
; A Finite State Acceptor is represented by a list of
; three items: the start state, the list of accepting states,
; and a table giving the transition function.  Each entry
; in the table consists of a key (a list consisting
; of a state and a symbol) and a value (a state).

; For example, here is a Finite State Acceptor
; that accepts all strings of a's and b's with
; an odd number of a's:

(define fsa1 
  '(q0 (q1) (((q0 a) q1)  ((q1 a) q0)
	     ((q0 b) q0)  ((q1 b) q1))))

; Here is another acceptor, which accepts strings of a's and strings
; of b's, but no string that contains both a's and b's.

(define fsa2
  '(q0 (q0 q1 q2) (((q0 a) q1)  ((q1 a) q1)  ((q2 a) q3)  ((q3 a) q3)
		   ((q0 b) q2)  ((q1 b) q3)  ((q2 b) q2)  ((q3 b) q3))))


; ********************************************************************
; ** problem 5 ** (11 points)
; Write a procedure (accepts? fsa str)
; that takes a finite state acceptor fsa (as just specified)
; and a Finite String str,
; and returns #t if the acceptor accepts the string, #f otherwise.

; Examples:
; (accepts? fsa1 '(a b b a b a)) => #t
; (accepts? fsa1 '(b b b)) => #f
; (accepts? fsa1 '(b a b a)) => #f
; (accepts? fsa1 '(a)) => #t
; (accepts? fsa2 '()) => #t
; (accepts? fsa2 '(a a a a)) => #t
; (accepts? fsa2 '(a a b b)) => #f
; (accepts? fsa2 '(b b)) => #t
; (accepts? fsa2 '(b a b)) => #f


(define state-lookup
  (lambda (state lst-states)
    (cond
      ((null? lst-states) '())
      ((equal? state (first (first (first lst-states)))) (cons (first lst-states) (state-lookup state (rest lst-states))))
      (else (state-lookup state (rest lst-states)))
      )))
;(state-lookup (first fsa1) (last fsa1))
;(state-lookup 'q0 '(((q0 a) q1)  ((q1 a) q0) ((q0 b) q0)  ((q1 b) q1))) =? '(((q0 a) q1) ((q0 b) q0))

(define lett-lookup
  (lambda (lett lst-states)
    (cond
      ((null? lst-states) '())
      ((equal? lett (list-ref (first (first lst-states)) 1)) (first lst-states))
      (else (lett-lookup lett (rest lst-states)))
      )))
;(trace lett-lookup)
(define state-change
  (lambda (state)
    (cond
      ((null? state) #f)
      (else (list-ref state 1)))))
;(state-change '((q0 a) q1)) => q1

(define one-iteration
  (lambda (fsa curr-state lett)
    (let ((state (lett-lookup lett (state-lookup curr-state (list-ref fsa 2)))))
    (cond
    ((null? state) #f)
    (else (state-change state))
    ))))
;(trace one-iteration)
;(one-iteration fsa1 'q1 'b) => q1
;(one-iteration fsa1 'q1 'a)

;(define)


(define accept-help
  (lambda (fsa str lst-state)
    (cond
      ((null? str) lst-state)
      ((equal? #f (one-iteration fsa (first lst-state) (first str))) #f)
      (else (accept-help fsa (rest str) (append (list (one-iteration fsa (first lst-state) (first str))) lst-state)))
     ; (else (append (list (one-iteration fsa (first str))) lst-state (accept-help fsa (rest str) lst-state)))
      )))
;(accept-help fsa1 '(a b b a b a) '(q0)) => '(q1 q0 q0 q1 q0 q1) 
;(trace accept-help)

;(accept-help fsa1 '(b b b) '(q0)) => '(q0 q0 q0 q0) returns list of all states first is most recent 

(define member?
  (lambda (item ls)
    (cond
      ((null? ls) #f)
      ((equal? item (first ls)) #t)
      (else (member? item (rest ls))))))


(define accepts?
  (lambda (fsa str)
    (let ((last-state (first (accept-help fsa str (list (first fsa))))))
    (cond
      ((equal? #f last-state) #f)
      (else (member? last-state (second fsa)))
      ))))







; ********************************************************************
; ** problem 6 (11 points)
; Define a procedure (complement fsa)
; that takes a Finite State Acceptor
; and returns a Finite State Acceptor
; that accepts the strings that fsa
; rejects and vice versa.
; (Therefore the complement of a regular language is regular.)

; Examples
; (accepts? (complement fsa1) '(a b b a b a)) => #f
; (accepts? (complement fsa1) '(b b b)) => #t
; (accepts? (complement fsa2) '()) => #f
; (accepts? (complement fsa2) '(a a a a)) => #f
; (accepts? (complement fsa2) '(a a b b)) => #t

(define state-dictionary
  (lambda (lst-state built-lst)
    (cond
      ((null? lst-state) built-lst)
      (else (state-dictionary (rest lst-state) (append built-lst (list (last (first lst-state))))))
      )))
;(trace state-dictionary)
;(state-dictionary '(((q0 a) q1)  ((q1 a) q0)((q0 b) q0)  ((q1 b) q1)) '()) => '(q1 q0 q0 q1)



;(member? 'a '(a b c))
(define remove
  (lambda (itm ls)
  (cond
    ((not (member? itm ls)) ls)
    ((not (equal? itm (first ls)) ) (cons (first ls) (remove itm (rest ls))))
    (else (remove itm (rest ls)))
    )))
;(remove 'q1 '(q1 q0 q0 q1)) => '(q0 q0)

(define remove-dupes
  (lambda (lst)
  (cond
    ((null? lst) empty)
    ((member? (first lst) (rest lst)) (remove-dupes (rest lst)))
    (else (cons (first lst) (remove-dupes (rest lst))))
    )))
;(remove-dupes '(q3 q3 q3 q4 q4 q4)) => '(q3 q4)
(define swap-accepted
  (lambda (acc-states state-dict)
    (cond
      ((null? acc-states) state-dict)
      (else (swap-accepted (rest acc-states) (remove (first acc-states) state-dict)))
      )))
;(swap-accepted '(q0 q1 q2) (state-dictionary (last fsa2) '())) => '(q3 q3 q3 q3)

(define fsa-builder
  (lambda (fsa repeated)
    (list (first repeated) (remove-dupes (swap-accepted (second fsa) (state-dictionary (last fsa) '()))) (last repeated))
    ))


(define complement
  (lambda (fsa)
    (fsa-builder fsa fsa)
    ))
;(complement fsa2)





; ********************************************************************
; Representation of context free grammars
; ********************************************************************
; A Context Free Grammar is a list of two items:
; the start symbol of the grammar, and the list
; of rules of the grammar.

; Each rule is a list of two items:
; a nonterminal of the grammar (the lefthand side of the rule) and
; a Finite String of terminals or nonterminals of the grammar
; (the righthand side of the rule).
; We will assume that the nonterminals of the grammar
; are those that appear on the lefthand side of a rule, and
; that symbols that appear on the righthand side of some
; rule but not on the lefthand side of any rule are terminal symbols.

; Here are the rules of a grammar for palindromes
; over the alphabet {a, b}.


(define rule1 '(s ()))
(define rule2 '(s (a)))
(define rule3 '(s (b)))
(define rule4 '(s (a s a)))
(define rule5 '(s (b s b)))

; Here is the corresponding Context Free Grammar

(define cfg1 (list 's (list rule1 rule2 rule3 rule4 rule5)))

; This is a grammar for a fragment of English

(define rule2-1 '(<sentence> (<subject> <verb1>)))
(define rule2-2 '(<sentence> (<subject> <verb2> that <sentence>)))
(define rule2-3 '(<subject> (<article> <noun>)))
(define rule2-4 '(<subject> (<pronoun>)))
(define rule2-5 '(<noun> (woman)))
(define rule2-6 '(<noun> (man)))
(define rule2-7 '(<noun> (truth)))
(define rule2-8 '(<noun> (lizard)))
(define rule2-9 '(<pronoun> (it)))
(define rule2-10 '(<pronoun> (he)))
(define rule2-11 '(<pronoun> (she)))
(define rule2-12 '(<article> (the)))
(define rule2-13 '(<article> (a)))
(define rule2-14 '(<article> (every)))
(define rule2-15 '(<verb1> (exists)))
(define rule2-16 '(<verb1> (swims)))
(define rule2-17 '(<verb1> (pauses)))
(define rule2-18 '(<verb2> (believes)))
(define rule2-19 '(<verb2> (hopes)))
(define rule2-20 '(<verb2> (fears)))

; and the grammar:

(define cfg2
  (list '<sentence>
	(list rule2-1 rule2-2 rule2-3 rule2-4 rule2-5 rule2-6 rule2-7
	      rule2-8 rule2-9 rule2-10 rule2-11 rule2-12 rule2-13 rule2-14
	      rule2-15 rule2-16 rule2-17 rule2-18 rule2-19 rule2-20)))

; ********************************************************************
; ** problem 7 ** (11 points)

; Write a Context Free Grammar named
;     my-cfg
; in this representation
; to generate a set of strings that form one of the following:

; a.) An entry for the Bulwer-Lytton contest https://www.bulwer-lytton.com
;  Your sentence may be simpler, but the original sentence that inspired the contest was:
; 'It was a dark and stormy night; the rain fell in torrents —
; except at occasional intervals, when it was checked by a violent
; gust of wind which swept up the streets (for it is in London that our scene lies),
; rattling along the housetops, and fiercely agitating the scanty flame of the lamps that
; struggled against the darkness."
;
; b.) A brief description of a fictional Yale building similar to:
; https://visitorcenter.yale.edu/tours/architecture-yale
; Your description does not have to be as long, but should include the building
; name, year, architect as well as brief text. For example brief text for
; the Beinecke Library could be "At first decried by many as bombastic, it now
; has a strangely quiet presence in the midst of the vast granite-paved
; expanse of its plaza, punctuated by a sunken courtyard"

; c.) A "Blue Book" entry 
; This could including course number, title, and brief description, e.g.
; a string your grammar might produce would be:
; CPSC 201
; Introduction to Computer Science
; An exciting overview of the most important topic taught at Yale, illuminating
; the intellectual adventure inherent in learning Racket programming.
; 
; Your strings do not have to follow these patterns exactly,
; but the strings your grammar produces should be at least as complex as
; those produced cfg2. 
; You do not have to be concerned with punctuation or separating the entries into
; lines. Have fun with this!!!

(define rule3-1 '(<entry> (<title> <description> <verb1> <preposition> <noun2> <noun3>)))
(define rule3-2 '(<entry> (<title> <description> <noun2>  This is similar to <entry>)))
(define rule3-3 '(<title> (<name> <number>)))
(define rule3-4 '(<description> (<article> <adjective> <noun1> <adverb> <noun2> <noun3> <adverb>)))
(define rule3-5 '(<name> (LIT)))
(define rule3-6 '(<name> (HEB)))
(define rule3-7 '(<name> (MATH)))
(define rule3-8 '(<name> (AFAM)))
(define rule3-9 '(<number> (300)))
(define rule3-10 '(<number> (465)))
(define rule3-11 '(<number> (110)))
(define rule3-12 '(<article> (the)))
(define rule3-13 '(<article> (a)))
(define rule3-14 '(<verb1> (showing)))
(define rule3-15 '(<verb1> (viewing)))
(define rule3-16 '(<verb1> (defining)))
(define rule3-17 '(<verb1> (informing)))
(define rule3-18 '(<preposition> (in)))
(define rule3-19 '(<preposition> (of)))
(define rule3-20 '(<preposition> (about)))
(define rule3-21 '(<verb3> (dealing)))
(define rule3-22 '(<verb3> (picking)))
(define rule3-23 '(<adjective> (definite)))
(define rule3-24 '(<adjective> (fulfilling)))
(define rule3-25 '(<adjective> (misunderstood)))
(define rule3-26 '(<noun1> (dive)))
(define rule3-27 '(<noun1> (leap)))
(define rule3-28 '(<noun1> (guide)))
(define rule3-29 '(<noun2> (world)))
(define rule3-30 '(<noun2> (British)))
(define rule3-31 '(<noun2> (film)))
(define rule3-32 '(<noun3> (politics)))
(define rule3-33 '(<noun3> (history)))
(define rule3-34 '(<noun3> (achievements)))
(define rule3-35 '(<adverb> (within)))
(define rule3-36 '(<adverb> (beyond)))
(define rule3-37 '(<adverb> (in)))

(define my-cfg
  (list '<entry>
	(list rule3-1 rule3-2 rule3-3 rule3-4 rule3-5 rule3-6 rule3-7
	      rule3-8 rule3-9 rule3-10 rule3-11 rule3-12 rule3-13 rule3-14
	      rule3-15 rule3-16 rule3-17 rule3-18 rule3-19 rule3-20
              rule3-21 rule3-22 rule3-23 rule3-24 rule3-25 rule3-26
              rule3-27 rule3-28 rule3-29 rule3-30 rule3-31 rule3-32
              rule3-33 rule3-34 rule3-35 rule3-36 rule3-37 
              )))

;(equal? '<subject> (first (third (second my-cfg))))

; ********************************************************************
; ** problem  8 ** (22 points)
; Write a procedure (generate cfg)
; to generate a random Finite String
; in the language of the Context Free Grammar cfg.
; Every string in the language of the grammar should
; be a possible output of your program, and no string
; not in the language of the grammar should be a possible output.
; You may assume that every nonterminal in the grammar
; generates at least one string of terminals.
; (This is to avoid problems like (s (s (t)) (t (s))).)
; Recall the procedure (pick lst) that you wrote above.

; Please include explanations of how your procedures work.

; Examples:
; (generate cfg1) => '()
; (generate cfg1) => '(a b b b a)
; (generate cfg1) => '(a b a b b b a b a)
; (generate cfg2) => '(a lizard exists)
; (generate cfg2) => '(a truth hopes that she pauses)
; (generate cfg2) => '(he exists)
; (generate cfg2) => '(it exists)
; (generate cfg2) => '(she exists)
; (generate cfg2) => '(he fears that the man believes that it pauses)
; (generate cfg2) => '(the woman hopes that she exists)
; (generate cfg2) => '(it pauses)
; (generate cfg2) => '(she believes that every man hopes that it exists)

;(define grammar-lookup
 ; (lambda (grammar cfg)
  ;  (cond
   ;   ((null? cfg) '())
    ;  ((equal? grammar (first (first (first cfg)))) (cons (first cfg) (grammar-lookup grammar (rest cfg))))
     ; (else (grammar-lookup grammar (rest cfg)))
      ;)))

(define symbol-finder
  (lambda (sym rules)
    (cond
      ((null? rules ) #f)
      ((equal? sym (first (first rules))) #t)
      (else (symbol-finder sym  (rest rules)))
     )))
;(trace symbol-finder)
;(symbol-finder '<sentence> (cadr cfg2))
;returns leftmost eg '<sentence>
;(symbol-finder '<sentence> (cadr cfg2)) 

(define leftmost-nonterm
  (lambda (str cfg)
    (cond
      ((null? str) #f)
      ((symbol-finder (first str) (cadr cfg)) (first str))
      (else (leftmost-nonterm (rest str) cfg)) 
      )))
;(trace leftmost-nonterm)
;(leftmost-nonterm '(yes) cfg2)

(define successors2
  (lambda (sym rules built-lst)
    (cond
      ((null? rules) built-lst)
      ((equal?  sym (first (first rules))) (successors2 sym (rest rules ) (cons (first rules) built-lst)))
      (else (successors2 sym (rest rules) built-lst))
      )))
;(successors '<sentence> (cadr cfg2) '()) =? '((<subject> <verb2> that <sentence>)
 ;             ;                                   (<subject> <verb1>))
;(successors '(<subject> <verb1>) (cadr cfg2) '())


;(successors2 '<sentence> (cadr cfg2) '())

(define replace-helper
  (lambda (str rules)
    (cond
      ((null? str) '())
      ((equal? (first str) (first rules)) (append (first (rest rules)) (rest str)))
      (else (cons (first str) (replace-helper(rest str) rules)))
      )))

(define successors
  (lambda (symbol cfg)
    (cond
      ((equal? #f (leftmost-nonterm symbol cfg)) '())
      (else (let* ((value (leftmost-nonterm symbol cfg))
                   (rule-lst (successors2 value (cadr cfg) '())))
              (map (lambda (element) (replace-helper symbol element)) rule-lst)
                   )))
      ))
;(successors2 '<sentence> (cadr cfg2) '())
;(successors '(<sentence>) cfg2)
  

(define generate-helper
  (lambda (str cfg)
    (cond
      ((equal? #f (leftmost-nonterm str cfg)) str)
      (else (generate-helper (pick (successors str  cfg)) cfg))
      )))
;(trace generate-helper)
;(generate-helper '(<sentence>) cfg2)

(define generate
  (lambda (cfg)
    (generate-helper (list (first cfg)) cfg)))

;(generate my-cfg)




; ********************************************************************
; End of homework #6
; ********************************************************************
