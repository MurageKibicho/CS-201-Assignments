#lang racket
(require racket/trace)

; Name: Murage Kibicho
; Email address: murage.kibicho@yale.edu

; CS 201a HW #2  DUE by Wednesday, September 30, 2020 11:59 pm, electronically,
; using the submit command.  

; The topics of this assignment are:
; a simulator for Turing machines and
; some Turing machine programs

; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment (excluding reading.)

(define hours 15)

; ****************************************************************
; Turing machines were described in the lectures.
; (See also the lecture notes under Files/Resources.)
; Here is a top-level procedure to simulate a Turing machine 
; starting from a given configuration, printing out the
; successive configurations of the machine, until it halts.

(define simulate 
  (lambda (mach config)
    (display config) (newline)
    (if (halted? mach config)
	'halted
	(simulate mach (next-config mach config)))))

; mach is a representation of a Turing machine, and
; config is a representation of a configuration of the machine,
; as described below.

; The procedures halted? and next-config will be
; developed in problems 2-6 below; you will then
; have a complete Turing machine simulator.

; A Turing machine is represented as a list of instructions, 
; where each instruction is a 5-tuple, represented as a list: 

; current state, current symbol, new state, new symbol, and move direction

; The current state and new state should be Racket symbols,
; the current symbol and new symbol should be Racket symbols or numbers,
; and the move direction must be either 'L or 'R.


; Example: (q1 0 q3 1 L)
; is an instruction
; with current state q1, current symbol 0,
; new state q3, new symbol 1,
; and move direction L.

; Here are selectors for the parts of an instruction:
; Use them in your code -- they will be a lot more
; mnemonic than the corresponding list-refs or (first(rest ...))

(define i-state (lambda (inst) (list-ref inst 0)))
(define i-symbol (lambda (inst) (list-ref inst 1)))
(define i-new-state (lambda (inst) (list-ref inst 2)))
(define i-new-symbol (lambda (inst) (list-ref inst 3)))
(define i-direction (lambda (inst) (list-ref inst 4)))


; A Turing machine is simply a list of instructions.

; Example: a Turing machine that when started in state q1
; on the leftmost of a string of 0's and 1's 
; changes all the 0's to 1's and ; all the 1's to 0's 
; and then returns the head to the leftmost symbol and halts.

(define tm1 (list
	     '(q1 0 q1 1 R)
	     '(q1 1 q1 0 R)
	     '(q1 b q2 b L)
	     '(q2 0 q2 0 L)
	     '(q2 1 q2 1 L)
	     '(q2 b q3 b R)))
(define tmtest (list
            '(q1 c q1 c R)
            '(q1 b q2 1 R)
            '(q2 b q2 2 L)
            '(q2 1 q2 1 L)
           ))

(define tmtrue (list
                '(q1 c q1 c R)
                '(q1 b q2 1 R)
                '(q2 b q1 2 L)
                '(q1 1 q2 1 L)))



; ** problem 1 (12 points)
; Write the following two procedures.
; Please use the instruction selectors defined above:
; i-state, i-symbol, i-new-state, i-new-symbol, i-direction

; (i-match? state symbol inst)
; returns #t if state and symbol are equal to 
; the state and symbol of instruction inst
; otherwise returns #f

; (i-find state symbol mach)
; returns #f if no instruction of Turing machine mach 
; has state and symbol equal to state and symbol
; otherwise returns the first instruction in mach that matches.

; Examples:
; (i-match? 'q1 'b '(q1 b q3 b L)) => #t
; (i-match? 'q1  0 '(q1 1 q4 1 L)) => #f

; (i-find 'q1 1 tm1) => '(q1 1 q1 0 R)
; (i-find 'q2 'b tm1) => '(q2 b q3 b R)
; (i-find 'q3 1 tm1) => #f

(define i-match?
  (lambda (state symbol inst)
    (cond
      ((and (equal? state (i-state inst)) (equal? symbol (i-symbol inst))) #t)
      (else #f)
      )))

(define i-find
  (lambda (state symbol mach)
    (cond
      ((null? mach) #f)
      ((and (equal? state (i-state (i-state mach))) (equal? symbol (i-symbol (i-state mach)))) (i-state mach))
      ((i-find state symbol (rest mach)))
      (else (i-find state symbol (rest mach)))
      )))


; A Turing machine tape is a list of items, each
; of which is a Racket number or symbol.
; The symbol 'b is special -- it stands for the blank tape square.

; Examples: two tapes

(define tape1 '(1 1 0 1 0))
(define tape2 '(b 1 1 b 0 1 1 b b))

; A configuration is a 3-tuple represented by a list of
; state, head position, tape 
; where state is a Racket symbol, head position is an integer
; greater than or equal to 0 and less than (length tape),
; and tape is a non-empty Turing machine tape.
; This is zero-based indexing: 0 stands for the head
; positioned on the leftmost symbol of the tape.

; Example: a configuration with state q1, head on the
; leftmost square, and tape contents tape1.

(define config1 (list 'q1 0 tape1)) 

; Here are selectors for the parts of a configuration.
; Use them in your code rather than cars, list-refs, etc.

(define c-state (lambda (config) (list-ref config 0)))
(define c-head-position (lambda (config) (list-ref config 1)))
(define c-tape (lambda (config) (list-ref config 2)))

; ** problem 2 ** (12 points)
; Write the following three procedures.

; (c-symbol config) 
; to return the symbol currently under the head 
; in the given configuration config.
; Assume that the head position is greater than
; or equal to 0 and less than the length of the tape.

; (change-state new-state config)
; to return a configuration equal to config
; with the current state changed to new-state

; (halted? mach config)
; that returns #t if Turing machine mach is halted in 
; configuration config (i.e., no instruction of the
; machine matches the current state and symbol in
; configuration) and #f otherwise.

; Examples:
; (c-symbol '(q6 3 (0 0 0 1 0 0 b))) => 1
; (c-symbol '(q4 6 (0 0 0 1 0 0 b))) => 'b
; (c-symbol config1) => 1
; (change-state 'q2 '(q6 3 (0 0 1 1 b))) => '(q2 3 (0 0 1 1 b))
; (change-state 'q1 config1) => '(q1 0 (1 1 0 1 0))
; (halted? tm1 config1) => #f
; (halted? tm1 '(q3 0 (b 0 0 1 0 1 b))) => #t



(define c-symbol
  (lambda (config)
    (list-ref (c-tape config) (c-head-position config)
     )))

(define change-state
  (lambda (new-state config)
    (cons new-state(rest config))))

(define halted?
  (lambda (mach config)
    (cond
    ((equal? #f (i-find (c-state config) (c-symbol config) mach)) #t)
    (else #f)
    )))

; ** problem 3 ** (12 points)
; Write a procedure (write-symbol s config)
; that returns a configuration equal to config
; with the currently scanned symbol replaced by 
; the symbol s.

; Examples:
; (write-symbol 0 config1) => '(q1 0 (0 1 0 1 0))
; (write-symbol 'b '(q6 3 (0 1 0 1 1))) => '(q6 3 (0 1 0 b 1))
; (write-symbol 'a '(q2 2 (w x y z))) => '(q2 2 (w x a z))

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

(define write-symbol
  (lambda (s config)
    (list (c-state config) (c-head-position config)
    (cond
    ((equal? 0 (c-head-position config)) (append (list s) (first (rest (split (c-tape config) ( + (c-head-position config) 1))))))
    (else (append (append  (first (split (c-tape config) (c-head-position config))) (list s))
                  (rest (first (rest (split (c-tape config) (c-head-position config))))))
          )))))



; ** problem 4 ** (12 points)
; Write a procedure (move-head dir config)
; that returns the value of config with the head moved one square
; in the direction dir (specified by symbol 'R or 'L)

; Make sure that if the head-position would "run off" the
; tape, that a blank is added to the tape to guarantee that
; the new head position is greater than or equal to 0 and
; less than the length of the tape.

; You may find it useful to review let and let*. 

; Examples:
; (move-head 'R config1) => '(q1 1 (1 1 0 1 0))
; (move-head 'L config1) => '(q1 0 (b 1 1 0 1 0))
; (move-head 'R '(q6 3 (0 1 1 0))) => '(q6 4 (0 1 1 0 b))

(define move-head
  (lambda (dir config)
    (cons (c-state config)
    (cond
      ((equal? dir 'R)
       (cond
       ((equal? (+ (c-head-position config) 1) (length (c-tape config))) (list (+ (c-head-position config) 1) (append (c-tape config) (list 'b))))
       (else (list (+ (c-head-position config) 1) (c-tape config)))))
      ((equal? dir 'L)
       (cond
        ((< (- (c-head-position config) 1) 0) (list 0 (append (list 'b) (c-tape config))))
        (else (list (- (c-head-position config) 1) (c-tape config)))))
      ))))   


; ** problem 5 ** (12 points)
; Write a procedure (next-config mach config)
; that returns the next configuration for the
; Turing machine mach in configuration config
; If there is no applicable instruction, the configuration
; remains the same.

; Hint: get halted?, i-find, change-state, move-head, and write-symbol
; working and use them!

; Examples:
; (next-config tm1 config1) => '(q1 1 (0 1 0 1 0))
; (next-config tm1 '(q1 1 (0 1 0 1 0))) => '(q1 2 (0 0 0 1 0))
; (the following example was corrected on 9-18-2020)
; (next-config tm1 '(q1 4 (0 0 1 0 1))) => '(q1 5 (0 0 1 0 0 b))
; (next-config tm1 '(q1 5 (0 0 1 0 1 b))) => '(q2 4 (0 0 1 0 1 b))
; (next-config tm1 '(q3 1 (b 0 0 1 0 1 b))) => '(q3 1 (b 0 0 1 0 1 b))

(define next-config
  (lambda (mach config)
    (cond
      ((equal? (i-find (c-state config) (c-symbol config) mach) #f) config)
      ((list (i-new-state (i-find (c-state config) (c-symbol config) mach))
             (i-symbol (move-head (i-direction (i-find (c-state config) (c-symbol config) mach)) config))
             (c-tape (move-head (i-direction (i-find (c-state config) (c-symbol config) mach))
                       (write-symbol (i-new-symbol (i-find (c-state config) (c-symbol config) mach))
              (write-symbol (i-symbol (i-find (c-state config) (c-symbol config) mach)) config))))
             ;(i-find (c-state config) (c-symbol config) mach)
       )))))

; (next-config tm1 config1) 
 ;(next-config tm1 '(q1 1 (0 1 0 1 0)))
 ;(next-config tm1 '(q1 4 (0 0 1 0 1))) 
 ;(next-config tm1 '(q1 5 (0 0 1 0 1 b)))
 ;(next-config tm1 '(q3 1 (b 0 0 1 0 1 b)))



; If your procedures are working, then you should
; be able to run the following example, which
; shows the successive configurations of Turing machine
; tm1 when run from configuration config1.

;   (simulate tm1 config1)      
; (q1 0 (1 1 0 1 0))
; (q1 1 (0 1 0 1 0))
; (q1 2 (0 0 0 1 0))
; (q1 3 (0 0 1 1 0))
; (q1 4 (0 0 1 0 0))
; (q1 5 (0 0 1 0 1 b))
; (q2 4 (0 0 1 0 1 b))
; (q2 3 (0 0 1 0 1 b))
; (q2 2 (0 0 1 0 1 b))
; (q2 1 (0 0 1 0 1 b))
; (q2 0 (0 0 1 0 1 b))
; (q2 0 (b 0 0 1 0 1 b))
; (q3 1 (b 0 0 1 0 1 b))
; 'halted




; ** problem 6  ** (11 points)
; Define (in the representation used above)
; a Turing machine 
;   tm-oddeven 
; to append a 0 or 1 to a string so that the result
; has an odd number of 1's. That is, if originally
; there is an odd number of 1's a 0 will be appended,
; if originally there is an even number of 1's
; a 1 will be appended.
; The input to the machine is
; a nonempty string 0's and 1's, starting with
; a 1.
; The machine will be started in state q1
; on the leftmost 1 of the string.
; When the machine halts, the head
; should be positioned on the leftmost
; 1 of the string of the input.
; You may use additional tape symbols
; during the computation.



; NOTE: you can still do this problem if your simulator
; is not working, assuming you understand Turing machines
; and the Racket representation of them given above.

; Examples of input => output
; input: 111    output: 1110
; input: 10001 output: 100011
; input: 101010    output: 1010100

; For this and all subsequent problems, be especially sure
; to test your code under the autograder before submitting!
; Although we cannot compare the output of calling
; simulate on your TM to that of calling simulate on
; a reference solution, we can call a slightly different version
; of simulate to ensure that the contents of your tape are
; correct and the head is appropriately positioned upon halting.

;q2 is odd
(define tm-oddeven
                 (list
                  '(q1 0 q1 0 R)
                  '(q1 1 q2 1 R)
                  '(q2 0 q2 0 R)
                  '(q2 1 q1 1 R)
                  '(q1 b q3 1 L)
                  '(q2 b q3 0 L)
                  '(q3 0 q3 0 L)
                  '(q3 1 q3 1 L)
                  '(q3 b q4 b R)
                  ))

(define oddeven-config1 '(q1 0 (1 1 1)))
(define oddeven-config2 '(q1 0 (1 0 0 0 1)))
(define oddeven-config3 '(q1 0 (1 0 1 0 1 0)))
 




; ** problem 7 (14 points)
; Find a Turing machine, tm-better, with *few* states,
; that when started on a blank tape
; in state q1 writes at least 50 non-blank symbols
; on the tape and then halts.
; The machine may use only the tape symbols
; b, 0, 1, 2, 3.

; Here is a very bad solution:
; the states are q1, q2, ..., q51.
; For each state qi with i = 1, 2, ..., 50,
; there is an instruction (qi, b, qi+1, 1, R).
; When started on a blank tape, this machine
; marches 50 steps to the right, writing 1's
; and then halts (because there is no instruction for
; q51.)  This machine has 51 states; your
; solution must have fewer states!!!


(define tm-better
                (list
                 '(q1 b q2 0 R)
                 '(q1 0 q1 1 L)
                 '(q1 1 q1 3 R)
                 '(q2 0 q3 1 R)
                 '(q2 b q3 0 R)
                 '(q2 1 q2 1 R)
                 '(q3 0 q2 0 L)
                 '(q3 b q4 0 R)
                 '(q4 b q5 0 R)
                 '(q5 b q6 0 R)
                 '(q6 b q7 0 R)
                 '(q7 b q8 0 R)
                 '(q8 b q9 0 R)
                 '(q9 b q10 0 R)
                 '(q10 b q11 0 R)
                 '(q11 b q12 0 R)
                 '(q12 b q13 0 R)
                 '(q13 b q14 0 R)
                 '(q14 b q15 0 R)
                 '(q15 b q16 0 R)
                 '(q16 b q17 0 R)
                 '(q17 b q18 0 R)
                 '(q18 b q1 1 L)
                           ))
                
(simulate tm-better (list 'q1 0 '(b)))  


; ** problem 8 ** (14 points)
; Define in the Racket format established above
; a Turing machine named tm-pal which
; when started on an input consisting of a string A's and C's,
; checks to see whether the string is a palindrome (ie,
; is equal to its reverse.)  If so, it leaves a single 1
; on the tape and halts; otherwise, it leaves a single 0
; on the tape and halts.
; In both cases, the head is positioned on the single
; non-blank symbol on the tape when the machine halts.
; Your machine should start in state q0.


; Note: you can still do this problem if your simulator
; is not working, assuming you understand Turing machines
; and the Racket representation of them given above.


; Examples:
; input: CCCC    output: 1
; input: CAACAAC output: 1
; input: AACA    output: 0
; input: A       output: 1
; input: empty   output: 1 (the empty string is a palindrome)

;   Here are some sample configurations for testing:

(define pal-config1 '(q0 0 (C C C C)))
(define pal-config2 '(q0 0 (C A A C A A C)))
(define pal-config3 '(q0 0 (A A C A)))
(define pal-config4 '(q0 0 (A)))
(define pal-config5 '(q0 0 (b)))


(define tm-pal
             (list
             
              '(q0 A q1 b R)
              '(q0 C q4 b R)
              '(q1 A q1 A R)
              '(q4 A q4 A R)
              '(q1 C q1 C R)
              '(q4 C q4 C R)
              '(q1 b q2 b L)
              '(q4 b q5 b L)
              '(q2 A q3 b L)
              '(q5 C q6 b L)
              '(q3 A q3 A L)
              '(q6 A q6 A L)
              '(q3 C q3 C L)
              '(q6 C q6 C L)
              '(q3 b q0 b R)
              '(q6 b q0 b R)
              '(q0 b q10 1 L)
              '(q10 b q11 b L)
              '(q5 b q10 1 L)
              '(q2 C q10 0 L)
              '(q2 b q10 1 L)
              '(q11 b q12 b R)
              '(q12 b q13 b R)
              ))


 
