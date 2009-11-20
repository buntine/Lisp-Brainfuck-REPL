; Brainfuck REPL, version (- 1 0.9999)
; By Andrew Buntine, 2009. info@andrewbuntine.com
;
; A very basic brainfuck repl (Read -> Evaluate -> Print -> Loop).
; Brainfuck, an esoteric programming language, was originally
; designed by Urban Muller in 1993.
;
; Main memory is represented as a vector of unrestricted
; length (you decide).
;
; I've added two additional instructions:
;   ! : Prints the contents of memory and the instruction
;       pointer to stdout.
;   @ : Resets all machine state (clears memory, pointers).
;
; Usage (tested with tinyscheme):
;     (load "brainfuck_repl.scm")


; Main REPL looping procedure. Accepts a pair of memory (vector)
; and a data pointer (positive integer).
(define mainloop
  (lambda (state)
    (begin 
      (display "-> ")
      (define result (brainfuck (read-input) state 0))
      (newline))
  (mainloop result)))

; A helper procedure for the instructions which don't directly
; affect the position of the instruction pointer.
(define next-instruction
  (lambda (m p pos)
    (list (cons m p) (inc pos))))

; Instruction: !
; Prints main memory and the data pointer to stdout.
(define bf-show-state
  (lambda (m p i pos)
    (begin
      (display m) (newline)
      (display p) (newline)
      (next-instruction m p pos))))

; Instruction: @
; Erases all machine state. 
(define bf-reboot
  (lambda (m p i pos)
    (next-instruction
      (make-vector (vector-length m) 0) 0 pos)))

; Instruction: >
; Increments the data pointer by one.
(define bf-inc-pointer
  (lambda (m p i pos)
    (next-instruction m
      (if (< p (vector-length m)) (inc p) p) pos)))

; Instruction: <
; Decrements the data pointer by one.
(define bf-dec-pointer
  (lambda (m p i pos)
    (next-instruction m
      (if (> p 0) (dec p) 0) pos)))

; Instruction: +
; Increments the byte at the current data pointer.
(define bf-inc-value
  (lambda (m p i pos)
    (next-instruction
      (vector-set!
        m p (inc (vector-ref m p))) p pos)))

; Instruction: -
; Decrements the byte at the current data pointer.
(define bf-dec-value
  (lambda (m p i pos)
    (next-instruction
      (vector-set!
        m p (dec (vector-ref m p))) p pos)))

; Instruction: .
; Prints the value of the byte at the current data pointer.
(define bf-print-out
  (lambda (m p i pos)
    (begin
      (display (integer->char (vector-ref m p)))
      (next-instruction m p pos))))

; Instruction: ,
; Reads in a byte and stores it at the current data pointer.
(define bf-read-in
  (lambda (m p i pos)
    (let ((char (char->integer (read-char))))
      (next-instruction
        (vector-set! m p char)
        p pos))))

; Instruction: [
; If the cell at the current data pointer is 0, then jump
; the instruction pointer forward to the instruction
; following the matching ].
(define bf-open-loop
  (lambda (m p i pos)
    (if (zero? (vector-ref m p))
      (next-instruction m p
        (find-loop-end i (inc pos) 0))
      (next-instruction m p pos))))

; Instruction: ]
; If the cell at the current data pointer is > 0, then jump
; the instruction pointer backwards to the instruction
; following the matching [.
(define bf-close-loop
  (lambda (m p i pos)
    (if (not (zero? (vector-ref m p)))
      (next-instruction m p
        (find-matching-brace i (dec pos) 0))
      (next-instruction m p pos))))

; Recurses forwards through an instruction set looking
; for the matching ] instruction.
(define find-loop-end
  (lambda (i pos nest)
    (if (>= pos (length i))
      (begin
        (display "Syntax error: No matching close brace")
        (dec pos))
      (let ((instruction (list-ref i pos)))
        (cond
          ; Close loop, make sure it's not nested.
          ((equal? instruction #\])
             (if (zero? nest)
               pos
               (find-loop-end i (inc pos) (dec nest))))
          ; Open loop, increase the nest count.
          ((equal? instruction #\[)
            (find-loop-end i (inc pos) (inc nest)))
          ; Neither open or close, just move on.
          (else (find-loop-end i (inc pos) nest)))))))

; Recurses backwards through an instruction set looking
; for the matching [ instruction.
(define find-loop-start
  (lambda (i pos nest)
    (if (< pos 0)
      (begin
        (display "Syntax error: No matching open brace")
        (dec (length i)))
      (let ((instruction (list-ref i pos)))
        (cond
          ; Close loop, increase the nest count.
          ((equal? instruction #\])
            (find-loop-start i (dec pos) (inc nest)))
          ; Open loop, make sure it's not nested.
          ((equal? instruction #\[)
             (if (zero? nest)
               pos
               (find-loop-start i (dec pos) (dec nest))))
          ; Neither open or close, just move on.
          (else (find-loop-start i (dec pos) nest)))))))

; A dictionary-like structure which maps instructions to procedures.
(define instruction-procedures
  (list `(#\! ,bf-show-state)
        `(#\@ ,bf-reboot)
        `(#\> ,bf-inc-pointer)
        `(#\< ,bf-dec-pointer)
        `(#\+ ,bf-inc-value)
        `(#\- ,bf-dec-value)
        `(#\. ,bf-print-out)
        `(#\, ,bf-read-in)
        `(#\[ ,bf-open-loop)
        `(#\] ,bf-close-loop)))

; Evaluates a list of chars (i) as a brainfuck program
; within the context (state) of memory and data pointer.
; The instruction pointer is denoted by pos.
(define brainfuck
  (lambda (i state pos)
    (if (< pos (length i))
      (let ((m (car state))
            (p (cdr state))
            (iproc (assoc (list-ref i pos)
                     instruction-procedures)))
        (if iproc
          ; Proceed recursively with the next instruction.
          (apply brainfuck
            (cons i ((cadr iproc) m p i pos)))
          (brainfuck i state (inc pos))))
      state)))

; Reads an arbitrary string of input and returns a list of chars.
(define read-input
  (lambda ()
    (let ((in (read-line)))
        (string->list in))))

; Reads a line of chars from STDIN port. Returns a string.
; Credit: Nils M Holm (http://www.bcl.hamilton.ie/~nmh/t3x.org/zzz).
(define (read-line)
  (letrec
    ((collect-chars
       (lambda (c s)
         (cond ((eof-object? c)
                  (if (null? s)
                    c
                    (apply string (reverse s))))
               ((char=? c #\newline)
                  (if (null? s)
                    ""
                    (apply string (reverse s))))
               (else
                 (collect-chars (read-char)
                                (cons c s)))))))
    (collect-chars (read-char) '())))

; Increments the argument by one.
(define inc
  (lambda (n)
    (+ n 1)))

; Decrements the argument by one.
(define dec
  (lambda (n)
    (- n 1)))


; Now, lets fire up the REPL.
(mainloop (cons (make-vector 60 0) 0))
