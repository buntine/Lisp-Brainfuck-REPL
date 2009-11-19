; Brainfuck REPL, version (- 1 0.9999)
; By Andrew Buntine, 2009
;
; A very basic brainfuck repl (Read -> Evaluate -> Print -> Loop).
; Brainfuck, an esoteric programming language, was originally
; designed by Urban Muller in 1993.
;
; Usage:
;     (load "brainfuck_repl.scm")
;     (mainloop (cons (make-vector 100 0) 0))


; Main REPL looping procedure. Accepts a pair of memory (vector)
; and a data pointer (positive integer).
(define mainloop
  (lambda (state)
    (begin 
      (display "-> ")
      (define result (brainfuck (read-input) state 0))
      (newline))
  (mainloop result)))

; Instruction: !
; Prints main memory and the data pointer to stdout.
(define bf-show-state
  (lambda (m p)
    (begin
      (display m)
      (newline)
      (display p)
      (cons m p))))

; Instruction: >
; Increments the data pointer by one.
(define bf-inc-pointer
  (lambda (m p)
    (cons m (inc p))))

; Instruction: <
; Decrements the data pointer by one.
(define bf-dec-pointer
  (lambda (m p)
    (cons m (dec p))))

; Instruction: +
; Increments the byte at the current data pointer.
(define bf-inc-value
  (lambda (m p)
    (cons (vector-set!
            m p (inc (vector-ref m p))) p)))

; Instruction: -
; Decrements the byte at the current data pointer.
(define bf-dec-value
  (lambda (m p)
    (cons (vector-set!
            m p (dec (vector-ref m p))) p)))

; Instruction: .
; Prints the value of the byte at the current data pointer.
(define bf-print-out
  (lambda (m p)
    (begin
      (display (integer->char (vector-ref m p)))
      (cons m p))))

; Instruction: ,
; Reads in a byte and stores it at the current data pointer.
; Note: The read-input procedure seems to prevent me from 
;       direct using read-char here.
(define bf-read-in
  (lambda (m p)
    (let ((char (char->integer (car (read-input)))))
      (cons (vector-set!
              m p char) p))))

; A dictionary-like structure which maps instructions to procedures.
(define instruction-procedures
  (list `(#\! ,bf-show-state)
        `(#\> ,bf-inc-pointer)
        `(#\< ,bf-dec-pointer)
        `(#\+ ,bf-inc-value)
        `(#\- ,bf-dec-value)
        `(#\. ,bf-print-out)
        `(#\, ,bf-read-in)))

; Evaluates a list of chars (i) as a brainfuck program
; within the context (state) of memory and data pointer.
(define brainfuck
  (lambda (i state pos)
    (if (< pos (length i))
      (let ((m (car state))
            (p (cdr state))
            (iproc (assoc (list-ref i pos) instruction-procedures)))
        (if iproc
          ; Proceed recursively with the next instruction.
          (brainfuck i ((cadr iproc) m p) (inc pos))
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
                 (apply string (reverse s)))
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

; Fire up the REPL.
(mainloop (cons (make-vector 50 0) 0))
