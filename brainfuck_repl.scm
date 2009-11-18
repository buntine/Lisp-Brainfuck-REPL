; Brainfuck REPL, version -0.00000000001
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
; and an instruction pointer (positive integer).
(define mainloop
  (lambda (state)
    (begin 
      (display "-> ")
      (define result (brainfuck (read-input) state))
      (newline))
  (mainloop result)))

; Instruction: !
; Prints main memory and the instruction pointer to stdout.
(define show-state
  (lambda (m p)
    (begin
      (display m)
      (newline)
      (display p)
      (cons m p))))

; Instruction: >
; Increments the instruction pointer by one.
(define inc-pointer
  (lambda (m p)
    (cons m (+ p 1))))

; Instruction: <
; Decrements the instruction pointer by one.
(define dec-pointer
  (lambda (m p)
    (cons m (- p 1))))

; A dictionary-like structure which maps instructions to procedures.
(define instruction-procedures
  (list `(#\! ,show-state)
        `(#\> ,inc-pointer)
        `(#\< ,dec-pointer)))

; Evaluates a list of chars (i) as a brainfuck program
; within the state of memory (m) and pointer (p).
(define brainfuck
  (lambda (i state)
    (let ((memory (car state))
          (pointer (cdr state)))
      (if (pair? i)
        (begin
          (define instruction-proc (assoc (car i) instruction-procedures))
          (if instruction-proc
            ; Proceed recursively with the remaining instructions.
            (brainfuck (cdr i) ((cadr instruction-proc) memory pointer))
            (brainfuck (cdr i) state)))
         
        state))))

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

(mainloop (cons (make-vector 50 0) 0))
