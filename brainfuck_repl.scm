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
      (define result (brainfuck (read-input) state))
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
    (cons m (+ p 1))))

; Instruction: <
; Decrements the data pointer by one.
(define bf-dec-pointer
  (lambda (m p)
    (cons m (- p 1))))

; Instruction: +
; Increments the byte at the current data pointer.
(define bf-inc-value
  (lambda (m p)
    (cons (vector-set!
            m p (+ (vector-ref m p) 1)) p)))

; Instruction: -
; Decrements the byte at the current data pointer.
(define bf-dec-value
  (lambda (m p)
    (cons (vector-set!
            m p (- (vector-ref m p) 1)) p)))

; Instruction: .
; Prints the value of the byte at the current data pointer.
(define bf-print-out
  (lambda (m p)
    (begin
      (display (integer->char (vector-ref m p)))
      (cons m p))))

; A dictionary-like structure which maps instructions to procedures.
(define instruction-procedures
  (list `(#\! ,bf-show-state)
        `(#\> ,bf-inc-pointer)
        `(#\< ,bf-dec-pointer)
        `(#\+ ,bf-inc-value)
        `(#\- ,bf-dec-value)
        `(#\. ,bf-print-out)))

; Evaluates a list of chars (i) as a brainfuck program
; within the context (state) of memory and pointer.
(define brainfuck
  (lambda (i state)
    (if (pair? i)
      (let ((memory (car state))
            (pointer (cdr state))
            (instruction-proc (assoc (car i) instruction-procedures)))
        (if instruction-proc
          ; Proceed recursively with the remaining instructions.
          (brainfuck (cdr i) ((cadr instruction-proc) memory pointer))
          (brainfuck (cdr i) state)))
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

(mainloop (cons (make-vector 50 0) 0))
