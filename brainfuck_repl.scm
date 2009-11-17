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
    (let ((memory (car state))
          (pointer (cdr state)))
      (begin 
        (display "-> ")
        (define result (brainfuck (read-input) memory pointer))
        (newline))
    (mainloop result))))

; Evaluates a list of chars (i) as a brainfuck program
; within the state of memory (m) and pointer (p).
(define brainfuck
  (lambda (i m p)
    (if (pair? i)
      (begin
        (display (cond 
          ((equal? (car i) #\>) 'IncValue)
          ((equal? (car i) #\<) 'DecValue)
          ((equal? (car i) #\+) 'IncPointer)
          ((equal? (car i) #\-) 'DecPointer)
          ((equal? (car i) #\,) 'StdIn)
          ((equal? (car i) #\.) 'StdOut)))
        (brainfuck (cdr i) m p))
      (cons m p))))

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
