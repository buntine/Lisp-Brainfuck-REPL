; Brainfuck REPL, version 0.000
; By Andrew Buntine, 2009
;
; Usage:
;     (load "brainfuck_repl.scm")
;     (mainloop '((make-vector 100 0) 0))


; Main REPL looping procedure.
(define mainloop
  (lambda (m)
    (let ((memory (car m))
          (pointer (cdr m)))
      (begin 
        (display "> ")
        (define result (brainfuck (read-input) memory pointer))
        (newline))
    (mainloop result))))

; Evaluates a list of chars as a brainfuck program
; within the context of m, p.
(define brainfuck
  (lambda (i m p)
    (begin
      (display i)
      '(m p))))

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
