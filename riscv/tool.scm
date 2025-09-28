;;; File: riscv/tool.scm

(define-module (riscv tool)
  #:export (print2 print16))

(define (print2 n)
  "Print the integer N in binary (base 2) without the #b prefix."
  (display (number->string n 2))
  (newline))

(define (print16 n)
  "Print the integer N in hexadecimal (base 16) without the #x prefix."
  (display (string-upcase (number->string n 16)))
  (newline))