#lang racket
(require "interp.rkt"
         "asm/interp.rkt" ; means students can't add to their own asm
         rackunit)

;; Code for submission needs to be in ".." directory
(require (only-in "../compile.rkt" compile)
         (only-in "../parse.rkt" parse)
         (only-in "../lex.rkt" lex-string))

(check-equal?
 (asm-interp (compile '(let ((x 1)) x)))
 (interp '(let ((x 1)) x)))

(check-equal?
 (parse (lex-string "#lang racket (let ((x 1)) x)"))
 '(let ((x 1)) x))
