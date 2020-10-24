#lang racket
(require "interp.rkt"
         rackunit)

(define (read-prog p)
  (regexp-match "^#lang racket" p)
  (read p))

;; Code for submission needs to be in ".." directory
(require (only-in "../compile.rkt" compile)
         (only-in "../asm/interp.rkt" asm-interp)
         (only-in "../parse.rkt" parse)
         (only-in "../syntax.rkt" expr? closed? sexpr->ast)
         (only-in "../lex.rkt" lex-string lex-port))


(check-equal?
 (parse (lex-string "#lang racket (let ((x 1)) x)"))
 (sexpr->ast '(let ((x 1)) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax tests

(check-true (expr? (sexpr->ast 7)))
(check-true (expr? (sexpr->ast "asdf")))
(check-true (expr? (sexpr->ast "")))
(check-true (expr? (sexpr->ast #t)))
(check-true (expr? (sexpr->ast #t)))
(check-true (expr? (sexpr->ast #\a)))
(check-true (expr? (sexpr->ast '(add1 #f))))
(check-true (expr? (sexpr->ast '(sub1 #f))))
(check-true (expr? (sexpr->ast '(abs #f))))
(check-true (expr? (sexpr->ast '(- #f))))
(check-true (expr? (sexpr->ast '(zero? #f))))
(check-true (expr? (sexpr->ast '(integer->char #f))))
(check-true (expr? (sexpr->ast '(char->integer #f))))
(check-true (expr? (sexpr->ast '(char? #f))))
(check-true (expr? (sexpr->ast '(integer? #f))))
(check-true (expr? (sexpr->ast '(boolean? #f))))
(check-true (expr? (sexpr->ast '(box "adsf"))))
(check-true (expr? (sexpr->ast '(+ 1 2))))
(check-true (expr? (sexpr->ast '(- 1))))
(check-true (expr? (sexpr->ast '(- 1 2))))
(check-true (expr? (sexpr->ast 'x)))
(check-true (expr? (sexpr->ast '(let () x))))
(check-true (expr? (sexpr->ast '(let ((x 1)) x))))
(check-true (expr? (sexpr->ast '(let ((x 1) (y 2)) x))))
(check-true (expr? (sexpr->ast '(let ((x 1) (y 2) (z 3)) x))))
(check-true (expr? (sexpr->ast '(string-length "asdf"))))
(check-true (expr? (sexpr->ast '(string-ref "asdf" 0))))
(check-true (expr? (sexpr->ast '(= #f #f))))
(check-true (expr? (sexpr->ast '(< #f #f))))
(check-true (expr? (sexpr->ast '(string? #f))))
(check-true (expr? (sexpr->ast '(box? #f))))
(check-true (expr? (sexpr->ast '(empty? #f))))
(check-true (expr? (sexpr->ast '(cons? #f))))
(check-true (expr? (sexpr->ast '(unbox #f))))
(check-true (expr? (sexpr->ast '(car #f))))
(check-true (expr? (sexpr->ast '(cdr #f))))
(check-true (expr? (sexpr->ast '(make-string #f #f))))
(check-true (expr? (sexpr->ast '(= #f #f))))
(check-true (expr? (sexpr->ast '(< #f #f))))
(check-true (expr? (sexpr->ast '(<= #f #f))))
(check-true (expr? (sexpr->ast '(char=? #f #f))))
(check-true (expr? (sexpr->ast '(boolean=? #f #f))))
(check-true (expr? (sexpr->ast '(+ #f #f))))
(check-true (expr? (sexpr->ast '(- #f #f))))
(check-true (expr? (sexpr->ast '(let ((string? 1)) 1))))
(check-true (expr? (sexpr->ast '(let ((abs 1)) 1))))
(check-true (expr? (sexpr->ast '(let ((string-ref 1)) 1))))
(check-true (expr? (sexpr->ast '(let ((+ 1)) 1))))

(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(let 1)))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(let x 1)))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(let x y 1)))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(let (x y) 1)))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(let ((x)) 1)))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(let ((1 2)) 1)))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(1)))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(box)))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(string-ref "asdf")))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(+ 1 2 3)))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(make-string #f)))))
(check-exn exn:fail? (lambda () (expr? (sexpr->ast '(make-string #f #f #f)))))

(check-true (closed? (sexpr->ast 7)))
(check-true (closed? (sexpr->ast "asdf")))
(check-true (closed? (sexpr->ast "")))
(check-true (closed? (sexpr->ast #t)))
(check-true (closed? (sexpr->ast #f)))
(check-true (closed? (sexpr->ast #\a)))
(check-true (closed? (sexpr->ast '(box "adsf"))))
(check-true (closed? (sexpr->ast '(+ 1 2))))
(check-true (closed? (sexpr->ast '(- 1))))
(check-true (closed? (sexpr->ast '(- 1 2))))
(check-true (closed? (sexpr->ast '(let ((x 1)) x))))
(check-true (closed? (sexpr->ast '(let ((x 1) (y 2)) x))))
(check-true (closed? (sexpr->ast '(let ((x 1) (y 2) (z 3)) x))))
(check-true (closed? (sexpr->ast '(string-length "asdf"))))
(check-true (closed? (sexpr->ast '(string-ref "asdf" 0))))
(check-true (closed? (sexpr->ast '(let ((x 1) (y 2))
                        (let ((z y))
                          (+ x z))))))

(check-false (closed? (sexpr->ast 'x)))
(check-false (closed? (sexpr->ast '(let () x))))
(check-false (closed? (sexpr->ast '(let ((x 1)) y))))
(check-false (closed? (sexpr->ast '(let ((x 1) (y x)) y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler tests

(check-equal?
 (asm-interp (compile (sexpr->ast '(let ((x 1)) x))))
 (interp (sexpr->ast '(let ((x 1)) x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random tests

(define tokens
  (parameterize ((current-directory "progs"))
    (for/list ([fn (directory-list)])
      (call-with-input-file fn lex-port))))

(define parses
  (parameterize ((current-directory "progs"))
    (for/list ([fn (directory-list)])
      (cons fn (sexpr->ast (call-with-input-file fn read-prog))))))

(for ([t tokens]
      [p parses])
  (match p
    [(cons fn p)
       (println (string-append "testing: " (path->string fn)))
       (check-not-exn (lambda () (parse t)) t)
       (check-equal? (parse t) p)]))

(for ([p parses])
  (match p
    [(cons fn p)
       (println (string-append "testing: " (path->string fn)))
       (check-true (and (expr? p)
                        (closed? p)))]))

(for ([p parses])
  (match p
    [(cons fn p)
       (println (string-append "testing: " (path->string fn)))
       (check-equal? (asm-interp (compile p))
                     (interp p)
                     p)]))
