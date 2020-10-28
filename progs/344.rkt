#lang racket
(cond [(string? (make-string 0 #\a)) #t]
      [else #f])
