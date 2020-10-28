#lang racket
(char=? (string-ref (make-string 100 #\q) 99) #\q)
