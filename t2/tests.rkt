#lang play
(require "T2.rkt")

(print-only-errors #t)

(test (parse-prop '(not true)) (p-not (tt)))
(test (parse-prop '(and true false true)) (p-and (list (tt) (ff) (tt))) )
(test (parse-prop '(or true false)) (p-or (list (tt) (ff))) )
(test (parse-prop '(false where [x true])) (p-where (ff) 'x (tt)) )
(test (parse-prop '(x where [x true])) (p-where (p-id 'x) 'x (tt)) )