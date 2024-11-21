#lang play
(require "T3.rkt")

(print-only-errors #t)

;; P1 b
; enunciado
(test (parse '(nil)) (nil))
(test (parse '(cons 1 2)) (conz (num 1) (num 2)))


(test  (parse '(list 1 2 3))
       (conz (num 1) 
             (conz (num 2) 
                   (conz (num 3) (nil))
       )))


;; P1 d
(test (parse-pattern '3) (numP 3))
(test (parse-pattern '(nil)) (nilP))
(test (parse-pattern 'x) (varP 'x))
(test (parse-pattern '(cons 1 x)) (conzP (numP 1) (varP 'x)))

(test (parse-pattern '(list 1 x 3))
      (conzP (numP 1) 
             (conzP (varP 'x) 
                    (conzP (numP 3) (nilP))))
)


;; P1 e
(test (parse '(fun x x)) (fun (varP 'x) (id 'x)))
(test (parse '(fun (cons x xs) x)) (fun (conzP (varP 'x) (varP 'xs)) (id 'x)))
(test (parse '(f x)) (app (id 'f) (id 'x)))

;; P1 g
(test (generate-substs (numP 3) (numV 3)) (success '()))
(test (generate-substs (numP 3) (numV 4)) 
      (failure "MatchError: given number does not match pattern"))
(test (generate-substs (varP 'x) (numV 3))
      (success (list (cons 'x (numV 3)))))

(test (generate-substs (conzP (varP 'x) (varP 'y)) (conzV (numV 3) (numV 4))) 
      (success (list (cons 'x (numV 3)) (cons 'y (numV 4)))))
(test (generate-substs (conzP (numP 1) (varP 'y)) (conzV (numV 3) (numV 4)))
      (failure "MatchError: given number does not match pattern"))


;; P2 a
(test (parse '(match 2 [1 1] [x 3]))
      (pmatch (num 2) (list (cons (numP 1) (num 1)) (cons (varP 'x) (num 3)))))

;; P2 b
(test/exn (parse '(match ( list 1 2 3)))
       "SyntaxError: match expression must have at least one case")