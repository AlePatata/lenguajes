#lang play
(require "T2.rkt")

(print-only-errors #t)

;; P1.b ;;
(test/exn (parse-prop '(and)) "parse-prop: and expects at least two operands")
(test/exn (parse-prop '(and true)) "parse-prop: and expects at least two operands")
(test (parse-prop '(and true false)) (p-and (list (tt) (ff))) )

;; tests de enunciado
(test (parse-prop '(not true)) (p-not (tt)))
(test (parse-prop '(and true false true)) (p-and (list (tt) (ff) (tt))) )
(test (parse-prop '(or true false)) (p-or (list (tt) (ff))) )
(test (parse-prop '(false where [x true])) (p-where (ff) 'x (tt)) )
(test (parse-prop '(x where [x true])) (p-where (p-id 'x) 'x (tt)) )

;; P1.c ;;
;; tests de enunciado
(test  (from-PValue (ttV)) (tt))
(test  (from-PValue (ffV)) (ff))

;; P1.d ;;
(test (p-subst (tt) 'x (ff)) (tt))
(test (p-subst (ff) 'x (tt)) (ff))
(test (p-subst (p-not (tt)) 'x (ff)) (p-not (tt)))
(test (p-subst (p-not (ff)) 'x (tt)) (p-not (ff)))
;(test (p-subst (p-and (list (tt) (p-id 'x))) 'x (tt)) (p-and (tt) (tt)))

;; tests de enunciado
(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))

;; P1.e ;;
(test (p-eval (tt)) (ttV))
(test (p-eval (ff)) (ffV))
(test (p-eval (p-not (tt))) (ffV))
(test (p-eval (p-not (ff))) (ttV))

(test (p-eval (p-and (list (tt) (ff)))) (ffV))
(test (p-eval (p-and (list (tt) (tt)))) (ttV))
(test (p-eval (p-and (list (ff) (tt)))) (ffV))
(test (p-eval (p-and (list (ff) (ff)))) (ffV))
(test (p-eval (p-and (list (tt) (tt) (ff)))) (ffV))
(test (p-eval (p-and (list (tt) (tt) (tt)))) (ttV))

(test (p-eval (p-or (list (ff) (tt)))) (ttV))
(test (p-eval (p-or (list (ff) (ff)))) (ffV))
(test (p-eval (p-or (list (tt) (ff)))) (ttV))
(test (p-eval (p-or (list (tt) (tt)))) (ttV))
(test (p-eval (p-or (list (ff) (ff) (tt)))) (ttV))
(test (p-eval (p-or (list (ff) (ff) (ff)))) (ffV))

(test (p-eval (p-where (p-id 'x) 'x (tt))) (ttV))

;; P2.b ;;
(test (parse '(with [(x 1)] (+ x 1))) (with (list (cons 'x (real 1))) (add (id 'x) (real 1)) ))

; tests de enunciado
(test (parse '1) (real 1))
(test (parse '(1 i )) (imaginary 1))
(test (parse '(+ 1 (2 i ))) (add (real 1) (imaginary 2)))
(test  (parse '(with [(x 1) (y 1)] (+ x y)))
    (with ( list (cons 'x ( real 1)) (cons 'y ( real 1))) (add (id 'x) (id 'y))))


;; P2.c ;;
; tests de enunciado
(test (subst (parse ' (with [ (x 2) (y z) ] (+ x z))) 'z ( real 1))
(with ( list (cons 'x ( real 2)) (cons 'y ( real 1))) (add (id 'x) ( real 1))))
(test (subst (parse ' (with [ (x 2) (y x)] (+ x x))) 'x ( real 1))
(with ( list (cons 'x ( real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))))


;; P2.d ;;
; tests de enunciado
(test (cmplx+ (compV 1 2) (compV 3 4)) (compV 4 6))
(test (cmplx- (compV 1 2) (compV 3 4)) (compV -2 -2))
(test (cmplx0? (compV 0 1)) #f)