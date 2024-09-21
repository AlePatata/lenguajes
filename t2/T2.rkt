#lang play

#|

Hizo Ud uso de la whiteboard policy: (Indique SI/NO)
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;


;;----- ;;
;; P1.a ;;
;;----- ;;


#|
<prop> ::= (tt)
         | (ff)
         | (p-not <prop>)
         | (p-and <prop> <prop>)
         | (p-or <prop> <prop>)
         | (where <sym> <prop>)
         | (id <sym>)
|#

(deftype Prop
  (tt)
  (ff)
  (p-not p)
  (p-and ps)
  (p-or ps)
  (where x p)
  (id x)
  )


;;----- ;;
;; P1.b ;;
;;----- ;;

#|
Concrete syntax of propositions:

<s-prop> ::= true
          | false
          | (list 'p-not <s-prop>)
          | (list 'p-and <s-prop> <s-prop>)
          | (list 'p-or <s-prop> <s-prop>)
          | (list 'where (list <sym> <s-prop>))
          | <sym>
|#

;; parse-prop : <s-prop> -> Prop
(define (parse-prop s-prop) 
  (match s-prop
    [(? true? tt) (true tt)]
    [(? false? ff) (false ff)]
    [(list 'p-not sprop) (not (parse sprop))]
    [(list 'p-and l-sprop r-sprop) (and (parse l-sprop) (parse r-sprop))]
    [(list 'p-or l-sprop r-sprop) (or (parse l-sprop) (parse r-sprop))]
    [(list 'where (list (? symbol? x) sprop))
     (where x (parse sprop))]
    [(? symbol? x) (id x)] 
  ))


;;----- ;;
;; P1.c ;;
;;----- ;;


#|
<value> ::= ...
|#

;; (deftype PValue ...)

;; from-Pvalue : PValue -> Prop
(define (from-Pvalue p-value) '???)


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
(define (p-subst target name substitution) '???)


;;----- ;;
;; P1.e ;;
;;----- ;;


;; eval-or : (Listof Prop) -> PValue
(define (eval-or ps) '???)

;; eval-and : (Listof Prop) -> PValue
(define (eval-and ps) '???)

;; p-eval : Prop -> PValue
(define (p-eval p) '???)

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;;----- ;;
;; P2.a ;;
;;----- ;;


#|
<expr> ::= ...
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | ...
|#
(deftype Expr
  ; ...
  (add l r)
  (sub l r)
  (if0 c t f)
  ; ...
  )

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= ...
        | (+ <s-expr> <s-expr>)
        | (- <s-expr> <s-expr>)
        | (if0 <s-expr> <s-expr> <s-expr>)
        | ...
|#

;; parse : <s-expr> -> Expr

(define (parse s-expr) '???)

;;----- ;;
;; P2.c ;;
;;----- ;;

;; subst :: Expr Symbol Expr -> Expr
(define (subst in what for) '???)

;;----- ;;
;; P2.d ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
(define (from-CValue v) '???)

;; cmplx+ :: CValue CValue -> CValue
(define (cmplx+ v1 v2) '???)

;; cmplx- :: CValue CValue -> CValue
(define (cmplx- v1 v2) '???)

;; cmplx0? :: CValue -> Boolean
(define (cmplx0? v) '???)


;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
(define (interp expr) '???)
