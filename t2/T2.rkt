#lang play

#|

Hizo Ud uso de la whiteboard policy: (Indique SI/NO)
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------ ;;
;;==== P1 ==== ;;
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
  (p-where p1 x p2)
  (p-id x)
)


;;----- ;;
;; P1.b ;;
;;----- ;;

#|
Concrete syntax of propositions:

<s-prop> ::= tt
          | ff
          | (list 'not <s-prop>)
          | (list 'and <s-prop> <s-prop>)
          | (list 'or <s-prop> <s-prop>)
          | (list <s-prop> 'where (list <sym> <s-prop>))
          | <sym>
|#

;; parse-prop : <s-prop> -> Prop
(define (parse-prop s-prop) 
  (match s-prop
    ['true (tt)]
    ['false (ff)]
    [(? symbol? x) (p-id x)] 
    [(list 'not sprop) (p-not (parse-prop sprop))]

    [(list 'and) (error 'parse-prop "and expects at least two operands")]
    [(list 'and first) (error 'parse-prop "and expects at least two operands") ]
    [(list 'and first rest ...) 
      (p-and (cons (parse-prop first)
                   (map parse-prop rest)
      ))
    ]

    [(list 'or) (error 'parse-prop "or expects at least two operands")]
    [(list 'or first) (error 'parse-prop "or expects at least two operands") ]
    [(list 'or first rest ...) 
      (p-or (cons (parse-prop first)
                  (map parse-prop rest)
      ))
    ]

    [(list sprop 'where (list (? symbol? x) second-sprop))
     (p-where (parse-prop sprop) x (parse-prop second-sprop))]
  ))


;;----- ;;
;; P1.c ;;
;;----- ;;


#|
<value> ::= ttV
         | ffV 
|#

(deftype PValue 
  (ttV)
  (ffV)
)


;; from-Pvalue : PValue -> Prop
(define (from-PValue p-value) 
  (match p-value
    [(ttV) (tt)]
    [(ffV) (ff)]
  ))


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
;; (subst target name substitution)
;; substituye todas las ocurrencias libres del identificador 'name'
;; en la proposición 'target' por la proposición 'substitution'
(define (p-subst target name substitution) 
  (match target
    [(tt) (tt)]
    [(ff) (ff)]
    [(p-not p) (p-not (p-subst p name substitution))]

    
    [(p-and ps) (p-and (map (lambda (p) (p-subst p name substitution)) ps))]

    [(p-or ps) (p-or (map (lambda (p) (p-subst p name substitution)) ps))]

    [(p-id x) 
      (if (symbol=? x name)
        substitution
        (p-id x)
    )]
    [(p-where p1 x p2)
      (if (symbol=? x name)
        (p-where p1 x p2)
        (p-where (p-subst p1 name substitution) x p2)
    )]
        
))


;;----- ;;
;; P1.e ;;
;;----- ;;


;; eval-or : (Listof Prop) -> PValue
(define (eval-or ps) 
  (if (null? ps)
    (error "eval-or expects at least one operand")
    (let ((first (p-eval (car ps))))
      (if (equal? first (ttV))
        (ttV)
        (if (null? (cdr ps))
          first
          (eval-or (cdr ps)))))))

;; eval-and : (Listof Prop) -> PValue
(define (eval-and ps) 
  (if (null? ps)
    (error "eval-and expects at least one operand")
    (let ((first (p-eval (car ps))))
      (if (equal? first (ffV))
        (ffV)
        (if (null? (cdr ps))
          first
          (eval-and (cdr ps)))))))


;; p-eval : Prop -> PValue
(define (p-eval p) 
  (match p
    [(tt) (ttV)]
    [(ff) (ffV)]
    [(p-not p) (if (equal? (p-eval p) (ttV))
                  (ffV)
                  (ttV)
    )]
    [(p-and p) (eval-and p)]
    [(p-or p) (eval-or p)]
    [(p-where p1 x p2) (p-eval (p-subst p1 x p2))]
    [(p-id x) (error 'p-eval "Open proposition (free occurrence of ~a)" x)
    ]
  ))

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;; 

;;----- ;;
;; P2.a ;;
;;----- ;;


#|
<expr> ::= (real <num>)
        | (imaginary <num>)
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | (id <sym>)
        | (with <sym> <expr> <expr>)
|#
(deftype Expr
  (real n)
  (imaginary n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (with expr body)
)

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= <num>
        | (<num> 'i)
        | (list + <s-expr> <s-expr>)
        | (list - <s-expr> <s-expr>)
        | (list 'if0 <s-expr> <s-expr> <s-expr>)
        | <sym>
        | (with (list <sym> <s-expr>) <s-expr>)
|#

;; parse : <s-expr> -> Expr

(define (parse s-expr) 
  (match s-expr
    [(? number? n) (real n)]
    [(list n 'i) (imaginary n)]
    [(? symbol? x) (id x)]
    [(list '+ l-sexpr r-sexpr) (add (parse l-sexpr) (parse r-sexpr))]
    [(list '- l-sexpr r-sexpr) (sub (parse l-sexpr) (parse r-sexpr))]
    [(list 'with (list (list (? symbol? x) sexpr)) body) (with (list (cons x (parse sexpr))) (parse body))]
    [(list 'with (list (cons x sexpr) rest ...) body)
      (with (cons (parse (list 'with (list x sexpr) body))
                  (map parse (list 'with (list (cons (car rest) (cdr rest))) body))))]
  )
)

;;----- ;;
;; P2.c ;;
;;----- ;;

;; subst :: Expr Symbol Expr -> Expr
(define (subst in what for) 
  )

;;----- ;;
;; P2.d ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
(define (from-CValue v) 
  (match v
    [(compV r i) (add (real r) (imaginary i))]
))

;; cmplx+ :: CValue CValue -> CValue
(define (cmplx+ v1 v2) 
  (match (list v1 v2)
    [(list (compV r1 i1) (compV r2 i2)) (compV (+ r1 r2) (+ i1 i2))]
))

;; cmplx- :: CValue CValue -> CValue
(define (cmplx- v1 v2) 
  (match (list v1 v2)
    [(list (compV r1 i1) (compV r2 i2)) (compV (- r1 r2) (- i1 i2))]
))

;; cmplx0? :: CValue -> Boolean
(define (cmplx0? v) 
  (match v
    [(compV r i)
      (if (and (zero? r) (zero? i))
        #t
        #f
    )]
))


;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
(define (interp expr) '???)
