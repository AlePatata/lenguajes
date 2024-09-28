#lang play

#|

Hizo Ud uso de la whiteboard policy: (Indique SI/NO) NO, solo consultas al aux
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
; Parsea una proposición en notación concreta a una proposición en notación abstracta
(define (parse-prop s-prop) 
  (match s-prop
    ['true (tt)]
    ['false (ff)]
    [(? symbol? x) (p-id x)] 
    [(list 'not sprop) (p-not (parse-prop sprop))]

    ; Caso 'and sin argumentos
    [(list 'and) (error 'parse-prop "and expects at least two operands")]
    [(list 'and first rest ...) 
      (if (null? rest) ; Caso 'and con un solo argumento
        (error 'parse-prop "and expects at least two operands")
        (p-and (cons (parse-prop first)
                   (map parse-prop rest)
        ))
    )]
    ; Caso 'or sin argumentos
    [(list 'or) (error 'parse-prop "or expects at least two operands")]
    [(list 'or first rest ...) 
      (if (null? rest) ; Caso 'or con un solo argumento
        (error 'parse-prop "or expects at least two operands")
        (p-or (cons (parse-prop first)
                  (map parse-prop rest)
        ))
    )]

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
; Convierte un valor de verdad en una proposición, los únicos
; casos válidos son ttV y ffV
(define (from-PValue p-value) 
  (match p-value
    [(ttV) (tt)]
    [(ffV) (ff)]
  ))


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
;; (subst in what for)
;; substituye todas las ocurrencias libres del identificador 'what'
;; en la proposición 'in' por la proposición 'for'
(define (p-subst in what for) 
  (match in
    [(tt) (tt)]
    [(ff) (ff)]
    [(p-not p) (p-not (p-subst p what for))]

    [(p-and ps) 
      (p-and (map (lambda (p) (p-subst p what for)) ps))]

    [(p-or ps) 
      (p-or (map (lambda (p) (p-subst p what for)) ps))]

    [(p-id x) 
      (if (symbol=? x what)
        for
        (p-id x)
    )]
    [(p-where p1 x p2)
      (if (symbol=? x what)
        (p-where p1 x p2)
        (p-where (p-subst p1 what for) x p2)
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
        | (id <sym>)
        | (with [(<sym> <expr>)*] <expr>)
|#
(deftype Expr
  (real n)
  (imaginary n)
  (add l r)
  (sub l r)
  (id x)
  (with par body)
)

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= <num>
        | (<num> 'i)
        | <sym>
        | (list '+ <s-expr> <s-expr>)
        | (list '- <s-expr> <s-expr>)
        | (list 'with (list <sym> <s-expr>) <s-expr>)
|#

;; parse : <s-expr> -> Expr
; Parsea una expresión en notación concreta a una expresión en notación abstracta
(define (parse s-expr) 
  (match s-expr
    [(? number? n) (real n)]
    [(list (? number? n) 'i) (imaginary n)]
    [(? symbol? x) (id x)]
    [(list '+ l-sexpr r-sexpr) (add (parse l-sexpr) (parse r-sexpr))]
    [(list '- l-sexpr r-sexpr) (sub (parse l-sexpr) (parse r-sexpr))]
    [(list 'with body) (error 'parse "'with' expects at least one definition")]
    [(list 'with pairs body)
      (with (map (lambda (par) (cons (car par) (parse (car (cdr par)))))
            pairs)
            (parse body))]
  )
)

;;----- ;;
;; P2.d ;;
;;----- ;;

;; aux-subst :: ListOf(Symbol Expr) Symbol Expr -> Expr
; Dada una lista de pares (variable, expresion) y una variable y una expresion
; reemplaza todas las ocurrencias de la variable por la expresion en la lista
(define (aux-subst pairs what for) 
  (map (lambda (par)
         (let ([var (car par)] 
               [expr (cdr par)])
           (if (symbol=? var what)
               (cons var for)  ; Keep var unchanged, replace its value
               (cons var (subst expr what for)))))  ; Substitute expr
       pairs)
)


;; subst :: Expr Symbol Expr -> Expr
; Realiza la substitución de una expresión por un identificador
(define (subst in what for) 
  (match in 
    [(real n) (real n)]
    [(imaginary n) (imaginary n)]
    [(id x) (if (symbol=? x what)
        for
        (id x)
    )]
    [(add l-expr r-expr) (add (subst l-expr what for) (subst r-expr what for))]
    [(sub l-expr r-expr) (sub (subst l-expr what for) (subst r-expr what for))]
    
    [(with pairs body) 
      (let ([local #f]) 
        (map (lambda (par)
          (if (symbol=? (car par) what)
            (set! local #t)
            par))  
          pairs)
        (if local
          (with pairs body)
          (with (aux-subst pairs what for) (subst body what for))
        )
    )] 
  ))

;;----- ;;
;; P2.c ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
; Convierte un valor complejo en una expresión
(define (from-CValue v) 
  (match v
    [(compV r i) (add (real r) (imaginary i))]
))

;; cmplx+ :: CValue CValue -> CValue
; Suma dos valores complejos
(define (cmplx+ v1 v2) 
  (match (list v1 v2)
    [(list (compV r1 i1) (compV r2 i2)) (compV (+ r1 r2) (+ i1 i2))]
))

;; cmplx- :: CValue CValue -> CValue
; Resta dos valores complejos
(define (cmplx- v1 v2) 
  (match (list v1 v2)
    [(list (compV r1 i1) (compV r2 i2)) (compV (- r1 r2) (- i1 i2))]
))

;; cmplx0? :: CValue -> Boolean
; Determina si un valor complejo es 0
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
; Reduce una expresión (Expr) en un valor del lenguaje (CValue)
(define (interp expr) 
  (match expr 
    [(real n) (compV n 0)]
    [(imaginary n) (compV 0 n)]
    [(id x) x]
    [(add l r) (cmplx+ (interp l) (interp r))]
    [(sub l r) (cmplx- (interp l) (interp r))]
    [(with pairs body)
      (interp (foldl (lambda (pair body)
           (subst body (car pair) (cdr pair)))
         body
         pairs))]
  ))
 