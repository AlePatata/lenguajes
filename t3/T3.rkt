#lang play

#|

Hizo Ud uso de la whiteboard policy: (Indique SI/NO)
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------------;;
;; P1.a, P1.e, P2.a ;;
;;------------------;;


#|
<expr> ::= (id <sym>)
         |(num <number>)
         | (+ <expr> <expr>)
         | ('nil)
         | ('conz <expr> <expr>)
         | ('list <expr> <expr> ...)
         | ('fun <sym> <expr>)
         | ('app <expr> <expr>)
         | ('match <expr> <expr> ...)
|#

(deftype Expr
  (id x)
  (num n)
  (add l r)
  (nil)
  (conz l r)
  (fun x body)
  (app f x)
  (match e cases ...)
  )


;;------------------;;
;; P1.b, P1.e, P2.b ;;
;;------------------;;

;; parse : s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(? symbol? x) (id x)]
    [(? number? n) (num n)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'nil) (nil)]
    [(list 'cons first second) (conz (parse first) (parse second))]
    [(list 'list first rest ...) 
      (if (null? rest) 
        (conz (parse first) (nil))
        (conz (parse first) 
              (parse (cons 'list rest)))
      )]
    [(list 'fun x body) (fun (parse-pattern x) (parse body))]
    [(list f x) (app (parse f) (parse x))]
    [(list 'match e cases ...) 
      (if (null? cases)
        (error "SyntaxError: match expression must have at least one case")
        (pmatch (parse e) 
                (map (lambda (case) 
                             (match case [(list pattern body) 
                                          (cons (parse-pattern pattern) 
                                                (parse body))])) 
                      cases)
        ))]
  ))

;;----- ;;
;; P1.c ;;
;;----- ;;

#|
<pattern> ::= (numP <number>)
            | (nilP)
            | (varP <sym>)
            | (conzP <pattern> <pattern>)
|#

(deftype Pattern
  (numP n)
  (nilP)
  (varP x)
  (conzP l r)
  )

;;----- ;;
;; P1.d ;;
;;----- ;;

;; parse-pattern : s-expr -> Pattern
(define (parse-pattern s-expr) 
  (match s-expr
    [(? number? n) (numP n)]
    [(list 'nil) (nilP)]
    [(? symbol? x) (varP x)]
    [(list 'cons first second) (conzP (parse-pattern first) (parse-pattern second))]
    [(list 'list first rest ...) 
      (if (null? rest) 
        (conzP (parse-pattern first) (nilP))
        (conzP (parse-pattern first) 
               (parse-pattern (cons 'list rest)))
      )]
))

;;----- ;;
;; P1.f ;;
;;----- ;;

#|
<value> ::= (numV <number>)
         | (nilV)
         | (conzV <value> <value>)
         | (closureV <pattern> <expr> <env>)
|#

(deftype Value
  (numV n)
  (nilV)
  (conzV l r)
  (closureV pattern body env)
  )

;;---------------------------;;
;; BEGIN utility definitions ;;
;;---------------------------;;
#|
<env> ::= (mtEnv)
       | (extEnv <sym> <value> <env>)
|#

(deftype Env
  (mtEnv)
  (extEnv x v env))

;; extend-env : Symbol Value Env -> Env
(define (extend-env x v env)
  (extEnv x v env))

;; empty-env : Env
(define empty-env (mtEnv))

;; extend-env* : (Listof (Symbol * Value)) Env -> Env
(define (extend-env* bindings env)
  (foldr
   (lambda (binding env) (match binding [(cons x v) (extend-env x v env)]))
   env
   bindings))

;; lookup-env : Symbol Env -> Value
(define (lookup-env x env)
  (match env
    [(mtEnv) (error "LookupError: variable ~a not found" x)]
    [(extEnv y v env) (if (equal? x y) v (lookup-env x env))]))

;; num+ : Value Value -> Value
(define (num+ v1 v2)
  (match v1
    [(numV n) (match v2
                [(numV m) (numV (+ n m))]
                [_ (error "TypeError: expected a number")])]
    [_ (error "TypeError: expected a number")]))
;;-------------------------;;
;; END utility definitions ;;
;;-------------------------;;

;;----- ;;
;; P1.g ;;
;;----- ;;

#|
<result> e v ::= (failure e)
               | (success v)
|#

(deftype Result
  (failure e)
  (success v))

;; generate-substs : Pattern Value -> (Result String (Listof (Symbol * Value)))
(define (generate-substs p v) 
  (match p
    [(numP n) 
      (match v
        [(numV m) (if (equal? n m) (success '()) (failure "MatchError: given number does not match pattern"))]
        [_ (failure "MatchError: expected a number")])] ; si el valor no es del tipo (numV m)
    [(nilP) 
      (match v
        [(nilV) (success '())]
        [_ (failure "MatchError: expected nil")])]
    [(varP) 
      (match v
        [(numV n) (success (list (cons varP v)))]
        [_ (failure "MatchError: expected a number")])]
    [(conzP l r) 
      (match v
        [(conzV l1 r1) 
          (match (generate-substs l l1)
            [(success substs1) 
              (match (generate-substs r r1)
                [(success substs2) (success (append substs1 substs2))]
                [(failure e) (failure e)])]
            [(failure e) (failure e)])]
        [_ (failure "MatchError: expected a cons constructor")]
      )]
  ))

;;------------;;
;; P1.h, P2.c ;;
;;------------;;

;; interp : Expr Env -> Value
(define (interp expr env) 
  (match expr
    [(id x) (lookup-env x env)]
    [(num n) (numV n)]
    ;[(add l r) (num+ (interp l env) (interp r env))] 
    [(nil) (nilV)]
    [(conz l r) (conzV (interp l env) (interp r env))]
    [(fun x body) (closureV x body env)]
    [(app f x) 
      (match (interp f env)
        [(closureV pattern body fenv) 
          (match (interp x env)
            [(success v) 
              (match (generate-substs pattern v)
                [(success substs) 
                  (interp body (extend-env* substs fenv))]
                [(failure e) (failure e)])]
            [(failure e) (failure e)])]
        [(failure e) (failure e)])]
    [(match e cases) 
      (match (generate-subst e cases)
        [(success v) 
          (match (v cases)
            [(success (cons pattern body)) 
              (interp body (extend-env* (generate-substs pattern v) env))]
            [(failure e) (failure e)])]
        [(failure e) (failure e)])]
    ))


;;----- ;;
;; P2.d ;;
;;----- ;;

#|
En función de lo implementado en la pregunta anterior, argumente porqué es útil que la función
generate-subst no lance un error (cuando el valor no calza con el patrón) y, en cambio, retorne
un mensaje.

R: Es útil para el programador que utilizará el lenguaje, ya que podrá seguir ejecutando 
las demás partes de su programa sin detenerse por el error que probablemente sea un error
de tipeo. Además, el mensaje de error puede ser más descriptivo y ayudar al programador a
entender que la causa del fallo no fue en su lógica si no en la utilización del lenguaje.

|#
