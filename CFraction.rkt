#lang play

(print-only-errors #t)

#| 
   <CFraction> ::= <simple> | <compound>  
   <simple> ::= <number>
   <compound> ::= <number> <number> <CFraction> |#
(deftype CFraction
   (simple value)
   (compound integer numerator denominator))

; ; eval :: CFraction -> Rational
; ; Evalúa una fracción continua, devolviendo el número racional que representa
(define (eval cf)
   (match cf
      [(simple v) v]
      [(compound int num den) (+ int (/ num (eval den)))]))

; ; test
(test (eval (simple 1)) 1)
(test (eval (compound 1 1 (simple 1))) (+ 1 (/ 1 1)))
(test (eval (compound 3 2 (compound 1 1 (simple 4)))) (+ 3 (/ 2 (+ 1 (/ 1 4)))))

; ; degree :: CFraction -> Integer
; ; Devuelve el grado de una fracción continua
(define (degree cf)
   (match cf
      [(simple _) 0]
      [(compound _ _ den) (+ 1 (degree den))]))

; ; test
(test (degree (simple 4567)) 0)
(test (degree (compound 1 1 (compound 1 1 (simple 1)))) 2)
(test (degree (compound 567 123 (compound 234 987 (compound 678 89 (simple 9))))) 3)

; ; fold-cfraction :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
; ; Captura el esquema de recursión asociado a CFraction
(define (fold-cfraction f g)
   (lambda (cf)
      (match cf
         [(simple v) (f v)]
         [(compound int num den) 
            (g int num
               ((fold-cfraction f g) den)
            )]
   )))

; ; eval-fold-cfraction :: CFraction -> Rational
(define eval-fold-cfraction
   (fold-cfraction
      (lambda (v) v)
      (lambda (int num den) (+ int (/ num den)))
   ))

; ; test
(test (eval-fold-cfraction (simple 1)) 1)
(test (eval-fold-cfraction (compound 1 1 (simple 1))) (+ 1 (/ 1 1)))
(test (eval-fold-cfraction (compound 3 2 (compound 1 1 (simple 4)))) (+ 3 (/ 2 (+ 1 (/ 1 4)))))

; ; degree-fold-cfraction :: CFraction -> Integer
(define degree-fold-cfraction 
   (fold-cfraction
      (lambda (_) 0)
      (lambda (_ num den) (+ 1 den))
   ))

; ; test
(test (degree-fold-cfraction (simple 4567)) 0)
(test (degree-fold-cfraction (compound 1 1 (compound 1 1 (simple 1)))) 2)
(test (degree-fold-cfraction (compound 567 123 (compound 234 987 (compound 678 89 (simple 9))))) 3)




; ; mysterious-cf :: Integer -> CFraction
(define (mysterious-cf n)
   (define (aux n odd)
      (if (= n 0)
         (simple 6)
         (compound 6 (sqr odd) (aux (- n 1) (+ odd 2)))
      ))
   (aux n 1))



; ; test
(test (mysterious-cf 0) (simple 6))
(test (mysterious-cf 1) (compound 6 (sqr 1) (simple 6)))
(test (mysterious-cf 2) (compound 6 (sqr 1) (compound 6 (sqr 3) (simple 6))))


; ; from-to :: Integer Integer -> ListOf Integer
(define (from-to i j)
   (if (< i j)
      (append (from-to i (- j 1)) (list j))
      (list i)))

; ; test
(test (from-to 0 0) (list 0))
(test (from-to 0 3) (list 0 1 2 3))


; ; mysterious-list :: Integer -> ListOf Float
(define (mysteriour-list))
