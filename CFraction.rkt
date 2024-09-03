#lang play

(print-only-errors #t)
(require math/flonum)


#| 
   <CFraction> ::= <simple> | <compound>  
   <simple> ::= <number>
   <compound> ::= <number> <number> <CFraction> 
|#
(deftype CFraction
   (simple value)
   (compound integer numerator denominator))

; ; eval :: CFraction -> Rational
; ; Evalúa una fracción continua, devolviendo el número racional que representa
(define (eval cf)
   (match cf
      [(simple v) v]
      [(compound int num den) (+ int (/ num (eval den)))]))

; ; test de (eval cf)
(test (eval (simple 1)) 1)
(test (eval (compound 1 1 (simple 1))) (+ 1 (/ 1 1)))
(test (eval (compound 3 2 (compound 1 1 (simple 4)))) (+ 3 (/ 2 (+ 1 (/ 1 4)))))
(test (eval (compound 6 1 (compound 6 9 (compound 6 25 (simple 6))))) (+ 6 (/ 1 (+ 6 (/ 9 (+ 6 (/ 25 6)))))))

; ; degree :: CFraction -> Integer
; ; Devuelve el grado de una fracción continua
(define (degree cf)
   (match cf
      [(simple _) 0]
      [(compound _ _ den) (+ 1 (degree den))]))

; ; test de (degree cf)
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
; ; Evalúa una fracción continua, devolviendo el número racional que representa
(define eval-fold-cfraction
   (fold-cfraction
      (lambda (v) v)
      (lambda (int num den) (+ int (/ num den)))
   ))

; ; test de eval-fold-cfraction
(test (eval-fold-cfraction (simple 1)) 1)
(test (eval-fold-cfraction (compound 1 1 (simple 1))) (+ 1 (/ 1 1)))
(test (eval-fold-cfraction (compound 3 2 (compound 1 1 (simple 4)))) (+ 3 (/ 2 (+ 1 (/ 1 4)))))

; ; degree-fold-cfraction :: CFraction -> Integer
; ; Devuelve el grado de una fracción continua
(define degree-fold-cfraction 
   (fold-cfraction
      (lambda (_) 0)
      (lambda (_ num den) (+ 1 den))
   ))

; ; test de degree-fold-cfraction
(test (degree-fold-cfraction (simple 4567)) 0)
(test (degree-fold-cfraction (compound 1 1 (compound 1 1 (simple 1)))) 2)
(test (degree-fold-cfraction (compound 567 123 (compound 234 987 (compound 678 89 (simple 9))))) 3)

; ; mysterious-cf :: Integer -> CFraction
; ; Genera la secuencia de fracciones continuas a continuacion
; ; 6, 6 + 1^2/6, 6 + 1^2/(6 + 3^2/6), 6 + 1^2/(6 + 3^2/(6 + 5^2/6)), ...
(define (mysterious-cf n)
   (define (aux n odd)
      (if (= n 0)
         (simple 6)
         (compound 6 (sqr odd) (aux (- n 1) (+ odd 2)))
      ))
   (aux n 1))

; ; test de (mysterious-cf n)
(test (mysterious-cf 0) (simple 6))
(test (mysterious-cf 1) (compound 6 (sqr 1) (simple 6)))
(test (mysterious-cf 2) (compound 6 (sqr 1) (compound 6 (sqr 3) (simple 6))))
(test (mysterious-cf 3) (compound 6 (sqr 1) (compound 6 (sqr 3) (compound 6 (sqr 5) (simple 6)))))


; ; from-to :: Integer Integer -> ListOf Integer
; ; Genera una lista de enteros desde i hasta j
(define (from-to i j)
   (if (< i j)
      (cons i (from-to (+ i 1) j))
       '()
       
    ))

; ; test de (from-to i j)
(test (from-to 0 1) (list 0))
(test (from-to 0 3) (list 0 1 2))
(test (from-to 3 0) '())
(test (from-to 5 11) (list 5 6 7 8 9 10))


; ; mysterious-list :: Integer -> ListOf Float
; ; Devuelve una lista tal que el i-ésimo elemento es calculado como la resta
; ; de la evaluación (utilizando eval o eval2) de (mysterious-cf i ) menos 3. Luego
; ; de evaluar cada elemento, utilice la función fl para transformar los números
; ; fraccionarios a su representación en punto flotante
(define (mysterious-list n)
  (map fl
       (map (lambda (i) (- i 3))
         (map eval 
            (map mysterious-cf (from-to 0 n))
         )
      )
   ))

; ; test de (mysterious-list n)
(test (mysterious-list 0)  '())
(test (mysterious-list 1) (list 3.0))
(test (mysterious-list 2) (list 3.0 3.1666666666666665))
(test (mysterious-list 3) (list 3.0 3.1666666666666665 3.1333333333333333))
(test (mysterious-list 4) (list 3.0 3.1666666666666665 3.1333333333333333 3.145238095238095))
(test (mysterious-list 5) (list 3.0 3.1666666666666665 3.1333333333333333 3.145238095238095 3.1396825396825395))
(test (mysterious-list 6) (list 3.0 3.1666666666666665 3.1333333333333333 3.145238095238095 3.1396825396825395 3.1427128427128425))
(test (mysterious-list 7) (list 3.0 3.1666666666666665 3.1333333333333333 3.145238095238095 3.1396825396825395 3.1427128427128425 3.1408813408813407))
; ; es dificil de determinar a qué converge esta secuencia cuando n tiende a infinito
; ; pero cada vez se parece más a pi = 3.14159265359


; ; rac-to-cf :: Rational -> CFraction
; ; que transforma un número racional no-negativo en su representación en forma
; ; de fracción continua
(define (rac-to-cf r)
  (if (= r (floor r))
      (simple (floor r))
      (compound (floor r) 1
               (rac-to-cf (/ 1 (- r (floor r)))))))

; ; test de (rac-to-cf r)
(test (rac-to-cf (+ 3 49/200)) (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
(test (rac-to-cf 3) (simple 3))