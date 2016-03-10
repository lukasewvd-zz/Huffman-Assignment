;; Axel Boix og Lukas Dijk
(load "huffman.scm")

;;Oppgave 1
;;a)
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))
(define (p-cdr proc)
  (proc (lambda (x y) y)))

;;b)
(define foo 30)

(let ((x foo)
      (y 20))
  (+ foo x y))

((lambda (x y)
   (+ foo x y))
foo 20)

(let ((foo 10))
  (let ((x foo)
        (y 20))
    (+ foo x y)))

((lambda (foo)
    ((lambda (x y)
       (+ foo x y))
      foo 20))
    10)

;;c)
(define a1 (list 1 2 3 4))
(define a2 (list + - * /))
(define a3 (list 5 6 7 8))
(map (lambda (x y z) (y x z))
a1 a2 a3)

;;Oppgave 2
;;a)

(define (member? symb items)
  (define (iter rest)
    (cond ((null? rest) #f)
          ((eq? (car rest) symb) #t)
          (else (iter (cdr rest)))))
  (iter items))
          
