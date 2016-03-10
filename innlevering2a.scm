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


;;Oppgave 2
;;a)

(define (member? symb items)
  (define (iter rest)
    (cond ((null? rest) #f)
          ((eq? (car rest) symb) #t)
          (else (iter (cdr rest)))))
  (iter items))
          