;; Axel Boix og Lukas Dijk
(load "huffman.scm")

;;Oppgave 1
;;a)
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car (proc x y))
  (x))
(define (p-cdr (proc x y))
  (y))
;;(p-cons "foo" "bar")

;;(p-car (p-cons "foo" "bar"))

(p-cdr (p-cons "foo" "bar"))