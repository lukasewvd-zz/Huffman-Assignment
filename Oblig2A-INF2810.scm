;; Axel Boix og Lukas Wijgaart van Dijk
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
;;Denne map-prossedyren bruker operatorene fra liste a2 og anvender dem på liste a1 og a3.
;;Det vil si at i dette eksemplet vil 6, -4, 21, 1/2 blir resultatet av map-operasjonen.
;;Under er et eksempel på hvordan (lambda (x y z) (y x z)) man kalles direkte:
((lambda (x y z) (y x z)) 1337 + 420)
;;Resultatet blir 1757.

;;Oppgave 2
;;a)

(define (member? symb items)
  (define (iter rest)
    (cond ((null? rest) #f)
          ((eq? (car rest) symb) #t)
          (else (iter (cdr rest)))))
  (iter items))

;;b)

;;Man lager en intern hjelpeprosedyre, fordi man vil endre på current-branch, mens
;;man fortsatt har tilgang til roten av treet (tree) som fortsatt er tilgjengelig
;;i decode. Dersom man bare hadde kalt på decode i stedenfor decode-1, ville man
;;ha mistet roten etter første rekursive kall på decode

;;c)

(define (haledecode bits tree)
  (define (haledecode-1 bits current-branch items)
    (if (null? bits)
        (reverse items)
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (haledecode-1 (cdr bits)
                            tree
                            (cons (symbol-leaf next-branch)
                                  items))
              (haledecode-1 (cdr bits)
                            next-branch
                            items))))) 
  (haledecode-1 bits tree '()))


;;d)
(haledecode sample-code sample-tree)
;;(ninjas fight ninjas by night)


;;e)
(define (encode items tree)
  (define (encode-1 items current-branch)
    (if (null? items)
      '()
      (if (leaf? current-branch)
          (encode-1 (cdr items) tree)
          (if (member? (car items) (symbols (left-branch current-branch)))
                (cons 0 (encode-1 items (left-branch current-branch)))
                (cons 1 (encode-1 items (right-branch current-branch)))))))
  (encode-1 items tree))

(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)
          
;;f)
(define (grow-huffman-tree freqs)
  (define (iter tree)
    (null? (cdr tree))
    
    

;;h)
(define (huffman-leaves tree)
  (if (leaf? tree)
      (list (list (symbol-leaf tree)
                  (weight-leaf tree)))
      (append (huffman-leaves (left-branch tree))
              (huffman-leaves (right-branch tree)))))

  