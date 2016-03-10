;;Oppgave 1
;;a til e: se eget ark

;; i oppgaver f til h antar jeg at listene allerede er definert
;;f)
;;(car (cdr (cdr (listf))))
;;g)
;;(car (car (cdr listg)))
;;h)
;;(car (car (cdr (cdr listh))))
;;i)
(cons (cons 1 (cons 2 '())) (cons (cons 3 (cons 4 '()))'()))
(list (list 1 2) (list 3 4))

;;Oppgave 2
;;a)


(define (length2 items)
  (define (length-rek items count)
    (if (null? items)
        count
        (length-rek (cdr items) (+ 1 count))))
  (length-rek items 0))


;;b)
(define (rev-list items)
  (define (iter in out)
    (if (null? in)
        out
        (iter (cdr in)
              (cons (car in) out))))
  (iter items '()))



;;denne er løst halerekursivt fordi det ikke er noen ventende prosedyre i rekursjonen.
;;valgte halerekusivt fordi det var den første løsningen jeg kom på.


;;c)

;;d)

(define (nth index items)
  (define (iter items count curr)
    (if (= count index)
        curr
        (iter (cdr items) (+ count 1) (car items))))
  (iter items -1 0))


;;e)
(define (where number items)
  (define (iter items count curr)
    (if (= curr number)
        (if (> 0 count)
        #f
        count)
        (iter (cdr items) (+ count 1) (car items))))
  (iter items -1 0))


;;f)
(define (map2 proc items1 items2)
  (if (or (null? items1)
          (null? items2))
      '()
      (cons (proc (car items1) (car items2))
            (map2 proc (cdr items1) (cdr items2)))))

;;h)
(define (both? pred)
  (lambda (x y)
    (and (pred x)
         (pred y))))

;;i)
(define (self proc)
  (lambda (x)
    (proc x x)))


