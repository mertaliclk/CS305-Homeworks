(define sort-area
  (lambda (tripleList)
    (if (null? tripleList)
        '()
        (insert-sort tripleList))))

(define insert-sort
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (insert (car lst)
                    (insert-sort (cdr lst)))))))

(define insert
  (lambda (elem lst)
    (cond
      ((null? lst) (list elem))
      ((< (get-area elem) (get-area (car lst)))
       (cons elem lst))
      (else (cons (car lst)
                  (insert elem (cdr lst)))))))

(define get-area
  (lambda (triple)
    (/ (* (car triple) (cadr triple)) 2)))