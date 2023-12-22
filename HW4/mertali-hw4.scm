(define check-triple?
  (lambda (tripleList)
    (cond
      ((null? tripleList) #t)
      ((not (list? tripleList)) #f)
      ((null? (car tripleList)) #f)
      ((not (list? (car tripleList))) #f)
      ((not (check-length? (car tripleList) 3)) #f)
      ((not (check-sides? (car tripleList))) #f)
      (else (check-triple? (cdr tripleList))))))

(define check-length?
    (lambda (inTriple count)
        (if (null? inTriple)
            (if (zero? count)
                #t
                #f
            )
            (check-length? (cdr inTriple) (- count 1)))))

(define check-sides?
  (lambda (inTriple)
    (and (number? (car inTriple))
         (number? (cadr inTriple))
         (number? (caddr inTriple))
         (> (car inTriple) 0)
         (> (cadr inTriple) 0)
         (> (caddr inTriple) 0))))

(define sort-all-triples
  (lambda (tripleList)
    (if (null? tripleList)
        '()
        (cons (sort-triple (car tripleList))
              (sort-all-triples (cdr tripleList))))))

(define sort-triple
  (lambda (inTriple)
    (list (car (sort inTriple <))
          (cadr (sort inTriple <))
          (caddr (sort inTriple <)))))

(define filter-triangle
  (lambda (tripleList)
    (cond
      ((null? tripleList) '())
      ((triangle? (car tripleList))
       (cons (car tripleList)
             (filter-triangle (cdr tripleList))))
      (else (filter-triangle (cdr tripleList))))))

(define triangle?
  (lambda (triple)
    (> (+ (car triple) (cadr triple)) (caddr triple))))

(define filter-pythagorean
  (lambda (tripleList)
    (cond
      ((null? tripleList) '())
      ((pythagorean-triangle? (car tripleList))
       (cons (car tripleList)
             (filter-pythagorean (cdr tripleList))))
      (else (filter-pythagorean (cdr tripleList))))))

(define pythagorean-triangle?
  (lambda (triple)
    (= (+ (* (car triple) (car triple))
          (* (cadr triple) (cadr triple)))
       (* (caddr triple) (caddr triple)))))

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

(define main-procedure
  (lambda (tripleList)
    (if (or (null? tripleList) (not (list? tripleList)))
        (error "ERROR305: the input should be a list full of triples")
        (if (check-triple? tripleList)
            (sort-area
             (filter-pythagorean
              (filter-triangle
               (sort-all-triples tripleList))))
            (error "ERROR305: the input should be a list full of triples")))))
