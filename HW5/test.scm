(define get-operator (lambda (op-symbol)
  (cond
    ((equal? op-symbol '+) +)
    ((equal? op-symbol '-) -)
    ((equal? op-symbol '*) *)
    ((equal? op-symbol '/) /)
    (else #f))
  )
)

(define get-operator? (lambda (op-symbol)
  (cond
    ((equal? op-symbol '+) #t)
    ((equal? op-symbol '-) #t)
    ((equal? op-symbol '*) #t)
    ((equal? op-symbol '/) #t)
    (else #f))
  ) 
)



(define cond-list?
        (lambda (hwlist)
                (if (null? hwlist)
                        #f
                        (if (and (list? (car hwlist)) (= (length (car hwlist)) 2))
                                (if (equal? (caar hwlist) 'else)
                                        (if (null? (cdr hwlist))
                                                #t
                                                #f
                                        )
                                        (cond-list? (cdr hwlist))
                                )
                                #f
                        )
                )
        )
)

(define cond-stmt?
        (lambda (hwlist)
                (and (list? hwlist) 
		(equal? (car hwlist) 'cond) 
		(> (length hwlist) 2) 
		(cond-list? (cdr hwlist)))
        )
)

(define if-stmt? (lambda (hwlist)
              (and (list? hwlist) 
		(equal? (car hwlist) 'if) 
		(= (length hwlist) 4))
	)
)


(define var_bind_list? (lambda (hwlist)
	(and (list? hwlist) 
		(list? (car hwlist)) 
		(= (length (car hwlist)) 2) 
		(symbol? (caar hwlist))
		(or (null? (cdr hwlist)) 
		(var_bind_list? (cdr hwlist)))
	)
))

(define letstar-stmt? (lambda (hwlist)
    ( and (list? hwlist) 
	(equal? (car hwlist) 'let*) 
	(= (length hwlist) 3) )
    )
)

(define let-stmt? (lambda (hwlist)
    (and (list? hwlist)
            (equal? (car hwlist) 'let) 
	    (or (equal? () (cadr hwlist)) 
	    (var_bind_list? (cadr hwlist))) 
	    (= (length hwlist) 3)
    )
))

(define define-stmt? (lambda (e)
    (and (list? e) 
	(= (length e) 3) 
	(equal? (car e) 'define) 
	(symbol? (cadr e))
    )
))

(define get-value
   (lambda (var env)
      (cond
	((null? env) (let*
        (
        (dummy1 (display "cs305: ERROR"))
        (dummy2 (newline))
        (dummy3 (newline))
         )
       (repl env)
        ))
        ((equal? var (caar env)) (cdar env))
        (else (get-value var (cdr env)))
      )
   )
)

(define unbind
  (lambda (var old-env)
     (cond
       ((null? old-env) '())
       ((eq? (caar old-env) var) (cdr old-env))
       (else (cons (car old-env) (unbind var (cdr old-env))))
     )
  )
)

(define update-env
  (lambda (var val old-env)
     (cons (cons var val) (unbind var old-env))
  )
)

(define repl (lambda (env)
  (let* (
         (dummy1 (display "cs305> "))
         (expr (read))
         (new-env 
		(if (define-stmt? expr)
			(update-env (cadr expr) (s7-interpret (caddr expr) env) env)
                      env)
		)
         (val (if (define-stmt? expr)
                  (cadr expr)
                  (s7-interpret expr env)))
         (dummy2 (display "cs305: "))
         (dummy3 (display val))
         (dummy4 (newline))
         (dummy5 (newline)))
     (repl new-env))
  )
)

(define s7-interpret (lambda (e env)
  (cond
    ((null? e) e)
    ((number? e) e)
    ((symbol? e) (get-value e env))
    ((not (list? e)) "ERROR")
    ((if-stmt? e) 
	(if (eq? (s7-interpret (cadr e ) env) 0)
        	(s7-interpret (cadddr e) env)
                (s7-interpret (caddr e) env)
	)
    )
    ((cond-stmt? e)
        (if (eq? (length e) 3)
                (if (eq? (s7-interpret (caadr e) env) 0)
                        (s7-interpret (car (cdaddr e)) env)
                        (s7-interpret (cadadr e) env)
                )
                (let ((condition (caadr e)) (then (cadadr e)) (elsebinding (cons 'cond (cddr e))) )
                (let ((tempvar (list 'if condition then elsebinding))) (s7-interpret tempvar env))
          )
        )
      )
    ((let-stmt? e)
        (let*
             ((members (map s7-interpret (map cadr (cadr e)) (make-list (length (map cadr (cadr e))) env)))
             (addedenv (append ( map cons (map car (cadr e)) members) env)))
             (s7-interpret (caddr e) addedenv)))

     ((letstar-stmt? e)
            (cond
                 ((eq? (length (cadr e)) 0) (s7-interpret (list 'let '() (caddr e)) env))
                 ((eq? (length (cadr e)) 1) (s7-interpret (list 'let (cadr e) (caddr e)) env))
                 (else
                 	(let*
                            ((partymemb (s7-interpret (car (cdaadr e)) env))
                              (addedenv (cons (cons (caaadr e) partymemb) env)))
                              (s7-interpret (list 'let* (cdadr e) (caddr e)) addedenv)))))
    ((get-operator(car e))
      (let ((operands (map s7-interpret (cdr e) (make-list (length (cdr e)) env)))
            (operator (get-operator (car e))))
        (apply operator operands)))
    (else "ERROR" ))
))

(define cs305 (lambda () (repl '())))