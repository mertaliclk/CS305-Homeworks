(define get-operator
  (lambda (op)
    (cond
      ((eq? op '+) +)
      ((eq? op '-) -)
      ((eq? op '*) *)
      ((eq? op '/) /)
      (else #f))))

(define get-operator?
  (lambda (op)
    (cond
      ((or (eq? op '+)
           (eq? op '-)
           (eq? op '*)
           (eq? op '/)) #t)
      (else #f))))

(define cond-stmt?
  (lambda (statement)
    (and (list? statement)
         (equal? (car statement) 'cond)
         (> (length statement) 2)
         (cond-list? (cdr statement)))
  )
)

(define if-stmt?
  (lambda (statement)
    (and (list? statement)
         (equal? (car statement) 'if)
         (= (length statement) 4))
  )
)

(define cond-list?
  (lambda (clause-list)
    (if (null? clause-list)
        #f
        (if (and (list? (car clause-list)) (= (length (car clause-list)) 2))
            (if (equal? (caar clause-list) 'else)
                (if (null? (cdr clause-list))
                    #t
                    #f
                )
                (cond-list? (cdr clause-list))
            )
            #f
        )
    )
  )
)

(define var-bind-list?
  (lambda (bindings)
    (and (list? bindings)
         (list? (car bindings))
         (= (length (car bindings)) 2)
         (symbol? (caar bindings))
         (or (null? (cdr bindings))
             (var-bind-list? (cdr bindings)))
    )
  )
)

(define let-stmt?
  (lambda (statement)
    (and (list? statement)
         (equal? (car statement) 'let)
         (or (equal? '() (cadr statement))
             (var-bind-list? (cadr statement)))
         (= (length statement) 3)
    )
  )
)

(define define-stmt?
  (lambda (expression)
    (and (list? expression)
         (= (length expression) 3)
         (equal? (car expression) 'define)
         (symbol? (cadr expression))
    )
  )
)

(define letstar-stmt?
  (lambda (statement)
    (and (list? statement)
         (equal? (car statement) 'let*)
         (= (length statement) 3)
    )
  )
)

(define get-value
  (lambda (variable environment)
    (cond
      ((null? environment)
       (begin
         (display "cs305: ERROR")
         (newline)
         (newline)
         (repl environment)))
      ((equal? variable (caar environment))
       (cdar environment))
      (else (get-value variable (cdr environment)))
    )
  )
)

(define unbind
  (lambda (variable old-environment)
    (cond
      ((null? old-environment) '())
      ((eq? (caar old-environment) variable)
       (cdr old-environment))
      (else (cons (car old-environment) (unbind variable (cdr old-environment))))
    )
  )
)

(define update-env
  (lambda (variable value old-environment)
    (cons (cons variable value) (unbind variable old-environment))
  )
)

(define s7-interpret
  (lambda (expression environment)
    (cond
      ((null? expression) expression)
      ((number? expression) expression)
      ((symbol? expression) (get-value expression environment))
      ((not (list? expression)) "ERROR")
      ((if-stmt? expression)
       (if (eq? (s7-interpret (cadr expression) environment) 0)
           (s7-interpret (cadddr expression) environment)
           (s7-interpret (caddr expression) environment)
       )
      )
      ((cond-stmt? expression)
       (if (eq? (length expression) 3)
           (if (eq? (s7-interpret (caadr expression) environment) 0)
               (s7-interpret (car (cdaddr expression)) environment)
               (s7-interpret (cadadr expression) environment)
           )
           (let ((condition (caadr expression)) (then (cadadr expression)) (else-binding (cons 'cond (cddr expression))))
             (let ((temp-var (list 'if condition then else-binding))) (s7-interpret temp-var environment))
           )
       )
      )
      ((let-stmt? expression)
       (let* ((bindings (map s7-interpret (map cadr (cadr expression)) (make-list (length (map cadr (cadr expression))) environment)))
              (added-environment (append (map cons (map car (cadr expression)) bindings) environment)))
         (s7-interpret (caddr expression) added-environment)))
      ((letstar-stmt? expression)
       (cond
         ((eq? (length (cadr expression)) 0)
          (s7-interpret (list 'let '() (caddr expression)) environment))
         ((eq? (length (cadr expression)) 1)
          (s7-interpret (list 'let (cadr expression) (caddr expression)) environment))
         (else
          (let* ((part-member (s7-interpret (car (cdaadr expression)) environment))
                 (added-environment (cons (cons (caaadr expression) part-member) environment)))
            (s7-interpret (list 'let* (cdadr expression) (caddr expression)) added-environment)
          )
         )
       )
      )
      ((get-operator (car expression))
       (let ((operands (map s7-interpret (cdr expression) (make-list (length (cdr expression)) environment)))
             (operator (get-operator (car expression))))
         (apply operator operands)))
      (else "ERROR")
    )
  )
)

(define repl
  (lambda (environment)
    (let* ((dummy1 (display "cs305> "))
           (expression (read))
           (new-environment (if (define-stmt? expression)
                               (update-env (cadr expression) (s7-interpret (caddr expression) environment) environment)
                               environment))
           (value (if (define-stmt? expression)
                      (cadr expression)
                      (s7-interpret expression environment)))
           (dummy2 (display "cs305: "))
           (dummy3 (display value))
           (dummy4 (newline))
           (dummy5 (newline)))
      (repl new-environment))
  )
)

(define cs305
  (lambda () (repl '()))
)


