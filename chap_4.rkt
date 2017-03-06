#lang sicp

;; Eval and apply
;;;;;;;;;;;;;;;;;;

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (do-apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (do-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define error display)

(define (list-of-values exps env)
  (if [no-operands? exps]
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if [true? (eval (if-predicate exp) env)]
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

; Quotations have the form
;
;   (quote <text-of-quotation>)
;
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

; List tagged by first element
(define (tagged-list? exp tag)
  (if [pair? exp] (eq? (car exp) tag) #f))

; Assignments have the form
;
;   (set! <var> <value>)
;
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; Definitions have the form
;
;   (define <var> <value>)
;
; Or
;
;   (define (<var> <parameter_1> ... <parameter_n>) <body>)
;
; As shortform for
;
;   (define <var> (lambda (<parameter_1> ... <parameter_n>) <body>))
;
(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if [symbol? (cadr exp)]     ;                   '(define VAR value)
    (cadr exp)
    (caadr exp)))              ;                   '(define (VAR opts) value)

(define (definition-value exp)
  (if [symbol? (cadr exp)]
    (caddr exp)                ;                   '(define var VALUE)
    (make-lambda (cdadr exp)   ; formal parameters '(define (var OPTS) value)
                 (cddr exp)))) ; body              '(define (var opts) VALUE)

; lambda expressions start with the symbol lambda or `lambda`
(define (lambda? exp) (or (tagged-list? exp 'lambda)
                          (tagged-list? exp 'λ)))

(define (lambda-parameters exp) (cadr exp)) ; '(lambda (PARAMETERS) body)
(define (lambda-body exp) (cddr exp))       ; '(lambda (parameters) BODY)
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

; Conditionals begin with if and have a predicate, a consequent, and an optional
; alternative. If no alternative is provided it returns `#f`
;
;   (if predicate consequent alternative)
;
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative)
  (if [not (null? (cdddr exp))]
    (cadddr exp)
    'false))

; `cond->if` transforms `cond` expressions to a set of nested `if` expressions.
;
;   '(cond [predicate action])
;
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if [null? clauses]
    'false ; no `else` clause
    (let [[first (car clauses)]
          [rest (cdr clauses)]]
      (if [cond-else-clause? first]
        (if [null? rest]
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first)
                                (expand-clauses rest)))))))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; Transform a sequence into a single expression, using `begin` if needed.
(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(define (make-begin seq) (cons 'being seq))

; Begin statements
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; A procedure application is any compound expression that is not one of the
; above expression types. The `car` of the expression if the operator,
; and the `cdr` is the list of operands.
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; True and false
(define (true? val) (not (eq? val #f)))
(define (false? val) (eq? val #f))

; Compound procedures are constructed from parameters, procedure bodies, and
; environments using the constructor `make-procedure`
(define (make-procedure params body env) (list 'procedure params body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; Operations On Environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; An environment is a sequence of frames (SICP chap 3.2), where each frame is
; a table of bindings that associate variables with their corresponding values.
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Each frame of an environment is represented as a pair of lists: a list of the
; variables bound in that frame and a list of the associated values.
(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

; Adds a binding to a frame:
;
;   (add-binding-to-frame! 'age 97 '(('name) ("Noam")))
;   '(('age 'name) (97 "Noam")))
;
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; To extend an environment by a new frame that associates variables with values
; we make a frame consisting of the list of variables and the list of values
; and we adjoin this to the environment. We error out if the number of
; variables doesn't match the number of values.
(define (extend-environment vars vals base-env)
  (if [= (length vars) (length vals)]
    (cons (make-frame vars vals) base-env)
    (if [< (length vars) (length vals)]
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

; To look up a variable in an environment, we scan the list of variables in the
; first frame. If we find the desired variable, we return the corresponding
; element in the list of values. If we do not find the variable in the current
; frame, we search the enclosing environment, and so on. If we reach the
; empty environment, we signal an error.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [[null? vars] (env-loop (enclosing-environment env))]
            [[eq? var (car vars)] (car vals)]
            [else (scan (cdr vars) (cdr vals))]))
    (if [eq? env the-empty-environment]
      (error "Unbound variable" var)
      (let [(frame (first-frame env))]
        (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

; To set a variable to a new value in a specified environment, we scan for the
; variable, just as in the lookup-variable-value and change the corresponding
; value when we find it.
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [[null? vars] (env-loop (enclosing-environment env))]
            [[eq? var (car vals)] (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (if [eq? env the-empty-environment]
      (error "Unbound variable -- SET!" var)
      (let [(frame (first-frame env))]
        (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let [(frame (first-frame env))]
    (define (scan vars vals)
      (cond [[null? vars] (add-binding-to-frame! var val frame)]
            [[eq? var (car vars)] (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (scan (frame-variables frame) (frame-values frame))))

; Running the evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;
(define (setup-environment)
  (let [(initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment))]
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

; REPL

(define input-prompt ";;; M-Eval input")
(define output-prompt ";;; M-Eval output")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let [(input (read))]
    (let [(output (eval input the-global-environment))]
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

; Avoid printing the environment of a compound procedure because it may be too
; long (or may even contain cycles).
(define (user-print object)
  (if [compound-procedure? object]
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)))
      (display object)))

; Initialize Global Environment and Start Driver Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-global-environment (setup-environment))

(driver-loop)
