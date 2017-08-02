;; Parse a line of text.
;; Passes around a LexerState record until reaching EOF.
(define-module (parser)
               #:export (parse parse-readline))

(use-modules ((lex)
              #:select (match? advance LexerState:new)))

(use-modules ((ice-9 readline)
              #:select (readline)))

;; NOTE: IMPLEMENTATION
;;
;; * Use readline for input:
;; https://www.gnu.org/software/guile/manual/html_ode/Readline-Functions.html#Readline-Functions


(define (parse-readline)
  (parse (readline "REPL> ")))

(define (parse string)
  (statements (LexerState:new string)))

;; SYNTAX TREE FUNCTIONS =======================================================
;;
;; All these functions return an updated LexerState record which can be passed
;; to the next function in the chain.

;; statements -> expression SEMI
;;             | expression SEMI statements
(define (statements state)
  (display "\n-> statements STATE=")(display state)
  (let ([do-statements (lambda ()
                         (let ([new-state (expression state)])
                           (display "\n-> statements(inner) RETURNED=")
                           (display new-state)
                           (if (match? 'SEMI new-state)
                             (advance new-state)
                             (error "SYNTXERR: missing semicolon."))))])

    (display "\n-> statements STATE=")(display state)
    (if (not (match? 'EOI state))
      (statements (do-statements))
      (display "\n    END OF INPUT\n"))))


;; expression  -> term expression'
;; expression' -> PLUS term expression'
;;              | NOTHING
(define (expression state)
  (display "\n-> expression STATE=")(display state)
  (let loop ([state state])
    (let ([new-state (term state)])

      (display "\n-> expression(loop) NEW-STATE=") (display new-state)
      (if (match? 'PLUS new-state)
        (loop (advance new-state))
        new-state))))


;; term  -> factor term'
;; term' -> TIMES factor term'
;;        | NOTHING
(define (term state)
  (display "\n-> term STATE=")(display state)
  (let loop ([state state])
    (let ([new-state (factor state)])

      (display "\n-> term(loop) NEW-STATE=")(display new-state)
      (if (match? 'TIMES new-state)
        (loop (advance new-state))
        new-state))))



;; factor -> NUM_OR_ID
;;         | L_PAREN expression R_PAREN
(define (factor state)
  (display "\n-> factor STATE=")(display state)
  (cond
    [(match? 'NUM_OR_ID state) (advance state)]
    [(match? 'L_PAREN state) (let ([new-state (expression (advance state))])
                               (if (match? 'R_PAREN new-state)
                                 (advance new-state)
                                 (error "SYNTXERR: missing R_PAREN.")))]
    [else (error "SYNTXERR: NUM_OR_ID or L_PAREN expected.")]))

;; =============================================================================
