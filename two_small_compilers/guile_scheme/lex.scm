;; Simple lexical analyzer / tokenizer. Call `match` or `advance` on the current
;; character in a <lex> record.
(define-module (lex)
               #:export (match advance))

(use-modules ((srfi srfi-9 gnu)
              #:select define-immutable-record-type))

;; NOTE: IMPLEMENTATION
;;
;; [ ] Use `(str-ref (lex-text mylex) (lex-offset mylex))` for getting the
;; current character:
;; [ ] Use `(char=? mychar)` to match with lexeme.
;;
;; * https://www.gnu.org/software/guile/manual/html_node/String-Selection.html#String-Selection
;; * https://www.gnu.org/software/guile/manual/html_node/Characters.html#Characters
;;
;; [ ] Just use readline for input:
;; https://www.gnu.org/software/guile/manual/html_node/Readline-Functions.html#Readline-Functions
;;
;; In the future might want to use string ports for flexibility of live REPL
;; input vs file input with same interface:
;; https://www.gnu.org/software/guile/manual/html_node/String-Ports.html#String-Ports


;; text: string of the current line.
;; offset: the offset since last call.
;; lineno: current line.
(define-immutable-record-type <lex>
  (lex:new text offset lineno)
  lex?
  (text lex:text)
  (offset lex:offset)
  (lineno lex:lineno))

;; Returns `lexeme` if it's one of the valid lexemes. Errors
;; out otherwise.
(define (lexeme symbol)
  (let ([valid-lexemes '(EOI SEMI PLUS TIMES L_PAREN R_PAREN NUM_OR_ID)])
    ; TODO
    (define (get-if-valid symbol) symbol)
    (get-if-valid symbol)))
