;; Simple lexical analyzer / tokenizer. Call `match` or `advance` on the current
;; character in a <lex> record.
(define-module (lex)
               #:export (LexerState:new match advance))

(use-modules ((srfi srfi-9 gnu)
              #:select (define-immutable-record-type set-fields)))

;; NOTE: IMPLEMENTATION
;;
;; * https://www.gnu.org/software/guile/manual/html_node/String-Selection.html#String-Selection
;; * https://www.gnu.org/software/guile/manual/html_node/Characters.html#Characters
;;
;; In the future might want to use string ports for flexibility of live REPL
;; input vs file input with same interface:
;; https://www.gnu.org/software/guile/manual/html_node/String-Ports.html#String-Ports


;; text: string of the current line.
;; token: the current token.
;; length: the length of the current token.
;; offset: the offset since last call (where the current token ends).
;; lineno: current line.
(define-immutable-record-type LexerState
  (LexerState:init text token token-length offset lineno)
  LexerState?
  (text LexerState:text)
  (token LexerState:token)
  (token-length LexerState:token-length)
  (offset LexerState:offset)
  (lineno LexerState:lineno))

(define (LexerState:new text) (advance (LexerState:init text 'NULL 0 0 0)))

;; Checks whther current lexer position matches lexeme:
;;
;;     (match? 'PLUS some-lexer-state)
;;
(define (match? lexeme state)
  (eq? lexeme (LexerState:token state)))

;; Returns a new LexerState pointing at the next token.
(define (advance state)
  (display "\n-> advance STATE=")(display state)
  (let* ([offset (LexerState:offset state)]
         [token-length (LexerState:token-length state)]
         [text (LexerState:text state)]
         [result (read-next-token text offset token-length)])

    (display "\n-> advance(inner) ")(display " RESULT=")(display result)
    (set-fields state [(LexerState:token) (car result)]
                            [(LexerState:token-length) (cadr result)]
                            [(LexerState:offset) (caddr result)])))

;; Returns list of the form '(token token-length offset), e.g: '('SEMI 1 3).
(define (read-next-token str curr-pos offset)
  (display "\n-> read-next-token")
  (display " STR=")(display str)
  (display " CURR-POS=")(display curr-pos)
  (display " OFFSET=")(display offset)
  (let ([next-pos (+ curr-pos offset)])

    (if (>= next-pos (string-length str))
      '[EOI 0 -1] ; End Of Input if offset is past end of string.
      [case (match-char (string-ref str next-pos))
        [(SEMI) `(SEMI 1 ,next-pos)]
        [(TIMES) `(TIMES 1 ,next-pos)]
        [(L_PAREN) `(L_PAREN 1 ,next-pos)]
        [(R_PAREN) `(R_PAREN 1 ,next-pos)]
        [(PLUS) `(PLUS 1 ,next-pos)]
        ; If it's a space, autoadvance
        [(SPACE) (read-next-token str next-pos 1)]
        [else (if (char-numeric? (string-ref str next-pos))
                `[NUM_OR_ID ,(number-length str next-pos) ,next-pos]
                [error (string-append "SYNTXERR: illegal character value '"
                                      (string (string-ref str next-pos))
                                      "'.")])]])))

; https://www.gnu.org/software/guile/manual/html_node/Characters.html#Characters
(define (match-char char)
  (display "\n-> match-char CHAR=")(display char)
  (case char [(#\;) 'SEMI]
             [(#\*) 'TIMES]
             [(#\() 'L_PAREN]
             [(#\)) 'R_PAREN]
             [(#\+) 'PLUS]
             [(#\tab #\null #\newline #\return #\space) 'SPACE]))

;; Returns length of number or raises error.
(define (number-length str position)
  (display "\n-> number-length POS=")(display position)
  (letrec ([do-number-length (lambda (str pos accum)
                               (if (char-numeric? (string-ref str pos))
                                 [do-number-length str (1+ pos) (1+ accum)]
                                 accum))])

    (do-number-length str position 0)))
