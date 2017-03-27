(import chicken scheme)
(require-extension defstruct srfi-13  srfi-1)
(use loops utf8 matchable)

(declare (unit parser))
(declare (uses lexer))

; Define structs
(defstruct ast-node type body)

; Instantiate index of tokens
(define i 0)
(define (iinc)
  (set! i (+ i 1)))

(define (walk acc tok)
 (define type (token-type tok))
 (print (token-value tok))
 (match type
     ("NEWLINE" (cons acc (make-ast-node type: "NEWLINE" body: '())))
     (_ acc)))

(define (parser toks)
  (define ast (make-ast-node type: "Program" body: '()))

  (define body (fold walk '() toks))
  (make-ast-node type: "Program" body: body))
