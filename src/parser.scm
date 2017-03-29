(import chicken scheme)
(require-extension defstruct srfi-13  srfi-1)
(use loops utf8 matchable)

(declare (unit parser) (uses lexer))

; Define structs
(defstruct ast-node type body value)

(define (parser toks)
 ; Instantiate index of tokens
 (define i 0)
 (define iinc (lambda () (set! i (+ i 1))))

 (define walk
   (lambda ()
    (print i)
    (define node (list-ref toks i))
    (define type (token-type node))
    (iinc)
    (match type
          ("NEWLINE" (make-ast-node type: "NEWLINE" body: '() value: "\n"))
          ("LPAREN" (begin
                     (let ((body '()))
                      (do-until (string=? (token-type (list-ref toks i)) "RPAREN")
                       (set! body (append body `(,(walk))))))
                     (make-ast-node type: "PARENLIST" value: "" body: body)))
          ("FUNCDEF" (begin
                      (let ((body '())
                            (delim (if (string=? (token-type (peek toks i)) "LBRACE")
                                    "RBRACE"
                                    "NEWLINE")))
                       (do-until (string=? (token-type (list-ref toks i)) delim)
                        (set! body (append body `(,(walk)))))
                       (make-ast-node type: "FUNCDEF" value: "func" body: body))))
          (_ (make-ast-node type: "GENERIC" body: '() value: (token-value node))))))

 (define body '())
 (do-until (= i (length toks))
  (set! body (append body `(,(walk)))))
 (make-ast-node type: "Program" body: body))

(define (peek toks i) (list-ref toks (+ i 1)))
(define (peek-n toks i n) (list-ref toks (+ i n)))
(define (check toks i) (list-ref toks (- i 1)))
(define (check-n toks i n) (list-ref toks (- i n)))
