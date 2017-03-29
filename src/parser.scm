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
    (define node (list-ref toks i))
    (print "NODE IS: " (token-value node))
    (define type (token-type node))
    (set! body
     (append body
      `(,(match type
          ("NEWLINE" (make-ast-node type: "NEWLINE" body: '() value: "\n"))
          ("LPAREN" (begin
                     (iinc)
                     (make-ast-node type: "PARENLIST" value: ""
                      body: (let ((node (list-ref toks i)))
                             (do-until (string=? (token-type node) "RPAREN")
                              (walk))))))
          (_ (make-ast-node type: "GENERIC" body: '() value: (token-value node)))))))
    (iinc)))

 (define body '())
 (do-until (= i (length toks))
  (walk))
 (make-ast-node type: "Program" body: body))

(define (peek toks i) (list-ref toks (+ i 1)))
(define (peek-n toks i n) (list-ref toks (+ i n)))
(define (check toks i) (list-ref toks (- i 1)))
(define (check-n toks i n) (list-ref toks (- i n)))
