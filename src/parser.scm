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

 ; Define the function for inpsecting lexer tokens
 (define walk
   (lambda ()
    (print i)
    (define node (list-ref toks i))
    (define type (token-type node))
    (iinc)
    (match type
          ("NEWLINE" (make-ast-node type: "NEWLINE" value: "\n"))
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
          ("NUMBER" (make-ast-node type: "NUMBERLITERAL" value: (token-value node)))
          ("STRING" (make-ast-node type: "STRINGLITERAL" value: (token-value node)))
          ("IDENT" (make-ast-node type: "IDENT" value: (token-value node)))
          ("ASSIGN" (begin
                      (if (string=? (token-type (check-til toks i "NEWLINE")) "LET")
                        (make-ast-node type: "ASSIGN" value: (token-value (check toks i)))
                        (make-ast-node type: "REASSIGN" value: (token-value (check toks i))))))
          (_ (make-ast-node type: "GENERIC" value: (token-value node))))))

 ; Create empty body list to contain program
 (define body '())
 ; Iterate over tokens and add them to the body list
 (do-until (= i (length toks))
  (set! body (append body `(,(walk)))))
 ; Return the newly create AST!
 (make-ast-node type: "Program" body: body))

; Peek forward one token
(define (peek toks i) (list-ref toks (+ i 1)))
; Peek forward n tokens
(define (peek-n toks i n) (list-ref toks (+ i n)))
; Peek forward until a delimiter is hit, then return the prior token
(define (peek-til toks i delim)
  (if (string=? (token-type (list-ref toks i)) delim)
    (list-ref toks (- i 1))
    (peek-til toks (+ i 1) delim)))
; Check back one token
(define (check toks i) (list-ref toks (- i 1)))
; Check back n tokens
(define (check-n toks i n) (list-ref toks (- i n)))
; Check back until a delimiter is hit, then return the next token
(define (check-til toks i delim)
  (if (string=? (token-type (list-ref toks i)) delim)
    (list-ref toks (+ i 1))
    (check-til toks (- i 1) delim)))
