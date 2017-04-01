(import chicken scheme)
(require-extension defstruct srfi-13  srfi-1)
(use loops utf8 matchable)

(declare (unit transformer) (uses parser))

(define (transformer ast)
  (define (traverse ast parent)
   (let ((i 0))
    (map
      (lambda (node)
        (set! i (+ i 1))
        (match (ast-node-type node)
          ("ASSIGN" (if (member (ast-node-type (peek-n ast i 2)) '("FUNCDEF" "PARENLIST"))
                      (let ((paren-body (traverse (peek ast i) node))
                            (func-body (travers (p))))
                        (make-ast-node type: "ASSIGNFUNC" body: body value: (ast-node-value node)))))
          ("PARENLIST" (if (string=? (ast-node-type (peek ast i)) "FUNCDEF")
                        '()))))
      ast)))

  (define body '())
  (do-until (= i (length (ast-node-body ast)))
    (set! body (append body `(,(walk)))))
  (make-ast-node type: "Program" body: body))
