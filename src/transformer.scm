(import chicken scheme)
(require-extension defstruct srfi-13  srfi-1)
(use loops utf8 matchable)

(declare (unit transformer) (uses parser))

(define (transformer ast)
  (define i 0)
  (define (iinc)
    (set! i (+ i 1)))

  (define (traverse ast parent)
    (map
      (lambda (node)
        (match (ast-node-type node)
          ("ASSIGN" '())))
      ast))

  (define body '())
  (do-until (= i (length (ast-node-body ast)))
    (set! body (append body `(,(walk)))))
  (make-ast-node type: "Program" body: body))
