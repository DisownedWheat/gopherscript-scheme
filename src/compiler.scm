(import chicken scheme)

(declare (uses lexer parser))

(define test-input "let x = 5
let y = (z) -> {
	print('Test')
	print(z + x)
}")

(define (main)
  (print test-input)
  (define tokens (tokeniser test-input))
  (define ast (parser tokens))
  (map print-ast-node (ast-node-body ast)))

(define (print-ast-node x)
  (print (ast-node-type x) " " (ast-node-value x))
  (if (> (length (ast-node-body x)) 0)
    (print "GOING INTO BODY"))
  (map print-ast-node (ast-node-body x)))

(main)
