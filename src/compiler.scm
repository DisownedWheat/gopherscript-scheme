(import chicken scheme)

(declare (uses lexer))

(define test-input "let x = 5
let y = (z) -> {
	print('Test')
	print(z + x)
}")

(define (main)
  (print test-input)
  (define tokens (tokeniser test-input))
  (map (lambda (x) (print (token-type x) " " (token-value x))) tokens))

(main)
