(import chicken scheme)
(require-extension defstruct srfi-13  srfi-1)
(use loops utf8)

(declare (unit lexer))

; Define token struct
(defstruct token type value)

; Define the keywords
(define keywords '(("let" . "LET")
                   ("->" . "FUNCDEF")
                   ("if" . "IF")
                   ("else" . "ELSE")
                   ("true" . "TRUE")
                   ("false" . "FALSE")
                   ("return" . "RETURN")
                   ("do" . "DO")))

; Global index of input
(define lexi 0)

; Main tokeniser function
(define (tokeniser str)
  (define input (string->list str)) ; Split the input string into a list of chars
  (define toks '()) ; Prepare the list
  (print toks)
  (do-until (= lexi (length input)) ; Iterate over the chars until the end of the input list
    (let ((char (list-ref input lexi)))
      (cond ; Match any potential tokens and iterate
        ((char=? #\newline char) (set! toks (add-token toks "NEWLINE" "\n")))
        ((char=? char #\tab) (set! toks (add-token toks "TAB" "\t")))
        ((char=? char #\-) (if (char=? (list-ref input (+ lexi 1)) #\>)
                             (set! toks (add-token toks "FUNCDEF" #f))
                             (set! toks (add-token toks "MINUS" "-"))))
        ((char=? char #\+) (set! toks (add-token toks "PLUS" "+")))
        ((char=? char #\{) (set! toks (add-token toks "LBRACE" "{")))
        ((char=? char #\}) (set! toks (add-token toks "RBRACE" "}")))
        ((char=? char #\=) (if (char=? (list-ref input (+ lexi 1)) #\=)
                            (begin
                             (set! toks (add-token toks "BOOLEQUAL" "=="))
                             (set! lexi (+ lexi 1)))
                            (set! toks (add-token toks "ASSIGN" "="))))
       ((or (char=? char #\") (char=? char #\')) (set! toks (check-string input toks)))
       ((char=? char lparen) (set! toks (add-token toks "LPAREN" "(")))
       ((char=? char rparen) (set! toks (add-token toks "RPAREN" ")")))
       ((char-alphabetic? char) (set! toks (check-word input toks)))
       ((char-numeric? char) (set! toks (check-number input toks)))))
   (set! lexi (+ lexi 1)))
  (set! lexi 0)
  (print toks)
  toks)

(define (add-token lst type val) (append lst `(,(make-token type: type value: val))))

; Helper for parsing identifiers or keywords
(define (check-word input lst)
  (define char-list '())
  (define char (list-ref input lexi))
  (do-until (or
             (char=? char lparen)
             (char=? char rparen)
             (char=? char #\[)
             (char=? char #\.)
             (char=? char #\,)
             (char=? char #\{)
             (char=? char #\')
             (char=? char #\")
             (char-whitespace? char))
   (set! char-list (append char-list `(,char)))
   (set! lexi (+ lexi 1))
   (set! char (list-ref input lexi)))
  (set! lexi (- lexi 1))
  (let ((word (list->string char-list))
        (keys (map (lambda (x) (car x)) keywords)))
   (if (member word keys)
    (let ((val (car (filter (lambda (x) (string=? (car x) word)) keywords))))
     (append lst `(,(make-token type: (cdr val) value: (car val)))))
    (append lst `(,(make-token type: "IDENT" value: word))))))

; Helper for parsing numbers into tokens
(define (check-number input lst)
 (define char-list '())
 (define char (list-ref input lexi))
 (do-until (or
             (char=? char lparen)
             (char=? char rparen)
             (char=? char #\[)
             (char=? char #\{)
             (char=? char #\')
             (char=? char #\")
             (char-whitespace? char))
   (set! char-list (append char-list `(,char)))
   (set! lexi (+ lexi 1))
   (set! char (list-ref input lexi)))
 (set! lexi (- lexi 1))
 (let ((num (string->number (list->string char-list))))
  (append lst `(,(make-token type: "NUMBER" value: num)))))

; Helper for parsing strings literals
(define (check-string input lst)
 (define char-list '())
 (define delim (list-ref input lexi))
 (set! lexi (+ lexi 1))
 (define char (list-ref input lexi))
 (do-until (char=? char delim)
  (set! char-list (append char-list `(,char)))
  (set! lexi (+ lexi 1))
  (set! char (list-ref input lexi)))
 (append lst `(,(make-token type: "STRING" value: (list->string char-list)))))

; Some small helpers to prevent parinfer from breaking
(define lparen #\()
(define rparen #\))
