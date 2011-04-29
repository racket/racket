#lang racket/base
(require racket/port lang/error net/url)
(provide with-input-from-string
         with-output-to-string
         with-input-from-file
         with-output-to-file
         with-input-from-url
         with-io-strings)

; with-io-strings : input(string) thunk -> string
(define (with-io-strings input thunk)
  (check-arg 'with-io-strings (string? input) "string" "first" input)
  (check-arg 'with-io-strings (and (procedure? thunk)
				   (procedure-arity-includes? thunk 0))
	     "0-parameter function" "second" thunk)
  (with-output-to-string
   (lambda ()
     (with-input-from-string input thunk))))

; with-input-from-url : url(string) thunk -> nothing
(define (with-input-from-url url-string thunk)
  (check-arg 'with-input-from-url (string? url-string) "string" "first" url-string)
  (check-arg 'with-input-from-url (and (procedure? thunk)
                                       (procedure-arity-includes? thunk 0))
             "0-parameter function" "second" thunk)
  (call/input-url (string->url url-string)
                  get-pure-port
                  (lambda (port)
                    (current-input-port port)
                    (thunk))))
