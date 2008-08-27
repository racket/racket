#lang scheme

(require scheme/include
         (for-syntax scheme)         
         (prefix-in text: scribble/text)
         (for-syntax (prefix-in text: scribble/text))
         (for-syntax (prefix-in at: scribble/reader)))

; XXX I have to do this because without it there is an infinite loop.
;     at:read-syntax-inside returns #'() instead of eof
(define-for-syntax (*read-syntax . args)
  (define r (apply at:read-syntax-inside args))
  (if (eof-object? r) r
      (if (null? (syntax->datum r))
          eof
          r)))

(define-syntax (include-template stx)
  (syntax-case stx ()
    [(_ a-path)
     ; XXX Not desireable, but necessary to get at the body, 
     ;     rather than it being used as a string applied to the rest
     (with-syntax ([(begin (#%app body ...))
                    (local-expand 
                     (with-syntax ([_stx stx])
                       (syntax/loc stx
                         (include-at/relative-to/reader
                          _stx _stx
                          (file a-path) *read-syntax)))
                     'module-begin 
                     empty)
                    ])
       (syntax/loc stx
         (with-output-to-string
          (begin/show body ...))))]))

(define-syntax with-output-to-string
  (syntax-rules ()
    [(_ e ...)
     (let ([os (open-output-string)])
       (parameterize ([current-output-port os])
         e ...)
       (get-output-string os))]))

; XXX Want to have this instead of every begin, but perhaps should make a list rather than use show directly
(define-syntax begin/show
  (syntax-rules ()
    [(_ e) e]
    [(_ e ...)
     ; XXX If scribble/text shared "show", then I would use it here
     (begin (display e) ...)]))
(define t list)

(define-syntax in
  (syntax-rules ()
    [(_ x xs e ...)
     (for/list ([x xs])
       e ...)]))

; Examples

(include-template "static.html")

(define (basic-template title clients client-surname client-firstname client-email)
  (include-template "basic.html"))

(basic-template "Title"
                (list (list "First 1" "Second 1" "Third 1")
                      (list "First 2" "Second 2" "Third 2")
                      (list "First 3" "Second 3" "Third 3")
                      (list "First 4" "Second 4" "Third 4"))
                first second third)

(local ()
  (define-struct client (surname firstname email))
  (basic-template "Title"
                  (list (make-client "First 1" "Second 1" "Third 1")
                        (make-client "First 2" "Second 2" "Third 2")
                        (make-client "First 3" "Second 3" "Third 3")
                        (make-client "First 4" "Second 4" "Third 4"))
                  client-surname client-firstname client-email))

(define (if-template #:monkeys monkeys
                     #:monkey-limit monkey-limit
                     #:monkey-minimum monkey-minimum)
  (include-template "if.html"))

(if-template #:monkeys 5
             #:monkey-limit 10
             #:monkey-minimum 2)
(if-template #:monkeys 11
             #:monkey-limit 10
             #:monkey-minimum 2)
(if-template #:monkeys 1
             #:monkey-limit 10
             #:monkey-minimum 2)
