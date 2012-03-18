#lang at-exp racket/base

(define verbose? (make-parameter #f))

(define global-ns (current-namespace))

(define stderr (current-error-port))

(define ((make-xrepl-test . args))
  (define show-all? (verbose?))
  (define-values [Ii Io] (make-pipe))
  (define-values [Oi Oo] (make-pipe))
  (define repl-thread
    (parameterize ([current-input-port  Ii]
                   [current-output-port Oo]
                   [current-error-port  Oo]
                   [current-namespace (make-empty-namespace)]
                   [exit-handler (λ (_) (kill-thread repl-thread))])
      (thread (λ ()
                (namespace-attach-module global-ns 'racket/base)
                (namespace-require 'racket/init)
                (dynamic-require 'xrepl #f)
                (parameterize ([current-namespace
                                (module->namespace 'xrepl/xrepl)])
                  ((namespace-variable-value 'wrap-column) 77))
                (read-eval-print-loop)))))
  (define (repl-> expected)
    (define output (read-string (string-length expected) Oi))
    (if (equal? output expected)
      (when show-all? (display output))
      (error 'xrepl "test failure at interaction #~a, expected ~s, got ~s"
             tests-num expected output)))
  (define tests-num 0)
  (let loop ([strs args] [input? #f])
    (cond
      [(and (pair? strs) (equal? "" (car strs)))
       (loop (cdr strs) input?)]
      [(and (thread-dead? repl-thread) (null? strs))
       (printf "~a interaction tests passed.\n" tests-num)]
      [(thread-dead? repl-thread)
       (error 'xrepl "test failure, repl thread died unexpectedly")]
      [(null? strs)
       (if (sync/timeout 1 repl-thread)
         (loop strs input?)
         (error 'xrepl "test failure, repl thread is alive at end of tests"))]
      [(eq? '« (car strs))
       (when input? (error 'xrepl "bad test: unterminated `«'"))
       (set! tests-num (add1 tests-num))
       (loop (cdr strs) #t)]
      [(eq? '» (car strs))
       (unless input? (error 'xrepl "bad test: redundant `»'"))
       (loop (cdr strs) 'newline)]
      [(regexp-match #rx"^(.*?)(?: *⇒[^\n]*)(.*)" (car strs))
       => (λ (m) (loop (list* (cadr m) (caddr m) (cdr strs)) input?))]
      [(regexp-match #rx"^(.*?)([«»])(.*)" (car strs))
       => (λ (m) (loop (list* (cadr m) (string->symbol (caddr m)) (cadddr m)
                              (cdr strs))
                       input?))]
      [(eq? 'newline input?)
       (unless (regexp-match? #rx"^\n" (car strs))
         (error 'xrepl "bad test: `»' followed by a non-newline"))
       (newline Io) (flush-output Io)
       (when show-all? (newline) (flush-output))
       (loop (cons (substring (car strs) 1) (cdr strs)) #f)]
      [input?
       (display (car strs) Io)
       (when show-all? (display (car strs)) (flush-output))
       (loop (cdr strs) #t)]
      [else
       (repl-> (car strs))
       (loop (cdr strs) #f)])))

(require setup/dirs)
(define tmp (path->string (find-system-path 'temp-dir)))
(define collects (path->string (find-collects-dir)))

(provide test-xrepl)
(module+ main (test-xrepl))
(define test-xrepl @make-xrepl-test{
  -> «^»
  ; ^: no saved values, yet [,bt for context]
  -> «(- 2 1)»
  1
  -> «^^»
  ; ^^: no 2 saved values, yet [,bt for context]
  -> «(values 2 3)»
  2
  3
  -> «(values 4)»
  4
  -> «(list ^ ^^ ^^^ ^^^^)»
  '(4 3 2 1)
  -> «(list $1 $2 $3 $4 $5)»
  '((4 3 2 1) 4 3 2 1)
  -> «(collect-garbage)»
  -> «^»
  ; ^: saved value #1 was garbage-collected [,bt for context]
  -> «(module foo racket (define x 123))»
  -> «,en foo»
  'foo> «x»
  123
  'foo> «,top»
  -> «(define enter! 123)»
  -> «(enter! 'foo)»
  ; procedure application: expected procedure, given: 123; arguments were: 'foo
  ;   [,bt for context]
  -> «(enter! 'fooo)»
  ; procedure application: expected procedure, given: 123; arguments were:
  ;   'fooo [,bt for context]
  -> «,en foo»                          ⇒ but this still works
  'foo> «,top»
  -> «,switch foo»
  ; *** Initializing a new `foo' namespace with 'foo ***
  ; *** Switching to the `foo' namespace ***
  foo::-> «,switch typed/racket»
  ; *** Initializing a new `typed/racket' namespace with typed/racket ***
  ; *** Switching to the `typed/racket' namespace ***
  typed/racket::-> «^»                  ⇒ works in TR too
  - : Integer [generalized from Positive-Byte]
  123
  typed/racket::-> «,switch *»
  ; *** Switching to the `*' namespace ***
  -> «bleh»
  ; reference to undefined identifier: bleh [,bt for context]
  -> «,ap BLEH»
  ; No matches found.
  -> «,ap path->»
  ; Matches: path->bytes, path->complete-path, path->directory-path,
  ;   path->string, some-system-path->string.
  -> «,desc cons»
  ; `cons' is a bound identifier,
  ;   defined in #%kernel
  ;   required through "racket/init.rkt"
  -> «,desc lambda»
  ; `lambda' is a bound identifier,
  ;   defined in racket/private/kw.rkt as `new-lambda'
  ;   required through "racket/init.rkt"
  -> «,desc racket/runtime-path»
  ; `racket/runtime-path' is a module,
  ;   located at racket/runtime-path.rkt
  ;   imports: mzlib/runtime-path.rkt, racket/base.rkt.
  ;   imports-for-syntax: racket/base.rkt.
  ;   direct syntax exports: define-runtime-module-path.
  -> «(current-directory "/( none )")»  ⇒ racket allows this
  ; now in /( none )                    ⇒ reports without ,cd
  -> «,cd @|tmp|»
  ; now in @tmp
  -> «,desc scribble/html»
  ; `scribble/html' is a module,
  ;   located at scribble/html.rkt
  ;   imports: racket/base.rkt, scribble/html/main.rkt.
  ;   no direct exports.
  -> «(module broken racket/base (define foo 123) (error "bleh!"))»
  -> «,en broken»
  bleh!                                 ⇒ threw an error...
  'broken> «foo»
  123                                   ⇒ ...but we still got in
  'broken> «,top»
  -> «string->jsexpr»
  ; reference to undefined identifier: string->jsexpr [,bt for context]
  -> «,r (only-in json string->jsexpr)» ⇒ works with an expression
  -> «string->jsexpr»
  #<procedure:string->jsexpr>
  -> «jsexpr->string»                   ⇒ didn't get this
  ; reference to undefined identifier: jsexpr->string [,bt for context]
  -> «,en json»
  json/main> «,sh echo $F»
  @|collects|/json/main.rkt
  json/main> «,top»
  -> «,ex»
  @||})
