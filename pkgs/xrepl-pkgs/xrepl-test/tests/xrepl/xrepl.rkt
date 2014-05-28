#lang at-exp racket/base

;; General note for anyone who tries to run these tests: since the tests check
;; interactions it can be hard to find the problem, and sometimes it's best to
;; just comment a suffix of the tests to find it.  In addition, when it fails
;; it tries to provide information that helps finding the problem, but
;; sometimes there's very little help, and it might also fail by just getting
;; stuck.

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
                  ((namespace-variable-value 'wrap-width) 77))
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
       (printf "~a interaction tests passed\n" tests-num)]
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

(define tmp-no-slash (let-values ([(base name dir?) (split-path tmp)])
                       (path->string (build-path base name))))

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
  ; application: not a procedure;
  ;  expected a procedure that can be applied to arguments
  ;   given: 123
  ; [,bt for context]
  -> «(enter! 'fooo)»
  ; application: not a procedure;
  ;  expected a procedure that can be applied to arguments
  ;   given: 123
  ; [,bt for context]
  -> «,en foo»                          ⇒ but this still works
  'foo> «,top»
  -> «,switch foo»
  ; *** Initializing a new `foo' namespace with 'foo ***
  ; *** Switching to the `foo' namespace ***
  foo::-> «,switch typed/racket»
  ; *** Initializing a new `typed/racket' namespace with typed/racket ***
  ; *** Switching to the `typed/racket' namespace ***
  typed/racket::-> «^»                  ⇒ works in TR too
  - : Integer [more precisely: Positive-Byte]
  123
  typed/racket::-> «,switch *»
  ; *** Switching to the `*' namespace ***
  -> «bleh»
  ; bleh: undefined;
  ;  cannot reference undefined identifier
  ; [,bt for context]
  -> «,ap BLEH»
  ; No matches found.
  -> «,ap path->»
  ; Matches: path->bytes, path->complete-path, path->directory-path,
  ;   path->string, some-system-path->string.
  -> «,desc cons»
  ; `cons' is a bound identifier,
  ;   defined in #%kernel
  ;   required through "<collects>/racket/init.rkt"
  -> «,desc lambda»
  ; `lambda' is a bound identifier,
  ;   defined in <collects>/racket/private/kw.rkt as `new-lambda'
  ;   required through "<collects>/racket/init.rkt"
  -> «,desc racket/runtime-path»
  ; `racket/runtime-path' is a module,
  ;   located at <collects>/racket/runtime-path.rkt
  ;   imports: <collects>/racket/base.rkt, <collects>/racket/list.rkt,
  ;     <collects>/racket/private/runtime-path-table.rkt,
  ;     <collects>/racket/private/this-expression-source-directory.rkt,
  ;     <collects>/setup/dirs.rkt.
  ;   imports-for-syntax: <collects>/racket/base.rkt.
  ;   direct syntax exports: define-runtime-module-path,
  ;     define-runtime-module-path-index, define-runtime-path,
  ;     define-runtime-path-list, define-runtime-paths, runtime-paths,
  ;     runtime-require.
  -> «(current-directory "/( none )")»  ⇒ racket allows this
  ; now in /( none )                    ⇒ reports without ,cd
  -> «,cd @|tmp|»
  ; now in @tmp-no-slash
  -> «,desc scribble/html»
  ; `scribble/html' is a module,
  ;   located at <pkgs>/scribble-html-lib/scribble/html.rkt
  ;   imports: <collects>/racket/base.rkt,
  ;     <pkgs>/scribble-html-lib/scribble/html/main.rkt.
  ;   no direct exports.
  -> «(module broken racket/base (define foo 123) (error "bleh!"))»
  -> «,en broken»
  bleh!                                 ⇒ threw an error...
  'broken> «foo»
  123                                   ⇒ ...but we still got in
  'broken> «,top»
  -> «string->jsexpr»
  ; string->jsexpr: undefined;
  ;  cannot reference undefined identifier
  ; [,bt for context]
  -> «,r (only-in json string->jsexpr)» ⇒ works with an expression
  -> «string->jsexpr»
  #<procedure:string->jsexpr>
  -> «jsexpr->string»                   ⇒ didn't get this
  ; jsexpr->string: undefined;
  ;  cannot reference undefined identifier
  ; [,bt for context]
  -> «,en json»
  <collects>/json/main> «,sh echo $F»
  @|collects|/json/main.rkt
  <collects>/json/main> «,top»
  -> «(display-to-file "#lang racket\n(provide x)\n(define x 1)\n"
                       "xrepl-test.rkt" #:exists 'truncate)»
  -> «,rr "xrepl-test.rkt"»             ⇒ first load
  ; requiring "xrepl-test.rkt"
  -> «x»
  1
  -> «(display-to-file "#lang racket\n(provide x)\n(define x 2)\n"
                       "xrepl-test.rkt" #:exists 'truncate)»
  -> «,rr xrepl-test.rkt»
  ; reloading "xrepl-test.rkt"
  -> «,rm xrepl-test.rkt»
  -> «x»
  2
  -> «,ex»
  @||})
