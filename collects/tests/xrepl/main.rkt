#lang at-exp racket/base

(define verbose? (make-parameter #t))

(define global-ns (current-namespace))

(define stderr (current-error-port))

(define (test-xrepl . args)
  (define show-all? (verbose?))
  (define-values [Ii Io] (make-pipe))
  (define-values [Oi Oo] (make-pipe))
  (define repl-thread
    (parameterize ([current-input-port  Ii]
                   [current-output-port Oo]
                   [current-error-port  Oo]
                   [current-namespace (make-empty-namespace)]
                   [error-print-context-length 0] ; easier output
                   [exit-handler (λ (_) (kill-thread repl-thread))])
      (thread (λ ()
                (namespace-attach-module global-ns 'racket/base)
                (namespace-require 'racket)
                (dynamic-require 'xrepl #f)
                (read-eval-print-loop)))))
  (define (repl-> expected)
    (define output (read-string (string-length expected) Oi))
    (if (equal? output expected)
      (when show-all? (display output))
      (error 'xrepl "test failure, expected ~s, got ~s" expected output)))
  (let loop ([strs args] [input? #f])
    (cond
      [(and (pair? strs) (equal? "" (car strs)))
       (loop (cdr strs) input?)]
      [(and (thread-dead? repl-thread) (null? strs))
       (printf "All tests passed.\n")]
      [(thread-dead? repl-thread)
       (error 'xrepl "test failure, repl thread died unexpectedly")]
      [(null? strs)
       (if (sync/timeout 1 repl-thread)
         (loop strs input?)
         (error 'xrepl "test failure, repl thread is alive at end of tests"))]
      [(eq? '« (car strs))
       (when input? (error 'xrepl "bad test: unterminated `«'"))
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

@test-xrepl|={
  -> «(- 2 1)»
  1
  -> «(values 2 3)»
  2
  3
  -> «(values 4)»
  4
  -> «(list ^ ^^ ^^^ ^^^^)»
  '(4 3 2 1)
  -> «(module foo racket (define x 123))»
  -> «,en foo»
  'foo> «x»
  123
  'foo> «,top»
  -> «(define enter! 123)»
  -> «(enter! 'foo)»
  procedure application: expected procedure, given: 123; arguments were: 'foo
  -> «,en foo»                          ⇒ but this still works
  'foo> «,top»
  -> «,switch foo»
  ; *** Initializing a new `foo' namespace with "racket/main.rkt" ***
  ; *** Switching to the `foo' namespace ***
  foo::-> «,switch *»
  ; *** Switching to the `*' namespace ***
  -> «,ex»
  |=@||}=|
