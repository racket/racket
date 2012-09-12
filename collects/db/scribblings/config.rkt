#lang racket/base
(require scribble/manual
         scribble/eval
         unstable/sandbox
         (for-label racket/base
                    racket/contract))
(provide (all-defined-out)
         (for-label (all-from-out racket/base)
                    (all-from-out racket/contract)))

(define (tech/reference . pre-flows)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") pre-flows))

(define (parheading . pre-flows)
  (elem (apply bold pre-flows) (hspace 1)))

(define (wplink path . pre-flows)
  (apply hyperlink (string-append "http://en.wikipedia.org/wiki/" path) pre-flows))

;; ----

#|
Whenever examples are changed, added, removed, or reordered, the
example log files must be regenerated. To do so, set log-mode below to
'record and run setup. Regenerating the logs require an environment
that defines the DSN 'db-scribble-env as a PostgreSQL data source.

Set log-mode back to 'replay before checking in the changes.

Use one evaluator (and log file) per scribble file, so that when DrDr
runs scribble files individually, they still work.
|#

(define log-mode 'replay)

(define (make-pg-eval log-file init?)
  (let ([ev (make-log-based-eval log-file log-mode)])
    (ev '(require racket/class
                  db
                  db/util/postgresql
                  db/util/datetime))
    (when init?
      (ev '(begin
             ;; Must be kept in sync with beginning of using-db.scrbl
             (define pgc (dsn-connect 'db-scribble-env))
             (query-exec pgc "create temporary table the_numbers (n integer, d varchar(20))")
             (query-exec pgc "insert into the_numbers values (0, 'nothing')")
             (query-exec pgc "insert into the_numbers values (1, 'the loneliest number')")
             (query-exec pgc "insert into the_numbers values (2, 'company')")
             (query-exec pgc "insert into the_numbers values (3, 'a crowd')"))))
    ev))

#|
The fake eval is for eg connection examples
|#

(define fake-eval (make-base-eval))
(fake-eval '(begin (require racket/class)
                   (define connection% (class object% (super-new)))))

(define-syntax-rule (fake-examples [example result] ...)
  (examples #:eval fake-eval (eval:alts example result) ...))
