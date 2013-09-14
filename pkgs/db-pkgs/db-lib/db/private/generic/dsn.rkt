#lang racket/base
(require racket/lazy-require
         syntax/parse/private/minimatch
         racket/file
         racket/list)
(provide dsn-connect
         (struct-out data-source)
         connector?
         arglist?
         writable-datum?
         current-dsn-file
         get-dsn
         put-dsn
         postgresql-data-source
         mysql-data-source
         sqlite3-data-source
         odbc-data-source)

(lazy-require
 [db (postgresql-connect
      mysql-connect
      odbc-connect
      sqlite3-connect)])

#|
DSN v0.1 format

A DSN (prefs) file maps symbol => <data-source>

<data-source> ::= (db <connector> <args> <extensions>)

<connector> ::= postgresql | mysql | sqlite3 | odbc

<args> ::= (<arg> ...)
<arg>  ::= <datum> | { <kw> <datum> }

<extensions> ::= ((<symbol> <datum>) ...)

Extensions associate arbitrary extra information with a data-source (for
example, SQL dialect information, testing flags, etc). Extension keys
starting with 'dsn:', 'db:', 'racket:', and 'plt:' are
reserved. Keys may occur multiple times, but the order should not be
considered important.

  db:description ::= <string>, short description
  db:comment ::= <string>, or maybe <xexpr>

|#

(struct data-source (connector args extensions) #:transparent #:mutable)

;; ----------------------------------------

(define none (gensym 'none))

(define (writable-datum? x)
  (or (symbol? x)
      (string? x)
      (number? x)
      (boolean? x)
      (null? x)
      (and (pair? x)
           (writable-datum? (car x))
           (writable-datum? (cdr x)))))

(define (connector? x)
  (memq x '(postgresql mysql sqlite3 odbc)))

(define (parse-arglist x [default none])
  (define (fail . args)
    (cond [(eq? default none) (apply error 'parse-arglist args)]
          [(procedure? default) (default)]
          [else default]))
  (if (list? x)
      (let loop ([x x] [pargs null] [kwargs null])
        (cond [(null? x)
               (list (reverse pargs)
                     (reverse kwargs))]
              [(keyword? (car x))
               (cond [(null? (cdr x)) (fail "keyword without argument: ~a" (car x))]
                     [(writable-datum? (cadr x))
                      (loop (cddr x) pargs (cons (list (car x) (cadr x)) kwargs))]
                     [else
                      (fail "expected readable datum: ~e" (cadr x))])]
              [(writable-datum? (car x))
               (loop (cdr x) (cons (car x) pargs) kwargs)]
              [else (fail "expected readable datum: ~e" (car x))]))
      (fail "expected list")))

(define (arglist? x)
  (and (parse-arglist x #f) #t))

(define (parse-extensions x [default none])
  (let/ec escape
    (define (fail . args)
      (cond [(eq? default none) (apply error 'parse-extensions args)]
            [(procedure? default) (escape (default))]
            [else (escape default)]))
    (if (list? x)
        (map (lambda (x)
               (match x
                 [(list (? symbol? key) (? writable-datum? value))
                  x]
                 [else (fail "expected extension entry: ~e" x)]))
             x)
        (fail "expected list: ~e" x))))

(define (extensions? x)
  (and (parse-extensions x #f) #t))

(define (sexpr->data-source x)
  (let/ec escape
    (match x
      [(list 'db (? connector? connector) (? arglist? args) (? extensions? exts))
       (data-source connector args exts)]
      [_ #f])))

(define (data-source->sexpr x)
  (match x
    [(data-source connector args exts)
     `(db ,connector ,args ,exts)]))

;; ----------------------------------------

(define current-dsn-file
  (make-parameter (build-path (find-system-path 'pref-dir) "db-dsn-0.rktd")))

(define (get-dsn name [default #f] #:dsn-file [file (current-dsn-file)])
  (let* ([sexpr (get-preference name (lambda () #f) 'timestamp file)])
    (or (and sexpr (sexpr->data-source sexpr))
        (if (procedure? default) (default) default))))

(define (put-dsn name value #:dsn-file [file (current-dsn-file)])
  (let* ([sexpr (and value (data-source->sexpr value))])
    (put-preferences (list name)
                     (list sexpr)
                     (lambda () (error 'put-dsn "DSN file locked"))
                     file)))

;; ----------------------------------------

(define (get-connect x)
  (case x
    ((postgresql) postgresql-connect)
    ((mysql) mysql-connect)
    ((sqlite3) sqlite3-connect)
    ((odbc) odbc-connect)))

(define dsn-connect
  (make-keyword-procedure
   (lambda (kws kwargs name . pargs)
     (let* ([kws (map list kws kwargs)]
            [file-entry (assq '#:dsn-file kws)]
            [kws* (if file-entry (remq file-entry kws) kws)]
            [file (if file-entry (cdr file-entry) (current-dsn-file))])
       (unless (or (symbol? name) (data-source? name))
         (error 'dsn-connect
                "expected symbol for first argument, got: ~e" name))
       (unless (or (path-string? file) (not file))
         (error 'dsn-connect
                "expected path or string for #:dsn-file keyword, got: ~e"
                file))
       (let ([r (if (data-source? name) name (get-dsn name #f #:dsn-file file))])
         (unless r
           (error 'dsn-connect "cannot find data source named ~e" name))
         (let* ([rargs (parse-arglist (data-source-args r))]
                [rpargs (first rargs)]
                [rkwargs (second rargs)]
                [allpargs (append rpargs pargs)]
                [allkwargs (sort (append rkwargs kws*) keyword<? #:key car)]
                [connect (get-connect (data-source-connector r))])
           (keyword-apply connect (map car allkwargs) (map cadr allkwargs) allpargs)))))))

;; ----

(define (mk-specialized name connector arity kws)
  (procedure-rename
   (procedure-reduce-keyword-arity
    (make-keyword-procedure
     (lambda (kws kwargs . pargs)
       (data-source connector (apply append pargs (map list kws kwargs)) '())))
    arity '() (sort kws keyword<?))
   name))

(define postgresql-data-source
  (mk-specialized 'postgresql-data-source 'postgresql 0
                  '(#:user #:database #:password #:server #:port #:socket
                    #:allow-cleartext-password? #:ssl
                    #:notice-handler #:notification-handler #:debug?)))

(define mysql-data-source
  (mk-specialized 'mysql-data-source 'mysql 0
                  '(#:user #:database #:password #:server #:port #:socket #:ssl
                    #:notice-handler #:debug?)))

(define sqlite3-data-source
  (mk-specialized 'sqlite3-data-source 'sqlite3 0
                  '(#:database #:mode #:busy-retry-limit #:busy-retry-delay
                    #:use-place #:debug?)))

(define odbc-data-source
  (mk-specialized 'odbc-data-source 'odbc 0
                  '(#:dsn #:user #:password #:notice-handler
                    #:strict-parameter-types? #:character-mode #:use-place)))
