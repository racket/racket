#lang racket/base

(require racket/format
         (for-syntax syntax/kerncase
                     racket/base))

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         sequential
         parallel
         machine
         site-config?
         site-config-tag
         site-config-options
         site-config-content
         current-mode
         current-stamp
         extract-options)

(module reader syntax/module-reader
  distro-build/config)

(struct site-config (tag options content))

(define-syntax-rule (module-begin form ...)
  (#%plain-module-begin (site-begin #f form ...)))

(define-syntax (site-begin stx)
  (syntax-case stx ()
    [(_ #t) #'(begin)]
    [(_ #f)
     (raise-syntax-error 'site
                         "did not find an expression for the site configuration")]
    [(_ found? next . rest) 
     (let ([expanded (local-expand #'next 'module (kernel-form-identifier-list))])
       (syntax-case expanded (begin)
         [(begin next1 ...)
          #`(site-begin found? next1 ... . rest)]
         [(id . _)
          (and (identifier? #'id)
               (ormap (lambda (kw) (free-identifier=? #'id kw))
                      (syntax->list #'(require
                                       provide
                                       define-values
                                       define-syntaxes
                                       begin-for-syntax
                                       module
                                       module*
                                       #%require
                                       #%provide))))
          #`(begin #,expanded (site-begin found? . rest))]
         [_else
          (if (syntax-e #'found?)
              (raise-syntax-error 'site
                                  "found second top-level expression"
                                  #'next)
              #`(begin
                 (provide site-config)
                 (define site-config (let ([v #,expanded])
                                       (unless (site-config? v)
                                         (error 'site
                                                (~a "expression did not produce a site configuration\n"
                                                    "  result: ~e\n"
                                                    "  expression: ~.s")
                                                v
                                                'next))
                                       v))
                 (site-begin
                  #t
                  . rest)))]))]))

(define sequential
  (make-keyword-procedure
   (lambda (kws kw-vals . subs)
     (constructor kws kw-vals subs
                  check-group-keyword 'sequential))))
(define parallel
  (make-keyword-procedure
   (lambda (kws kw-vals . subs)
     (constructor kws kw-vals subs
                  check-group-keyword 'parallel))))
(define machine
  (make-keyword-procedure
   (lambda (kws kw-vals)
     (constructor kws kw-vals null
                  check-machine-keyword 'machine))))

(define (constructor kws kw-vals subs check tag)
  (site-config
   tag
   (for/hash ([kw (in-list kws)]
              [val (in-list kw-vals)])
     (define r (check kw val))
     (when (eq? r 'bad-keyword)
       (error tag
              (~a "unrecognized keyword for option\n"
                  "  keyword: ~s")
              kw))
     (unless (check kw val)
       (error tag
              (~a "bad value for keyword\n"
                  "  keyword: ~s\n"
                  "  value: ~e")
              kw
              val))
     (values kw val))
   (for/list ([sub subs])
     (unless (site-config? sub)
       (raise-argument-error tag "site-config?" sub))
     sub)))

(define (check-group-keyword kw val)
  (case kw
    [(#:pkgs) (and (list? val) (andmap simple-string? val))]
    [(#:doc-search) (string? val)]
    [(#:dist-name) (string? val)]
    [(#:dist-base) (simple-string? val)]
    [(#:dist-dir) (simple-string? val)]
    [(#:dist-suffix) (simple-string? val)]
    [(#:dist-catalogs) (and (list? val) (andmap string? val))]
    [(#:dist-base-url) (string? val)]
    [(#:install-name) (string? val)]
    [(#:build-stamp) (string? val)]
    [(#:max-vm) (real? val)]
    [(#:server) (simple-string? val)]
    [(#:server-port) (port-no? val)]
    [(#:server-hosts) (and (list? val) (andmap simple-string? val))]
    [(#:host) (simple-string? val)]
    [(#:user) (or (not val) (simple-string? val))]
    [(#:port) (port-no? val)]
    [(#:dir) (path-string? val)]
    [(#:vbox) (string? val)]
    [(#:platform) (memq val '(unix macosx windows windows/bash))]
    [(#:configure) (and (list? val) (andmap string? val))]
    [(#:bits) (or (equal? val 32) (equal? val 64))]
    [(#:vc) (string? val)]
    [(#:sign-identity) (string? val)]
    [(#:timeout) (real? val)]
    [(#:j) (exact-positive-integer? val)]
    [(#:repo) (string? val)]
    [(#:clean?) (boolean? val)]
    [(#:pull?) (boolean? val)]
    [(#:release?) (boolean? val)]
    [(#:source?) (boolean? val)]
    [(#:source-runtime?) (boolean? val)]
    [(#:source-pkgs?) (boolean? val)]
    [(#:versionless?) (boolean? val)]
    [(#:mac-pkg?) (boolean? val)]
    [(#:site-dest) (path-string? val)]
    [(#:site-help) (hash? val)]
    [(#:site-title) (string? val)]
    [(#:pdf-doc?) (boolean? val)]
    [(#:max-snapshots) (real? val)]
    [(#:plt-web-style?) (boolean? val)]
    [(#:pause-before) (and (real? val) (not (negative? val)))]
    [(#:pause-after) (and (real? val) (not (negative? val)))]
    [(#:readme) (or (string? val)
                    (and (procedure? val)
                         (procedure-arity-includes? val 1)))]
    [(#:email-to) (and (list? val) (andmap email? val))]
    [(#:email-from) (email? val)]
    [(#:smtp-server) (simple-string? val)]
    [(#:smtp-port) (port-no? val)]
    [(#:smtp-connect) (memq val '(plain ssl tls))]
    [(#:smtp-user) (or (not val) (string? val))]
    [(#:smtp-password) (or (not val) (string? val))]
    [(#:custom) (and (hash? val)
                     (for/and ([k (in-hash-keys val)])
                       (keyword? k)))]
    [else 'bad-keyword]))

(define (check-machine-keyword kw val)
  (case kw
    [(#:name) (string? val)]
    [else (check-group-keyword kw val)]))

(define (port-no? val)
  (and (exact-integer? val) (<= 1 val 65535)))

(define (simple-string? s)
  (and (string? s)
       ;; No spaces, quotes, or other things that could
       ;; break a command-line, path, or URL construction:
       (regexp-match #rx"^[-a-zA-A0-9.]*$" s)))

(define (email? s)
  (and (string? s)
       (regexp-match? #rx"@" s)))

(define current-mode (make-parameter "default"))

(define current-stamp
  (let* ([f (build-path "build" "stamp.txt")]
         [s (and (file-exists? f)
                 (call-with-input-file* f read-line))])
    (lambda ()
      (if (string? s)
          s
          "now"))))

(define (extract-options config-file config-mode)
  (parameterize ([current-mode config-mode])
    (site-config-options 
     (dynamic-require (path->complete-path config-file) 'site-config))))
