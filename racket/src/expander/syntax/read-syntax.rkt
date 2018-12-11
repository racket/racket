#lang racket/base
(require "../common/performance.rkt"
         (rename-in "../read/main.rkt" 
                    [read main:read]
                    [read-language main:read-language])
         "syntax.rkt"
         "property.rkt"
         "original.rkt"
         "../eval/dynamic-require.rkt"
         "../namespace/api-module.rkt"
         "../namespace/namespace.rkt"
         "srcloc.rkt"
         "../compile/read-linklet.rkt")

(provide read
         read/recursive
         read-syntax
         read-syntax/recursive
         read-language)

(define (read-syntax src in)
  (cond
    [(default-read-handler? in)
     (maybe-flush-stdout in)
     (read* in
            #:for-syntax? #t
            #:source src)]
    [else
     ;; `values` forces a single result value:
     (values ((port-read-handler in) in src))]))

(define (read-syntax/recursive src in start readtable graph?)
  (read* in
         #:for-syntax? #t
         #:recursive? #t
         #:source src
         #:init-c start
         #:readtable readtable
         #:local-graph? (not graph?)))

(define (read in)
  (cond
    [(default-read-handler? in)
     (maybe-flush-stdout in)
     (read* in
            #:for-syntax? #f)]
    [else
     ;; `values` forces a single result value:
     (values ((port-read-handler in) in))]))

(define (read/recursive in start readtable graph?)
  (read* in
         #:for-syntax? #f
         #:recursive? #t
         #:init-c start
         #:readtable readtable
         #:local-graph? (not graph?)))

(define (read* in
               #:for-syntax? for-syntax?
               #:recursive? [recursive? #f]
               #:source [source #f]
               #:init-c [init-c #f]
               #:readtable [readtable (current-readtable)]
               #:local-graph? [local-graph? #f])
  (performance-region
   ['read]
   (main:read in
              #:for-syntax? for-syntax?
              #:recursive? recursive?
              #:source source
              #:wrap (and for-syntax?
                          read-to-syntax)
              #:init-c init-c
              #:readtable readtable
              #:local-graph? local-graph?
              #:read-compiled read-linklet-bundle-or-directory
              #:call-with-root-namespace call-with-root-namespace
              #:dynamic-require dynamic-require
              #:module-declared? read-module-declared?
              #:coerce read-coerce
              #:coerce-key read-coerce-key)))

(define (read-language in fail-thunk)
  (main:read-language in fail-thunk
                      #:for-syntax? #t
                      #:wrap read-to-syntax
                      #:read-compiled read-linklet-bundle-or-directory
                      #:call-with-root-namespace call-with-root-namespace
                      #:dynamic-require dynamic-require
                      #:module-declared? read-module-declared?
                      #:coerce read-coerce
                      #:coerce-key read-coerce-key))

(define (read-to-syntax s-exp srcloc rep)
  (struct-copy syntax empty-syntax
               [content (datum-intern-literal s-exp)]
               [srcloc srcloc]
               [props (case rep
                        [(#\[) original-square-props]
                        [(#\{) original-curly-props]
                        [else original-props])]))
  
(define original-props
  (syntax-props (syntax-property empty-syntax original-property-sym #t)))
(define original-square-props
  (syntax-props (syntax-property (syntax-property empty-syntax original-property-sym #t)
                                 'paren-shape #\[)))
(define original-curly-props
  (syntax-props (syntax-property (syntax-property empty-syntax original-property-sym #t)
                                 'paren-shape #\{)))

(define (read-module-declared? mod-path)
  (module-declared? mod-path #t))

(define (read-coerce for-syntax? v srcloc)
  (cond
   [(not for-syntax?)
    (cond
     [(syntax? v) (syntax->datum v)]
     [else v])]
   [(syntax? v) v]
   [(list? v)
    (read-to-syntax (for/list ([e (in-list v)])
                      (read-coerce #t e srcloc))
                    srcloc
                    #f)]
   [(pair? v)
    (read-to-syntax (cons (read-coerce #t (car v) srcloc)
                          (read-coerce #t (cdr v) srcloc))
                    srcloc
                    #f)]
   [else
    (read-to-syntax v srcloc #f)]))

(define (read-coerce-key for-syntax? k)
  (cond
   [for-syntax? (datum-intern-literal k)]
   [else k]))

;; ----------------------------------------

;; Initialized on first port that we read from, on the
;; assuption that we have to read some file before a
;; read handler can possibly be set:
(define default-read-handler #f)

(define (default-read-handler? in)
  (cond
   [(not default-read-handler)
    (set! default-read-handler (port-read-handler in))
    #t]
   [else
    (eq? default-read-handler (port-read-handler in))]))

(define orig-input-port (current-input-port))
(define orig-output-port (current-output-port))
(define orig-error-port (current-error-port))

(define (maybe-flush-stdout in)
  (when (eq? in orig-input-port)
    (flush-output orig-output-port)
    (flush-output orig-error-port)))

;; ----------------------------------------

(define (call-with-root-namespace thunk)
  (define root-ns (namespace-root-namespace (current-namespace)))
  (if root-ns
      ;; Switch to the root namespace:
      (parameterize ([current-namespace root-ns])
        (thunk))
      (thunk)))
