#lang racket/base

(require "rewrite.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     (only-in syntax/path-spec resolve-path-spec)
                     "rewrite.rkt"))

(provide define-rewrite
         define-rewrite/compose
         include/rewrite)

(begin-for-syntax
  (define-syntax-class identifier-list
    (pattern (id:id ...))))

(define-syntax (define-rewrite stx)
  (syntax-parse stx #:datum-literals (==>)
   [(define-rewriter rw:id from:expr ==> to:expr
      (~or (~optional (~seq #:variables variables:identifier-list)
                      #:name "#:variables <list of identifiers> option")
           (~optional (~seq #:context contexts:identifier-list)
                      #:name "#:context <context specification> option")
           (~optional (~seq (~and #:once-only once-only-kw))
                      #:name "#:one-only option")
           (~optional (~seq (~and #:exactly-once exactly-once-kw))
                      #:name "#:exactly-once option")) ...)
    (with-syntax* ([vars (if (attribute variables) #'variables #'())]
                   [ctxs (if (attribute contexts) #'contexts #'())]
                   [once-val (if (attribute once-only-kw) #t #f)]
                   [exactly-once-val (if (attribute exactly-once-kw) #t #f)]
                   [ids (syntax-ids #'from (syntax->list #'vars))])
      #`(define-syntax rw (rewriter/ids from to ids ctxs once-val exactly-once-val)))]))
       
(define-syntax (define-rewrite/compose stx)
  (syntax-case stx ()
    [(_ id rws ...)
     (with-syntax* ([the-stx #'the-stx]
                    [body (apply-to #'(rws ...) #'the-stx)])
       #`(define-syntax-rule (id the-stx)
           body))]))

(define-syntax (include/rewrite stx)
  (syntax-case stx ()
    [(_ mod-path new-name rws ...)
     (syntax-case (include-mod #'mod-path stx) (module)
       [(module old-name . body)
        (apply-to #'(rws ...) #'(module new-name . body))])]))

(define-for-syntax (include-mod mod-path context-stx)
  (define file (resolve-path-spec mod-path mod-path mod-path))
  (define file-stx
         (parameterize ([read-accept-lang #t]
                        [read-accept-reader #t]
                        [port-count-lines-enabled #t])
           (call-with-input-file file
             (λ (in)
               (read-syntax file in)))))
  (let loop ([content file-stx])
           (cond
             [(pair? content)
              (cons (loop (car content))
                    (loop (cdr content)))]
             [(null? content) null]
             [else
              (define v (syntax-e content))
              (datum->syntax
               context-stx
               (cond
                 [(pair? v) 
                  (loop v)]
                 [(vector? v)
                  (list->vector (loop (vector->list v)))]
                 [(box? v)
                  (box (loop (unbox v)))]
                 [else
                  v])
               content
               content)])))
