#lang typed/racket

;; This currently does nothing useful with `->*' types
;; TODO: make it do something useful after Vincent fixes PR 13354

(require (for-syntax racket/base
                     racket/list))

(provide require/typed*)

(define-for-syntax (list-accum lst)
  (for/list ([i  (in-range (+ 1 (length lst)))])
    (take lst i)))

(define-for-syntax (interpret-opts stx)
  (let loop ([stxs  (syntax->list stx)] [opts  empty] [kws  empty])
    (cond [(empty? stxs)  #`(#,(list-accum (reverse opts)) #,(reverse kws))]
          [else
           (syntax-case (first stxs) ()
             [[kw T]
              (keyword? (syntax->datum #'kw))
              (loop (rest stxs) opts (cons (first stxs) kws))]
             [_
              (loop (rest stxs) (cons (first stxs) opts) kws)])])))

(define-for-syntax (interpret-clause stx)
  (syntax-case stx ()
    [[name  ((Rs ...) (opts ...) arrow T)]
     (and (identifier? #'arrow) (eq? '->* (syntax->datum #'arrow)))
     (with-syntax ([(((Os ...) ...) (Ks ...))  (interpret-opts #'(opts ...))])
       (quasisyntax/loc stx
         [name  (case-> (Rs ... Os ... Ks ... -> T) ...)]))]
    [_  stx]))

(define-syntax (require/typed* stx)
  (syntax-case stx ()
    [(_ module-name clause ...)
     (with-syntax ([(clause ...)  (map interpret-clause (syntax->list #'(clause ...)))])
       (syntax/loc stx
         (require/typed module-name clause ...)))]))
