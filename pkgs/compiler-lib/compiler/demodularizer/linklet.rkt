#lang racket/base
(require racket/match
         racket/linklet
         compiler/zo-structs
         compiler/private/deserialize
         compiler/faslable-correlated)

(provide linklet*-exports
         linklet*-internals
         linklet*-importss
         linklet*-internal-exports
         linklet*-internal-importss
         linklet*-import-shapess
         linklet*-lifts
         linklet*-body
         s-exp->linklet
         linklet->s-exp)

(define (get-exports linkl select)
  (cond
    [(linkl? linkl) (linkl-exports linkl)]
    [(faslable-correlated-linklet? linkl)
     (match (faslable-correlated-linklet-expr linkl)
       [`(linklet ,imports ,exports . ,_) (for/list ([ex (in-list exports)])
                                            (strip-correlated
                                             (if (pair? ex)
                                                 (select ex)
                                                 ex)))])]
    [else (unsupported linkl)]))

(define (linklet*-exports linkl)
  (get-exports linkl cadr))

(define (linklet*-internal-exports linkl)
  (get-exports linkl car))

(define (linklet*-internals linkl)
  (cond
    [(linkl? linkl) (linkl-internals linkl)]
    [(faslable-correlated-linklet? linkl)
     (match (faslable-correlated-linklet-expr linkl)
       [`(linklet ,imports ,exports . ,body)
        (for/fold ([l '()]) ([form (in-list body)])
          (match form
            [`(define-values ,ids ,_)
             (append (map strip-correlated ids) l)]
            [else l]))])]
    [else (unsupported linkl)]))

(define (linklet*-importss linkl)
  (get-importss linkl car))

(define (linklet*-internal-importss linkl)
  (get-importss linkl cadr))

(define (get-importss linkl select)
  (cond
    [(linkl? linkl) (linkl-importss linkl)]
    [(faslable-correlated-linklet? linkl)
     (match (faslable-correlated-linklet-expr linkl)
       [`(linklet ,importss ,exports . ,_) (for/list ([imports (in-list importss)])
                                             (for/list ([im (in-list imports)])
                                               (strip-correlated
                                                (if (pair? im)
                                                    (select im)
                                                    im))))])]
    [else (unsupported linkl)]))

(define (linklet*-import-shapess linkl)
  (cond
    [(linkl? linkl) (linkl-import-shapess linkl)]
    [(faslable-correlated-linklet? linkl)
     (match (faslable-correlated-linklet-expr linkl)
       [`(linklet ,importss ,exports . ,_) (for/list ([imports (in-list importss)])
                                             (for/list ([im (in-list imports)])
                                               #f))])]
    [else (unsupported linkl)]))

(define (linklet*-lifts linkl)
  (cond
    [(linkl? linkl) (linkl-lifts linkl)]
    [(faslable-correlated-linklet? linkl) '()]
    [else (unsupported linkl)]))

(define (linklet*-body linkl)
  (cond
    [(faslable-correlated-linklet? linkl)
     ;; keep correlated wrappers only on `lambda` and `case-lambda` forms:
     (match (faslable-correlated-linklet-expr linkl)
       [`(linklet ,imports ,exports . ,body)
        (let loop ([v body])
          (cond
            [(faslable-correlated? v)
             (define e (faslable-correlated-e v))
             (cond
               [(and (pair? e)
                     (or (eq? (car e) 'lambda)
                         (eq? (car e) 'case-lambda)))
                (struct-copy faslable-correlated v
                             [e (loop (faslable-correlated-e v))])]
               [else
                (loop (faslable-correlated-e v))])]
            [(pair? v)
             (define a (loop (car v)))
             (if (eq? a 'quote)
                 (cons a (strip-correlated (cdr v)))
                 (cons a (loop (cdr v))))]
            [else v]))])]
    [else #f]))

(define (s-exp->linklet name expr)
  (faslable-correlated-linklet expr name))

(define (linklet->s-exp linkl)
  (faslable-correlated-linklet-expr linkl))

(define (unsupported linkl)
  (error 'demodularize "unsupported linklet format"))
