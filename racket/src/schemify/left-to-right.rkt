#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "simple.rkt")

(provide left-to-right/let
         left-to-right/let-values
         left-to-right/app

         make-let-values)

;; Convert a `let` to nested lets to enforce order; we
;; rely on the fact that the Racket expander generates
;; expressions that have no shadowing (and introduce
;; shadowing here)
(define (left-to-right/let ids rhss bodys
                           prim-knowns knowns imports mutated)
  (cond
   [(null? ids) (if (null? (cdr bodys))
                    (car bodys)
                    `(begin . ,bodys))]
   [(null? (cdr ids))
    `(let ([,(car ids) ,(car rhss)]) . ,bodys)]
   [else
    (let loop ([ids ids] [rhss rhss] [all-simple? #t] [binds null])
      (cond
        [(null? (cdr rhss))
         (define id (car ids))
         (define rhs (car rhss))
         (if (and all-simple?
                  (simple? rhs prim-knowns knowns imports mutated))
             `(let ([,id ,rhs])
                . ,bodys)
             `(let ([,id ,rhs])
                (let ,binds ; <- allocate ids after all `rhs`s are evaluated
                  . ,bodys)))]
       [else
        (define id (car ids))
        (define rhs (car rhss))
        `(let ([,id ,rhs])
           ,(loop (cdr ids)
                  (cdr rhss)
                  (and all-simple?
                       (simple? rhs prim-knowns knowns imports mutated))
                  (cons `[,id ,id] binds)))]))]))

;; Convert a `let-values` to nested `let-values`es to
;; enforce order
(define (left-to-right/let-values idss rhss bodys mutated for-cify?)
  (cond
    [(null? (cdr idss))
     (define e (if (null? (cdr bodys))
                   (car bodys)
                   `(begin . ,bodys)))
     (make-let-values (car idss) (car rhss) e for-cify?)]
   [else
    (let loop ([idss idss] [rhss rhss] [binds null])
      (cond
       [(null? (cdr rhss))
        (make-let-values
         (car idss) (car rhss)
         `(let ,binds
            . ,bodys)
         for-cify?)]
       [else
        (define ids (car idss))
        (make-let-values
         ids
         (car rhss)
         (loop (cdr idss) (cdr rhss) (append (for/list ([id (in-wrap-list ids)])
                                               `[,id ,id])
                                             binds))
         for-cify?)]))]))

;; Convert an application to enforce left-to-right
;; evaluation order
(define (left-to-right/app rator rands plain-app? for-cify?
                           prim-knowns knowns imports mutated)
  (cond
    [for-cify? (cons rator rands)]
    [else
     (let loop ([l (cons rator rands)] [accum null] [pending-non-simple #f] [pending-id #f])
       (cond
         [(null? l)
          (let ([app
                 (cond
                   [pending-non-simple
                    ;; Since the last non-simple was followed only by simples,
                    ;; we don't need that variable
                    (let loop ([accum accum] [rev-accum null])
                      (cond
                        [(null? accum) rev-accum]
                        [(eq? (car accum) pending-id)
                         (loop (cdr accum) (cons pending-non-simple rev-accum))]
                        [else
                         (loop (cdr accum) (cons (car accum) rev-accum))]))]
                   [else (reverse accum)])])
            (if plain-app?
                app
                `(|#%app| . ,app)))]
         [(simple? (car l) prim-knowns knowns imports mutated)
          (loop (cdr l) (cons (car l) accum) pending-non-simple pending-id)]
         [pending-non-simple
          `(let ([,pending-id ,pending-non-simple])
             ,(loop l accum #f #f))]
         [else
          (define g (gensym "app_"))
          (loop (cdr l) (cons g accum) (car l) g)]))]))

;; ----------------------------------------

(define (make-let-values ids rhs body for-cify?)
  (cond
   [(and (pair? ids) (null? (cdr ids)))
    `(let ([,(car ids) ,rhs]) ,body)]
   [else
    (match (and (null? ids) rhs)
      [`(begin ,rhs (values))
       `(begin ,rhs ,body)]
      [`,_
       (cond
         [for-cify?
          ;; No checking
          `(call-with-values (lambda () ,rhs)
             (lambda ,ids ,body))]
         [else
          `(call-with-values (lambda () ,rhs)
             (case-lambda 
               [,ids ,body]
               [args (raise-binding-result-arity-error ',ids args)]))])])]))
