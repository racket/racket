#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "simple.rkt"
         "gensym.rkt"
         "aim.rkt")

(provide left-to-right/let
         left-to-right/let-values
         left-to-right/app

         make-let-values)

;; Convert a `let` to nested lets to enforce order; we
;; rely on the fact that the Racket expander generates
;; expressions that have no shadowing (and introduce
;; shadowing here)
(define (left-to-right/let ids rhss bodys
                           prim-knowns knowns imports mutated simples unsafe-mode?)
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
                  (simple? rhs prim-knowns knowns imports mutated simples unsafe-mode?))
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
                       (simple? rhs prim-knowns knowns imports mutated simples unsafe-mode?))
                  (cons `[,id ,id] binds)))]))]))

;; Convert a `let-values` to nested `let-values`es to
;; enforce order
(define (left-to-right/let-values idss rhss bodys mutated target)
  (cond
    [(null? (cdr idss))
     (define e (if (null? (cdr bodys))
                   (car bodys)
                   `(begin . ,bodys)))
     (make-let-values (car idss) (car rhss) e target)]
   [else
    (let loop ([idss idss] [rhss rhss] [binds null])
      (cond
       [(null? (cdr rhss))
        (make-let-values
         (car idss) (car rhss)
         `(let ,binds
            . ,bodys)
         target)]
       [else
        (define ids (car idss))
        (make-let-values
         ids
         (car rhss)
         (loop (cdr idss) (cdr rhss) (append (for/list ([id (in-wrap-list ids)])
                                               `[,id ,id])
                                             binds))
         target)]))]))

;; Convert an application to enforce left-to-right evaluation order.
(define (left-to-right/app rator rands app-form target
                           prim-knowns knowns imports mutated simples unsafe-mode?)
  (cond
    [(aim? target 'cify) (cons rator rands)]
    [else
     (define l (cons rator rands))
     (define modes
       ;; If an argument is pure, we don't have to order it explicitly.
       ;; If an argument is pure except for allocation, then we only have to
       ;; order it if a later argument is non-pure.
       (let loop ([l l])
         (cond
           [(null? l) 'pure]
           [else
            (define modes (loop (cdr l)))
            (cond
              [(simple? (car l) prim-knowns knowns imports mutated simples unsafe-mode? #:no-alloc? #t)
               (if (symbol? modes)
                   modes
                   (cons 'pure modes))]
              [(simple? (car l) prim-knowns knowns imports mutated simples unsafe-mode?) ; allocates
               (if (symbol? modes)
                   'alloc
                   (cons 'bind modes))]
              [else
               (if (eq? modes 'pure)
                   (cons 'non-simple modes)
                   (cons 'bind modes))])])))
     (define no-bind-needed?
       (let loop ([modes modes])
         (cond
           [(symbol? modes) #t]
           [(eq? (car modes) 'pure) (loop (cdr modes))]
           [(eq? (car modes) 'non-simple) #t]
           [else #f])))
     (cond
       [no-bind-needed?
        (if app-form
            (cons app-form l)
            l)]
       [else
        (let loop ([l l] [modes modes] [accum null])
          (cond
            [(or (symbol? modes)
                 (eq? (car modes) 'non-simple))
             (define app (append (reverse accum) l))
             (if app-form
                 (cons app-form app)
                 app)]
            [(eq? (car modes) 'bind)
             (define g (deterministic-gensym "app_"))
             `(let ([,g ,(car l)])
                ,(loop (cdr l) (cdr modes) (cons g accum)))]
            [else
             (loop (cdr l) (cdr modes) (cons (car l) accum))]))])]))
          
;; ----------------------------------------

(define (make-let-values ids rhs body target)
  (cond
   [(and (pair? ids) (null? (cdr ids)))
    `(let ([,(car ids) ,rhs]) ,body)]
   [else
    (match (and (null? ids) rhs)
      [`(begin ,rhs (values))
       `(begin ,rhs ,body)]
      [`,_
       (cond
         [(aim? target 'cify)
          ;; No checking
          `(call-with-values (lambda () ,rhs)
             (lambda ,ids ,body))]
         [else
          `(call-with-values (lambda () ,rhs)
             (case-lambda 
               [,ids ,body]
               [args (,@(if (aim? target 'system) '() '(#%app/no-return))
                      raise-binding-result-arity-error ,(length ids) args)]))])])]))
