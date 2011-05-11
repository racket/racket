#lang racket/base
(require racket/match
         "reductions-config.rkt"
         "../util/mpi.rkt")
(provide policy->predicate)

;; A Policy is one of
;;   'disable
;;   'standard
;;   (list 'custom boolean boolean boolean boolean (listof Entry))

;; An Entry is one of
;; (list 'show-if Condition)
;; (list 'hide-if Condition)

;; A Condition is one of:
;; (list 'free=? identifier)
;; (list 'lexical)
;; (list 'unbound)
;; (list 'binding IdentifierBinding)
;; (list 'symbol=? symbol)
;; (list 'symbol-like regexp)
;; (list 'from-kernel-module)
;; (list 'from-def-module ModulePath)
;; (list 'from-nom-module ModulePath)
;; (list 'from-collection (listof String))
;; (list 'from-planet-collection String/#f String/#f (listof String))
;; (list 'phase>=? nat)
;; (cons 'and Condition)
;; (cons 'or Condition)

;; policy->predicate
(define (policy->predicate policy)
  (define fun (policy->function policy))
  (lambda (id)
    (case (fun id)
      [(show) #t]
      [(hide) #f]
      [else (error 'policy->predicate "incomplete policy (returned ~s): ~s"
                   (fun id)
                   policy)])))

;; policy->function : Policy -> (id -> choice)
(define (policy->function policy)
  (match policy
    ['disable
     (lambda (id) 'show)]
    ['standard
     (policy->function '(custom #t #t #t #t ()))]
    [(list 'custom hide-racket? hide-libs? hide-contracts? hide-phase1? entries)
     (entries->function entries
                        (policy-base->function hide-racket?
                                               hide-libs?
                                               hide-contracts?
                                               hide-phase1?))]))

;; policy-base->function : boolean boolean boolean boolean -> (id -> choice)
(define (policy-base->function hide-racket? hide-libs? hide-contracts? hide-phase1?)
  (entries->function
   `[(hide-if
      (or ,@(filter values
                    (list (and hide-racket?
                               '(or (from-kernel-module)
                                    (from-collection ("racket"))))
                          (and hide-libs?
                               '(or (from-collection ())
                                    #|(from-planet-collection #f #f ())|#))
                          (and hide-contracts?
                               '(symbol-like #rx"^provide/contract-id-"))
                          (and hide-phase1?
                               '(phase>=? 1))))))]
   (lambda (id) 'show)))

;; entries->function : (listof Entry) (id -> choice) -> (id -> choice)
(define (entries->function entries base-fun)
  (if (pair? entries)
      (entry->function (car entries)
                       (entries->function (cdr entries) base-fun))
      base-fun))

;; entry->function : Entry -> (id -> choice)
(define (entry->function entry base-fun)
  (match entry
    [(list 'show-if condition)
     (let ([pred (condition->predicate condition)])
       (lambda (id)
         (if (pred id) 'show (base-fun id))))]
    [(list 'hide-if condition)
     (let ([pred (condition->predicate condition)])
       (lambda (id)
         (if (pred id) 'hide (base-fun id))))]
    [(list 'splice entries)
     (entries->function entries base-fun)]))

;; condition->predicate : condition -> (id -> boolean)
(define (condition->predicate condition)
  (match condition
    [(list 'free=? the-id)
     (lambda (id)
       (free-identifier=? id the-id (phase)))]
    [(list 'lexical)
     (lambda (id)
       (eq? (get-binding id) 'lexical))]
    [(list 'unbound)
     (lambda (id)
       (eq? (get-binding id) #f))]
    [(list 'binding module-binding)
     (lambda (id)
       (let ([binding (get-binding id)])
         (and (pair? binding)
              (same-binding? binding module-binding))))]
    [(list 'symbol=? name)
     (lambda (id)
       (eq? (syntax-e id) name))]
    [(list 'symbol-like rx)
     (lambda (id)
       (regexp-match? rx (symbol->string (syntax-e id))))]
    [(list 'from-kernel-module)
     (lambda (id)
       (let ([binding (get-binding id)])
         (and (pair? binding)
              (kernel-module? (binding-def-module binding)))))]
    [(list 'from-def-module module-path)
     (lambda (id)
       (let ([binding (get-binding id)])
         (and (pair? binding)
              (same-module-path? (binding-def-module binding)
                                 module-path))))]
    [(list 'from-nom-module module-path)
     (lambda (id)
       (let ([binding (get-binding id)])
         (and (pair? binding)
              (same-module-path? (binding-nom-module binding)
                                 module-path))))]
    [(list 'from-collection collection)
     (lambda (id)
       (let ([binding (get-binding id)])
         (and (pair? binding)
              (collection-prefix? collection
                                  (binding-def-module binding)))))]
    [(list 'phase>=? num)
     (lambda (id)
       (>= (phase) num))]
    [(cons 'and conditions)
     (let ([predicates (map condition->predicate conditions)])
       (lambda (id)
         (for/and ([predicate predicates])
           (predicate id))))]
    [(cons 'or conditions)
     (let ([predicates (map condition->predicate conditions)])
       (lambda (id)
         (for/or ([predicate predicates])
           (predicate id))))]))

(define (kernel-module? mpi)
  (cond [(module-path-index? mpi)
         (let-values ([(a b) (module-path-index-split mpi)])
           (match a
             [`(quote ,name)
              (regexp-match? #rx"^#%" (symbol->string name))]
             [_ #f]))]
        [else #f]))

;; same-module-path? : mpi mpi -> boolean
(define (same-module-path? actual expected)
  (equal? (module-path-index-resolve actual)
          (module-path-index-resolve expected)))

;; same-binding? : binding binding -> boolean
(define (same-binding? actual expected)
  (and (list? actual)
       (same-module-path? (car actual) (car expected))
       (eq? (cadr actual) (cadr expected))))

;; collection-prefix? : (listof string) mpi -> boolean
(define (collection-prefix? collection mpi)
  (define library-frame
    (expanded-mpi-sexpr->library
     (mpi-sexpr->expanded-mpi-sexpr
      (mpi->mpi-sexpr mpi))))
  (match library-frame
    [`(LIB ,paths)
     (let loop ([cpaths collection] [paths paths])
       (cond [(and (pair? cpaths) (pair? paths))
              (and (equal? (car cpaths) (car paths))
                   (loop (cdr cpaths) (cdr paths)))]
             [(pair? cpaths) #f]
             [(pair? paths) #t]))]
    [_ #f]))


;; get-binding : id -> binding
(define (get-binding id)
  (identifier-binding id (phase)))

;; binding-def-module : binding -> module-path
(define (binding-def-module binding)
  (car binding))

;; binding-def-name : binding -> module-path
(define (binding-def-name binding)
  (cadr binding))

;; binding-nom-module : binding -> module-path
(define (binding-nom-module binding)
  (caddr binding))

;; binding-nom-name : binding -> module-path
(define (binding-nom-name binding)
  (cadddr binding))


;; ----

;; Conversion to and from S-expr form.
;; Conversion is lossy (identifier policies)

;; policy->policy-sexpr
(define (policy->policy-sexpr policy)
  (match policy
    [`(custom ,b1 ,b2 ,b3 ,b4 ,entries)
     `(CUSTOM ,b1 ,b2 ,b3 ,b4 ,(map entry->entry-sexpr entries))]
    [_ policy]))

;; policy-sexpr->policy
(define (policy-sexpr->policy sexpr)
  (match sexpr
    [`(CUSTOM ,b1 ,b2 ,b3 ,b4 ,entries)
     `(custom ,b1 ,b2 ,b3 ,b4 ,(map entry-sexpr->entry entries))]
    [_ sexpr]))

;; entry->entry-sexpr
(define (entry->entry-sexpr entry)
  (match entry
    [`(show-if ,condition)
     `(show-if ,(condition->condition-sexpr condition))]
    [`(hide-if ,condition)
     `(hide-if ,(condition->condition-sexpr condition))]))

;; entry-sexpr->entry
(define (entry-sexpr->entry sexpr)
  (match sexpr
    [`(show-if ,condition)
     `(show-if ,(condition-sexpr->condition condition))]
    [`(hide-if ,condition)
     `(hide-if ,(condition-sexpr->condition condition))]))

;; condition->condition-sexpr
(define (condition->condition-sexpr condition)
  (match condition
    [(list 'free=? id)
     (let ([binding (identifier-binding id)])
       (cond [(list? binding)
              (condition->condition-sexpr `(binding ,binding))]
             [(eq? binding 'lexical)
              `(and (lexical)
                    (symbol=? ,(syntax-e id)))]
             [else
              `(and (unbound)
                    (symbol=? ,(syntax-e id)))]))]
    [`(binding (,mod1 ,name1 ,mod2 ,name2 . ,rest))
     `(BINDING (,(mpi->mpi-sexpr mod1)
                ,name1
                ,(mpi->mpi-sexpr mod2)
                ,name2
                . ,rest))]
    [`(from-def-module ,mod)
     `(FROM-DEF-MODULE ,(mpi->mpi-sexpr mod))]
    [`(from-nom-module ,mod)
     `(FROM-NOM-MODULE ,(mpi->mpi-sexpr mod))]
    [`(and . ,conditions)
     `(and ,@(map condition->condition-sexpr conditions))]
    [`(or . ,conditions)
     `(or ,@(map condition->condition-sexpr conditions))]
    [_
     condition]))

;; condition-sexpr->condition
(define (condition-sexpr->condition sexpr)
  (match sexpr
    [`(BINDING (,mod1 ,name1 ,mod2 ,name2 . ,rest))
     `(binding (,(mpi-sexpr->mpi mod1)
                ,name1
                ,(mpi-sexpr->mpi mod2)
                ,name2
                . ,rest))]
    [`(FROM-DEF-MODULE ,mod)
     `(from-def-module ,(mpi-sexpr->mpi mod))]
    [`(FROM-NOM-MODULE ,mod)
     `(from-nom-module ,(mpi-sexpr->mpi mod))]
    [`(and . ,sexprs)
     `(and ,@(map condition-sexpr->condition sexprs))]
    [`(or . ,sexprs)
     `(or ,@(map condition-sexpr->condition sexprs))]
    [_ sexpr]))


;; ----

(provide same-condition?)

;; same-condition? : condition condition -> boolean
(define (same-condition? a b)
  (and (eq? (car a) (car b))
       (match a
         [`(free=? ,aid)
          (let ([bid (cadr b)])
            (for/and ([n '(0 #| 1 -1 |#)])
              (free-identifier=? aid bid n)))]
         [`(binding ,ab)
          (let ([bb (cadr b)]) 
            (and (same-module-path? (car ab) (car bb))
                 (eq? (cadr ab) (cadr bb))
                 (equal? (list-tail ab 4) (list-tail bb 4))))]
         [`(from-def-module ,ampi)
          (same-module-path? ampi (cadr b))]
         [`(from-nom-module ,ampi)
          (same-module-path? ampi (cadr b))]
         [`(and . ,aconditions)
          (let ([bconditions (cdr b)])
            (and (= (length aconditions) (length bconditions))
                 (andmap same-condition? aconditions (cdr b))))]
         [`(or . ,aconditions)
          (let ([bconditions (cdr b)])
            (and (= (length aconditions) (length bconditions))
                 (andmap same-condition? aconditions (cdr b))))]
         [_
          (equal? a b)])))


;; ----

(provide standard-policy
         base-policy
         hide-all-policy
         hide-none-policy)

(define standard-policy
  (policy->predicate 'standard))

(define base-policy
  (policy->predicate
   '(custom #t #f #f #f ())))

(define (hide-all-policy id) #f)
(define (hide-none-policy id) #t)
