
(module contract mzscheme

  ;; povide contracts for objects
  (require "private/contract-object.ss")
  (provide (all-from "private/contract-object.ss"))
   
  (require "private/contract.ss"
           "private/contract-arrow.ss"
           "private/contract-guts.ss"
           "private/contract-ds.ss"
           "private/contract-opt-guts.ss"
           "private/contract-opt.ss"
           "private/contract-basic-opters.ss")
  
  (provide 
   opt/c define-opt/c ;(all-from "private/contract-opt.ss")
   (all-from-except "private/contract-ds.ss"
                    lazy-depth-to-look)

   (all-from-except "private/contract-arrow.ss"
                    check-procedure)
   (all-from-except "private/contract.ss"
                    check-between/c
                    check-unary-between/c))
  
  ;; from contract-guts.ss
  
  (provide any
           and/c
           any/c
           none/c
           make-none/c 
           
           guilty-party
           contract-violation->string
           
           contract?
           contract-name
           contract-proc
           
           flat-contract?
           flat-contract
           flat-contract-predicate
           flat-named-contract
           
           contract-first-order-passes?
           
           ;; below need docs
           
           make-proj-contract
           
           contract-stronger?
           
           coerce-contract 
           flat-contract/predicate?

           build-compound-type-name
           raise-contract-error

           proj-prop proj-pred? proj-get
           name-prop name-pred? name-get
           stronger-prop stronger-pred? stronger-get
           flat-prop flat-pred? flat-get
           first-order-prop first-order-get))
  

;; ======================================================================
;; The alternate implementation disables contracts. Its useful mainly to
;; measure the cost of contracts. It's not necessarily complete, but it
;;  works well enough for starting DrScheme.

#;
(module contract mzscheme

  (define-syntax provide/contract
    (syntax-rules ()
      [(_ elem ...)
       (begin (provide-one elem) ...)]))

  (define-syntax provide-one
    (syntax-rules (struct rename)
      [(_ (struct (id par-id) ([field . rest] ...)))
       (provide-struct id par-id (field ...))]
      [(_ (struct id ([field . rest] ...)))
       (provide (struct id (field ...)))]
      [(_ (rename id1 id2 c))
       (provide (rename id1 id2))]
      [(_ (id c))
       (provide id)]))

  (define-syntax (provide-struct stx)
    (syntax-case stx ()
      [(_ id par-id . rest)
       (let ([info (syntax-local-value #'id (lambda () #f))]
             [p-info (syntax-local-value #'par-id (lambda () #f))]
             [prefix (lambda (l n)
                       (let loop ([l l][len (length l)])
                         (if (= n len)
                             null
                             (cons (car l) (loop (cdr l)
                                                 (- len 1))))))]
             [ids (lambda (l) (let loop ([l l])
                                (cond
                                 [(null? l) null]
                                 [(car l) (cons (car l) (loop (cdr l)))]
                                 [else (loop (cdr l))])))])
         (if (and info
                  p-info
                  (list? info)
                  (list? p-info)
                  (= (length info) 6)
                  (= (length p-info) 6))
             #`(provide #,@(append
                            (list #'id
                                  (list-ref info 0)
                                  (list-ref info 1)
                                  (list-ref info 2))
                            (ids (prefix (list-ref info 3) (length (list-ref p-info 3))))
                            (ids (prefix (list-ref info 4) (length (list-ref p-info 4))))))
             (raise-syntax-error 
              #f
              (cond
               [(not info) "cannot find struct info"]
               [(not p-info) "cannot find parent-struct info"]
               [else (format "struct or parent-struct info has unexpected shape: ~e and ~e"
                             info p-info)])
              #'id)))]))

  (define-syntax define-contract-struct
    (syntax-rules ()
      [(_ . rest) (define-struct . rest)]))

  (define-syntax define/contract
    (syntax-rules ()
      [(_ id c expr) (define id expr)]))

  (define-syntax contract
    (syntax-rules ()
      [(_ c expr . rest) expr]))

  (provide provide/contract
           define-contract-struct
           define/contract
           contract)

  (define mk*
    (lambda args (lambda (x) x)))

  (define-syntax mk 
    (syntax-rules ()
      [(_ id) (begin
                (define-syntax (id stx) (quote-syntax mk*))
                (provide id))]
      [(_ id ...)
       (begin (mk id) ...)]))

  (mk ->
      ->*
      opt->
      case->
      ->r
      or/c
      and/c
      any/c
      flat-named-contract
      flat-contract
      flat-contract-predicate
      object-contract
      union
      listof
      is-a?/c)

  (define-syntax symbols
    (syntax-rules ()
      [(_ sym ...)
       (lambda (v) (memq v '(sym ...)))]))
  (provide symbols)
  
  )
