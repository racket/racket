(module contract-ds-helpers mzscheme
  (provide ensure-well-formed
           build-func-params
           build-clauses
           generate-arglists)
  
  (require (lib "list.ss"))
  (require-for-template mzscheme)

#|

With this definition:

(define-contract s (a b c))

this:

(s/dc [x e-x]
      [y () e-y]
      [z (x y) e-z])

expands into procedures & structs like this:

(let ([c-x e-x]
      [c-y (lambda (_) e-y)]
      [c-z (lambda (x y) e-z)])
  ... c-* variables get put into the contract struct ...

which are then called when the contract's fields are explored

|#

  (define (build-clauses name coerce-contract stx clauses)
    (let* ([field-names 
            (map (λ (clause)
                   (syntax-case clause ()
                     [(id . whatever) (syntax id)]
                     [else (raise-syntax-error name 
                                               "expected a field name and a contract together"
                                               stx
                                               clause)]))
                 (syntax->list clauses))]
           [all-ac-ids (generate-temporaries field-names)]
           [defeat-inlining 
             ;; makes the procedure "big enough" so
             ;; that inlining doesn't consider it.
             (λ (e)
               (let loop ([i 30])
                 (cond
                   [(zero? i) e]
                   [else #`(values #,(loop (- i 1)))])))])
      (let loop ([clauses (syntax->list clauses)]
                 [ac-ids all-ac-ids]
                 [prior-ac-ids '()]
                 [maker-args '()])
        (cond
          [(null? clauses)
           (reverse maker-args)]
          [else 
           (let ([clause (car clauses)]
                 [ac-id (car ac-ids)])
             (syntax-case clause ()
               [(id (x ...) ctc-exp)
                (and (identifier? (syntax id))
                     (andmap identifier? (syntax->list (syntax (x ...)))))
                (let ([maker-arg #`(λ #,(match-up (reverse prior-ac-ids)
                                                  (syntax (x ...))
                                                  field-names)
                                     #,(defeat-inlining
                                         #`(#,coerce-contract '#,name ctc-exp)))])
                  (loop (cdr clauses)
                        (cdr ac-ids)
                        (cons (car ac-ids) prior-ac-ids)
                        (cons maker-arg maker-args)))]
               [(id (x ...) ctc-exp)
                (begin
                  (unless (identifier? (syntax id))
                    (raise-syntax-error name "expected identifier" stx (syntax id)))
                  (for-each (λ (x) (unless (identifier? x)
                                     (raise-syntax-error name "expected identifier" stx x)))
                            (syntax->list (syntax (x ...)))))]
               [(id ctc-exp)
                (identifier? (syntax id))
                (loop (cdr clauses)
                      (cdr ac-ids)
                      (cons (car ac-ids) prior-ac-ids)
                      (cons #`(#,coerce-contract '#,name ctc-exp) maker-args))]
               [(id ctc-exp)
                (raise-syntax-error name "expected identifier" stx (syntax id))]))]))))
  
  ;; generate-arglists : (listof X) -> (listof (listof X))
  ;; produces the list of arguments to the dependent contract
  ;; functions, given the names of some variables.
  ;; eg: (generate-arglists '(x y z w))
  ;;  =  (list '() '(x) '(x y) '(x y z))
  (define (generate-arglists vars)
    (reverse
     (let loop ([vars (reverse vars)])
       (cond
         [(null? vars) null]
         [else (cons (reverse (cdr vars)) 
                     (loop (cdr vars)))]))))
  
  (define (match-up prior-ac-ids used-field-names field-names)
    (let ([used-field-ids (syntax->list used-field-names)])
      (let loop ([prior-ac-ids prior-ac-ids]
                 [field-names field-names])
        (cond
          [(null? prior-ac-ids) null]
          [else (let* ([ac-id (car prior-ac-ids)]
                       [field-name (car field-names)]
                       [id-used 
                        (ormap (λ (used-field-id) 
                                 (and (eq? (syntax-e field-name) (syntax-e used-field-id))
                                      used-field-id))
                               used-field-ids)])
                  (if id-used
                      (cons id-used
                            (loop (cdr prior-ac-ids)
                                  (cdr field-names)))
                      (cons (car (generate-temporaries '(ignored-arg)))
                            (loop (cdr prior-ac-ids)
                                  (cdr field-names)))))]))))
  
  (define (sort-wrt name stx ids current-order-field-names desired-order-field-names)
    (let ([id/user-specs (map cons ids current-order-field-names)]
          [ht (make-hash-table)])
      (let loop ([i 0]
                 [orig-field-names desired-order-field-names])
        (unless (null? orig-field-names) 
          (hash-table-put! ht (syntax-e (car orig-field-names)) i)
          (loop (+ i 1) (cdr orig-field-names))))
      (let* ([lookup
              (λ (id-pr)
                (let ([id (car id-pr)]
                      [use-field-name (cdr id-pr)])
                  (hash-table-get ht
                                  (syntax-e use-field-name) 
                                  (λ () 
                                    (raise-syntax-error name "unknown field name" stx use-field-name)))))]
             [cmp (λ (x y) (<= (lookup x) (lookup y)))]
             [sorted-id/user-specs (sort id/user-specs cmp)])
        (map car sorted-id/user-specs))))
    

  (define (find-matching all-ac-ids chosen-ids field-names)
    (map (λ (chosen-id)
           (let* ([chosen-sym (syntax-e chosen-id)]
                  [id (ormap (λ (ac-id field-name)
                               (and (eq? (syntax-e field-name) chosen-sym)
                                    ac-id))
                             all-ac-ids
                             field-names)])
             (unless id
               (error 'find-matching "could not find matching for ~s" chosen-id))
             id))
         (syntax->list chosen-ids)))

  
  (define (build-func-params ids)
    (let ([temps (generate-temporaries ids)])
      (let loop ([ids (syntax->list ids)]
                 [temps temps]
                 [can-refer-to '()])
        (cond
          [(null? ids) null]
          [else (cons
                 (append (reverse can-refer-to) temps)
                 (loop (cdr ids)
                       (cdr temps)
                       (cons (car ids) can-refer-to)))]))))
  
  (define (ensure-well-formed stx field-count)
    (syntax-case stx ()
      [(_ [id exp] ...)
       (and (andmap identifier? (syntax->list (syntax (id ...))))
            (equal? (length (syntax->list (syntax (id ...))))
                    field-count))
       (void)]
      [(_ [id exp] ...)
       (andmap identifier? (syntax->list (syntax (id ...))))
       (raise-syntax-error 'struct/dc 
                           (format "expected ~a clauses, but found ~a"
                                   field-count
                                   (length (syntax->list (syntax (id ...)))))
                           stx)]
      [(_ [id exp] ...)
       (for-each
        (λ (id) (unless (identifier? id) (raise-syntax-error 'struct/dc "expected identifier" stx id)))
        (syntax->list (syntax (id ...))))])))

  
