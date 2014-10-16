#lang racket/base
(require (for-syntax racket/base
                     racket/stxparam
                     syntax/parse)
         racket/stxparam
         racket/undefined
         "class-wrapped.rkt"
         "class-internal.rkt"
         "../contract/base.rkt"
         "../contract/combinator.rkt"
         (only-in "../contract/private/arrow.rkt" making-a-method method-contract?))

(provide make-class/c class/c-proj
         blame-add-method-context blame-add-field-context blame-add-init-context
         class/c ->m ->*m ->dm case->m object/c instanceof/c
         make-wrapper-object
         check-object-contract
         (for-syntax parse-class/c-specs)
         (struct-out internal-class/c)
         just-check-existence just-check-existence?
         build-internal-class/c internal-class/c-proj
         class/c-internal-name-clauses)

;; Shorthand contracts that treat the implicit object argument as if it were
;; contracted with any/c.
(define-syntax-rule (->m . stx)
  (syntax-parameterize ([making-a-method #t] [method-contract? #t]) (-> . stx)))

(define-syntax-rule (->*m . stx)
  (syntax-parameterize ([making-a-method #t] [method-contract? #t]) (->* . stx)))

(define-syntax-rule (case->m . stx)
  (syntax-parameterize ([making-a-method #t] [method-contract? #t]) (case-> . stx)))

(define-syntax-rule (->dm . stx)
  (syntax-parameterize ([making-a-method #'this-param] [method-contract? #t]) (->d . stx)))

(define (class/c-check-first-order ctc cls fail)
  (unless (class? cls)
    (fail '(expected: "a class" given: "~v") cls))
  (define method-ht (class-method-ht cls))
  (define methods (class-methods cls))
  (define beta-methods (class-beta-methods cls))
  (define meth-flags (class-meth-flags cls))
  (for ([m (in-list (class/c-methods ctc))]
        [c (in-list (class/c-method-contracts ctc))])
    (define mth (hash-ref method-ht m #f))
    (unless mth (fail "no public method ~a" m))
    (when c
      (define meth-proc
        (let loop ([m/l (vector-ref methods mth)])
          (cond
            [(pair? m/l) (loop (car m/l))]
            [else m/l])))
      (unless (contract-first-order-passes? c meth-proc)
        (fail "public method ~a doesn't match contract" m))))
  (unless (class/c-opaque? ctc)
    (for ([m (class/c-absents ctc)])
      (when (hash-ref method-ht m #f)
        (fail "class already contains public method ~a" m))))
  (when (class/c-opaque? ctc)
    (for ([m (in-hash-keys method-ht)])
      (unless (memq m (class/c-methods ctc))
        (if (symbol-interned? m)
            (fail "method ~a not specified in contract" m)
            (fail "some local member not specified in contract")))))
  
  (define field-ht (class-field-ht cls))
  (for ([f (class/c-fields ctc)])
    (unless (hash-ref field-ht f #f)
      (fail "no public field ~a" f)))
  (unless (class/c-opaque? ctc)
    (for ([f (class/c-absent-fields ctc)])
      (when (hash-ref field-ht f #f)
        (fail "class already contains public field ~a" f))))
  (when (class/c-opaque? ctc)
    (for ([f (in-hash-keys field-ht)])
      (unless (memq f (class/c-fields ctc))
        (if (symbol-interned? f)
            (fail "field ~a not specified in contract" f)
            (fail "some local member field not specified in contract")))))
  #t)

(define (internal-class/c-check-first-order internal-ctc cls fail)
  (define method-ht (class-method-ht cls))
  (define methods (class-methods cls))
  (define beta-methods (class-beta-methods cls))
  (define meth-flags (class-meth-flags cls))
  (for ([m (internal-class/c-inherits internal-ctc)])
    (unless (hash-ref method-ht m #f)
      (fail "no public method ~a" m)))
  (for ([m (internal-class/c-overrides internal-ctc)])
    (let ([index (hash-ref method-ht m #f)])
      (unless index
        (fail "no public method ~a" m))
      (let ([vec (vector-ref beta-methods index)])
        (unless (zero? (vector-length vec))
          (fail "method ~a was previously augmentable" m)))
      (let ([flag (vector-ref meth-flags index)])
        (when (eq? flag 'final)
          (fail "method ~a is final" m)))))
  (for ([m (internal-class/c-augments internal-ctc)])
    (let ([index (hash-ref method-ht m #f)])
      (unless index
        (fail "no public method ~a" m))
      (let* ([vec (vector-ref beta-methods index)])
        (when (zero? (vector-length vec))
          (fail "method ~a has never been augmentable" m))
        (when (vector-ref vec (sub1 (vector-length vec)))
          (fail "method ~a is currently overrideable, not augmentable" m)))))
  (for ([m (internal-class/c-augrides internal-ctc)])
    (let ([index (hash-ref method-ht m #f)])
      (unless index
        (fail "no public method ~a" m))
      (let ([vec (vector-ref beta-methods index)])
        (when (zero? (vector-length vec))
          (fail "method ~a has never been augmentable" m))
        (unless (vector-ref vec (sub1 (vector-length vec)))
          (fail "method ~a is currently augmentable, not overrideable" m)))))
  (for ([s (internal-class/c-supers internal-ctc)])
    (let ([index (hash-ref method-ht s #f)])
      (unless index
        (fail "no public method ~a" s))
      (let ([flag (vector-ref meth-flags index)])
        (when (eq? flag 'final)
          (fail "method ~a is final" s))
        (when (eq? flag 'augmentable)
          (fail "method ~a is augmentable, not overrideable" s)))))
  (for ([i (internal-class/c-inners internal-ctc)])
    (let ([index (hash-ref method-ht i #f)])
      (unless index
        (fail "no public method ~a" i))
      (let ([vec (vector-ref beta-methods index)])
        (when (zero? (vector-length vec))
          (fail "method ~a has never been augmentable" i)))
      (let ([flag (vector-ref meth-flags index)])
        (when (eq? flag 'final)
          (fail "method ~a is final" i)))))
  (define field-ht (class-field-ht cls))
  (for ([f (internal-class/c-inherit-fields internal-ctc)])
    (unless (hash-ref field-ht f #f)
      (fail "no public field ~a" f)))
  
  #t)

(define (class/c-proj ctc)
  (define ep (class/c-external-proj ctc))
  (define ip (internal-class/c-proj (class/c-internal ctc)))
  (λ (blame)
    (define eb (ep blame))
    (define ib (ip blame))
    (λ (val)
      (ib (eb val)))))

(define (class/c-external-proj ctc)
  (define ctc-methods (class/c-methods ctc))
  (λ (blame)
    (define public-method-projections
      (for/list ([name (in-list ctc-methods)]
                 [c (in-list (class/c-method-contracts ctc))])
        (and c
             ((contract-projection c) (blame-add-method-context blame name)))))
    
    (define external-field-projections
      (for/list ([f (in-list (class/c-fields ctc))]
                 [c (in-list (class/c-field-contracts ctc))])
        (and c
             (let ([p-pos ((contract-projection c)
                           (blame-add-field-context blame f #:swap? #f))]
                   [p-neg ((contract-projection c)
                           (blame-add-field-context blame f #:swap? #t))])
               (cons p-pos p-neg)))))
    
    ;; zip the inits and contracts together for ordered selection
    (define inits+contracts 
      (for/list ([init (in-list (class/c-inits ctc))]
                 [ctc (in-list (class/c-init-contracts ctc))])
        (if ctc
            (list init ((contract-projection ctc) 
                        (blame-add-init-context blame init)))
            (list init #f))))
    
    (λ (cls)
      (class/c-check-first-order ctc cls (λ args (apply raise-blame-error blame cls args)))
      (let* ([name (class-name cls)]
             [never-wrapped? (eq? (class-orig-cls cls) cls)]
             ;; Only add a new slot if we're not projecting an already contracted class.
             [supers (if never-wrapped?
                         (list->vector (append (vector->list (class-supers cls))
                                               (list #f)))                           
                         (list->vector (vector->list (class-supers cls))))]
             [pos (if never-wrapped?
                      (add1 (class-pos cls))
                      (class-pos cls))]
             [method-width (class-method-width cls)]
             [method-ht (class-method-ht cls)]
             [method-ictcs (class-method-ictcs cls)]
             [methods (if (null? ctc-methods)
                          (class-methods cls)
                          (make-vector method-width))]
             [field-pub-width (class-field-pub-width cls)]
             [no-field-ctcs? (null? (class/c-fields ctc))]
             
             [field-ht (if no-field-ctcs?
                           (class-field-ht cls)
                           (hash-copy (class-field-ht cls)))]
             [init (class-init cls)]
             [class-make (if name
                             (make-naming-constructor struct:class name "class")
                             make-class)]
             [c (class-make name
                            pos
                            supers
                            (class-self-interface cls)
                            void ;; No inspecting
                            
                            method-width
                            method-ht
                            (class-method-ids cls)
                            (class-abstract-ids cls)
                            (remq* ctc-methods method-ictcs)
                            
                            #f
                            
                            methods
                            (class-super-methods cls)
                            (class-int-methods cls)
                            (class-beta-methods cls)
                            (class-meth-flags cls)
                            
                            (class-inner-projs cls)
                            (class-dynamic-idxs cls)
                            (class-dynamic-projs cls)
                            
                            (class-field-width cls)
                            field-pub-width
                            field-ht
                            (class-field-ids cls)
                            (class-all-field-ids cls)
                            
                            'struct:object 'object? 'make-object
                            'field-ref 'field-set!
                            
                            ;; class/c introduced subclasses do not consume init args
                            null
                            'normal
                            #f
                            
                            (class-orig-cls cls)
                            #f #f ; serializer is never set
                            
                            (class-check-undef? cls)
                            #f)]
             [obj-name (if name
                           (string->symbol (format "object:~a" name))
                           'object)])
        (define (make-method proc meth-name)
          (procedure-rename
           (procedure->method proc)
           (string->symbol
            (format "~a method~a~a"
                    meth-name
                    (if name " in " "")
                    (or name "")))))
        
        (vector-set! supers pos c)
        
        ;; --- Make the new object struct ---
        (let-values ([(struct:object object-make object? object-field-ref object-field-set!)
                      (make-struct-type obj-name
                                        (class-struct:object cls)
                                        0 ;; No init fields
                                        0 ;; No new fields in this class replacement
                                        undefined
                                        ;; Map object property to class:
                                        (list (cons prop:object c)))])
          (set-class-struct:object! c struct:object)
          (set-class-object?! c object?)
          (set-class-make-object! c object-make)
          (set-class-field-ref! c object-field-ref)
          (set-class-field-set!! c object-field-set!))
        
        ;; Handle public method contracts
        (unless (null? ctc-methods)
          ;; First, fill in from old methods
          (vector-copy! methods 0 (class-methods cls))
          ;; Concretize any interface contracts handled by this ctc
          (unless (null? (class-method-ictcs cls))
            (for ([m (in-list (class-method-ictcs cls))])
              ;; only concretize if class/c takes responsibility for it
              (when (memq m ctc-methods)
                (define i (hash-ref method-ht m))
                (define entry (vector-ref methods i))
                ;; we're passing through a contract boundary, so the positive blame (aka
                ;; value server) is taking responsibility for any interface-contracted
                ;; methods)
                (define info (replace-ictc-blame (cadr entry) #f (blame-positive blame)))
                (vector-set! methods i (concretize-ictc-method m (car entry) info)))))
          ;; Now apply projections
          (for ([m (in-list ctc-methods)]
                [p (in-list public-method-projections)])
            (when p
              (define i (hash-ref method-ht m))
              (define mp (vector-ref methods i))
              (vector-set! methods i (make-method (p mp) m)))))
        
        ;; Handle external field contracts
        (unless no-field-ctcs?
          (for ([f (in-list (class/c-fields ctc))]
                [p-pr (in-list external-field-projections)])
            (when p-pr
              (define fi (hash-ref field-ht f))
              (define p-pos (car p-pr))
              (define p-neg (cdr p-pr))
              (hash-set! field-ht f (field-info-extend-external fi p-pos p-neg)))))
        
        ;; Unlike the others, we always want to do this, even if there are no init contracts,
        ;; since we still need to handle either calling the previous class/c's init or
        ;; calling continue-make-super appropriately.
        (let ()
          ;; grab all the inits+contracts that involve the same init arg
          ;; (assumes that inits and contracts were sorted in class/c creation)
          (define (grab-same-inits lst)
            (if (null? lst)
                (values null null)
                (let loop ([inits/c (cdr lst)]
                           [prefix (list (car lst))])
                  (cond
                    [(null? inits/c) 
                     (values (reverse prefix) inits/c)]
                    [(eq? (list-ref (car inits/c) 0) (list-ref (car prefix) 0))
                     (loop (cdr inits/c)
                           (cons (car inits/c) prefix))]
                    [else (values (reverse prefix) inits/c)]))))
          ;; run through the list of init-args and apply contracts for same-named
          ;; init args
          (define (apply-init-contracts inits/c init-args)
            (let loop ([init-args init-args]
                       [inits/c inits/c]
                       [handled-args null])
              (cond
                [(null? init-args)
                 (reverse handled-args)]
                [(null? inits/c)
                 (append (reverse handled-args) init-args)]
                [(eq? (list-ref (car inits/c) 0) (car (car init-args)))
                 (let ([init-arg (car init-args)]
                       [p (list-ref (car inits/c) 1)])
                   (loop (cdr init-args)
                         (cdr inits/c)
                         (cons (cons (car init-arg) (if p
                                                        (p (cdr init-arg))
                                                        (cdr init-arg)))
                               handled-args)))]
                [else (loop (cdr init-args)
                            inits/c
                            (cons (car init-args) handled-args))])))
          (set-class-init! 
           c
           (lambda (the-obj super-go si_c si_inited? si_leftovers init-args)
             (let ([init-args
                    (let loop ([inits/c inits+contracts]
                               [handled-args init-args])
                      (if (null? inits/c)
                          handled-args
                          (let-values ([(prefix suffix) (grab-same-inits inits/c)])
                            (loop suffix
                                  (apply-init-contracts prefix init-args)))))])
               ;; Since we never consume init args, we can ignore si_leftovers
               ;; since init-args is the same.
               (if never-wrapped?
                   (super-go the-obj si_c si_inited? init-args null null)
                   (init the-obj super-go si_c si_inited? init-args init-args))))))
        
        c))))

(define (internal-class/c-proj internal-ctc)
  (define dynamic-features
    (append (internal-class/c-overrides internal-ctc)
            (internal-class/c-augments internal-ctc)
            (internal-class/c-augrides internal-ctc)
            (internal-class/c-inherits internal-ctc)))
  (define dynamic-contracts
    (append (internal-class/c-override-contracts internal-ctc)
            (internal-class/c-augment-contracts internal-ctc)
            (internal-class/c-augride-contracts internal-ctc)
            (internal-class/c-inherit-contracts internal-ctc)))
  (λ (blame)
    (define bswap (blame-swap blame))
    (define super-projections
      (for/list ([name (in-list (internal-class/c-supers internal-ctc))]
                 [c (in-list (internal-class/c-super-contracts internal-ctc))])
        (and c
             ((contract-projection c) (blame-add-method-context blame name)))))
    (define inner-projections
      (for/list ([name (in-list (internal-class/c-inners internal-ctc))]
                 [c (in-list (internal-class/c-inner-contracts internal-ctc))])
        (and c
             ((contract-projection c) (blame-add-method-context bswap name)))))
    
    (define internal-field-projections
      (for/list ([f (in-list (internal-class/c-inherit-fields internal-ctc))]
                 [c (in-list (internal-class/c-inherit-field-contracts internal-ctc))])
        (and c
             (let ([p-pos ((contract-projection c) blame)]
                   [p-neg ((contract-projection c) bswap)])
               (cons p-pos p-neg)))))
    
    (define override-projections
      (for/list ([m (in-list (internal-class/c-overrides internal-ctc))]
                 [c (in-list (internal-class/c-override-contracts internal-ctc))])
        (and c
             ((contract-projection c) (blame-add-method-context bswap m)))))
    
    (define augment/augride-projections
      (for/list ([m (in-list (append (internal-class/c-augments internal-ctc)
                                     (internal-class/c-augrides internal-ctc)))]
                 [c (in-list (append (internal-class/c-augment-contracts internal-ctc)
                                     (internal-class/c-augride-contracts internal-ctc)))])
        (and c
             ((contract-projection c) (blame-add-method-context blame m)))))
    
    (define inherit-projections
      (for/list ([m (in-list (internal-class/c-inherits internal-ctc))]
                 [c (in-list (internal-class/c-inherit-contracts internal-ctc))])
        (and c
             ((contract-projection c) (blame-add-method-context blame m)))))
    (λ (cls)
      (internal-class/c-check-first-order internal-ctc
                                          cls 
                                          (λ args (apply raise-blame-error blame cls args)))
      
      (let* ([name (class-name cls)]
             [never-wrapped? (eq? (class-orig-cls cls) cls)]
             ;; Only add a new slot if we're not projecting an already contracted class.
             [supers (if never-wrapped?
                         (list->vector (append (vector->list (class-supers cls))
                                               (list #f)))                           
                         (list->vector (vector->list (class-supers cls))))]
             [pos (if never-wrapped?
                      (add1 (class-pos cls))
                      (class-pos cls))]
             [method-width (class-method-width cls)]
             [method-ht (class-method-ht cls)]
             [method-ictcs (class-method-ictcs cls)]
             
             [field-pub-width (class-field-pub-width cls)]
             [no-field-ctcs? (null? (internal-class/c-inherit-fields internal-ctc))]
             
             [field-ht (if no-field-ctcs?
                           (class-field-ht cls)
                           (hash-copy (class-field-ht cls)))]
             [init (class-init cls)]
             [class-make (if name
                             (make-naming-constructor struct:class name "class")
                             make-class)]
             [super-methods (if (null? (internal-class/c-supers internal-ctc))
                                (class-super-methods cls)
                                (make-vector method-width))]
             [int-methods (if (null? dynamic-features)
                              (class-int-methods cls)
                              (make-vector method-width))]
             [inner-projs (if (null? (internal-class/c-inners internal-ctc))
                              (class-inner-projs cls)
                              (make-vector method-width))]
             [dynamic-idxs (if (null? dynamic-features)
                               (class-dynamic-idxs cls)
                               (make-vector method-width))]
             [dynamic-projs (if (null? dynamic-features)
                                (class-dynamic-projs cls)
                                (make-vector method-width))][field-ht (if no-field-ctcs?
                           (class-field-ht cls)
                           (hash-copy (class-field-ht cls)))]
             [init (class-init cls)]
             [class-make (if name
                             (make-naming-constructor struct:class name "class")
                             make-class)]
             [c (class-make name
                            pos
                            supers
                            (class-self-interface cls)
                            void ;; No inspecting
                            
                            method-width
                            method-ht
                            (class-method-ids cls)
                            (class-abstract-ids cls)
                            method-ictcs
                            
                            #f
                            
                            (class-methods cls)
                            super-methods
                            int-methods
                            (class-beta-methods cls)
                            (class-meth-flags cls)
                            
                            inner-projs
                            dynamic-idxs
                            dynamic-projs
                            
                            (class-field-width cls)
                            field-pub-width
                            field-ht
                            (class-field-ids cls)
                            (class-all-field-ids cls)
                            
                            'struct:object 'object? 'make-object
                            'field-ref 'field-set!
                            
                            ;; class/c introduced subclasses do not consume init args
                            null
                            'normal
                            #f
                            
                            (class-orig-cls cls)
                            #f #f ; serializer is never set

                            (class-check-undef? cls)

                            #f)]
             [obj-name (if name
                           (string->symbol (format "object:~a" name))
                           'object)])
      
        (define (make-method proc meth-name)
          (procedure-rename
           (procedure->method proc)
           (string->symbol
            (format "~a method~a~a"
                    meth-name
                    (if name " in " "")
                    (or name "")))))
        
        (vector-set! supers pos c)
        
        ;; --- Make the new object struct ---
        (let-values ([(struct:object object-make object? object-field-ref object-field-set!)
                      (make-struct-type obj-name
                                        (class-struct:object cls)
                                        0 ;; No init fields
                                        0 ;; No new fields in this class replacement
                                        undefined
                                        ;; Map object property to class:
                                        (list (cons prop:object c)))])
          (set-class-struct:object! c struct:object)
          (set-class-object?! c object?)
          (set-class-make-object! c object-make)
          (set-class-field-ref! c object-field-ref)
          (set-class-field-set!! c object-field-set!))
        
        ;; Handle super contracts
        (unless (null? (internal-class/c-supers internal-ctc))
          ;; First, fill in from old (possibly contracted) super methods
          (vector-copy! super-methods 0 (class-super-methods cls))
          ;; Now apply projections.
          (for ([m (in-list (internal-class/c-supers internal-ctc))]
                [p (in-list super-projections)])
            (when p
              (define i (hash-ref method-ht m))
              (define mp (vector-ref super-methods i))
              (vector-set! super-methods i (make-method (p mp) m)))))
        
        ;; Add inner projections
        (unless (null? (internal-class/c-inners internal-ctc))
          (vector-copy! inner-projs 0 (class-inner-projs cls))
          (for ([m (in-list (internal-class/c-inners internal-ctc))]
                [p (in-list inner-projections)])
            (when p
              (define i (hash-ref method-ht m))
              (define old-proj (vector-ref inner-projs i))
              (vector-set! inner-projs i (λ (v) (old-proj (p v)))))))
        
        ;; Handle external field contracts
        (unless no-field-ctcs?
          (for ([f (in-list (internal-class/c-inherit-fields internal-ctc))]
                [p-pr (in-list internal-field-projections)])
            (when p-pr
              (define fi (hash-ref field-ht f))
              (define p-pos (car p-pr))
              (define p-neg (cdr p-pr))
              (hash-set! field-ht f (field-info-extend-internal fi p-pos p-neg)))))
        
        ;; Now the trickiest of them all, internal dynamic dispatch.
        ;; First we update any dynamic indexes, as applicable.
        (let ([old-idxs (class-dynamic-idxs (class-orig-cls cls))])
          (unless (null? dynamic-features)
            ;; Go ahead and do all the copies here.
            (vector-copy! dynamic-projs 0 (class-dynamic-projs cls))
            (vector-copy! int-methods 0 (class-int-methods cls))
            (vector-copy! dynamic-idxs 0 (class-dynamic-idxs cls))
            (for ([m (in-list dynamic-features)]
                  [c (in-list dynamic-contracts)])
              (when c
                (let* ([i (hash-ref method-ht m)]
                       [old-idx (vector-ref old-idxs i)]
                       [new-idx (vector-ref dynamic-idxs i)])
                  ;; We need to extend all the vectors, so let's do that here.
                  (when (= old-idx new-idx)
                    (let* ([new-idx (add1 old-idx)]
                           [new-proj-vec (make-vector (add1 new-idx))]
                           [old-proj-vec (vector-ref dynamic-projs i)]
                           [new-int-vec (make-vector (add1 new-idx))]
                           [old-int-vec (vector-ref int-methods i)])
                      (vector-set! dynamic-idxs i new-idx)
                      (vector-copy! new-proj-vec 0 old-proj-vec)
                      (vector-set! new-proj-vec new-idx values)
                      (vector-set! dynamic-projs i new-proj-vec)
                      (vector-copy! new-int-vec 0 old-int-vec)
                      ;; Just copy over the last entry here.  We'll
                      ;; update it appropriately later.
                      (vector-set! new-int-vec new-idx
                                   (vector-ref old-int-vec old-idx))
                      (vector-set! int-methods i new-int-vec)))))))
          
          ;; Now we handle updating override contracts... here we just
          ;; update the projections, and not the methods (which we must
          ;; do during class composition).
          (unless (null? (internal-class/c-overrides internal-ctc))
            (for ([m (in-list (internal-class/c-overrides internal-ctc))]
                  [p (in-list override-projections)])
              (when p
                (let* ([i (hash-ref method-ht m)]
                       [old-idx (vector-ref old-idxs i)]
                       [proj-vec (vector-ref dynamic-projs i)]
                       [old-proj (vector-ref proj-vec old-idx)])
                  (vector-set! proj-vec old-idx (λ (v) (old-proj (p v))))))))
          
          ;; For augment and augride contracts, we both update the projection
          ;; and go ahead and apply the projection to the last slot (which will
          ;; only be used by later classes).
          (unless (and (null? (internal-class/c-augments internal-ctc))
                       (null? (internal-class/c-augrides internal-ctc)))
            (for ([m (in-list (append (internal-class/c-augments internal-ctc)
                                      (internal-class/c-augrides internal-ctc)))]
                  [p (in-list augment/augride-projections)])
              (when p
                (let* ([i (hash-ref method-ht m)]
                       [old-idx (vector-ref old-idxs i)]
                       [new-idx (vector-ref dynamic-idxs i)]
                       [proj-vec (vector-ref dynamic-projs i)]
                       [int-vec (vector-ref int-methods i)]
                       [old-proj (vector-ref proj-vec old-idx)])
                  (vector-set! proj-vec old-idx (λ (v) (p (old-proj v))))
                  (vector-set! int-vec new-idx
                               (make-method (p (vector-ref int-vec new-idx)) m))))))
          
          ;; Now (that things have been extended appropriately) we handle
          ;; inherits.
          (unless (null? (internal-class/c-inherits internal-ctc))
            (for ([m (in-list (internal-class/c-inherits internal-ctc))]
                  [p (in-list inherit-projections)])
              (when p
                (let* ([i (hash-ref method-ht m)]
                       [new-idx (vector-ref dynamic-idxs i)]
                       [int-vec (vector-ref int-methods i)])
                  (vector-set! int-vec new-idx
                               (make-method (p (vector-ref int-vec new-idx)) m)))))))
        
        ;; Unlike the others, we always want to do this, even if there are no init contracts,
        ;; since we still need to handle either calling the previous class/c's init or
        ;; calling continue-make-super appropriately.
        (let ()
          ;; grab all the inits+contracts that involve the same init arg
          ;; (assumes that inits and contracts were sorted in class/c creation)
          (define (grab-same-inits lst)
            (if (null? lst)
                (values null null)
                (let loop ([inits/c (cdr lst)]
                           [prefix (list (car lst))])
                  (cond
                    [(null? inits/c) 
                     (values (reverse prefix) inits/c)]
                    [(eq? (list-ref (car inits/c) 0) (list-ref (car prefix) 0))
                     (loop (cdr inits/c)
                           (cons (car inits/c) prefix))]
                    [else (values (reverse prefix) inits/c)]))))
          ;; run through the list of init-args and apply contracts for same-named
          ;; init args
          (define (apply-init-contracts inits/c init-args)
            (let loop ([init-args init-args]
                       [inits/c inits/c]
                       [handled-args null])
              (cond
                [(null? init-args)
                 (reverse handled-args)]
                [(null? inits/c)
                 (append (reverse handled-args) init-args)]
                [(eq? (list-ref (car inits/c) 0) (car (car init-args)))
                 (let ([init-arg (car init-args)]
                       [p (list-ref (car inits/c) 1)])
                   (loop (cdr init-args)
                         (cdr inits/c)
                         (cons (cons (car init-arg) (if p
                                                        (p (cdr init-arg))
                                                        (cdr init-arg)))
                               handled-args)))]
                [else (loop (cdr init-args)
                            inits/c
                            (cons (car init-args) handled-args))])))
          (set-class-init! 
           c
           (lambda (the-obj super-go si_c si_inited? si_leftovers init-args)
             ;; Since we never consume init args, we can ignore si_leftovers
             ;; since init-args is the same.
             (if never-wrapped?
                 (super-go the-obj si_c si_inited? init-args null null)
                 (init the-obj super-go si_c si_inited? init-args init-args)))))
        
        c))))

(define (blame-add-init-context blame name)
  (blame-add-context blame
                     (format "the ~a init argument in" name)
                     #:swap? #t))

(define (blame-add-method-context blame name)
  (cond
    [(symbol? name)
     (blame-add-context blame 
                        (format "the ~a method in" name)
                        #:important
                        name)]
    [(not name)
     (blame-add-context blame "an unnamed method in")]
    [else (error 'blame-add-method-context "uhoh ~s" name)]))

(define (blame-add-field-context blame f #:swap? swap?)
  (blame-add-context blame (format "the ~a field in" f) #:swap? swap?))

(define (build-internal-class/c inherits inherit-contracts inherit-fields inherit-field-contracts
                                supers super-contracts inners inner-contracts
                                overrides override-contracts augments augment-contracts
                                augrides augride-contracts)
  (internal-class/c inherits (adjust-jce inherit-contracts)
                    inherit-fields (adjust-jce inherit-field-contracts)
                    supers (adjust-jce super-contracts)
                    inners (adjust-jce inner-contracts)
                    overrides (adjust-jce override-contracts)
                    augments (adjust-jce augment-contracts)
                    augrides (adjust-jce augride-contracts)))
(struct internal-class/c
  (inherits inherit-contracts inherit-fields inherit-field-contracts
   supers super-contracts inners inner-contracts
   overrides override-contracts augments augment-contracts
   augrides augride-contracts))

(define (build-class/c methods method-contracts fields field-contracts inits init-contracts
                       absents absent-fields 
                       internal opaque? name)
  (make-class/c
   methods (adjust-jce method-contracts)
   fields (adjust-jce field-contracts)
   inits (adjust-jce init-contracts)
   absents absent-fields
   internal opaque? name))
(define (adjust-jce objs)
  (for/list ([obj (in-list objs)])
    (cond
      [(just-check-existence? obj) #f]
      [else (coerce-contract 'class/c obj)])))

(define (build-class/c-name ctc)
  (or (class/c-name ctc)
      (let* ([handled-methods
              (for/list ([i (in-list (class/c-methods ctc))]
                         [ctc (in-list (class/c-method-contracts ctc))])
                (cond
                  [ctc (build-compound-type-name i ctc)]
                  [else i]))])
        (apply build-compound-type-name
               'class/c 
               (append
                handled-methods
                (handle-optional 'init (class/c-inits ctc) (class/c-init-contracts ctc))
                (handle-optional 'field (class/c-fields ctc) (class/c-field-contracts ctc))
                (class/c-internal-name-clauses (class/c-internal ctc))
                (handle-absents (class/c-absents ctc) (class/c-absent-fields ctc)))))))

(define (class/c-internal-name-clauses internal-ctc)
  (append
   (handle-optional 'inherit 
                    (internal-class/c-inherits internal-ctc)
                    (internal-class/c-inherit-contracts internal-ctc))
   (handle-optional 'inherit-field 
                    (internal-class/c-inherit-fields internal-ctc)
                    (internal-class/c-inherit-field-contracts internal-ctc))
   (handle-optional 'super
                    (internal-class/c-supers internal-ctc)
                    (internal-class/c-super-contracts internal-ctc))
   (handle-optional 'inner 
                    (internal-class/c-inners internal-ctc) 
                    (internal-class/c-inner-contracts internal-ctc))
   (handle-optional 'override
                    (internal-class/c-overrides internal-ctc)
                    (internal-class/c-override-contracts internal-ctc))
   (handle-optional 'augment
                    (internal-class/c-augments internal-ctc)
                    (internal-class/c-augment-contracts internal-ctc))
   (handle-optional 'augride
                    (internal-class/c-augrides internal-ctc)
                    (internal-class/c-augride-contracts internal-ctc))))

(define (pair-ids-ctcs is ctcs)
  (for/list ([i (in-list is)]
             [ctc (in-list ctcs)])
    (if (not ctc)
        i
        (build-compound-type-name i ctc))))
(define (handle-optional name is ctcs)
  (if (null? is)
      null
      (list (cons name (pair-ids-ctcs is ctcs)))))
(define (handle-absents meths fields)
  (cond
    [(and (null? meths) (null? fields))
     null]
    [(null? fields)
     (list (cons 'absent meths))]
    [else
     (list `(absent ,@meths (field ,@fields)))]))

(define (class/c-stronger this that)
  (define this-internal (class/c-internal this))
  (cond
    [(class/c? that)
     (define that-internal (class/c-internal that))
     (and 
      ;; methods
      (check-one-stronger class/c-methods class/c-method-contracts this that)
      
      ;; inits
      (check-one-stronger class/c-inits class/c-init-contracts this that)
      
      ;; check both ways for fields (since mutable)
      (check-one-stronger class/c-fields class/c-field-contracts this that) 
      (check-one-stronger class/c-fields class/c-field-contracts that this)
      

      ;; inherits
      (check-one-stronger internal-class/c-inherits internal-class/c-inherit-contracts
                          this-internal that-internal)
      ;; inherit fields, both ways
      (check-one-stronger internal-class/c-inherit-fields internal-class/c-inherit-field-contracts
                          this-internal that-internal)
      (check-one-stronger internal-class/c-inherit-fields internal-class/c-inherit-field-contracts
                          that-internal this-internal)
      ;; supers
      (check-one-stronger internal-class/c-supers internal-class/c-super-contracts
                          this-internal that-internal)
      ;; inners
      (check-one-stronger internal-class/c-inners internal-class/c-inner-contracts
                          this-internal that-internal)
      ;; overrides
      (check-one-stronger internal-class/c-overrides internal-class/c-override-contracts
                          this-internal that-internal)
      ;; augments
      (check-one-stronger internal-class/c-augments internal-class/c-augment-contracts
                          this-internal that-internal)
      ;; augrides
      (check-one-stronger internal-class/c-augrides internal-class/c-augride-contracts
                          this-internal that-internal)
      
      (if (class/c-opaque? this) (class/c-opaque? that) #t)
      (all-included? (class/c-absent-fields that) (class/c-absent-fields this))
      (all-included? (class/c-absents that) (class/c-absents this)))]
    [else #f]))

(define (all-included? this-items that-items)
  (for/and ([this-item (in-list this-items)])
    (for/or ([that-item (in-list that-items)])
      (equal? this-item that-item))))

(define (check-one-stronger names-sel ctcs-sel this that)
  ;; this is an O(n^2) loop that could be made asymptotically
  ;; better with sorting, but since there are generally not a
  ;; ton of methods, the naive loop appears to be faster.
  ;; in the current racket, and assuming the code below is
  ;; representative of the two approaches, the tradeoff point
  ;; appears to be somewhere around 60 or 70 methods.
#|
    #lang racket
    
    (define (n2-way l1 l2)
      (for/and ([x (in-list l1)])
        (or (for/or ([y (in-list l2)])
              #f)
            #t)))
    
    (define (nlgn-way l1 l2)
      (define sl1 (sort l1 <))
      (define sl2 (sort l2 <))
      (let loop ([l1 l1][l2 l2])
        (cond
          [(null? l1) #t]
          [(null? l2) #t]
          [(< (car l1) (car l2)) (loop (cdr l1) l2)]
          [(< (car l2) (car l1)) (loop l1 (cdr l2))]
          [else (loop (cdr l1) (cdr l2))])))
    
    
    (define (try n c)
      (define l1 (build-list n (λ (_) (random))))
      (define l2 (build-list n (λ (_) (random))))
      (time (for ([x (in-range c)])
              (n2-way l1 l2) (n2-way l1 l2) (n2-way l1 l2)
              (n2-way l1 l2) (n2-way l1 l2) (n2-way l1 l2)
              (n2-way l1 l2) (n2-way l1 l2) (n2-way l1 l2)
              (n2-way l1 l2) (n2-way l1 l2) (n2-way l1 l2)))
      (time (for ([x (in-range c)])
              (nlgn-way l1 l2) (nlgn-way l1 l2) (nlgn-way l1 l2)
              (nlgn-way l1 l2) (nlgn-way l1 l2) (nlgn-way l1 l2)
              (nlgn-way l1 l2) (nlgn-way l1 l2) (nlgn-way l1 l2)
              (nlgn-way l1 l2) (nlgn-way l1 l2) (nlgn-way l1 l2))))
    
    
    50
    (try 50 10000)
    60
    (try 60 10000)
    70
    (try 70 10000)
    80
    (try 80 10000)
 |#
  
  (for/and ([this-name (in-list (names-sel this))]
            [this-ctc (in-list (ctcs-sel this))])
    (for/or ([that-name (in-list (names-sel that))]
             [that-ctc (in-list (ctcs-sel that))])
      (and (equal? this-name that-name)
           (contract-stronger? this-ctc that-ctc)))))

(define-struct class/c 
  (methods method-contracts fields field-contracts inits init-contracts
   absents absent-fields 
   internal opaque? name)
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:projection class/c-proj
   #:name build-class/c-name
   #:stronger class/c-stronger
   #:first-order
   (λ (ctc)
     (λ (cls)
       (let/ec ret
         (and (class/c-check-first-order ctc cls (λ args (ret #f)))
              (internal-class/c-check-first-order (class/c-internal ctc) cls (λ args (ret #f)))))))))

(define-values (just-check-existence just-check-existence?)
  (let ()
    (struct just-check-existence ())
    (values (just-check-existence) 
            just-check-existence?)))

(define-for-syntax (parse-class/c-specs forms object/c?)
  (define parsed-forms (make-hasheq))
  (define bindings '())
  (define form-name (if object/c? 'object/c 'class/c))
  (define (parse-name-ctc stx)
    (syntax-case stx ()
      [x
       (identifier? #'x)
       (with-syntax ([id (localize #'x)])
         (values #'`id #'just-check-existence))]
      [(x ctc)
       (identifier? #'x)
       (with-syntax ([id (localize #'x)])
         (values #'`id #'ctc))]
      [_
       (raise-syntax-error form-name "expected identifier or (id contract)" stx)]))
  (define (parse-names-ctcs stx)
    (for/fold ([names null]
               [ctcs null])
      ([stx (in-list (syntax->list stx))])
      (let-values ([(name ctc) (parse-name-ctc stx)])
        (values (cons name names) (cons ctc ctcs)))))
  (define (parse-absents stx)
    (for/fold ([meths null]
               [fields null])
      ([stx (in-list (syntax->list stx))])
      (syntax-case stx (field)
        [(field f-id ...)
         (let ([symbols (for/list ([id (in-list (syntax->list #'(f-id ...)))])
                          (unless (identifier? id)
                            (raise-syntax-error 'class/c "expected identifier" stx))
                          (with-syntax ([id (localize id)])
                            #'`id))])
           (values meths (append (reverse symbols) fields)))]
        [id
         (identifier? #'id)
         (with-syntax ([id (localize #'id)])
           (values (cons #'`id meths) fields))]
        [_
         (raise-syntax-error 'class/c "expected identifier or (field id ...)" stx)])))
  (define (parse-spec stx)
    (syntax-case stx (field inherit inherit-field init init-field 
                            super inner override augment augride absent)
      [(field f-spec ...)
       (let-values ([(names ctcs) (parse-names-ctcs #'(f-spec ...))])
         (hash-set! parsed-forms 'fields
                    (append names (hash-ref parsed-forms 'fields null)))
         (hash-set! parsed-forms 'field-contracts
                    (append (add-bindings/return-vars ctcs)
                            (hash-ref parsed-forms 'field-contracts null))))]
      [(init i-spec ...)
       (let-values ([(names ctcs) (parse-names-ctcs #'(i-spec ...))])
         (hash-set! parsed-forms 'inits
                    (append names (hash-ref parsed-forms 'inits null)))
         (hash-set! parsed-forms 'init-contracts
                    (append (add-bindings/return-vars ctcs)
                            (hash-ref parsed-forms 'init-contracts null))))]
      [(init-field i-spec ...)
       (let-values ([(names ctcs) (parse-names-ctcs #'(i-spec ...))])
         (define ctc-xs (add-bindings/return-vars ctcs))
         (hash-set! parsed-forms 'inits
                    (append names (hash-ref parsed-forms 'inits null)))
         (hash-set! parsed-forms 'init-contracts
                    (append ctc-xs (hash-ref parsed-forms 'init-contracts null)))
         (hash-set! parsed-forms 'fields
                    (append names (hash-ref parsed-forms 'fields null)))
         (hash-set! parsed-forms 'field-contracts
                    (append ctc-xs (hash-ref parsed-forms 'field-contracts null))))]
      [(inherit m-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "inherit contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(m-spec ...))])
           (hash-set! parsed-forms 'inherits
                      (append names (hash-ref parsed-forms 'inherits null)))
           (hash-set! parsed-forms 'inherit-contracts
                      (append (add-bindings/return-vars ctcs) 
                              (hash-ref parsed-forms 'inherit-contracts null)))))]
      [(inherit-field f-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "inherit-field contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(f-spec ...))])
           (hash-set! parsed-forms 'inherit-fields
                      (append names (hash-ref parsed-forms 'inherit-fields null)))
           (hash-set! parsed-forms 'inherit-field-contracts
                      (append (add-bindings/return-vars ctcs)
                              (hash-ref parsed-forms 'inherit-field-contracts null)))))]
      [(super s-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "super contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(s-spec ...))])
           (hash-set! parsed-forms 'supers
                      (append names (hash-ref parsed-forms 'supers null)))
           (hash-set! parsed-forms 'super-contracts
                      (append (add-bindings/return-vars ctcs)
                              (hash-ref parsed-forms 'super-contracts null)))))]
      [(inner i-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "inner contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(i-spec ...))])
           (hash-set! parsed-forms 'inners
                      (append names (hash-ref parsed-forms 'inners null)))
           (hash-set! parsed-forms 'inner-contracts
                      (append (add-bindings/return-vars ctcs)
                              (hash-ref parsed-forms 'inner-contracts null)))))]
      [(override o-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "override contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(o-spec ...))])
           (hash-set! parsed-forms 'overrides
                      (append names (hash-ref parsed-forms 'overrides null)))
           (hash-set! parsed-forms 'override-contracts
                      (append (add-bindings/return-vars ctcs)
                              (hash-ref parsed-forms 'override-contracts null)))))]
      [(augment a-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "augment contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(a-spec ...))])
           (hash-set! parsed-forms 'augments
                      (append names (hash-ref parsed-forms 'augments null)))
           (hash-set! parsed-forms 'augment-contracts
                      (append (add-bindings/return-vars ctcs)
                              (hash-ref parsed-forms 'augment-contracts null)))))]
      [(augride a-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "augride contract not allowed in object/c" stx))
         (let-values ([(names ctcs) (parse-names-ctcs #'(a-spec ...))])
           (hash-set! parsed-forms 'augrides
                      (append names (hash-ref parsed-forms 'augrides null)))
           (hash-set! parsed-forms 'augride-contracts
                      (append (add-bindings/return-vars ctcs)
                              (hash-ref parsed-forms 'augride-contracts null)))))]
      [(absent a-spec ...)
       (begin
         (when object/c?
           (raise-syntax-error 'object/c "absent specification not allowed in object/c" stx))
         (let-values ([(meths fields) (parse-absents #'(a-spec ...))])
           (hash-set! parsed-forms 'absents
                      (append meths (hash-ref parsed-forms 'absents null)))
           (hash-set! parsed-forms 'absent-fields
                      (append fields (hash-ref parsed-forms 'absent-fields null)))))]
      [m-spec
       (let-values ([(name ctc1) (parse-name-ctc #'m-spec)])
         (hash-set! parsed-forms 'methods
                    (cons name (hash-ref parsed-forms 'methods null)))
         (hash-set! parsed-forms 'method-contracts
                    (append (add-bindings/return-vars (list ctc1))
                            (hash-ref parsed-forms 'method-contracts null))))]
      [else
       (raise-syntax-error form-name "expected class/c subform" stx)]))
  
  (define (add-bindings/return-vars ctcs)
    (define ctc-xs (generate-temporaries ctcs))
    (with-syntax ([(ctc-x ...) ctc-xs]
                  [(ctc ...) ctcs])
      (set! bindings (append (syntax->list #'([ctc-x ctc] ...))
                             bindings)))
    ctc-xs)
  
  (for ([form (in-list forms)])
    (parse-spec form))
  (values (reverse bindings) parsed-forms))

;; check keyword and pass off to -class/c
(define-syntax (class/c stx)

  (define-splicing-syntax-class opaque-keyword
    (pattern (~seq #:opaque) #:with opaque? #'#t)
    (pattern (~seq) #:with opaque? #'#f))

  (syntax-parse stx
    [(_ kwd:opaque-keyword form ...)
     (syntax/loc stx (-class/c kwd.opaque? form ...))]))

(define-syntax (-class/c stx)
  (syntax-case stx ()
    [(_ opaque? form ...)
     (let ()
       (define-values (bindings pfs)
         (parse-class/c-specs (syntax->list #'(form ...)) #f))
       (with-syntax ([methods #`(list #,@(reverse (hash-ref pfs 'methods null)))]
                     [method-ctcs #`(list #,@(reverse (hash-ref pfs 'method-contracts null)))]
                     [fields #`(list #,@(reverse (hash-ref pfs 'fields null)))]
                     [field-ctcs #`(list #,@(reverse (hash-ref pfs 'field-contracts null)))]
                     [(i ...) (reverse (hash-ref pfs 'inits null))]
                     [(i-c ...) (reverse (hash-ref pfs 'init-contracts null))]
                     [inherits #`(list #,@(reverse (hash-ref pfs 'inherits null)))]
                     [inherit-ctcs #`(list #,@(reverse (hash-ref pfs 'inherit-contracts null)))]
                     [inherit-fields #`(list #,@(reverse (hash-ref pfs 'inherit-fields null)))]
                     [inherit-field-ctcs #`(list #,@(reverse (hash-ref pfs 'inherit-field-contracts
                                                                       null)))]
                     [supers #`(list #,@(reverse (hash-ref pfs 'supers null)))]
                     [super-ctcs #`(list #,@(reverse (hash-ref pfs 'super-contracts null)))]
                     [inners #`(list #,@(reverse (hash-ref pfs 'inners null)))]
                     [inner-ctcs #`(list #,@(reverse (hash-ref pfs 'inner-contracts null)))]
                     [overrides #`(list #,@(reverse (hash-ref pfs 'overrides null)))]
                     [override-ctcs #`(list #,@(reverse (hash-ref pfs 'override-contracts null)))]
                     [augments #`(list #,@(reverse (hash-ref pfs 'augments null)))]
                     [augment-ctcs #`(list #,@(reverse (hash-ref pfs 'augment-contracts null)))]
                     [augrides #`(list #,@(reverse (hash-ref pfs 'augrides null)))]
                     [augride-ctcs #`(list #,@(reverse (hash-ref pfs 'augride-contracts null)))]
                     [absents #`(list #,@(reverse (hash-ref pfs 'absents null)))]
                     [absent-fields #`(list #,@(reverse (hash-ref pfs 'absent-fields null)))])
         (with-syntax ([name 
                        ;; same as syntax-local-infer-name, except doesn't
                        ;; make a name up from the src loc; in that case,
                        ;; we just use the big ole (class/c  ...)-based name
                        (or (let loop ([prop (syntax-property stx 'inferred-name)])
                              (cond
                                [(symbol? prop) prop]
                                [(pair? prop) (or (loop (car prop))
                                                  (loop (cdr prop)))]
                                [else #f]))
                            (syntax-local-name))]
                       [bindings bindings])
           (syntax/loc stx
             (let bindings
               (let-values ([(inits init-ctcs) (sort-inits+contracts (list (cons i i-c) ...))])
                 (build-class/c methods method-ctcs
                                fields field-ctcs
                                inits init-ctcs
                                absents absent-fields
                                (build-internal-class/c
                                 inherits inherit-ctcs
                                 inherit-fields inherit-field-ctcs
                                 supers super-ctcs
                                 inners inner-ctcs
                                 overrides override-ctcs
                                 augments augment-ctcs
                                 augrides augride-ctcs)
                                opaque?
                                'name)))))))]))

(define (sort-inits+contracts lst)
  (define sorted
    (sort lst
          string<?
          #:key (compose symbol->string car)))
  (values (map car sorted) (map cdr sorted)))

(define (check-object-contract obj methods fields fail)
  (unless (object? obj)
    (fail '(expected: "an object" given: "~e") obj))
  (let ([cls (object-ref/unwrap obj)])
    (let ([method-ht (class-method-ht cls)])
      (for ([m methods])
        (unless (hash-ref method-ht m #f)
          (fail "no public method ~a" m))))
    (let ([field-ht (class-field-ht cls)])
      (for ([m fields])
        (unless (hash-ref field-ht m #f)
          (fail "no public field ~a" m)))))
  #t)

(define (object/c-proj ctc)
  (λ (blame)
    (λ (obj)
      (make-wrapper-object ctc obj blame 
                           (base-object/c-methods ctc) (base-object/c-method-contracts ctc)
                           (base-object/c-fields ctc) (base-object/c-field-contracts ctc)))))

(define (object/c-first-order ctc)
  (λ (obj)
    (let/ec ret
      (check-object-contract obj 
                             (base-object/c-methods ctc) 
                             (base-object/c-fields ctc)
                             (λ args (ret #f))))))

(define-struct base-object/c (methods method-contracts fields field-contracts)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property 
   #:projection object/c-proj
   #:name
   (λ (ctc)
     (let* ([pair-ids-ctcs
             (λ (is ctcs)
               (map (λ (i ctc)
                      (build-compound-type-name i ctc))
                    is ctcs))]
            [handle-optional
             (λ (name is ctcs)
               (if (null? is)
                   null
                   (list (cons name (pair-ids-ctcs is ctcs)))))])
       (apply build-compound-type-name
              'object/c 
              (append
               (pair-ids-ctcs (base-object/c-methods ctc) (base-object/c-method-contracts ctc))
               (handle-optional 'field 
                                (base-object/c-fields ctc)
                                (base-object/c-field-contracts ctc))))))
   #:first-order object/c-first-order))

(define-syntax (object/c stx)
  (syntax-case stx ()
    [(_ form ...)
     (let ()
       (define-values (bindings pfs)
         (parse-class/c-specs (syntax->list #'(form ...)) #t))
       (with-syntax ([methods #`(list #,@(reverse (hash-ref pfs 'methods null)))]
                     [method-ctcs #`(list #,@(reverse (hash-ref pfs 'method-contracts null)))]
                     [fields #`(list #,@(reverse (hash-ref pfs 'fields null)))]
                     [field-ctcs #`(list #,@(reverse (hash-ref pfs 'field-contracts null)))]
                     [bindings bindings])
         (syntax/loc stx
           (let bindings
             (make-base-object/c methods method-ctcs fields field-ctcs)))))]))

(define (instanceof/c-proj ctc)
  (define proj (contract-projection (base-instanceof/c-class-ctc ctc)))
  (λ (blame)
    (define p (proj blame))
    (λ (val)
      (unless (object? val)
        (raise-blame-error blame val '(expected: "an object" given: "~e") val))
      (define original-obj (if (has-original-object? val) (original-object val) val))
      (define new-cls (p (object-ref val)))
      (cond
        [(impersonator-prop:has-wrapped-class-neg-party? new-cls)
         (define the-info (impersonator-prop:get-wrapped-class-info new-cls))
         (define neg-party (impersonator-prop:get-wrapped-class-neg-party new-cls))
         (wrapped-object
          val
          (wrapped-class-info-neg-extra-arg-vec the-info)
          (wrapped-class-info-pos-field-projs the-info)
          (wrapped-class-info-neg-field-projs the-info)
          neg-party)]
        [else
         (impersonate-struct val object-ref (λ (o c) new-cls)
                             impersonator-prop:contracted ctc
                             impersonator-prop:original-object original-obj)]))))

(define (instanceof/c-first-order ctc)
  (let ([cls-ctc (base-instanceof/c-class-ctc ctc)])
    (λ (val)
      (and (object? val)
           (contract-first-order-passes? cls-ctc (object-ref val))))))

 
(define (instanceof/c-stronger this that)
  (and (base-instanceof/c? that)
       (contract-stronger? (base-instanceof/c-class-ctc this)
                           (base-instanceof/c-class-ctc that))))

(define-struct base-instanceof/c (class-ctc)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property 
   #:projection instanceof/c-proj
   #:name
   (λ (ctc)
     (build-compound-type-name 'instanceof/c (base-instanceof/c-class-ctc ctc)))
   #:first-order instanceof/c-first-order
   #:stronger instanceof/c-stronger))

(define (instanceof/c cctc)
  (let ([ctc (coerce-contract 'instanceof/c cctc)])
    (make-base-instanceof/c ctc)))

;; make-wrapper-object: contract object blame 
;;                      (listof symbol) (listof contract?) (listof symbol) (listof contract?)
;;                   -> wrapped object
(define (make-wrapper-object ctc obj blame methods method-contracts fields field-contracts)
  (check-object-contract obj methods fields (λ args (apply raise-blame-error blame obj args)))
  (let ([original-obj (if (has-original-object? obj) (original-object obj) obj)]
        [new-cls (make-wrapper-class (object-ref obj)  ;; TODO: object-ref audit
                                     blame
                                     methods method-contracts fields field-contracts)])
    (impersonate-struct obj object-ref (λ (o c) new-cls) ;; TODO: object-ref audit
                        impersonator-prop:contracted ctc
                        impersonator-prop:original-object original-obj)))


(define (make-wrapper-class cls blame methods method-contracts fields field-contracts)
  (let* ([name (class-name cls)]
         [method-width (class-method-width cls)]
         [method-ht (class-method-ht cls)]
         [meths (if (null? methods)
                    (class-methods cls)
                    (make-vector method-width))]
         [field-pub-width (class-field-pub-width cls)]
         [field-ht (if (null? fields)
                       (class-field-ht cls)
                       (hash-copy (class-field-ht cls)))]
         [class-make (if name
                         (make-naming-constructor struct:class name "class")
                         make-class)]
         [c (class-make name
                        (class-pos cls)
                        (list->vector (vector->list (class-supers cls)))
                        (class-self-interface cls)
                        void ;; No inspecting
                        
                        method-width
                        method-ht
                        (class-method-ids cls)
                        (class-abstract-ids cls)
                        (class-method-ictcs cls)

                        (class-ictc-classes cls)
                        
                        meths
                        (class-super-methods cls)
                        (class-int-methods cls)
                        (class-beta-methods cls)
                        (class-meth-flags cls)
                        
                        (class-inner-projs cls)
                        (class-dynamic-idxs cls)
                        (class-dynamic-projs cls)
                        
                        (class-field-width cls)
                        field-pub-width
                        field-ht
                        (class-field-ids cls)
                        (class-all-field-ids cls)
                        
                        'struct:object 'object? 'make-object
                        'field-ref 'field-set!
                        
                        (class-init-args cls)
                        (class-init-mode cls)
                        (class-init cls)
                        
                        (class-orig-cls cls)
                        #f #f ; serializer is never set

                        (class-check-undef? cls)
                        #f)]
         [obj-name (if name
                       (string->symbol (format "wrapper-object:~a" name))
                       'object)])
    (define (make-method proc meth-name)
      (procedure-rename
       (procedure->method proc)
       (string->symbol
        (format "~a method~a~a"
                meth-name
                (if name " in " "")
                (or name "")))))

    (vector-set! (class-supers c) (class-pos c) c)
    
    ;; --- Make the new object struct ---
    (let-values ([(struct:object object-make object? object-field-ref object-field-set!)
                  (make-struct-type obj-name
                                    (class-struct:object cls)
                                    0 ;; No init fields
                                    0 ;; No new fields in this class replacement
                                    undefined
                                    ;; Map object property to class:
                                    (list (cons prop:object c)))])
      (set-class-struct:object! c struct:object)
      (set-class-object?! c object?)
      (set-class-make-object! c object-make)
      (set-class-field-ref! c object-field-ref)
      (set-class-field-set!! c object-field-set!))
    
    ;; Handle public method contracts
    (unless (null? methods)
      ;; First, fill in from old methods
      (vector-copy! meths 0 (class-methods cls))
      ;; Now apply projections
      (for ([m (in-list methods)]
            [c (in-list method-contracts)])
        (when c
          (let ([i (hash-ref method-ht m)]
                [p ((contract-projection c) (blame-add-context blame (format "the ~a method in" m)
                                                               #:important m))])
            (vector-set! meths i (make-method (p (vector-ref meths i)) m))))))
    
    ;; Handle external field contracts
    (unless (null? fields)
      (for ([f (in-list fields)]
            [c (in-list field-contracts)])
        (unless (just-check-existence? c)
          (define fi (hash-ref field-ht f))
          (define p-pos ((contract-projection c) (blame-add-field-context blame f #:swap? #f)))
          (define p-neg ((contract-projection c) (blame-add-field-context blame f #:swap? #t)))
          (hash-set! field-ht f (field-info-extend-external fi p-pos p-neg)))))
    
    c))
