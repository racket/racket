#lang racket/base
(require racket/symbol
         "match.rkt"
         "wrap.rkt"
         "struct-type-info.rkt"
         "mutated-state.rkt"
         "find-definition.rkt"
         "gensym.rkt"
         "known.rkt"
         "aim.rkt")

(provide struct-convert
         struct-convert-local)

(define (struct-convert form prim-knowns knowns imports exports mutated
                        schemify target no-prompt? top?)
  (match form
    [`(define-values (,struct:s ,make-s ,s? ,acc/muts ...)
        (let-values (((,struct: ,make ,?1 ,-ref ,-set!) ,mk))
          (values ,struct:2
                  ,make2
                  ,?2
                  ,make-acc/muts ...)))
     ;; Convert a `make-struct-type` binding into a 
     ;; set of bindings that Chez's cp0 recognizes,
     ;; and push the struct-specific extra work into
     ;; `struct-type-install-properties!`
     (define sti (and (wrap-eq? struct: struct:2)
                      (wrap-eq? make make2)
                      (wrap-eq? ?1 ?2)
                      (for/and ([acc/mut (in-list acc/muts)]
                                [make-acc/mut (in-list make-acc/muts)])
                        (define (ok-contract? contract)
                          (match contract
                            [`',sym (symbol? sym)]
                            [`,_ (or (not contract) (string? contract))]))
                        (match make-acc/mut
                          [`(make-struct-field-accessor ,ref-id ,pos ',field-name)
                           (and (wrap-eq? ref-id -ref)
                                (symbol? field-name)
                                (exact-nonnegative-integer? pos))]
                          [`(make-struct-field-accessor ,ref-id ,pos ',field/proc-name ,contract)
                           (and (wrap-eq? ref-id -ref)
                                (symbol? field/proc-name)
                                (exact-nonnegative-integer? pos)
                                (ok-contract? contract))]
                          [`(make-struct-field-accessor ,ref-id ,pos ',field/proc-name ,contract ',realm)
                           (and (wrap-eq? ref-id -ref)
                                (symbol? field/proc-name)
                                (exact-nonnegative-integer? pos)
                                (ok-contract? contract)
                                (symbol? realm))]
                          [`(make-struct-field-mutator ,set-id ,pos ',field-name)
                           (and (wrap-eq? set-id -set!)
                                (symbol? field-name)
                                (exact-nonnegative-integer? pos))]
                          [`(make-struct-field-mutator ,set-id ,pos ',field/proc-name ,contract)
                           (and (wrap-eq? set-id -set!)
                                (symbol? field/proc-name)
                                (exact-nonnegative-integer? pos)
                                (ok-contract? contract))]
                          [`(make-struct-field-mutator ,set-id ,pos ',field/proc-name ,contract ',realm)
                           (and (wrap-eq? set-id -set!)
                                (symbol? field/proc-name)
                                (exact-nonnegative-integer? pos)
                                (ok-contract? contract)
                                (symbol? realm))]
                          [`,_ #f]))
                      (make-struct-type-info mk prim-knowns knowns imports mutated)))
     (cond
       [(and sti
             ;; make sure all accessor/mutator positions are in range:
             (for/and ([make-acc/mut (in-list make-acc/muts)])
               (match make-acc/mut
                 [`(,_ ,_ ,pos . ,_) (pos . < . (struct-type-info-immediate-field-count sti))]))
             ;; make sure `struct:` isn't used too early, since we're
             ;; reordering it's definition with respect to some arguments
             ;; of `make-struct-type`:
             (simple-mutated-state? (hash-ref mutated (unwrap struct:) #f))
             ;; If any properties, need the first LHS to be non-set!ed, because that will
             ;; let us reject multi-return from continuation capture in property expressions
             (or no-prompt?
                 (null? (struct-type-info-rest sti))
                 (not (set!ed-mutated-state? (hash-ref mutated (unwrap struct:s) #f)))))
        (define can-impersonate? (not (struct-type-info-authentic? sti)))
        (define generate-check? (or can-impersonate?
                                    (not (aim? target 'system))))
        (define raw-s? (if generate-check? (deterministic-gensym (unwrap s?)) s?))
        (define system-opaque? (and (aim? target 'system)
                                    (or (not exports)
                                        (eq? 'no (hash-ref exports (unwrap struct:s) 'no)))))
        (define finish!-id (and (or (pair? (struct-type-info-rest sti))
                                    (and (struct-type-info-prefab-immutables sti)
                                         ;; to ensure that the super is also a prefab:
                                         (unwrap (struct-type-info-parent sti))))
                                (deterministic-gensym "finish")))
        `(begin
           ,@(if finish!-id
                 `((define ,finish!-id
                     (make-struct-type-install-properties ',(if system-opaque?
                                                                ;; list is recognized by `struct-type-install-properties!`
                                                                ;; to indicate a system structure type:
                                                                (list (struct-type-info-name sti))
                                                                (struct-type-info-name sti))
                                                          ,(struct-type-info-immediate-field-count sti)
                                                          0
                                                          ,(schemify (struct-type-info-parent sti) knowns)
                                                          ,@(schemify-body schemify knowns (struct-type-info-rest sti)))))
                 null)
           (define ,struct:s (make-record-type-descriptor ',(struct-type-info-name sti)
                                                          ,(schemify (struct-type-info-parent sti) knowns)
                                                          ,(if (not (struct-type-info-prefab-immutables sti))
                                                               (if (and top?
                                                                        (aim? target 'system))
                                                                   `(#%nongenerative-uid ,(struct-type-info-name sti))
                                                                   #f)
                                                               `(structure-type-lookup-prefab-uid
                                                                 ',(struct-type-info-name sti)
                                                                 ,(schemify (struct-type-info-parent sti) knowns)
                                                                 ,(struct-type-info-immediate-field-count sti)
                                                                 0 #f
                                                                 ',(struct-type-info-prefab-immutables sti)))
                                                          ,(struct-type-info-sealed? sti)
                                                          #f
                                                          '(,(struct-type-info-immediate-field-count sti)
                                                            .
                                                            ,(let* ([n (struct-type-info-immediate-field-count sti)]
                                                                    [mask (sub1 (arithmetic-shift 1 n))])
                                                               (cond
                                                                 [(struct-type-info-non-prefab-immutables sti)
                                                                  =>
                                                                  (lambda (immutables)
                                                                    (let loop ([imms immutables] [mask mask])
                                                                      (cond
                                                                        [(null? imms) mask]
                                                                        [else
                                                                         (let ([m (bitwise-not (arithmetic-shift 1 (car imms)))])
                                                                           (loop (cdr imms) (bitwise-and mask m)))])))]
                                                                 [else
                                                                  mask])))))
           ,@(if finish!-id
                 `((define ,(deterministic-gensym "effect") (,finish!-id ,struct:s)))
                 null)
           (define ,make-s ,(let ([ctr `(record-constructor
                                         (make-record-constructor-descriptor ,struct:s #f #f))])
                              (define ctr-expr
                                (if (struct-type-info-pure-constructor? sti)
                                    ctr
                                    `(struct-type-constructor-add-guards ,ctr ,struct:s ',(struct-type-info-name sti))))
                              (define name-expr (struct-type-info-constructor-name-expr sti))
                              (define c
                                (match name-expr
                                  [`#f
                                   (wrap-property-set ctr-expr 'inferred-name (struct-type-info-name sti))]
                                  [`',sym
                                   (if (symbol? sym)
                                       (wrap-property-set ctr-expr 'inferred-name sym)
                                       `(procedure-rename ,ctr-expr ,name-expr))]
                                  [`,_
                                   `(procedure-rename ,ctr-expr ,name-expr)]))
                              (if system-opaque?
                                  c
                                  `(#%struct-constructor ,c ,(arithmetic-shift 1 (struct-type-info-field-count sti))))))
           (define ,raw-s? ,(let ([p (name-procedure
                                      (build-name "" (struct-type-info-name sti) "" '|| "?")
                                      `(record-predicate ,struct:s))])
                              (if (or generate-check?
                                      system-opaque?)
                                  p
                                  `(#%struct-predicate ,p))))
           ,@(if generate-check?
                 `((define ,s? ,(let ([p (name-procedure
                                          (build-name "" (struct-type-info-name sti) "" '|| "?")
                                          `(lambda (v)
                                             ,(if can-impersonate?
                                                  `(if (,raw-s? v) #t ($value (if (impersonator? v) (,raw-s? (impersonator-val v)) #f)))
                                                  `(,raw-s? v))))])
                                  (if system-opaque?
                                      p
                                      `(#%struct-predicate ,p)))))
                 null)
           ,@(for/list ([acc/mut (in-list acc/muts)]
                        [make-acc/mut (in-list make-acc/muts)])
               (define raw-acc/mut (if generate-check? (deterministic-gensym (unwrap acc/mut)) acc/mut))
               (define (make-err-args field/proc-name proc-name contract realm need-type-name?)
                 (cond
                   [(and (not contract) (eq? realm 'racket))
                    (if need-type-name?
                        `(',(struct-type-info-name sti) ',field/proc-name)
                        `(',field/proc-name))]
                   [else
                    (let ([contract (or contract
                                        `',(string->symbol
                                            (string-append-immutable
                                             (symbol->immutable-string (struct-type-info-name sti))
                                             "?")))])
                      `(',proc-name ,contract ',realm))]))
               (define (build-accessor pos field/proc-name contract realm)
                 (define proc-name (if contract
                                       field/proc-name
                                       (build-name "" (struct-type-info-name sti) "-" field/proc-name "")))
                 (define raw-def `(define ,raw-acc/mut
                                    ,(let ([p (name-procedure
                                               proc-name
                                               `(record-accessor ,struct:s ,pos))])
                                       (if (or generate-check?
                                               system-opaque?)
                                           p
                                           `(#%struct-field-accessor ,p ,struct:s ,pos)))))
                 (define (err-args need-type-name?) (make-err-args field/proc-name proc-name contract realm need-type-name?))
                 (if generate-check?
                      `(begin
                         ,raw-def
                         (define ,acc/mut
                           ,(let ([p (name-procedure
                                      proc-name
                                      `(lambda (s) (if (,raw-s? s)
                                                       (,raw-acc/mut s)
                                                       ,(if can-impersonate?
                                                            `($value (impersonate-ref ,raw-acc/mut ,struct:s ,pos s ,@(err-args #f)))
                                                            `(#%struct-ref-error s ,@(err-args #t))))))])
                              (if system-opaque?
                                  p
                                  `(#%struct-field-accessor ,p ,struct:s ,pos)))))
                      raw-def))
               (define (build-mutator pos field/proc-name contract realm)
                 (define proc-name (if contract
                                       field/proc-name
                                       (build-name "set-" (struct-type-info-name sti) "-" field/proc-name "!")))
                 (define raw-def `(define ,raw-acc/mut
                                      ,(let ([p (name-procedure
                                                 proc-name
                                                 `(record-mutator ,struct:s ,pos))])
                                         (if (or generate-check?
                                                 system-opaque?)
                                             p
                                             `(#%struct-field-mutator ,p ,struct:s ,pos)))))
                 (define abs-pos (+ pos (- (struct-type-info-field-count sti)
                                           (struct-type-info-immediate-field-count sti))))
                 (define (err-args need-type-name?) (make-err-args field/proc-name proc-name contract realm need-type-name?))
                 (if generate-check?
                     `(begin
                        ,raw-def
                        (define ,acc/mut
                          ,(let ([p (name-procedure
                                     proc-name
                                     `(lambda (s v) (if (,raw-s? s)
                                                        (,raw-acc/mut s v)
                                                        ,(if can-impersonate?
                                                             `($value (impersonate-set! ,raw-acc/mut ,struct:s ,pos ,abs-pos s v ,@(err-args #f)))
                                                             `(#%struct-set!-error s ,@(err-args #t))))))])
                             (if system-opaque?
                                 p
                                 `(#%struct-field-mutator ,p ,struct:s ,pos)))))
                     raw-def))
               (match make-acc/mut
                 [`(make-struct-field-accessor ,_ ,pos ',field-name)
                  (build-accessor pos field-name #f 'racket)]
                 [`(make-struct-field-accessor ,_ ,pos ',field/proc-name ,contract)
                  (build-accessor pos field/proc-name contract 'racket)]
                 [`(make-struct-field-accessor ,_ ,pos ',field/proc-name ,contract ',realm)
                  (build-accessor pos field/proc-name contract realm)]
                 [`(make-struct-field-mutator ,_ ,pos ',field-name)
                  (build-mutator pos field-name #f 'racket)]
                 [`(make-struct-field-mutator ,_ ,pos ',field-name ,contract)
                  (build-mutator pos field-name contract 'racket)]
                 [`(make-struct-field-mutator ,_ ,pos ',field-name ,contract ',realm)
                  (build-mutator pos field-name contract realm)]
                 [`,_ (error "oops")])))]
       [else #f])]
    [`,_ #f]))

(define (struct-convert-local form #:letrec? [letrec? #f]
                              prim-knowns knowns imports mutated simples
                              schemify
                              #:unsafe-mode? unsafe-mode?
                              #:target target)
  (match form
    [`(,_ ([,ids ,rhs]) ,bodys ...)
     (define defn `(define-values ,ids ,rhs))
     (define new-seq
       (struct-convert defn
                       prim-knowns knowns imports #f mutated
                       schemify target #t #f))
     (and new-seq
          (match new-seq
            [`(begin . ,new-seq)
             (define-values (new-knowns info)
               (find-definitions defn prim-knowns knowns imports mutated simples unsafe-mode? target
                                 #:optimize? #f))
             (cond
               [letrec?
                `(letrec* ,(let loop ([new-seq new-seq])
                             (match new-seq
                               [`() null]
                               [`((begin ,forms ...) . ,rest)
                                (loop (append forms rest))]
                               [`((define ,id ,rhs) . ,rest)
                                (cons `[,id ,rhs] (loop rest))]))
                   ,@(schemify-body schemify new-knowns bodys))]
               [else
                (let loop ([new-seq new-seq])
                  (match new-seq
                    [`()
                     (define exprs (schemify-body schemify new-knowns bodys))
                     (if (and (pair? exprs) (null? (cdr exprs)))
                         (car exprs)
                         `(begin ,@exprs))]
                    [`((begin ,forms ...) . ,rest)
                     (loop (append forms rest))]
                    [`((define ,id ,rhs) . ,rest)
                     `(let ([,id ,rhs])
                        ,(loop rest))]))])]))]
    [`,_ #f]))

(define (schemify-body schemify knowns l)
  (for/list ([e (in-list l)])
    (schemify e knowns)))

(define (name-procedure proc-name proc-expr)
  (wrap-property-set proc-expr
                     'inferred-name
                     proc-name))

(define (build-name pre st sep fld post)
  (string->symbol
   (string-append-immutable pre
                            (symbol->immutable-string st)
                            sep
                            (symbol->immutable-string fld)
                            post)))
