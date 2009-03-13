#lang scheme/base
(require (for-template scheme/base
                       syntax/stx
                       scheme/stxparam
                       "runtime.ss")
         scheme/match
         scheme/contract
         scheme/private/sc
         syntax/stx
         syntax/boundmap
         "rep-data.ss"
         "rep.ss"
         "codegen-data.ss"
         "../util.ss")
(provide/contract
 [parse:rhs (rhs? (listof sattr?) (listof identifier?) . -> . syntax?)]
 [parse:clauses (syntax? identifier? identifier? . -> . syntax?)]
 [announce-failures? parameter?])

(define announce-failures? (make-parameter #f))

;; parse:rhs : RHS (listof SAttr) (listof identifier) -> stx
;; Takes a list of the relevant attrs; order is significant!
;; Returns either fail or a list having length same as 'relsattrs'
(define (parse:rhs rhs relsattrs args)
  (with-syntax ([(arg ...) args])
    #`(lambda (x arg ...)
        (define (fail-rhs x expected frontier frontier-stx)
          #,(if (rhs-transparent? rhs)
                #`(make-failed x expected frontier frontier-stx)
                #'#f))
        (syntax-parameterize ((this-syntax (make-rename-transformer #'x)))
          #,(let ([pks (rhs->pks rhs relsattrs #'x)])
              (unless (pair? pks)
                (wrong-syntax (rhs-ostx rhs)
                              "syntax class has no variants"))
              (parse:pks (list #'x)
                         (list (empty-frontier #'x))
                         #'fail-rhs
                         (list #f)
                         pks))))))

;; parse:clauses : stx identifier identifier -> stx
(define (parse:clauses stx var phi)
  (define clauses-kw-table
    (list (list '#:literals check-literals-list)))
  (define-values (chunks clauses-stx)
    (chunk-kw-seq/no-dups stx clauses-kw-table))
  (define literals
    (cond [(assq '#:literals chunks) => caddr]
          [else null]))
  (define (clause->pk clause)
    (syntax-case clause ()
      [(p . rest)
       (let-values ([(rest decls _ sides)
                     (parse-pattern-directives #'rest
                                               #:sc? #f
                                               #:literals literals)])
         (let* ([pattern (parse-whole-pattern #'p decls)])
           (syntax-case rest ()
             [(b0 b ...)
              (let ([body #'(let () b0 b ...)])
                (make-pk (list pattern)
                         (wrap-pvars (pattern-attrs pattern)
                                     (convert-sides sides var body))))]
             [_
              (wrong-syntax clause "expected body")])))]))
  (unless (stx-list? clauses-stx)
    (wrong-syntax clauses-stx "expected sequence of clauses"))
  (let ([pks (map clause->pk (stx->list clauses-stx))])
    (unless (pair? pks)
      (wrong-syntax stx "no variants"))
    (parse:pks (list var)
               (list (empty-frontier var))
               phi
               (list #f)
               pks)))

;; rhs->pks : RHS (listof SAttr) identifier -> (listof PK)
(define (rhs->pks rhs relsattrs main-var)
  (match rhs
    [(struct rhs:union (_ attrs transparent? description patterns))
     (for*/list ([rhs patterns] [pk (rhs-pattern->pks rhs relsattrs main-var)]) 
       pk)]))

;; rhs-pattern->pks : RHS (listof SAttr) identifier -> (listof PK)
(define (rhs-pattern->pks rhs relsattrs main-var)
  (match rhs
    [(struct rhs:pattern (ostx attrs pattern decls remap sides))
     (parameterize ((current-syntax-context ostx))
       (define iattrs
         (append-attrs
          (cons (pattern-attrs pattern)
                (for/list ([side sides] #:when (clause:with? side))
                  (pattern-attrs (clause:with-pattern side))))))
       (define base-expr
         (success-expr iattrs relsattrs remap main-var))
       (define expr
         (wrap-pvars (pattern-attrs pattern)
                     (convert-sides sides main-var base-expr)))
       (list (make-pk (list pattern) expr)))]))

;; convert-sides : (listof SideClause) id stx -> stx
(define (convert-sides sides main-var body-expr)
  (match sides
    ['() body-expr]
    [(cons (struct clause:when (e)) rest)
     #`(if #,e
           #,(convert-sides rest main-var body-expr)
           #,(fail #'enclosing-fail main-var
                   #:pattern (expectation-of/message "side condition failed")
                   #:fce (done-frontier main-var)))]
    [(cons (struct clause:with (p e)) rest)
     (let ([inner
            (wrap-pvars (pattern-attrs p)
                        (convert-sides rest main-var body-expr))])
       (with-syntax ([(x fail-k) (generate-temporaries #'(x fail-k))])
         #`(let ([x #,e]
                 [fail-k enclosing-fail])
             #,(parse:pks (list #'x)
                          (list (done-frontier #'x))
                          #'fail-k
                          (list #f)
                          (list (make-pk (list p) inner))))))]))

;; success-expr : (listof IAttr) (listof SAttr) RemapEnv stx -> stx
(define (success-expr iattrs relsattrs remap main-var)
  (let* ([reliattrs (reorder-iattrs relsattrs iattrs remap)]
         [flat-reliattrs (flatten-attrs* reliattrs)]
         [relids (map attr-name flat-reliattrs)])
    (with-syntax ([main main-var]
                  [(relid ...) relids])
      #'(list main (attribute relid) ...))))

;; fail : id id #:pattern datum #:reason datum #:fce FCE #:fstx id -> stx
(define (fail k x #:pattern p #:fce fce)
  (with-syntax ([k k]
                [x x]
                [p p]
                [fc-expr (frontier->dfc-expr fce)]
                [fstx-expr (frontier->fstx-expr fce)])
    #`(let ([failcontext fc-expr]
            [failcontext-syntax fstx-expr])
        #,(when (announce-failures?)
            #`(printf "failing on ~s\n  reason: ~s\n" x p))
        (k x p failcontext failcontext-syntax))))

;; Parsing

#|

The parsing algorithm is based on the classic backtracking
algorithm (see Optimizing Pattern Matching for an overview). A PK
corresponds to a row in the pattern matrix. The failure argument
corresponds to the static catch continuation.

The FCs (frontier contexts, one per column) are an addition for error
reporting. They track the matcher's progress into the term. The
matcher compares failures on backtracking, and reports the "furthest
along" failure, based on the frontiers.

Conventions:
  <ParseConfig> =
    vars : listof identifiers, variables, one per column
    fcs : listof FCEs, failure contexts, one per column
    phi : id, failure continuation
    ds : listof (string/#f), description string

|#


;; parse:pks : <ParseConfig> (listof PK) -> stx
;; Each PK has a list of |vars| patterns.
;; The list of PKs must not be empty.
(define (parse:pks vars fcs phi ds pks)
  (cond [(null? pks)
         (error 'parse:pks "internal error: empty list of rows")]
        [(null? vars)
         ;; Success!
         (let* ([failvar (generate-temporary 'fail-k)]
                [exprs
                 (for/list ([pk pks])
                   #`(with-enclosing-fail #,failvar #,(pk-k pk)))])
           (with-syntax ([failvar failvar]
                         [(expr ...) exprs])
             #`(try failvar [expr ...] #,phi)))]
        [else
         (let-values ([(vars groups) (split-pks vars pks)])
           (let* ([failvar (generate-temporary 'fail-k)]
                  [exprs
                   (for/list ([group groups])
                     (parse:group vars fcs failvar ds group))])
             (with-syntax ([failvar failvar]
                           [(expr ...) exprs])
               #`(try failvar [expr ...] #,phi))))]))


;; parse:group : <ParseConfig> Group -> stx
;; Pre: vars is not empty
(define (parse:group vars fcs phi ds group)
  (match group
    [(struct idG (stxclass args pks))
     (if stxclass
         (parse:group:id/stxclass vars fcs phi ds stxclass args pks)
         (parse:group:id/any vars fcs phi ds args pks))]
    [(struct descrimG (datumSGs literalSGs kindSGs))
     (parse:group:descrim vars fcs phi ds datumSGs literalSGs kindSGs)]
    [(struct pk ((cons (? pat:and? and-pattern) rest-patterns) k))
     (parse:group:and vars fcs phi ds and-pattern rest-patterns k)]
    [(struct pk ((cons (? pat:gseq? gseq-pattern) rest-patterns) k))
     (parse:group:gseq vars fcs phi ds gseq-pattern rest-patterns k)]))

;; parse:group:id/stxclass : <ParseConfig> SC stx (listof pk)
;;                        -> stx
(define (parse:group:id/stxclass vars fcs phi ds stxclass args pks)
  (with-syntax ([var0 (car vars)]
                [(arg ...) args]
                [(arg-var ...) (generate-temporaries args)]
                [parser (sc-parser-name stxclass)]
                [result (generate-temporary 'result)])
    #`(let ([arg-var arg] ...)
        (let ([result (parser var0 arg-var ...)])
          (if (ok? result)
              #,(parse:pks (cdr vars) (cdr fcs) phi (cdr ds) (shift-pks:id pks #'result))
              #,(fail phi (car vars)
                      #:pattern (expectation-of-stxclass stxclass #'(arg-var ...) #'result)
                      #:fce (join-frontiers (car fcs) #'result)))))))

;; parse:group:id/any : <ParseConfig> stx (listof pk) -> stx
(define (parse:group:id/any vars fcs phi ds args pks)
  (with-syntax ([var0 (car vars)]
                [(arg ...) args]
                [(arg-var ...) (generate-temporaries args)]
                [result (generate-temporary 'result)])
    #`(let ([arg-var arg] ...)
        (let ([result (list var0)])
          #,(parse:pks (cdr vars) (cdr fcs) phi (cdr ds) (shift-pks:id pks #'result))))))

;; parse:group:descrim : <ParseConfig>
;;                       (listof DatumSG) (listof LiteralSG) (listof CompoundSG)
;;                    -> stx
(define (parse:group:descrim vars fcs phi ds datumSGs literalSGs compoundSGs)
  (define var (car vars))
  (define datum-var (generate-temporary 'datum))
  (define (datumSG-test datumSG)
    (let ([datum (datumSG-datum datumSG)])
      #`(equal? #,datum-var (quote #,datum))))
  (define (datumSG-rhs datumSG)
    (let ([pks (datumSG-pks datumSG)])
      (parse:pks (cdr vars) (cdr fcs) phi (cdr ds) (shift-pks:datum pks))))
  (define (literalSG-test literalSG)
    (let ([literal (literalSG-literal literalSG)])
      #`(and (identifier? #,var)
             (free-identifier=? #,var (quote-syntax #,literal)))))
  (define (literalSG-rhs literalSG)
    (let ([pks (literalSG-pks literalSG)])
      (parse:pks (cdr vars) (cdr fcs) phi (cdr ds) (shift-pks:literal pks))))
  (define (compoundSG-test compoundSG)
    (let ([kind (compoundSG-kind compoundSG)])
      #`(#,(kind-predicate kind) #,datum-var)))
  (define (compoundSG-rhs compoundSG)
    (let* ([pks (compoundSG-pks compoundSG)]
           [kind (compoundSG-kind compoundSG)]
           [selectors (kind-selectors kind)]
           [frontier-procs (kind-frontier-procs kind)]
           [part-vars (for/list ([selector selectors]) (generate-temporary 'part))]
           [part-frontiers
            (for/list ([fproc frontier-procs] [part-var part-vars])
              (fproc (car fcs) part-var))]
           [part-ds (for/list ([selector selectors]) (car ds))])
      (with-syntax ([(part-var ...) part-vars]
                    [(part-expr ...)
                     (for/list ([selector selectors]) (selector var datum-var))])
        #`(let ([part-var part-expr] ...)
            #,(parse:pks (append part-vars (cdr vars))
                         (append part-frontiers (cdr fcs))
                         phi
                         (append part-ds (cdr ds))
                         (shift-pks:compound pks))))))
  (define-pattern-variable var0 var)
  (define-pattern-variable dvar0 datum-var)
  (define-pattern-variable head-var (generate-temporary 'head))
  (define-pattern-variable tail-var (generate-temporary 'tail))
  (with-syntax ([(datum-clause ...)
                 (for/list ([datumSG datumSGs])
                   #`[#,(datumSG-test datumSG) #,(datumSG-rhs datumSG)])]
                [(lit-clause ...)
                 (for/list ([literalSG literalSGs])
                   #`[#,(literalSG-test literalSG) #,(literalSG-rhs literalSG)])]
                [(compound-clause ...)
                 (for/list ([compoundSG compoundSGs])
                   #`[#,(compoundSG-test compoundSG) #,(compoundSG-rhs compoundSG)])])
    #`(let ([dvar0 (if (syntax? var0) (syntax-e var0) var0)])
        (cond compound-clause ...
              lit-clause ...
              datum-clause ...
              [else
               #,(fail phi (car vars)
                       #:pattern (expectation-of-constants
                                  (pair? compoundSGs)
                                  (for/list ([d datumSGs])
                                    (datumSG-datum d))
                                  (for/list ([l literalSGs])
                                    (literalSG-literal l))
                                  (car ds))
                       #:fce (car fcs))]))))

;; parse:gseq:and : <ParseConfig> pat:and (listof Pattern) stx
;;               -> stx
(define (parse:group:and vars fcs phi ds and-pattern rest-patterns k)
  (match-define (struct pat:and (_ _ _ description patterns))
                and-pattern)
  ;; FIXME: handle description
  (let ([var0-copies (for/list ([p patterns]) (car vars))]
        [fc0-copies (for/list ([p patterns]) (car fcs))]
        [ds-copies (for/list ([p patterns]) (or description (car ds)))])
    (parse:pks (append var0-copies (cdr vars))
               (append fc0-copies (cdr fcs))
               phi
               (append ds-copies (cdr ds))
               (list (make pk (append patterns rest-patterns) k)))))

;; parse:compound:gseq : <ParseConfig> pat:gseq (listof Pattern) stx
;;                    -> stx
(define (parse:group:gseq vars fcs phi ds gseq-pattern rest-patterns k)
  (match-define (struct pat:gseq (ostx attrs depth heads tail)) gseq-pattern)
  (define xvar (generate-temporary 'x))
  (define head-lengths (for/list ([head heads]) (length (head-ps head))))
  (define head-attrss (for/list ([head heads]) (flatten-attrs* (head-attrs head))))
  (define hid-initss
    (for/list ([head heads] [head-attrs head-attrss])
      (for/list ([head-attr head-attrs])
        (cond [(head-as-list? head) #'null]
              [else #'#f]))))
  (define combinerss
    (for/list ([head heads] [head-attrs head-attrss])
      (for/list ([head-attr head-attrs])
        (if (head-as-list? head) #'cons #'or))))
  (define finalizess
    (for/list ([head heads] [head-attrs head-attrss])
      (for/list ([head-attr head-attrs])
        (if (head-as-list? head) #'reverse #'values))))
  (define head-idss
    (for/list ([head-attrs head-attrss])
      (map attr-name head-attrs)))
  (define completed-heads
    (for/list ([head heads])
      (complete-heads-pattern head xvar (add1 depth) ostx)))
  (define hid-argss (map generate-temporaries head-idss))
  (define hid-args (apply append hid-argss))
  (define mins (map head-min heads))
  (define maxs (map head-max heads))
  (define as-list?s (map head-as-list? heads))
  (define reps (generate-temporaries (for/list ([head heads]) 'rep)))

  (with-syntax ([x xvar]
                [var0 (car vars)]
                [((hid ...) ...) head-idss]
                [((hid-arg ...) ...) hid-argss]
                [((hid-init ...) ...) hid-initss]
                [((combine ...) ...) combinerss]
                [((finalize ...) ...) finalizess]
                [(head-length ...) head-lengths]
                [(rep ...) reps]
                [(maxrepconstraint ...)
                 ;; FIXME: move to side condition to appropriate pattern
                 (for/list ([repvar reps] [maxrep maxs])
                   (if maxrep
                       #`(< #,repvar #,maxrep)
                       #`#t))]
                [(parse-loop failkv fail-tail)
                 (generate-temporaries #'(parse-loop failkv fail-tail))])

    (define (gen-head-rhs my-hids my-hid-args combiners repvar maxrep)
      (with-syntax ([(my-hid ...) my-hids]
                    [(my-hid-arg ...) my-hid-args]
                    [(combine ...) combiners]
                    [rep repvar]
                    [maxrep-constraint
                     (if maxrep
                         #`(< #,repvar #,maxrep)
                         #`'#t)])
        #`(let ([my-hid-arg (combine my-hid my-hid-arg)] ...)
            (if maxrep-constraint
                (let ([rep (add1 rep)])
                  (parse-loop x #,@hid-args #,@reps enclosing-fail))
                #,(fail #'enclosing-fail #'var0
                        #:pattern (expectation-of/message "maximum rep constraint failed")
                        #:fce (frontier:add-index (car fcs)
                                                  #`(calculate-index #,@reps)))))))

    (define tail-rhs-expr
      (with-syntax ([(minrep-clause ...)
                     (for/list ([repvar reps] [minrep mins] #:when minrep)
                       #`[(< #,repvar #,minrep)
                          #,(fail #'enclosing-fail (car vars)
                                  #:pattern (expectation-of/message "mininum rep constraint failed")
                                  #:fce (frontier:add-index (car fcs)
                                                            #`(calculate-index #,@reps)))])])
        #`(cond minrep-clause ...
                [else
                 (let ([hid (finalize hid-arg)] ... ...
                       [fail-tail enclosing-fail])
                   #,(parse:pks (cdr vars)
                                (cdr fcs)
                                #'fail-tail
                                (cdr ds)
                                (list (make-pk rest-patterns k))))])))

    (with-syntax ([tail-rhs tail-rhs-expr]
                  [(rhs ...)
                   (for/list ([hids head-idss]
                              [hid-args hid-argss]
                              [combiners combinerss]
                              [repvar reps]
                              [maxrep maxs])
                     (gen-head-rhs hids hid-args combiners repvar maxrep))])
      #`(let ()
          (define (calculate-index rep ...)
            (+ (* rep head-length) ...))
          (define (parse-loop x hid-arg ... ... rep ... failkv)
            #,(parse:pks (list #'x)
                         (list (frontier:add-index (car fcs)
                                                   #'(calculate-index rep ...)))
                         #'failkv
                         (list (car ds))
                         (append
                          (map make-pk
                               (map list completed-heads)
                               (syntax->list #'(rhs ...)))
                          (list (make-pk (list tail) #`tail-rhs)))))
          (let ([hid hid-init] ... ...
                [rep 0] ...)
            (parse-loop var0 hid ... ... rep ... #,phi))))))

;; complete-heads-patterns : Head identifier number -> Pattern
(define (complete-heads-pattern head rest-var depth seq-ostx)
  (define (loop ps pat)
    (if (pair? ps)
        (make pat:compound
              (cons (pattern-ostx (car ps)) (pattern-ostx pat))
              (append (pattern-attrs (car ps)) (pattern-attrs pat))
              depth
              pairK
              (list (car ps) (loop (cdr ps) pat)))
        pat))
  (define base 
    (make pat:id
          seq-ostx
          (list (make-attr rest-var depth null))
          depth rest-var #f null))
  (loop (head-ps head) base))

;; split-pks : (listof identifier) (listof PK)
;;          -> (values (listof identifier) (listof ExtPK)
(define (split-pks vars pks)
  (values vars
          (if (pair? vars)
              (split-pks/first-column pks)
              pks)))

;; split-pks/first-column : (listof PK) -> (listof ExtPK)
;; Pre: the PKs have at least one column
(define (split-pks/first-column pks)
  (define (get-pat x) (car (pk-ps x)))
  (define (constructor-pat? p)
    (or (pat:compound? p) (pat:datum? p) (pat:literal? p)))
  (define (constructor-pk? pk)
    (constructor-pat? (get-pat pk)))
  (define (id-pk? pk)
    (pat:id? (get-pat pk)))

  (define pk-cache (make-hasheq))
  (define pattern-cache (make-hasheq))
  (define (commutes? pk1 pk2)
    (let ([pk1-ht (hash-ref pk-cache pk1
                            (lambda ()
                              (let ([pk1-ht (make-hasheq)])
                                (hash-set! pk-cache pk1 pk1-ht)
                                pk1-ht)))])
      (hash-ref pk1-ht pk2
                (lambda ()
                  (let ([result (ormap pattern-commutes?
                                       (pk-ps pk1)
                                       (pk-ps pk2))])
                    (hash-set! pk1-ht pk2 result)
                    result)))))

  (define (pattern-commutes? p1 p2)
    (let ([result (not (pattern-intersects? p1 p2))])
      (when #f ;; result
        (printf "commutes!\n    ~s\n  & ~s\n"
                (syntax->datum (pattern-ostx p1))
                (syntax->datum (pattern-ostx p2))))
      result))

  (define (pattern-intersects? p1 p2)
    (let ([p1-ht (hash-ref pattern-cache p1
                           (lambda ()
                             (let ([p1-ht (make-hasheq)])
                               (hash-set! pattern-cache p1 p1-ht)
                               p1-ht)))])
      (hash-ref p1-ht p2
                (lambda ()
                  (let ([result (do-pattern-intersects? p1 p2)])
                    (hash-set! p1-ht p2 result)
                    result)))))

  (define (do-pattern-intersects? p1 p2)
    (or (pat:id? p1)
        (pat:id? p2)
        (and (pat:datum? p1) (pat:datum? p2)
             (equal? (pat:datum-datum p1) (pat:datum-datum p2)))
        (and (pat:compound? p1) (pat:compound? p2)
             (eq? (pat:compound-kind p1) (pat:compound-kind p2))
             (andmap pattern-intersects?
                     (pat:compound-patterns p1)
                     (pat:compound-patterns p2)))
        ;; FIXME: conservative
        (and (pat:literal? p1) (pat:literal? p2))
        (pat:gseq? p1)
        (pat:gseq? p2)
        (pat:and? p1)
        (pat:and? p2)))

  (define (major-loop pks epks)
    (match pks
      ['() (reverse epks)]
      [(cons (? constructor-pk? head) tail)
       (let-values ([(r-constructor-pks tail)
                     (gather constructor-pat? tail (list head) null)])
         (let ([c-epk (group-constructor-pks r-constructor-pks)])
           (major-loop tail (cons c-epk epks))))]
      [(cons (? id-pk? head) tail)
       (let* ([this-pat (get-pat head)]
              [this-stxclass (pat:id-stxclass this-pat)]
              [this-args (pat:id-args this-pat)])
         (let-values ([(r-id-pks tail)
                       (gather (lambda (p)
                                 (and (pat:id? p)
                                      (equal? (pat:id-stxclass p) this-stxclass)
                                      (equal? (pat:id-args p) this-args)))
                               tail
                               (list head)
                               null)])
           (let ([id-epk (make idG this-stxclass this-args (reverse r-id-pks))])
             (major-loop tail (cons id-epk epks)))))]
      ;; Leave gseq- and and-patterns by themselves (at least for now)
      [(cons head tail)
       (major-loop tail (cons head epks))]))

  ;; gather : (PK -> boolean) (listof PK) (listof PK) (listof PK)
  ;;       -> (listof PK) (listof PK)
  (define (gather pred pks taken prefix)
    (match pks
      ['()
       (values taken (reverse prefix))]
      [(cons pk tail)
       ;; We can have it if it can move past everything in the prefix.
       (if (and (pred (get-pat pk))
                (for/and ([prefixpk prefix])
                  (commutes? pk prefixpk)))
           (gather pred tail (cons pk taken) prefix)
           (gather pred tail taken (cons pk prefix)))]))

  ;; group-constructor-pks : (listof PK) -> ExtPK
  (define (group-constructor-pks reversed-pks)
    (define compound-ht (make-hasheq))
    (define datum-ht (make-hash))
    (define lit-ht (make-bound-identifier-mapping))
    (for ([pk reversed-pks])
      (let ([p (get-pat pk)])
        (cond [(pat:compound? p)
               (let ([kind (pat:compound-kind p)])
                 (hash-set! compound-ht
                            kind (cons pk (hash-ref compound-ht kind null))))]
              [(pat:datum? p)
               (let ([d (pat:datum-datum p)])
                 (hash-set! datum-ht d (cons pk (hash-ref datum-ht d null))))]
              [(pat:literal? p)
               (let ([lit (pat:literal-literal p)])
                 (bound-identifier-mapping-put!
                  lit-ht
                  lit
                  (cons pk
                        (bound-identifier-mapping-get lit-ht lit
                                                      (lambda () null)))))])))
    (let ([datumSGs (hash-map datum-ht make-datumSG)]
          [literalSGs (bound-identifier-mapping-map lit-ht make-literalSG)]
          [compoundSGs (hash-map compound-ht make-compoundSG)])
      (make descrimG datumSGs literalSGs compoundSGs)))

  (major-loop pks null))

;; shift-pks:id : (listof PK) identifier -> (listof PK)
(define (shift-pks:id pks matches-var)
  (map (lambda (pk) (shift-pk:id pk matches-var))
       pks))

;; shift-pk:id : PK identifier identifier -> PK
;; FIXME: Assumes that all attrs are relevant!!!
(define (shift-pk:id pk0 matches-var0)
  (match pk0
    [(struct pk ((cons (struct pat:id (_ attrs depth name _ _)) rest-ps) k))
     (let* ([flat-attrs (flatten-attrs* attrs depth #f #f)]
            ;; FIXME: depth already included, right???
            [ids (map attr-name flat-attrs)]
            [depths (map attr-depth flat-attrs)])
       (with-syntax ([(id ...) ids]
                     [(depth ...) depths])
         (make-pk rest-ps
                  (if (pair? ids)
                      #`(let-values ([(id ...)
                                      #,(if name
                                            #`(apply values #,matches-var0)
                                            #`(apply values (cdr #,matches-var0)))])
                          #,k)
                      k))))]))

;; shift-pks:datum : (listof PK) -> (listof PK)
(define (shift-pks:datum pks)
  (define (shift-pk pk)
    (make-pk (cdr (pk-ps pk)) (pk-k pk)))
  (map shift-pk pks))

;; shift-pks:literal : (listof PK) -> (listof PK)
(define (shift-pks:literal pks)
  (define (shift-pk pk)
    (make-pk (cdr (pk-ps pk)) (pk-k pk)))
  (map shift-pk pks))

;; shift-pks:compound : (listof PK) -> (listof PK)
(define (shift-pks:compound pks)
  (define (shift-pk pk0)
    (match pk0
      [(struct pk ((cons (struct pat:compound (_ _ _ _ patterns)) rest-ps)
                   k))
       (make-pk (append patterns rest-ps) k)]))
  (map shift-pk pks))

;; wrap-pvars : (listof IAttr) stx -> stx
(define (wrap-pvars iattrs expr)
  (let* ([flat-iattrs (flatten-attrs* iattrs 0 #f #f)]
         [ids (map attr-name flat-iattrs)]
         [depths (map attr-depth flat-iattrs)])
    (with-syntax ([(id ...) ids]
                  [(depth ...) depths]
                  [expr expr])
      #'(let-attributes ([id depth id] ...)
          expr))))
