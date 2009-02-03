#lang scheme/base
(require (for-template scheme/base
                       syntax/stx
                       scheme/stxparam
                       "kws.ss"
                       "messages.ss")
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
 [parse:clauses (syntax? identifier? identifier? . -> . syntax?)])

;; parse:rhs : RHS (listof SAttr) (listof identifier) -> stx
;; Takes a list of the relevant attrs; order is significant!
;; Returns either fail or a list having length same as 'relsattrs'
(define (parse:rhs rhs relsattrs args)
  (cond [(rhs:union? rhs)
         (with-syntax ([(arg ...) args])
           #`(lambda (x arg ...)
               (define (fail-rhs x expected frontier)
                 (make-failed x expected frontier))
               #,(let ([pks (rhs->pks rhs relsattrs #'x)])
                   (unless (pair? pks)
                     (wrong-syntax (rhs-orig-stx rhs)
                                   "syntax class has no variants"))
                   (parse:pks (list #'x)
                              (list (empty-frontier #'x))
                              pks
                              #'fail-rhs))))]
        [(rhs:basic? rhs)
         (rhs:basic-parser rhs)]))

;; parse:clauses : stx identifier identifier -> stx
(define (parse:clauses stx var failid)
  (define clauses-kw-table
    (list (list '#:literals check-literals-list)))
  (define-values (chunks clauses-stx) (chunk-kw-seq/no-dups stx clauses-kw-table))
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
         (syntax-case rest ()
           [(b)
            (let* ([pattern (parse-pattern #'p decls 0)])
              (make-pk (list pattern)
                       (expr:convert-sides sides
                                           (pattern-attrs pattern)
                                           var
                                           (lambda (iattrs)
                                             (wrap-pattern-body/attrs iattrs
                                                                      0
                                                                      #'b)))))]
           [_
            (wrong-syntax clause "expected single body expression")]))]))
  (unless (stx-list? clauses-stx)
    (wrong-syntax clauses-stx "expected sequence of clauses"))
  (let ([pks (map clause->pk (stx->list clauses-stx))])
    (unless (pair? pks)
      (wrong-syntax stx "no variants"))
    (parse:pks (list var)
               (list (empty-frontier var))
               pks
               failid)))

;; rhs->pks : RHS (listof SAttr) identifier -> (listof PK)
(define (rhs->pks rhs relsattrs main-var)
  (match rhs
    [(struct rhs:union (orig-stx attrs transparent? description patterns))
     (for*/list ([rhs patterns] [pk (rhs-pattern->pks rhs relsattrs main-var)]) 
       pk)]))

;; rhs-pattern->pks : RHS (listof SAttr) identifier -> (listof PK)
(define (rhs-pattern->pks rhs relsattrs main-var)
  (match rhs
    [(struct rhs:pattern (orig-stx attrs pattern decls remap sides))
     (list (make-pk (list pattern)
                    (expr:convert-sides sides
                                        (pattern-attrs pattern)
                                        main-var
                                        (lambda (iattrs)
                                          (expr:sc iattrs
                                                   relsattrs
                                                   remap
                                                   main-var)))))]))

;; expr:convert-sides : (listof SideClause) (listof IAttr) id stx -> stx
(define (expr:convert-sides sides iattrs main-var k)
  (match sides
    ['() (k iattrs)]
    [(cons (struct clause:when (e)) rest)
     (let* ([k-rest (expr:convert-sides rest iattrs main-var k)])
       (with-syntax ([(x) (generate-temporaries #'(x))])
         #`(let ([x #,(wrap-pattern-body/attrs iattrs 0 e)])
             (if x
                 #,k-rest
                 #,(fail #'enclosing-fail main-var
                         #:pattern (expectation-of/message "side condition failed")
                         #:fce (done-frontier main-var))))))]
    [(cons (struct clause:with (p e)) rest)
     (let* ([new-iattrs (append (pattern-attrs p) iattrs)]
            [k-rest (expr:convert-sides rest new-iattrs main-var k)])
       (with-syntax ([(x fail-k) (generate-temporaries #'(x fail-k))])
         #`(let ([x #,(wrap-pattern-body/attrs iattrs 0 e)]
                 [fail-k enclosing-fail])
             #,(parse:pks (list #'x)
                          (list (done-frontier #'x))
                          (list (make-pk (list p) k-rest))
                          #'fail-k))))]))

;; expr:sc : (listof IAttr) (listof SAttr) env stx -> stx
(define (expr:sc iattrs relsattrs remap main-var)
  (let* ([reliattrs (reorder-iattrs relsattrs iattrs remap)]
         [flat-reliattrs (flatten-attrs* reliattrs)]
         [relids (map attr-name flat-reliattrs)])
    (with-syntax ([main main-var]
                  [(relid ...) relids])
      #'(list main relid ...))))

;; check-literals-list : syntax -> (listof id)
(define (check-literals-list stx)
  (unless (stx-list? stx)
    (wrong-syntax stx "expected list of identifiers"))
  (for ([id (syntax->list stx)])
    (unless (identifier? id)
      (wrong-syntax id "expected identifier")))
  (syntax->list stx))

;; fail : id id #:pattern datum #:reason datum #:fce FCE -> stx
(define (fail k x #:pattern p #:fce fce)
  (with-syntax ([k k]
                [x x]
                [p p]
                [fc-expr (frontier->expr fce)])
    #`(let ([failcontext fc-expr])
        (k x p failcontext))))


;; Parsing

;; parse:pks : (listof identifier) (listof FCE) (listof PK) identifier -> stx
;; Each PK has a list of |vars| patterns.
;; The list of PKs must not be empty.
(define (parse:pks vars fcs pks failid)
  (cond [(null? pks)
         (error 'parse:pks "internal error: empty list of rows")]
        [(null? vars)
         ;; Success!
         (let* ([failvar (car (generate-temporaries #'(fail-k)))]
                [exprs
                 (for/list ([pk pks])
                   #`(with-enclosing-fail #,failvar #,(pk-k pk)))])
           (with-syntax ([failvar failvar]
                         [(expr ...) exprs])
             #`(try failvar [expr ...] #,failid)))]
        [else
         (let-values ([(vars extpks) (split-pks vars pks)])
           (let* ([failvar (car (generate-temporaries #'(fail-k)))]
                  [exprs
                   (for/list ([extpk extpks])
                     (parse:extpk vars fcs extpk failvar))])
             (with-syntax ([failvar failvar]
                           [(expr ...) exprs])
               #`(try failvar [expr ...] #,failid))))]))


;; parse:extpk : (listof identifier) (listof FCE) ExtPK identifier -> stx
;; Pre: vars is not empty
(define (parse:extpk vars fcs extpk failid)
  (match extpk
    [(struct idpks (stxclass args pks))
     (if stxclass
         (parse:pk:id/stxclass vars fcs failid stxclass args pks)
         (parse:pk:id/any  vars fcs failid args pks))]
    [(struct cpks (pairpks datumpkss literalpkss))
     (parse:pk:c vars fcs failid pairpks datumpkss literalpkss)]
    [(struct pk ((cons (? pat:gseq? gseq-pattern) rest-patterns) k))
     (parse:pk:gseq vars fcs failid gseq-pattern rest-patterns k)]))

;; parse:pk:id/stxclass : (listof id) (listof FCE) id SC stx (listof pk) -> stx
(define (parse:pk:id/stxclass vars fcs failid stxclass args pks)
  (with-syntax ([var0 (car vars)]
                [(arg ...) args]
                [(arg-var ...) (generate-temporaries args)]
                [parser (sc-parser-name stxclass)]
                [result (generate-temporary 'result)])
    #`(let ([arg-var arg] ...)
        (let ([result (parser var0 arg-var ...)])
          (if (ok? result)
              #,(parse:pks (cdr vars) (cdr fcs) (shift-pks:id pks #'result) failid)
              #,(fail failid (car vars)
                      #:pattern (expectation-of-stxclass stxclass #'(arg-var ...))
                      #:fce (car fcs)))))))

;; parse:pk:id/any : (listof id) (listof FCE) id stx (listof pk) -> stx
(define (parse:pk:id/any vars fcs failid args pks)
  (with-syntax ([var0 (car vars)]
                [(arg ...) args]
                [(arg-var ...) (generate-temporaries args)]
                [result (generate-temporary 'result)])
    #`(let ([arg-var arg] ...)
        (let ([result (list var0)])
          #,(parse:pks (cdr vars) (cdr fcs) (shift-pks:id pks #'result) failid)))))

;; parse:pk:c : (listof id) (listof FCE) id ??? ... -> stx
(define (parse:pk:c vars fcs failid pairpks datumpkss literalpkss)
  (define var (car vars))
  (define datum-var (generate-temporary 'datum))
  (define (datumpks-test datumpks)
    (let ([datum (datumpks-datum datumpks)])
      #`(equal? #,datum-var (quote #,datum))))
  (define (datumpks-rhs datumpks)
    (let ([pks (datumpks-pks datumpks)])
      (parse:pks (cdr vars) (cdr fcs) (shift-pks:datum pks) failid)))
  (define (literalpks-test literalpks)
    (let ([literal (literalpks-literal literalpks)])
      #`(and (identifier? #,var)
             (free-identifier=? #,var (quote-syntax #,literal)))))
  (define (literalpks-rhs literalpks)
    (let ([pks (literalpks-pks literalpks)])
      (parse:pks (cdr vars) (cdr fcs) (shift-pks:literal pks) failid)))
  (define-pattern-variable var0 var)
  (define-pattern-variable dvar0 datum-var)
  (define-pattern-variable head-var (generate-temporary 'head))
  (define-pattern-variable tail-var (generate-temporary 'tail))
  (with-syntax ([(datum-clause ...)
                 (for/list ([datumpks datumpkss])
                   #`[#,(datumpks-test datumpks) #,(datumpks-rhs datumpks)])]
                [(lit-clause ...)
                 (for/list ([literalpks literalpkss])
                   #`[#,(literalpks-test literalpks) #,(literalpks-rhs literalpks)])])
    #`(let ([dvar0 (if (syntax? var0) (syntax-e var0) var0)])
        (cond #,@(if (pair? pairpks)
                     #`([(pair? dvar0)
                         (let ([head-var (car dvar0)]
                               [tail-var (datum->syntax var0 (cdr dvar0) var0)])
                           #,(parse:pks (list* #'head-var #'tail-var (cdr vars))
                                        (list* (frontier:add-car (car fcs) #'head-var)
                                               (frontier:add-cdr (car fcs))
                                               (cdr fcs))
                                        (shift-pks:pair pairpks)
                                        failid))])
                     #`())
              lit-clause ...
              datum-clause ...
              [else
               #,(fail failid (car vars)
                       #:pattern (expectation-of-constants
                                  (pair? pairpks)
                                  (for/list ([d datumpkss])
                                    (datumpks-datum d))
                                  (for/list ([l literalpkss])
                                    (literalpks-literal l)))
                       #:fce (car fcs))]))))

;; parse:pk:gseq : (listof id) (listof FCE) id
;;                 pat:gseq (listof Pattern)
;;                 ???
;;              -> stx
(define (parse:pk:gseq vars fcs failid gseq-pattern rest-patterns k)
  (match-define (struct pat:gseq (orig-stx attrs depth heads tail)) gseq-pattern)
  (define xvar (generate-temporary 'x))
  (define head-lengths (for/list ([head heads]) (length (head-ps head))))
  (define head-attrss (for/list ([head heads]) (flatten-attrs* (head-attrs head))))
  (define hid-initss
    (for/list ([head heads] [head-attrs head-attrss])
      (for/list ([head-attr head-attrs])
        (cond [(head-default head)
               => (lambda (x) #`(quote-syntax #,x))]
              [(head-as-list? head) #'null]
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
      (complete-heads-pattern head xvar (add1 depth) orig-stx)))
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
                [(occurs-binding ...)
                 (for/list ([head heads] [rep reps] #:when (head-occurs head))
                   #`[#,(head-occurs head) (positive? #,rep)])]
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
                       occurs-binding ...
                       [fail-tail enclosing-fail])
                   #,(parse:pks (cdr vars)
                                (cdr fcs)
                                (list (make-pk rest-patterns k))
                                #'fail-tail))])))

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
                         (append
                          (map make-pk
                               (map list completed-heads)
                               (syntax->list #'(rhs ...)))
                          (list (make-pk (list tail) #`tail-rhs)))
                         #'failkv))
          (let ([hid hid-init] ... ...
                [rep 0] ...)
            (parse-loop var0 hid ... ... rep ... #,failid))))))


;; complete-heads-patterns : Head identifier number stx -> Pattern
(define (complete-heads-pattern head rest-var depth seq-orig-stx)
  (define (loop ps pat)
    (if (pair? ps)
        (make-pat:pair (cons (pattern-orig-stx (car ps)) (pattern-orig-stx pat))
                       (append (pattern-attrs (car ps)) (pattern-attrs pat))
                       depth
                       (car ps)
                       (loop (cdr ps) pat))
        pat))
  (define base 
    (make-pat:id seq-orig-stx
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
    (or (pat:pair? p) (pat:datum? p) (pat:literal? p)))
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
                (syntax->datum (pattern-orig-stx p1))
                (syntax->datum (pattern-orig-stx p2))))
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
        (and (pat:pair? p1) (pat:pair? p2)
             (pattern-intersects? (pat:pair-head p1) (pat:pair-head p2))
             (pattern-intersects? (pat:pair-tail p1) (pat:pair-tail p2)))
        ;; FIXME: conservative
        (and (pat:literal? p1) (pat:literal? p2))
        (pat:gseq? p1)
        (pat:gseq? p2)))

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
           (let ([id-epk (make idpks this-stxclass this-args (reverse r-id-pks))])
             (major-loop tail (cons id-epk epks)))))]
      [(cons head tail)
       (major-loop tail (cons head epks))]))

  ;; gather : (PK -> boolean) (listof PK) (listof PK) (listof PK)
  ;;       -> (listof PK) (listof PK)
  (define (gather pred pks taken prefix)
    #;(printf "called gather (~s pks, ~s prefix)\n" (length pks) (length prefix))
    (match pks
      ['()
       #;(printf "took ~s, left ~s\n" (length taken) (length prefix))
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
    (define pairpks null)
    (define ht (make-hash))
    (define lit-ht (make-bound-identifier-mapping))
    (for ([pk reversed-pks])
      (let ([p (get-pat pk)])
        (cond [(pat:pair? p)
               (set! pairpks (cons pk pairpks))]
              [(pat:datum? p)
               (let ([d (pat:datum-datum p)])
                 (hash-set! ht d (cons pk (hash-ref ht d null))))]
              [(pat:literal? p)
               (let ([lit (pat:literal-literal p)])
                 (bound-identifier-mapping-put!
                  lit-ht
                  lit
                  (cons pk
                        (bound-identifier-mapping-get lit-ht lit
                                                      (lambda () null)))))])))
    (let ([datumpkss (hash-map ht make-datumpks)]
          [litpkss (bound-identifier-mapping-map lit-ht make-literalpks)])
      (make cpks pairpks datumpkss litpkss)))

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

;; shift-pks:pair : (listof PK) -> (listof PK)
(define (shift-pks:pair pks)
  (define (shift-pk pk0)
    (match pk0
      [(struct pk ((cons (struct pat:pair (orig-stx attrs depth head tail)) rest-ps)
                   k))
       (make-pk (list* head tail rest-ps) k)]))
  (map shift-pk pks))

;; wrap-pattern-body : (listof IAttr) nat stx -> stx
(define (wrap-pattern-body/attrs iattrs depth b)
  (let* ([flat-iattrs (flatten-attrs* iattrs depth #f #f)]
         [ids (map attr-name flat-iattrs)]
         [depths (map attr-depth flat-iattrs)])
    (with-syntax ([(id ...) ids]
                  [(depth ...) depths]
                  [b b])
      #`(let-syntax ([id (make-syntax-mapping 'depth (quote-syntax id))] ...)
          b))))
