
#lang scheme/base
(require (for-template scheme/base
                       syntax/stx
                       scheme/stxparam
                       "kws.ss")
         scheme/match
         scheme/contract
         scheme/private/sc
         syntax/stx
         syntax/boundmap
         "rep.ss"
         "util.ss")
(provide/contract
 [parse:rhs (rhs? (listof sattr?) (listof identifier?) . -> . syntax?)]
 [parse:clauses (syntax? identifier? identifier? . -> . syntax?)])

;; A PK is (make-pk (listof Pattern) stx)
;; k is the rhs expression:
;;   - open term with the attr names as free variables
;;   - attr name must be bound to variable of (listof^depth value)
;;   - 'fail' stxparameterized to (non-escaping!) failure procedure
(define-struct pk (ps k) #:transparent)

;; A FrontierContext (FC) is ({nat id}*)
(define (empty-frontier x)
  (list 0 x))
(define (done-frontier x)
  (list +inf.0 x))
(define (frontier:add-car fc x)
  (list* 0 x fc))
(define (frontier:add-cdr fc)
  (cons (match (car fc)
          [(? number? n)
           (add1 n)]
          [`(+ ,n . ,rest)
           `(+ ,(add1 n) . ,rest)])
        (cdr fc)))
(define (frontier:add-index fc expr)
  (cons (match (car fc)
          [(? number? n)
           `(+ ,n ,expr)]
          [`(+ ,n . ,rest)
           `(+ ,n ,expr . ,rest)])
        (cdr fc)))
(define (frontier->expr fc)
  #`(list #,@(reverse (or fc null))))

;; A FrontierContext (FC) is (listof (cons id nat))

;; parse:rhs : RHS (listof SAttr) (listof identifier) -> stx
;; Takes a list of the relevant attrs; order is significant!
;; Returns either fail or a list having length same as 'relsattrs'
(define (parse:rhs rhs relsattrs args)
  (with-syntax ([(arg ...) args])
    #`(lambda (x arg ...)
        (define (fail-rhs x expected reason frontier)
          (make-failed x expected reason))
        #,(parse:pks (list #'x)
                     (list (empty-frontier #'x))
                     (rhs->pks rhs relsattrs #'x)
                     #'fail-rhs))))

;; fail : id id #:pattern datum #:reason datum #:fc FC -> stx
(define (fail k x #:pattern [p #f] #:reason [reason #f] #:fc [fc #f])
  (with-syntax ([k k] [x x] [p p] [reason reason]
                [fc-expr (frontier->expr fc)])
    #`(let ([failcontext fc-expr])
        #;(printf "failing at ~s\n" failcontext)
        (k x 'p 'reason failcontext))))

;; rhs->pks : RHS (listof SAttr) identifier -> (listof PK)
(define (rhs->pks rhs relsattrs main-var)
  (match rhs
    [(struct rhs:union (orig-stx attrs transparent? description patterns))
     (for*/list ([rhs patterns] [pk (rhs-pattern->pks rhs relsattrs main-var)]) 
       pk)]))

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

(define (expr:convert-sides sides iattrs main-var k)
  (match sides
    ['() (k iattrs)]
    [(cons (struct clause:where (e)) rest)
     (let* ([k-rest (expr:convert-sides rest iattrs main-var k)])
       (with-syntax ([(x) (generate-temporaries #'(x))])
         #`(let ([x #,(wrap-pattern-body/attrs iattrs 0 e)])
             (if x
                 #,k-rest
                 #,(fail #'enclosing-fail main-var
                         #:reason "side condition failed"
                         #:fc (done-frontier main-var))))))]
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

(define (check-literals-list stx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected list of identifiers" stx))
  (for ([id (syntax->list stx)])
    (unless (identifier? id)
      (raise-syntax-error #f "expected identifier" id)))
  (syntax->list stx))

(define clauses-kw-table
  (list (list '#:literals check-literals-list)))

;; parse:clauses : stx identifier identifier -> stx
(define (parse:clauses stx var failid)
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
            (raise-syntax-error #f "expected single body expression" clause)]))]))
  #;(printf "literals: ~s\n" literals)
  (unless (stx-list? clauses-stx)
    (raise-syntax-error #f "expected sequence of clauses" clauses-stx))
  (parse:pks (list var)
             (list (empty-frontier var))
             (map clause->pk (stx->list clauses-stx))
             failid))

;; An ExtPK is one of
;;   - PK
;;   - (make-idpks stxclass (listof stx) (listof PK))
;;   - (make-cpks (listof PK) (listof DatumPKS) (listof LiteralPKS))
(define-struct idpks (stxclass args idpks))
(define-struct cpks (pairpks datumpks literalpks))

;; A DatumPKS is (make-datumpks datum (listof PK))
(define-struct datumpks (datum pks))

;; A LiteralPKS is (make-literalpks identifier (listof PK))
(define-struct literalpks (literal pks))

;; parse:pks : (listof identifier) (listof FC) (listof PK) identifier -> stx
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
             #`(let-syntax ([failvar (make-rename-transformer (quote-syntax #,failid))])
                 (try failvar (expr ...)))))]
        [else
         (let-values ([(vars extpks) (split-pks vars pks)])
           (let* ([failvar (car (generate-temporaries #'(fail-k)))]
                  [exprs
                   (for/list ([extpk extpks])
                     (parse:extpk vars fcs extpk failvar))])
             (with-syntax ([failvar failvar]
                           [(expr ...) exprs])
               #`(let-syntax ([failvar (make-rename-transformer (quote-syntax #,failid))])
                   (try failvar (expr ...))))))]))

(define (report-stxclass stxclass)
  (and stxclass
       (format "expected ~a"
               (or (sc-description stxclass)
                   (sc-name stxclass)))))

(define (report-constants pairs? data literals)
  (cond [pairs? #f]
        [(null? data)
         (format "expected ~a" (report-choices-literals literals))]
        [(null? literals)
         (format "expected ~a" (report-choices-data data))]
        [else
         (format "expected ~a; or ~a"
                 (report-choices-data data)
                 (report-choices-literals literals))]))

(define (report-choices-literals literals0)
  (define literals
    (sort (map syntax-e literals0) 
          string<? 
          #:key symbol->string
          #:cache-keys? #t))
  (case (length literals)
    [(1) (format "the literal identifier ~s" (car literals))]
    [else (format "one of the following literal identifiers: ~a"
                  (comma-list literals))]))

(define (report-choices-data data)
  (case (length data)
    [(1) (format "the datum ~s" (car data))]
    [else (format "one of the following literals: ~a"
                  (comma-list data))]))

(define (comma-list items0)
  (define items (for/list ([item items0]) (format "~s" item)))
  (define (loop items)
    (cond [(null? items)
           null]
          [(null? (cdr items))
           (list ", or " (car items))]
          [else
           (list* ", " (car items) (loop (cdr items)))]))
  (case (length items)
    [(2) (format "~a or ~a" (car items) (cadr items))]
    [else (let ([strings (list* (car items) (loop (cdr items)))])
            (apply string-append strings))]))


;; parse:extpk : (listof identifier) (listof FC) ExtPK identifier -> stx
;; Pre: vars is not empty
(define (parse:extpk vars fcs extpk failid)
  (match extpk
    [(struct idpks (stxclass args pks))
     (with-syntax ([sub-parse-expr
                    (if stxclass
                        #`(#,(sc-parser-name stxclass) #,(car vars) #,@args)
                        #`(list #,(car vars)))]
                   [var0 (car vars)]
                   [(r) (generate-temporaries #'(r))])
       #`(let ([r sub-parse-expr])
           (if (ok? r)
               #,(parse:pks (cdr vars) (cdr fcs) (shift-pks:id pks #'r) failid)
               #,(fail failid (car vars)
                       #:pattern (report-stxclass stxclass)
                       #:fc (car fcs)))))]
    [(struct cpks (pairpks datumpkss literalpkss))
     (with-syntax ([var0 (car vars)]
                   [(dvar0) (generate-temporaries (list (car vars)))])
       (with-syntax ([(head-var tail-var) (generate-temporaries #'(head tail))]
                     [(pair-pattern ...)
                      (for*/list ([pk pairpks])
                        (pattern-orig-stx (car (pk-ps pk))))]
                     [(datum-pattern ...)
                      (for*/list ([datumpk datumpkss]
                                  [pk (datumpks-pks datumpk)])
                        (pattern-orig-stx (car (pk-ps pk))))]
                     [(datum-test ...)
                      (for/list ([datumpk datumpkss])
                        (let ([datum (datumpks-datum datumpk)])
                          #`(equal? dvar0 (quote #,datum))))]
                     [(datum-rhs ...)
                      (map (lambda (pks)
                             (parse:pks (cdr vars)
                                        (cdr fcs)
                                        (shift-pks:datum pks)
                                        failid))
                           (map datumpks-pks datumpkss))]
                     [(lit-test ...)
                      (for/list ([literalpks literalpkss])
                        (let ([literal (literalpks-literal literalpks)])
                          #`(and (identifier? var0)
                                 (free-identifier=? var0 (quote-syntax #,literal)))))]
                     [(lit-rhs ...)
                      (map (lambda (pks)
                             (parse:pks (cdr vars)
                                        (cdr fcs)
                                        (shift-pks:literal pks)
                                        failid))
                           (map literalpks-pks literalpkss))])
         #`(let ([dvar0 (if (syntax? var0) (syntax-e var0) var0)])
             (cond #,@(if (pair? pairpks)
                          #`([(pair? dvar0)
                              (let ([head-var (car dvar0)]
                                    [tail-var (cdr dvar0)])
                                #,(parse:pks (list* #'head-var #'tail-var (cdr vars))
                                             (list* (frontier:add-car (car fcs) #'head-var)
                                                    (frontier:add-cdr (car fcs))
                                                    (cdr fcs))
                                             (shift-pks:pair pairpks)
                                             failid))])
                          #`())
                   #,@(if (pair? literalpkss)
                          #'([lit-test lit-rhs] ...)
                          #'())
                   [datum-test datum-rhs] ...
                   [else
                    #,(fail failid (car vars)
                            #:pattern (report-constants (pair? pairpks)
                                                        (for/list ([d datumpkss])
                                                                  (datumpks-datum d))
                                                        (for/list ([l literalpkss])
                                                                  (literalpks-literal l)))
                            #:fc (car fcs))]))))]
    #;
    [(struct pk ((cons (struct pat:splice (orig-stx attrs depth head tail))
                       rest-ps)
                 k))
     (let-match ([(struct pat:id-splice (_ head-attrs _ name ssc args)) head])
       (let* ([head-flat-attrs (flatten-attrs* head-attrs)]
              [head-ids (map attr-name head-flat-attrs)])
         (with-syntax* ([var0 (car vars)]
                        [(hid ...) head-ids]
                        [(fail-k) (generate-temporaries #'(fail-k))]
                        [ok-k
                         #`(lambda (fail-k hid ...)
                             #,(parse:pks (cons #'t (cdr vars))
                                          fcs ;; FIXME: must update!
                                          (cons tail
                                                (shift-pks:id pks #'r))
                                          #'fail-k))]
                        [sub-parse-expr
                         #`(#,(ssc-parser-name ssc) #,(car vars) #,@args)])
           #'sub-parse-expr)))]
    [(struct pk ((cons (struct pat:gseq (orig-stx attrs depth heads tail))
                       rest-ps)
                 k))
     (let* ([xvar (car (generate-temporaries (list #'x)))]
            [head-lengths
             (for/list ([head heads]) (length (head-ps head)))]
            [head-attrss
             (for/list ([head heads])
               (flatten-attrs* (head-attrs head)))]
            [hid-initss
             (for/list ([head heads] [head-attrs head-attrss])
               (for/list ([head-attr head-attrs])
                 (cond [(head-default head)
                        => (lambda (x) #`(quote-syntax #,x))]
                       [(head-as-list? head) #'null]
                       [else #'#f])))]
            [combinerss
             (for/list ([head heads] [head-attrs head-attrss])
               (for/list ([head-attr head-attrs])
                 (if (head-as-list? head) #'cons #'or)))]
            [finalizess
             (for/list ([head heads] [head-attrs head-attrss])
               (for/list ([head-attr head-attrs])
                 (if (head-as-list? head) #'reverse #'values)))]
            [head-idss
             (for/list ([head-attrs head-attrss])
               (map attr-name head-attrs))]
            [completed-heads
             (for/list ([head heads])
               (complete-heads-pattern head xvar (add1 depth) orig-stx))]
            [hid-argss (map generate-temporaries head-idss)]
            [hid-args (apply append hid-argss)]
            [mins (map head-min heads)]
            [maxs (map head-max heads)]
            [as-list?s (map head-as-list? heads)]
            [reps (generate-temporaries (for/list ([head heads]) 'rep))])
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
                     [(minrepclause ...)
                      (for/list ([repvar reps] [minrep mins] #:when minrep)
                        #`[(< #,repvar #,minrep)
                           #,(fail #'enclosing-fail (car vars)
                                   #:reason "minimum repetition constraint failed")])]
                     [(occurs-binding ...)
                      (for/list ([head heads] [rep reps] #:when (head-occurs head))
                        #`[#,(head-occurs head) (positive? #,rep)])]
                     [(parse-loop failkv fail-tail)
                      (generate-temporaries #'(parse-loop failkv fail-tail))])
         (with-syntax ([(rhs ...)
                        #`[(let ([hid-arg (combine hid hid-arg)] ...)
                             (if maxrepconstraint
                                 (let ([rep (add1 rep)])
                                   (parse-loop x #,@hid-args #,@reps enclosing-fail))
                                 #,(fail #'enclosing-fail #'var0
                                         #:reason "maxiumum repetition constraint failed")))
                           ...]]
                       [tail-rhs
                        #`(cond minrepclause ...
                                [else
                                 (let ([hid (finalize hid-arg)] ... ...
                                       occurs-binding ...
                                       [fail-tail enclosing-fail])
                                   #,(parse:pks (cdr vars)
                                                (cdr fcs)
                                                (list (make-pk rest-ps k))
                                                #'fail-tail))])])
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
                 (parse-loop var0 hid ... ... rep ... #,failid))))))]))

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
      (when result
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
        (pat:splice? p1)
        (pat:splice? p2)
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
