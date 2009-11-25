#lang scheme/base
(require "../utils/utils.ss")

(require mzlib/struct 
         scheme/match scheme/list
         syntax/boundmap
         "interning.ss"
	 unstable/syntax unstable/match
         mzlib/etc
         scheme/contract         
         (for-syntax 
          scheme/list
          (only-in unstable/syntax generate-temporary)
          scheme/match
          (except-in syntax/parse id identifier keyword)
          scheme/base
          syntax/struct
          syntax/stx
          scheme/contract
	  unstable/syntax
          (rename-in (except-in (utils utils stxclass-util) bytes byte-regexp regexp byte-pregexp #;pregexp)
                     [id* id]
                     [keyword* keyword])))

(provide == defintern hash-id (for-syntax fold-target))



(define-for-syntax fold-target #'fold-target)

(define-for-syntax (mk par ht-stx key? name?)
  (define-syntax-class opt-cnt-id
    #:attributes (i cnt)
    (pattern i:id
             #:with cnt #'any/c)
    (pattern [i:id cnt]))
  (define-syntax-class no-provide-kw
    (pattern #:no-provide))
  (define-syntax-class idlist
    #:attributes ((i 1) (cnt 1) fs)
    (pattern (oci:opt-cnt-id ...)               
             #:with (i ...) #'(oci.i ...)
             #:with (cnt ...) #'(oci.cnt ...)
             #:with fs #'(i ...)))
  (define (combiner f flds)
    (syntax-parse flds
      [() #'empty-hash-table]
      [(e) #`(#,f e)]
      [(e ...) #`(combine-frees (list (#,f e) ...))]))
  (define-splicing-syntax-class frees-pat
    #:transparent
    #:attributes (f1 f2 def)
    (pattern (~seq f1:expr f2:expr)
             #:with def #'(begin))
    (pattern #f
             #:with f1 #'empty-hash-table
             #:with f2 #'empty-hash-table
             #:with def #'(begin))
    (pattern e:expr
             #:with id (generate-temporary)
             #:with def #'(define id e)
             #:with f1 #'(id free-vars*)
             #:with f2 #'(id free-idxs*)))
  (define-syntax-class fold-pat
    #:transparent
    #:attributes (e)
    (pattern #:base
             #:with e fold-target)
    (pattern ex:expr
             #:with e #'#'ex))
  (unless (equal? key? name?)
    (error "key? not name"))
  (lambda (stx)          
    (syntax-parse stx 
      [(dform nm:id flds:idlist (~or 
                                 (~optional [#:key key-expr:expr])
                                 (~optional [#:intern intern?:expr])
                                 (~optional [#:frees frees:frees-pat])
                                 (~optional [#:fold-rhs fold-rhs:fold-pat])
                                 (~optional [#:contract cnt:expr])
                                 (~optional no-provide?:no-provide-kw)) ...)
       (with-syntax* 
        ([ex (format-id #'nm "~a:" #'nm)]
         [fold-name (format-id #f "~a-fold" #'nm)]
         [kw-stx (string->keyword (symbol->string (attribute nm.datum)))]
         [parent par]
         [(s:ty maker pred acc ...) (build-struct-names #'nm (syntax->list #'flds.fs) #f #t #'nm)]
         [*maker (format-id #'nm "*~a" #'nm)]
         [**maker (format-id #'nm "**~a" #'nm)]
         [*maker-cnt (if enable-contracts?
                         (or (attribute cnt) #`((flds.cnt ...) #,(if name? #'(any/c) #'()) . ->* . pred))
                         #'any/c)]
         [ht-stx ht-stx]
         [bfs-fold-rhs (cond [(attribute fold-rhs)
                              #`(procedure-rename
                                 (lambda () #,#'fold-rhs.e)
                                 'fold-name)]
                             ;; otherwise we assume that everything is a type, 
                             ;; and recur on all the arguments
                             [else #'(procedure-rename
                                      (lambda () 
                                        #`(*maker (#,type-rec-id flds.i) ...))
                                      'fold-name)])]
         [provides (if (attribute no-provide?)
                       #'(begin)                                 
                       #`(begin 
                           (provide #;nm ex pred acc ...)
                           (p/c (rename *maker maker *maker-cnt))))]
         [intern 
          (let ([mk (lambda (int) 
                      (if (and key? name?)
                          #`(defintern (**maker name-val . flds.fs) maker #,int #:extra-arg #,(attribute key-expr))
                          #`(defintern (**maker . flds.fs) maker #,int)))])
            (syntax-parse #'flds.fs
                          [_  #:fail-unless (attribute intern?) #f
                              (mk #'intern?)]
                          [() (mk #'#f)]
                          [(f) (mk #'f)]
                          [_ (mk #'(list . flds.fs))]))]
         [(ign-pats ...) (if (and name? key?) #'(_ _ _) #'(_))]
         [frees-def (if (attribute frees) #'frees.def #'(begin))]
         [frees 
          (with-syntax ([(f1 f2) (if (attribute frees)
                                     #'(frees.f1 frees.f2)
                                     (list (combiner #'free-vars* #'flds.fs)
                                           (combiner #'free-idxs* #'flds.fs)))]
                        [(fs ...) #'flds.fs]
                        [name-val-formal
                         (if name? #'([name-val #f]) #'())]
                        [name-val-expr (if name? #'(name-val) #'())])
            (quasisyntax/loc stx
		(w/c nm ([*maker *maker-cnt])
                   (define (*maker fs ... #,@#'name-val-formal)
                     (define v (**maker #,@#'name-val-expr fs ...))
                     frees-def
                     (unless-in-table 
                      var-table v
                      (define fvs f1)
                      (define fis f2)
                      (hash-set! var-table v fvs)
                      (hash-set! index-table v fis))
                     v))))])
        #`(begin
            (define-struct (nm parent) flds.fs #:inspector #f)
            (define-match-expander ex
              (lambda (s)
                (syntax-parse s 
                  [(_ . fs) 
                   #:with pat (syntax/loc s (ign-pats ... . fs))
                   (syntax/loc s (struct nm pat))])))
            (begin-for-syntax
              (hash-set! ht-stx 'kw-stx (list #'ex #'flds.fs bfs-fold-rhs #'#,stx)))
            intern
            provides
            frees))])))

(define-for-syntax (mk-fold ht type-rec-id rec-ids kws)
  (lambda (stx)
    (define new-ht (hash-copy ht))
    (define (mk-matcher kw) 
      (datum->syntax stx (string->symbol (string-append (keyword->string kw) ":"))))
    (define/contract (put k lst)
      (keyword? (list/c syntax?
                        syntax?
                        (-> syntax?)
                        syntax?) 
                . -> . void?)
      (hash-set! new-ht k lst))
    (define (add-clause cl)
      (syntax-parse cl
        [(kw:keyword #:matcher mtch pats ... expr)
         (put (syntax-e #'kw) (list #'mtch 
                                    (syntax/loc cl (pats ...))
                                    (lambda () #'expr)
                                    cl))]
        [(kw:keyword pats ... expr) 
         (put (syntax-e #'kw) (list (mk-matcher (syntax-e #'kw)) 
                                    (syntax/loc cl (pats ...))
                                    (lambda () #'expr)
                                    cl))]))
    (define-syntax-class clause
      (pattern  
       (k:keyword #:matcher mtch pats ... e:expr)
       #:attr kw (attribute k.datum)
       #:attr val (list #'mtch 
                        (syntax/loc this-syntax (pats ...))
                        (lambda () #'e)
                        this-syntax))
      (pattern
       (k:keyword pats ... e:expr) 
       #:attr kw (syntax-e #'k)
       #:attr val (list (mk-matcher (attribute kw)) 
                        (syntax/loc this-syntax (pats ...))
                        (lambda () #'e)
                        this-syntax)))
    (define (gen-clause k v)
      (match v
        [(list match-ex pats body-f src)
         (let ([pat (quasisyntax/loc src (#,match-ex  . #,pats))])
           (quasisyntax/loc src (#,pat #,(body-f))))]))
    (define-syntax-class (keyword-in kws)
      #:attributes (datum)
      (pattern k:keyword
               #:fail-unless (memq (attribute k.datum) kws) #f
               #:attr datum (attribute k.datum)))
    (define-syntax-class (sized-list kws)
      #:description (format "keyword expr pairs matching with keywords in the list ~a" kws)
      (pattern ((~or (~seq k e:expr)) ...)
               #:declare k (keyword-in kws)
               #:fail-unless (equal? (length (attribute k.datum)) (length (remove-duplicates (attribute k.datum)))) #f
               #:attr mapping (for/hash ([k* (attribute k.datum)]
                                         [e* (attribute e)])
                                (values k* e*))
               ))
    (syntax-parse stx
      [(tc recs ty clauses:clause ...)
       #:declare recs (sized-list kws)
       (begin 
         (for ([k (attribute clauses.kw)]
               [v (attribute clauses.val)])
           (put k v))
         (with-syntax ([(let-clauses ...)
                        (for/list ([rec-id rec-ids]
                                   [k kws])
                          #`[#,rec-id #,(hash-ref (attribute recs.mapping) k
                                                  #'values)])])
           #`(let (let-clauses ...
                   [#,fold-target ty])
               ;; then generate the fold
               #,(quasisyntax/loc stx
                   (match #,fold-target
                     #,@(hash-map new-ht gen-clause))))))])))


(define-syntax (make-prim-type stx)
  (define default-flds #'(seq))
  (define-syntax-class type-name-base
    #:attributes (i lower-s first-letter key? (fld-names 1) name?)
    #:transparent
    (pattern i:id
             #:attr lower-s (string-downcase (symbol->string (attribute i.datum)))
             #:with (fld-names ...) default-flds
	     #:with key? #'#f
	     #:with name? #'#f
             #:attr first-letter (string-ref (attribute lower-s) 0))
    (pattern [i:id #:d d-name:id]
             #:with (fld-names ...) default-flds
             #:attr lower-s (string-downcase (symbol->string (attribute i.datum)))
	     #:with key? #'#f
	     #:with name? #'#f
             #:attr first-letter (symbol->string (attribute d-name.datum)))
    (pattern [i:id #:key (~optional (~and name-kw #:name))]
             #:with (fld-names ...) (datum->syntax #f (append (syntax->list default-flds) 
                                                              (list #'key)
                                                              (if (attribute name-kw)
                                                                  (list #'name)
                                                                  null)))
             #:attr lower-s (string-downcase (symbol->string (attribute i.datum)))
	     #:with key? #'#t
	     #:with name? (if (attribute name-kw) #'#t #'#f)
             #:attr first-letter (string-ref (attribute lower-s) 0)))
  (define-syntax-class type-name
    #:transparent
    #:auto-nested-attributes
    (pattern :type-name-base
             #:with name #'i
             #:with keyword (datum->syntax #f (string->keyword (symbol->string (syntax-e #'i))))
             #:with tmp-rec-id (generate-temporary)
             #:with case (format-id #'i "~a-case" (attribute lower-s))
             #:with printer (format-id #'i "print-~a*" (attribute lower-s))
             #:with ht (format-id #'i "~a-name-ht" (attribute lower-s))
             #:with rec-id (format-id #'i "~a-rec-id" (attribute lower-s))
             #:with d-id (format-id #'i "d~a" (attribute first-letter))
             #:with (_ _ pred? seq-acc accs ...)
                    (datum->syntax #f (build-struct-names #'name (syntax->list #'(fld-names ...)) #f #t #'name))))
  (syntax-parse stx
    [(_ i:type-name ...)
     (with-syntax* ([(fresh-ids ...) (generate-temporaries #'(i.name ...))]
                    [(default-ids ...) (generate-temporaries #'(i.name ...))]
                    [fresh-ids-list #'(fresh-ids ...)]
                    [(anys ...) (for/list ([i (syntax->list #'fresh-ids-list)]) #'any/c)])
       #'(begin
           (provide i.d-id ... i.printer ... i.name ... i.pred? ... i.seq-acc ... i.accs ... ...
                    (for-syntax i.ht ... i.rec-id ...))
           (define-syntax i.d-id (mk #'i.name #'i.ht i.key? i.name?)) ...
           (define-for-syntax i.ht (make-hasheq)) ...
           (define-struct/printer i.name (i.fld-names ...) (lambda (a b c) ((unbox i.printer) a b c))
             [prop:equal+hash (list (lambda (a b rec)
                                      (eq? (i.seq-acc a) (i.seq-acc b)))
                                    (lambda (a rec) (i.seq-acc a))
                                    (lambda (a secondary) (secondary a)))]) ...
           (define-for-syntax i.rec-id #'i.rec-id) ...   
           (provide i.case ...)           
           (define-syntaxes (i.case ...)
             (let ()               
               (apply values
                      (map (lambda (ht) 
                             (mk-fold ht 
                                      (car (list #'i.rec-id ...))
                                      (list #'i.rec-id ...)
                                      '(i.keyword ...)))
                           (list i.ht ...)))))))]))

(make-prim-type [Type #:key #:name]
                Filter
                [LatentFilter #:d lf]
                Object 
                [LatentObject #:d lo]
                [PathElem #:d pe])

;; free-variance.ss starts here:

(require "../utils/utils.ss")

(require (for-syntax scheme/base)
         (utils tc-utils)
         scheme/contract
         mzlib/etc)

;; this file contains support for calculating the free variables/indexes of types
;; actual computation is done in rep-utils.ss  and type-rep.ss

(define-values (Covariant Contravariant Invariant Constant Dotted)
  (let ()
    (define-struct Variance () #:inspector #f)
    (define-struct (Covariant Variance) () #:inspector #f)
    (define-struct (Contravariant Variance) () #:inspector #f)
    (define-struct (Invariant Variance) () #:inspector #f)
    (define-struct (Constant Variance) () #:inspector #f)
    ;; not really a variance, but is disjoint with the others
    (define-struct (Dotted Variance) () #:inspector #f)
    (values (make-Covariant) (make-Contravariant) (make-Invariant) (make-Constant) (make-Dotted))))

(define (variance? e)
  (memq e (list Covariant Contravariant Invariant Constant Dotted)))


(provide Covariant Contravariant Invariant Constant Dotted)

;; hashtables for keeping track of free variables and indexes
(define index-table (make-weak-hash))
;; maps Type to List[Cons[Number,Variance]]
(define var-table (make-weak-hash))
;; maps Type to List[Cons[Symbol,Variance]]

(define input/c (or/c Type? Filter? LatentFilter? Object? LatentObject? PathElem?))

(d/c (free-idxs* t)
     (->  input/c (hash/c integer? variance?))
     (hash-ref index-table t (lambda _ (int-err "type ~a not in index-table" t))))
(d/c (free-vars* t)
     (-> input/c  (hash/c symbol? variance?))
     (hash-ref var-table t (lambda _ (int-err "type ~a not in var-table ~a" t (take (reverse (hash-map var-table list)) 20)))))


(define empty-hash-table (make-immutable-hasheq null))

;; Type? is not available here! grrr
(p/c
 [free-vars* (-> input/c  (hash/c symbol? variance?))]
 [free-idxs* (-> input/c  (hash/c integer? variance?))])

(provide empty-hash-table)

;; frees = HT[Idx,Variance] where Idx is either Symbol or Number
;; (listof frees) -> frees
(define (combine-frees freess)    
  (define ht (make-hasheq))
  (define (combine-var v w)
    (cond
      [(eq? v w) v]
      [(eq? v Dotted) w]
      [(eq? w Dotted) v]
      [(eq? v Constant) w]
      [(eq? w Constant) v]
      [else Invariant]))
  (for* ([old-ht (in-list freess)]
         [(sym var) (in-hash old-ht)])
        (let* ([sym-var (hash-ref ht sym (lambda () #f))])
          (if sym-var
              (hash-set! ht sym (combine-var var sym-var))
              (hash-set! ht sym var))))
  ht)

;; given a set of free variables, change bound to ...
;; (if bound wasn't free, this will add it as Dotted
;;  appropriately so that things that expect to see
;;  it as "free" will -- fixes the case where the
;;  dotted pre-type base doesn't use the bound).
(define (fix-bound vs bound)
  (define vs* (hash-map* (lambda (k v) v) vs))
  (hash-set! vs* bound Dotted)
  vs*)

;; frees -> frees
(define (flip-variances vs)
  (hash-map* 
   (lambda (k v) 
     (evcase 
         v
       [Covariant Contravariant]
       [Contravariant Covariant]
       [v v]))
   vs))

(define (make-invariant vs)
  (hash-map* 
   (lambda (k v) Invariant)
   vs))

(define (hash-map* f ht)
  (define new-ht (make-hasheq))
  (for ([(k v) (in-hash ht)])
     (hash-set! new-ht k (f k v)))
  new-ht)

(define (without-below n frees)
  (define new-ht (make-hasheq))
  (for ([(k v) (in-hash frees)])
       (when (>= k n) (hash-set! new-ht k v)))
  new-ht)

(define table/c (hash/c (or/c integer? symbol?) variance?))

(p/c [combine-frees (-> (listof table/c) table/c)]
     [flip-variances (-> table/c table/c)]
     [make-invariant (-> table/c table/c)]
     [without-below (-> integer? table/c table/c)])

(provide unless-in-table var-table index-table empty-hash-table fix-bound)

(define-syntax (unless-in-table stx) 
  (syntax-case stx ()
    [(_ table val . body)
     (quasisyntax/loc stx
       (hash-ref table val #,(syntax/loc #'body (lambda () . body))))]))


;; free-variance.ss ends here
