#lang scheme/base
(require "../utils/utils.ss")

(require mzlib/struct 
         mzlib/plt-match
         syntax/boundmap
         "free-variance.ss"
         "interning.ss"
         mzlib/etc
         scheme/contract
         (for-meta 1 stxclass/util)
         (for-syntax 
          stxclass
          scheme/base
          syntax/struct
          syntax/stx
	  (rename-in (utils utils) [id mk-id])))

(provide == defintern hash-id (for-syntax fold-target))

(define-for-syntax fold-target #'fold-target)

(define-for-syntax (mk par ht-stx key?)
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
  (define-syntax-class frees-pat
    #:transparent
    #:attributes (f1 f2 def)
    (pattern (f1:expr f2:expr)
             #:with def #'(begin))
    (pattern (#f)
             #:with f1 #'empty-hash-table
             #:with f2 #'empty-hash-table
             #:with def #'(begin))
    (pattern (e:expr)
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
  (lambda (stx)          
    (syntax-parse stx 
      [(dform nm:id flds:idlist (~or [[#:key key-expr:expr]] #:opt 
                                     [[#:intern intern?:expr]] #:opt
                                     [[#:frees . frees:frees-pat]] #:opt
                                     [[#:fold-rhs fold-rhs:fold-pat]] #:opt
                                     [[#:contract cnt:expr]] #:opt
                                     [no-provide?:no-provide-kw] #:opt) ...)
       (with-syntax* 
        ([ex (mk-id #'nm #'nm ":")]
         [kw-stx (string->keyword (symbol->string #'nm.datum))]
         [parent par]
         [(s:ty maker pred acc ...) (build-struct-names #'nm (syntax->list #'flds.fs) #f #t #'nm)]
         [*maker (mk-id #'nm "*" #'nm)]
         [**maker (mk-id #'nm "**" #'nm)]
         [*maker-cnt (if enable-contracts?
                         (or #'cnt #'(flds.cnt ... . -> . pred))
                         #'any/c)]
         [ht-stx ht-stx]
         [bfs-fold-rhs (cond [#'fold-rhs #`(lambda (tr er) #,#'fold-rhs.e)]
                             [else #'(lambda (type-rec-id effect-rec-id) 
                                       #`(*maker (#,type-rec-id flds.i) ...))])]
         [provides (if #'no-provide?
                       #'(begin)                                 
                       #`(begin 
                           (provide ex pred acc ...)
                           (p/c (rename *maker maker *maker-cnt))))]
         [intern 
          (let ([mk (lambda (int) 
		      (if key? 
			  #`(defintern (**maker . flds.fs) maker #,int #:extra-arg key-expr)
			  #`(defintern (**maker . flds.fs) maker #,int)))])
         (syntax-parse #'flds.fs
              [_  #:when #'intern?
                 (mk #'intern?)]
              [() (mk #'#f)]
              [(f) (mk #'f)]
              [_ (mk #'(list . flds.fs))]))]
         [frees-def (if #'frees #'frees.def #'(begin))]
         [frees 
          (with-syntax ([(f1 f2) (if #'frees
                                     #'(frees.f1 frees.f2)
                                     (list (combiner #'free-vars* #'flds.fs)
                                           (combiner #'free-idxs* #'flds.fs)))])
            (quasisyntax/loc stx
		(w/c nm ([*maker *maker-cnt])
                   (define (*maker . flds.fs)
                     (define v (**maker . flds.fs))
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
                   #:with pat (syntax/loc s (_ _ . fs))
                   (syntax/loc s (struct nm pat))])))
            (begin-for-syntax
              (hash-set! ht-stx 'kw-stx (list #'ex #'flds.fs bfs-fold-rhs #'#,stx)))
            intern
            provides
            frees))])))


(define-syntax (make-prim-type stx)
  (define default-flds #'(seq))
  (define-syntax-class type-name-base
    #:attributes (i lower-s first-letter key? (fld-names 1))
    #:transparent
    (pattern i:id
             #:with lower-s (string-downcase (symbol->string #'i.datum))
             #:with (fld-names ...) default-flds
	     #:with key? #'#f
             #:with first-letter (string-ref #'lower-s 0))
    (pattern [i:id #:d d-name:id]
             #:with (fld-names ...) default-flds
             #:with lower-s (string-downcase (symbol->string #'i.datum))
	     #:with key? #'#f
             #:with first-letter (symbol->string #'d-name.datum))
    (pattern [i:id #:key]
             #:with (fld-names ...) (datum->syntax #f (append (syntax->list default-flds) 
                                                              (syntax->list #'(key))))
             #:with lower-s (string-downcase (symbol->string #'i.datum))
	     #:with key? #'#t
             #:with first-letter (string-ref #'lower-s 0)))
  (define-syntax-class type-name
    #:transparent
    (pattern :type-name-base
             #:with name #'i
             #:with tmp-rec-id (generate-temporary)
             #:with case (mk-id #'i #'lower-s "-case")
             #:with printer (mk-id #'i "print-" #'lower-s "*")
             #:with ht (mk-id #'i #'lower-s "-name-ht")
             #:with rec-id (mk-id #'i #'lower-s "-rec-id")
             #:with d-id (mk-id #'i "d" #'first-letter)
             #:with (_ _ pred? accs ...) 
                    (datum->syntax #f (build-struct-names #'name (syntax->list #'(fld-names ...)) #f #t #'name))))
  (syntax-parse stx
    [(_ i:type-name ...)
     (with-syntax* ([(fresh-ids ...) (generate-temporaries #'(i.name ...))]
                    [fresh-ids-list #'(fresh-ids ...)])
       #'(begin
           (provide i.d-id ... i.printer ... i.name ... i.pred? ... i.accs ... ...
                    (for-syntax i.ht ... i.rec-id ...))
           (define-syntax i.d-id (mk #'i.name #'i.ht i.key?)) ...
           (define-for-syntax i.ht (make-hasheq)) ...
           (define-struct/printer i.name (i.fld-names ...) (lambda (a b c) ((unbox i.printer) a b c))) ...
           (define-for-syntax i.rec-id #'i.rec-id) ...   
           (provide i.case ...)
           (define-syntaxes (i.case ...)
             (let ()
               (define (mk ht)
                 (lambda (stx)
                   (let ([ht (hash-copy ht)])
                     (define (mk-matcher kw) 
                       (datum->syntax stx (string->symbol (string-append (keyword->string kw) ":"))))
                     (define (add-clause cl)
                       (...
                        (syntax-case cl ()
                          [(kw #:matcher mtch pats ... expr)
                           (hash-set! ht (syntax-e #'kw) (list #'mtch 
                                                               (syntax/loc cl (pats ...))
                                                               (lambda fresh-ids-list #'expr)
                                                               cl))]
                          [(kw pats ... expr) 
                           (hash-set! ht (syntax-e #'kw) (list (mk-matcher (syntax-e #'kw)) 
                                                               (syntax/loc cl (pats ...))
                                                               (lambda fresh-ids-list #'expr)
                                                               cl))])))
                     (define i.tmp-rec-id i.rec-id) ...
                     (define (gen-clause k v)
                       (define match-ex (car v))
                       (define pats (cadr v))
                       (define body-f (caddr v))
                       (define src (cadddr v))
                       (define pat (quasisyntax/loc src (#,match-ex  . #,pats)))
                       (define cl (quasisyntax/loc src (#,pat #,(body-f i.tmp-rec-id ...))))
                       cl)
                     (syntax-case stx ()
                       [(tc fresh-ids ... ty . clauses)
                        (begin 
                          (map add-clause (syntax->list #'clauses))
                          (with-syntax ([old-rec-id type-rec-id])
                            #`(let ([#,i.tmp-rec-id fresh-ids] ...
                                    [#,fold-target ty])
                                ;; then generate the fold
                                #,(quasisyntax/loc stx
                                    (match #,fold-target
                                      #,@(hash-map ht gen-clause))))))]))))
               (apply values
                      (map mk (list i.ht ...)))))))]))

(make-prim-type [Type #:key] Filter [LatentFilter #:d lf] Object [LatentObject #:d lo]
                [PathElem #:d pe])
