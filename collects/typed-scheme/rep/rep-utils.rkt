#lang scheme/base
(require "../utils/utils.rkt")

(require mzlib/struct mzlib/pconvert
         scheme/match
         syntax/boundmap
         "free-variance.rkt"
         "interning.rkt"
	 unstable/syntax unstable/match unstable/struct
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

(define-struct Rep (seq free-vars free-idxs stx) #:transparent)

(define-for-syntax fold-target #'fold-target)
(define-for-syntax default-fields (list #'seq #'free-vars #'free-idxs #'stx))

(define-for-syntax (mk par ht-stx key?)
  (define-syntax-class opt-cnt-id
    #:attributes (i cnt)
    (pattern i:id
             #:with cnt #'any/c)
    (pattern [i:id cnt]))  
  ;; fields
  (define-syntax-class (idlist name)
    #:attributes ((i 1) (cnt 1) fs maker pred (acc 1))
    (pattern (oci:opt-cnt-id ...)               
             #:with (i ...) #'(oci.i ...)
             #:with (cnt ...) #'(oci.cnt ...)
             #:with fs #'(i ...)
             #:with (_ maker pred acc ...) (build-struct-names name (syntax->list #'fs) #f #t name)))
  
  (define (combiner f flds)
    (syntax-parse flds
      [() #'#hasheq()]
      [(e) #`(#,f e)]
      [(e ...) #`(combine-frees (list (#,f e) ...))]))
  (define-splicing-syntax-class frees-pat
    #:transparent
    #:attributes (f1 f2)
    (pattern (~seq f1:expr f2:expr))
    (pattern #f
             #:with f1 #'#hasheq()
             #:with f2 #'#hasheq())
    (pattern e:expr
             #:with f1 #'(e Rep-free-vars)
             #:with f2 #'(e Rep-free-idxs)))
  (define-syntax-class (fold-pat fold-name)
    #:transparent
    #:attributes (e proc)
    (pattern #:base
             #:with e fold-target
             #:with proc #`(procedure-rename
                            (lambda () #,fold-target)
                            '#,fold-name))
    (pattern ex:expr
             #:with e #'#'ex
             #:with proc #`(procedure-rename
                            (lambda () #'ex)
                            '#,fold-name)))
  (define-syntax-class form-nm
    (pattern nm:id
             #:with ex (format-id #'nm "~a:" #'nm)
             #:with fold (format-id #f "~a-fold" #'nm)
             #:with kw (string->keyword (symbol->string (syntax-e #'nm)))
             #:with *maker (format-id #'nm "*~a" #'nm)))
  (lambda (stx)          
    (syntax-parse stx 
      [(dform nm:form-nm
              (~var flds (idlist #'nm))
              (~or 
               (~optional (~and (~fail #:unless key? "#:key not allowed")
                                [#:key key-expr:expr])
                          #:defaults ([key-expr #'#f]))
               (~optional [#:intern intern?:expr]
                          #:defaults
                          ([intern? (syntax-parse #'flds.fs
                                      [() #'#f]
                                      [(f) #'f]
                                      [(fs ...) #'(list fs ...)])]))
               (~optional [#:frees frees:frees-pat]
                          #:defaults 
                          ([frees.f1 (combiner #'Rep-free-vars #'flds.fs)]
                           [frees.f2 (combiner #'Rep-free-idxs #'flds.fs)]))
               (~optional [#:fold-rhs (~var fold-rhs (fold-pat #'nm.fold))]
                          #:defaults
                          ([fold-rhs.proc
                            #'(procedure-rename
                               (lambda () 
                                 #`(nm.*maker (#,type-rec-id flds.i) ...))
                               'nm.fold)]))
               (~optional [#:contract cnt:expr]
                          #:defaults ([cnt #'((flds.cnt ...) (#:syntax (or/c syntax? #f)) . ->* . flds.pred)]))
               (~optional (~and #:no-provide no-provide?))) ...)
       (with-syntax
        ([(ign-pats ...) (append (map (lambda (x) #'_) default-fields) (if key? (list #'_) (list)))]
         ;; has to be down here to refer to #'cnt
         [provides (if (attribute no-provide?)
                       #'(begin)
                       #'(begin 
                           (provide nm.ex flds.pred flds.acc ...)
                           (p/c (rename nm.*maker flds.maker cnt))))])
        #`(begin
            (define-struct (nm #,par) flds.fs #:inspector #f)
            (define-match-expander nm.ex
              (lambda (s)
                (syntax-parse s 
                  [(_ . fs) 
                   #:with pat (syntax/loc s (ign-pats ... . fs))
                   (syntax/loc s (struct nm pat))])))
            (begin-for-syntax
              (hash-set! #,ht-stx 'nm.kw (list #'nm.ex #'flds.fs fold-rhs.proc #f)))
            #,(quasisyntax/loc stx
                (w/c nm ([nm.*maker cnt])
                     #,(quasisyntax/loc #'nm
                         (defintern (nm.*maker . flds.fs) flds.maker intern?
                           #:extra-args
                           frees.f1 frees.f2 #:syntax [orig-stx #f]
                           #,@(if key? (list #'key-expr) null)))))
            provides))])))

(define-for-syntax (mk-fold ht type-rec-id rec-ids kws)
  (lambda (stx)
    (define new-ht (hash-copy ht))    
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
       #:attr val (list (format-id stx "~a:" (attribute kw))
                        (syntax/loc this-syntax (pats ...))
                        (lambda () #'e)
                        this-syntax)))
    (define (gen-clause k v)
      (match v
        [(list match-ex pats body-f src)
         (let ([pat (quasisyntax/loc (or stx stx) (#,match-ex  . #,pats))])
           (quasisyntax/loc (or src stx) (#,pat #,(body-f))))]))
    (define-syntax-class (keyword-in kws)
      #:attributes (datum)
      (pattern k:keyword
               #:fail-unless (memq (attribute k.datum) kws) (format "expected keyword in ~a" kws)
               #:attr datum (attribute k.datum)))
    (define-syntax-class (sized-list kws)
      #:description (format "keyword expr pairs matching with keywords in the list ~a" kws)
      (pattern ((~or (~seq (~var k (keyword-in kws)) e:expr)) ...)
               #:when (equal? (length (attribute k.datum)) 
                              (length (remove-duplicates (attribute k.datum))))
               #:attr mapping (for/hash ([k* (attribute k.datum)]
                                         [e* (attribute e)])
                                (values k* e*))))
    (syntax-parse stx
      [(tc (~var recs (sized-list kws)) ty clauses:clause ...)
       (for ([k (attribute clauses.kw)]
             [v (attribute clauses.val)])
         (hash-set! new-ht k v))
       (with-syntax ([(let-clauses ...)
                      (for/list ([rec-id rec-ids]
                                 [k kws])
                        #`[#,rec-id #,(hash-ref (attribute recs.mapping) k
                                                #'values)])]
                     [(match-clauses ...)
                      (hash-map new-ht gen-clause)]
                     [error-msg (quasisyntax/loc stx (error 'tc "no pattern for ~a" #,fold-target))])
         #`(let (let-clauses ...
                 [#,fold-target ty])
             ;; then generate the fold
             #,(quasisyntax/loc stx
                 (match #,fold-target
                   match-clauses ...
                   [_ error-msg]))))])))


(define-syntax (make-prim-type stx)    
  (define-syntax-class type-name-base
    #:attributes (i d-id key? (fld-names 1))
    #:transparent
    (pattern [i:id (~optional (~and #:key
                                    (~bind [key? #'#t]
                                           [(fld-names 1) (list #'key)]))
                              #:defaults ([key? #'#f]
                                          [(fld-names 1) null]))
                   #:d d-id:id]))
  (define-syntax-class type-name
    #:transparent
    #:auto-nested-attributes
    (pattern :type-name-base
             #:with lower-s (string->symbol (string-downcase (symbol->string (syntax-e #'i))))
             #:with name #'i
             #:with keyword (string->keyword (symbol->string (syntax-e #'i)))
             #:with tmp-rec-id (generate-temporary)
             #:with case (format-id #'i "~a-case" #'lower-s)
             #:with printer (format-id #'i "print-~a*" #'lower-s)
             #:with ht (format-id #'i "~a-name-ht" #'lower-s)
             #:with rec-id (format-id #'i "~a-rec-id" #'lower-s)
             #:with (_ _ pred? accs ...)
                    (build-struct-names #'name (syntax->list #'(fld-names ...)) #f #t #'name)))
  (syntax-parse stx
    [(_ i:type-name ...)
     #'(begin
         (provide i.d-id ... i.printer ... i.name ... i.pred? ... i.accs ... ...
                  (for-syntax i.ht ... i.rec-id ...))
         (define-syntax i.d-id (mk #'i.name #'i.ht i.key?)) ...
         (define-for-syntax i.ht (make-hasheq)) ...
         (define-struct/printer (i.name Rep) (i.fld-names ...) (lambda (a b c) ((unbox i.printer) a b c))) ...
         (define-for-syntax i.rec-id #'i.rec-id) ...   
         (provide i.case ...)           
         (define-syntaxes (i.case ...)
           (let ()               
             (apply values
                    (map (lambda (ht) 
                           (define rec-ids (list i.rec-id ...))
                           (mk-fold ht 
                                    (car rec-ids)
                                    rec-ids
                                    '(i.keyword ...)))
                         (list i.ht ...))))))]))

(make-prim-type [Type #:key #:d dt]
                [Filter #:d df]
                [Object #:d do] 
                [PathElem #:d dpe])

(provide PathElem? (rename-out [Rep-seq Type-seq]
                               [Rep-free-vars free-vars*]
                               [Rep-free-idxs free-idxs*]))

(p/c (struct Rep ([seq exact-nonnegative-integer?] 
                  [free-vars (hash/c symbol? variance?)]                   
                  [free-idxs (hash/c symbol? variance?)]
                  [stx (or/c #f syntax?)]))
     [replace-syntax (Rep? syntax? . -> . Rep?)])

(define (replace-field val new-val idx)
  (define-values (type skipped) (struct-info val))
  (define maker (struct-type-make-constructor type))
  (define flds (struct->list val))
  (apply maker (list-set flds idx new-val)))

(define (replace-syntax rep stx)
  (replace-field rep stx 3))

(define (converter v basic sub)
  (define (gen-constructor sym)
    (string->symbol (string-append "make-" (substring (symbol->string sym) 7))))
  (match v
    [(? (lambda (e) (or (Filter? e)
                        (Object? e)
                        (PathElem? e)))
        (app (lambda (v) (vector->list (struct->vector v))) (list-rest tag seq fv fi stx vals)))
     `(,(gen-constructor tag) ,@(map sub vals))]
    [(? Type?
        (app (lambda (v) (vector->list (struct->vector v))) (list-rest tag seq fv fi stx key vals)))
     `(,(gen-constructor tag) ,@(map sub vals))]
    [_ (basic v)]))

(current-print-convert-hook converter)