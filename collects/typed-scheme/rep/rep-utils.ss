#lang scheme/base
(require "../utils/utils.ss")

(require mzlib/struct 
         mzlib/plt-match
         syntax/boundmap
         "free-variance.ss"
         "interning.ss"
         mzlib/etc
         scheme/contract
         (for-syntax 
          stxclass
          scheme/base
          syntax/struct
          syntax/stx
	  (rename-in (utils utils) [id mk-id])))

(provide == dt de print-type* print-effect* Type Type? Effect Effect? defintern hash-id Type-seq Effect-seq Type-key)



;; hash table for defining folds over types
(define-values-for-syntax (type-name-ht effect-name-ht)
  (values (make-hasheq) (make-hasheq)))

(provide (for-syntax type-name-ht effect-name-ht))


;; all types are Type?
(define-struct/printer Type (seq key) (lambda (a b c) ((unbox print-type*) a b c)))

(define-struct/printer Effect (seq key) (lambda (a b c) ((unbox print-effect*) a b c)))





;; type/effect definition macro

(define-for-syntax type-rec-id #'type-rec-id)
(define-for-syntax effect-rec-id #'effect-rec-id)
(define-for-syntax fold-target #'fold-target)

(define-for-syntax enable-contracts? #t)

(provide (for-syntax type-rec-id effect-rec-id fold-target))

(define-syntaxes (dt de)
  (let ()
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
      #:attributes (f1 f2)
      (pattern (f1:expr f2:expr))
      (pattern (#f)
               #:with f1 #'empty-hash-table
               #:with f2 #'empty-hash-table))
    (define-syntax-class fold-pat
      #:transparent
      #:attributes (e)
      (pattern #:base
               #:with e fold-target)
      (pattern ex:expr
	       #:with e #'#'ex))
    (define (mk par ht-stx)        
      (lambda (stx)          
        (syntax-parse stx 
          [(dform nm:id flds:idlist ([[#:key key-expr:expr]] #:opt 
				     [[#:intern intern?:expr]] #:opt
				     [[#:frees . frees:frees-pat]] #:opt
				     [[#:fold-rhs fold-rhs:fold-pat]] #:opt
                                     [[#:contract cnt:expr]] #:opt
				     [no-provide?:no-provide-kw] #:opt) ...*)
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
				(provide/contract (rename *maker maker *maker-cnt))))]
	      [intern 
	       (let ([mk (lambda (int) #`(defintern (**maker . flds.fs) maker #,int #:extra-arg key-expr))])
		 (syntax-parse #'flds.fs
		   [_ #:when #'intern?
		    (mk #'intern?)]
		   [() (mk #'#f)]
		   [(f) (mk #'f)]
		   [_ (mk #'(list . flds.fs))]))]
              [frees 
               (with-syntax ([(f1 f2) (if #'frees
                                          #'(frees.f1 frees.f2)
                                          (list (combiner #'free-vars* #'flds.fs)
                                                (combiner #'free-idxs* #'flds.fs)))])
                 (quasisyntax/loc stx
                   (with-contract nm ([*maker *maker-cnt])
                     (define (*maker . flds.fs)
                       (define v (**maker . flds.fs))
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
  (values (mk #'Type #'type-name-ht) (mk #'Effect #'effect-name-ht))))

