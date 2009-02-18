#lang scheme/base
(require "../utils/utils.ss")

(require mzlib/struct 
         mzlib/plt-match
         syntax/boundmap
         "free-variance.ss"
         "interning.ss"
         mzlib/etc
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

(provide (for-syntax type-rec-id effect-rec-id fold-target))

(define-syntaxes (dt de)
  (let ()
    (define-syntax-class no-provide-kw
      (pattern #:no-provide))
    (define-syntax-class idlist
      (pattern (i:id ...)))
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
          [(dform nm:id flds:idlist (~or [[#:key key-expr:expr]] #:opt 
                                         [[#:intern intern?:expr]] #:opt
                                         [[#:frees . frees:frees-pat]] #:opt
                                         [[#:fold-rhs fold-rhs:fold-pat]] #:opt
                                         [no-provide?:no-provide-kw] #:opt) ...)
	   (with-syntax* 
	     ([ex (mk-id #'nm #'nm ":")]
	      [kw-stx (string->keyword (symbol->string #'nm.datum))]
	      [parent par]
	      [(s:ty maker pred acc ...) (build-struct-names #'nm (syntax->list #'flds) #f #t #'nm)]
	      [*maker (mk-id #'nm "*" #'nm)]
	      [**maker (mk-id #'nm "**" #'nm)]
	      [ht-stx ht-stx]
	      [bfs-fold-rhs (cond [#'fold-rhs #`(lambda (tr er) #,#'fold-rhs.e)]
				  [else #'(lambda (type-rec-id effect-rec-id) 
					    #`(*maker (#,type-rec-id flds.i) ...))])]
	      [provides (if #'no-provide?
			    #'(begin)                                 
			    #`(begin 
				(provide ex pred acc ...)
				(provide (rename-out [*maker maker]))))]
	      [intern 
	       (let ([mk (lambda (int) #`(defintern (**maker . flds) maker #,int #:extra-arg key-expr))])
		 (syntax-parse #'flds
		   [_ #:when #'intern?
		    (mk #'intern?)]
		   [() (mk #'#f)]
		   [(f) (mk #'f)]
		   [_ (mk #'(list . flds))]))]
	      [frees 
	       (with-syntax ([(f1 f2) (if #'frees
					  #'(frees.f1 frees.f2)
					  (list (combiner #'free-vars* #'flds)
						(combiner #'free-idxs* #'flds)))])
			    (quasisyntax/loc stx
					     (define (*maker . flds)
					       (define v (**maker . flds))
					       (unless-in-table 
						var-table v
						(define fvs f1)
						(define fis f2)
						(hash-set! var-table v fvs)
						(hash-set! index-table v fis))
					       v)))])
	     #`(begin
		 (define-struct (nm parent) flds #:inspector #f)
		 (define-match-expander ex
		   (lambda (s)
		     (syntax-parse s 
		       [(_ . fs) 
			#:with pat (syntax/loc s (_ _ . fs))
			(syntax/loc s (struct nm pat))])))
		 (begin-for-syntax
		  (hash-set! ht-stx 'kw-stx (list #'ex #'flds bfs-fold-rhs #'#,stx)))
		 intern
		 provides
		 frees))])))
  (values (mk #'Type #'type-name-ht) (mk #'Effect #'effect-name-ht))))

