#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

add struct contracts for immutable structs?

|#

(module contract mzscheme
  
  ;; these are only used for building other (presumably experimental)
  ;; contract combinators outside this library. They are not provided
  ;; from ../contract.ss
  (provide make-contract
           contract-proc
           raise-contract-error
           build-compound-type-name
           coerce-contract)
  
  (provide (rename -contract contract)
           any
           recursive-contract
           ->
           ->d
           ->*
           ->d*
           ->r
           ->pp
           ->pp-rest
           case->
	   opt->
           opt->*
           object-contract
           provide/contract
           define/contract
	   contract?
           contract-name
           flat-contract?
           flat-contract
           flat-contract-predicate
           flat-named-contract)

  (require-for-syntax mzscheme
                      "../list.ss"
                      (lib "stx.ss" "syntax")
                      (lib "name.ss" "syntax"))
  
  (require "class-internal.ss"
           "../etc.ss"
           "../list.ss"
           "../pretty.ss")
  
  (require "contract-helpers.scm")
  (require-for-syntax (prefix a: "contract-helpers.scm"))

  
  
;                                                                                                    
;                                                                                                    
;                                                                                                    
;       ;           ;;; ;                     ;                                                      
;       ;          ;                          ;                                                      
;       ;          ;                         ;                           ;                       ;   
;    ;; ;    ;;;  ;;;;  ;   ; ;;     ;;;     ;     ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;  ;;   ;   ;  ;    ;   ;;  ;   ;   ;    ;    ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;  ;    ;  ;    ;  ;    ;   ;   ;  ;    ;    ;   ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;  ;    ;  ;;;;;;  ;    ;   ;   ;  ;;;;;;   ;    ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;  ;    ;  ;       ;    ;   ;   ;  ;        ;    ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;  ;;   ;      ;    ;   ;   ;   ;       ;     ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;    ;; ;    ;;;;  ;    ;   ;   ;    ;;;;  ;       ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;                                          ;                                                         
;                                          ;                                                         
;                                                                                                    


  (define-for-syntax (make-define/contract-transformer contract-id id)
    (make-set!-transformer
     (lambda (stx)
       (with-syntax ([neg-blame-str (or (a:build-src-loc-string stx) "")]
		     [contract-id contract-id]
		     [id id])
	 (syntax-case stx (set!)
	   [(set! _ arg) 
	    (raise-syntax-error 'define/contract
				"cannot set! a define/contract variable" 
				stx 
				(syntax _))]
	   [(_ arg ...)
	    (syntax/loc stx
	      ((-contract contract-id
			  id
			  (syntax-object->datum (quote-syntax _))
			  (string->symbol neg-blame-str)
			  (quote-syntax _))
	       arg
	       ...))]
	   [_
	    (identifier? (syntax _))
	    (syntax/loc stx
	      (-contract contract-id
			 id  
			 (syntax-object->datum (quote-syntax _)) 
			 (string->symbol neg-blame-str)
			 (quote-syntax _)))])))))

  (define-for-syntax (make-provide/contract-transformer contract-id id pos-module-source)
    (make-set!-transformer
     (lambda (stx)
       (with-syntax ([neg-stx (datum->syntax-object stx 'here)]
		     [contract-id contract-id]
		     [id id]
		     [pos-module-source pos-module-source])
	 (syntax-case stx (set!)
	   [(set! _ body) (raise-syntax-error
			   #f 
			   "cannot set! provide/contract identifier"
			   stx
			   (syntax _))]
	   [(_ arg ...)
	    (syntax 
	     ((begin-lifted
	       (-contract contract-id
			  id
			  pos-module-source
			  (module-source-as-symbol #'neg-stx)
			  (quote-syntax _)))
	      arg
	      ...))]
	   [_
	    (identifier? (syntax _))
	    (syntax 
	     (begin-lifted
	      (-contract contract-id
			 id  
			 pos-module-source 
			 (module-source-as-symbol #'neg-stx)
			 (quote-syntax _))))])))))
  
  ;; (define/contract id contract expr)
  ;; defines `id' with `contract'; initially binding
  ;; it to the result of `expr'.  These variables may not be set!'d.
  (define-syntax (define/contract define-stx)
    (syntax-case define-stx ()
      [(_ name contract-expr expr)
       (identifier? (syntax name))
       (with-syntax ([contract-id 
                      (a:mangle-id define-stx
                                   "define/contract-contract-id"
                                   (syntax name))]
                     [id (a:mangle-id define-stx
                                      "define/contract-id"
                                      (syntax name))])
         (syntax/loc define-stx 
          (begin
            (define contract-id contract-expr)
            (define-syntax name
	      (make-define/contract-transformer (quote-syntax contract-id)
						(quote-syntax id)))
            (define id (let ([name expr]) name))  ;; let for procedure naming
            )))]
      [(_ name contract-expr expr)
       (raise-syntax-error 'define/contract "expected identifier in first position"
                           define-stx
                           (syntax name))])) 
  
  
;                                                                                                            
;                                                                                                            
;                                                                                                            
;                               ;       ;             ;                                                      
;                                       ;             ;                                                      
;                                       ;            ;                           ;                       ;   
;   ; ;;    ; ;   ;;;   ;     ; ;    ;; ;    ;;;     ;     ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;;  ;   ;;   ;   ;   ;   ;  ;   ;  ;;   ;   ;    ;    ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;   ;    ;  ;   ;     ;  ;   ;  ;  ;    ;  ;    ;    ;   ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;   ;    ;  ;   ;     ;   ; ;   ;  ;    ;  ;;;;;;   ;    ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;   ;    ;  ;   ;     ;   ; ;   ;  ;    ;  ;        ;    ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;;  ;   ;    ;   ;     ;    ;   ;  ;;   ;       ;     ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;   ; ;;    ;     ;;;      ;    ;    ;; ;    ;;;;  ;       ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;   ;                                              ;                                                         
;   ;                                              ;                                                         
;   ;                                                                                                        

  
  ;; (provide/contract p/c-ele ...)
  ;; p/c-ele = (id expr) | (rename id id expr) | (struct (id expr) ...)
  ;; provides each `id' with the contract `expr'.
  (define-syntax (provide/contract provide-stx)
    (syntax-case provide-stx (struct)
      [(_ p/c-ele ...)
       (let ()
         
         ;; code-for-each-clause : (listof syntax) -> (listof syntax)
         ;; constructs code for each clause of a provide/contract
         (define (code-for-each-clause clauses)
           (cond
             [(null? clauses) null]
             [else
              (let ([clause (car clauses)])
                (syntax-case clause (struct rename)
                  [(rename this-name new-name contract)
                   (and (identifier? (syntax this-name))
                        (identifier? (syntax new-name)))
                   (cons (code-for-one-id provide-stx (syntax this-name) (syntax contract) (syntax new-name))
                         (code-for-each-clause (cdr clauses)))]
                  [(rename this-name new-name contract)
                   (identifier? (syntax this-name))
                   (raise-syntax-error 'provide/contract 
                                       "malformed rename clause, expected an identifier" 
                                       provide-stx
                                       (syntax new-name))]
                  [(rename this-name new-name contract)
                   (identifier? (syntax new-name))
                   (raise-syntax-error 'provide/contract 
                                       "malformed rename clause, expected an identifier" 
                                       provide-stx
                                       (syntax this-name))]
                  [(rename . _)
                   (raise-syntax-error 'provide/contract "malformed rename clause" provide-stx clause)]
                  [(struct struct-name ((field-name contract) ...))
                   (and (well-formed-struct-name? (syntax struct-name))
                        (andmap identifier? (syntax->list (syntax (field-name ...)))))
                   (let ([sc (build-struct-code provide-stx
                                                (syntax struct-name)
                                                (syntax->list (syntax (field-name ...)))
                                                (syntax->list (syntax (contract ...))))])
                     (cons sc (code-for-each-clause (cdr clauses))))]
                  [(struct name)
                   (identifier? (syntax name))
                   (raise-syntax-error 'provide/contract
                                       "missing fields"
                                       provide-stx
                                       clause)]
                  [(struct name . rest)
                   (not (well-formed-struct-name? (syntax name)))
                   (raise-syntax-error 'provide/contract "name must be an identifier or two identifiers with parens around them"
                                       provide-stx
                                       (syntax name))]
                  [(struct name (fields ...))
                   (for-each (lambda (field)
                               (syntax-case field ()
                                 [(x y) 
                                  (identifier? (syntax x)) 
                                  (void)]
                                 [(x y) 
                                  (raise-syntax-error 'provide/contract
                                                      "malformed struct field, expected identifier"
                                                      provide-stx
                                                      (syntax x))]
                                 [else
                                  (raise-syntax-error 'provide/contract
                                                      "malformed struct field"
                                                      provide-stx
                                                      field)]))
                             (syntax->list (syntax (fields ...))))
                   
                   ;; if we didn't find a bad field something is wrong!
                   (raise-syntax-error 'provide/contract "internal error" provide-stx clause)]
                  [(struct name . fields)
                   (raise-syntax-error 'provide/contract
                                       "malformed struct fields"
                                       provide-stx
                                       clause)]
                  [(name contract)
                   (identifier? (syntax name))
                   (cons (code-for-one-id provide-stx (syntax name) (syntax contract) #f)
                         (code-for-each-clause (cdr clauses)))]
                  [(name contract)
                   (raise-syntax-error 'provide/contract
                                       "expected identifier"
                                       provide-stx
                                       (syntax name))]
                  [unk
                   (raise-syntax-error 'provide/contract
                                       "malformed clause"
                                       provide-stx
                                       (syntax unk))]))]))
         
         ;; well-formed-struct-name? : syntax -> bool
         (define (well-formed-struct-name? stx)
           (or (identifier? stx)
               (syntax-case stx ()
                 [(name super)
                  (and (identifier? (syntax name))
                       (identifier? (syntax super)))
                  #t]
                 [else #f])))
         
         ;; build-struct-code : syntax syntax (listof syntax) (listof syntax) -> syntax
         ;; constructs the code for a struct clause
         ;; first arg is the original syntax object, for source locations
         (define (build-struct-code stx struct-name-position field-names field-contracts)
           (let* ([struct-name (syntax-case struct-name-position ()
                                 [(a b) (syntax a)]
                                 [else struct-name-position])]
                  [super-id (syntax-case struct-name-position ()
                                [(a b) (syntax b)]
                                [else #t])]
                  [struct-info (extract-struct-info struct-name-position)]
                  [constructor-id (list-ref struct-info 1)]
                  [predicate-id (list-ref struct-info 2)]
                  [selector-ids (reverse (list-ref struct-info 3))]
                  [mutator-ids (reverse (list-ref struct-info 4))]
                  [parent-struct-count (let ([parent-info (extract-parent-struct-info struct-name-position)])
					 (and parent-info
                                              (let ([fields (cadddr parent-info)])
                                                (cond
                                                  [(null? fields) 0]
                                                  [(not (car (last-pair fields)))
                                                   (raise-syntax-error 
                                                    'provide/contract
                                                    "cannot determine the number of fields in super struct"
                                                    provide-stx
                                                    struct-name)]
                                                  [else (length fields)]))))]
                  [field-contract-ids (map (lambda (field-name) 
                                             (a:mangle-id provide-stx
                                                          "provide/contract-field-contract"
                                                          field-name
                                                          struct-name))
                                           field-names)]
                  [struct:struct-name
                   (datum->syntax-object
                    struct-name
                    (string->symbol
                     (string-append
                      "struct:"
                      (symbol->string (syntax-e struct-name)))))]
                  
                  
                   
                   [is-new-id?
                    (λ (index)
                      (or (not parent-struct-count)
                          (parent-struct-count . <= . index)))])
             
             (let ([unknown-info
                    (lambda (what names)
                      (raise-syntax-error
                       'provide/contract
                       (format "cannot determine ~a, found ~s" what names)
                       provide-stx
                       struct-name))]
		   [is-id-ok?
		    (lambda (id i)
                      (if (or (not parent-struct-count)
			      (parent-struct-count . <= . i))
			  id
			  #t))])
               
               (unless constructor-id (unknown-info "constructor" constructor-id))
               (unless predicate-id (unknown-info "predicate" predicate-id))
               (unless (andmap/count is-id-ok? selector-ids)
		 (unknown-info "selectors"
			       (map (lambda (x) (if (syntax? x)
						    (syntax-object->datum x)
						    x))
						selector-ids)))
               (unless (andmap/count is-id-ok? mutator-ids)
		 (unknown-info "mutators"
			       (map (lambda (x) (if (syntax? x)
						    (syntax-object->datum x)
						    x))
				    mutator-ids))))
             
             (unless (equal? (length selector-ids)
                             (length field-contract-ids))
               (raise-syntax-error 'provide/contract
                                   (format "found ~a fields in struct, but ~a contracts"
                                           (length selector-ids)
                                           (length field-contract-ids))
                                   provide-stx
                                   struct-name))
             (unless (equal? (length mutator-ids)
                             (length field-contract-ids))
               (raise-syntax-error 'provide/contract
                                   (format "found ~a fields in struct, but ~a contracts"
                                           (length mutator-ids)
                                           (length field-contract-ids))
                                   provide-stx
                                   struct-name))
             
             (with-syntax ([((selector-codes selector-new-names) ...)
                            (filter
                             (lambda (x) x)
                             (map/count (lambda (selector-id field-contract-id index)
                                          (if (is-new-id? index)
                                              (code-for-one-id/new-name
                                               stx
                                               selector-id
                                               (build-selector-contract struct-name 
                                                                        predicate-id
                                                                        field-contract-id)
                                               #f)
                                              #f))
                                        selector-ids
                                        field-contract-ids))]
                           [(rev-selector-old-names ...)
                            (reverse
                             (filter
                              (lambda (x) x)
                              (map/count (lambda (selector-id index)
                                           (if (not (is-new-id? index))
                                               selector-id
                                               #f))
                                         selector-ids)))]
                           [((mutator-codes mutator-new-names) ...)
                            (filter
                             (lambda (x) x)
                             (map/count (lambda (mutator-id field-contract-id index)
                                          (if (is-new-id? index)
                                              (code-for-one-id/new-name stx
                                                                        mutator-id 
                                                                        (build-mutator-contract struct-name 
                                                                                                predicate-id
                                                                                                field-contract-id)
                                                                        #f)
                                              #f))
                                        mutator-ids
                                        field-contract-ids))]
                           [(rev-mutator-old-names ...)
                            (reverse
                             (filter
                              (lambda (x) x)
                              (map/count (lambda (mutator-id index)
                                           (if (not (is-new-id? index))
                                               mutator-id
                                               #f))
                                         mutator-ids)))]
                           [(predicate-code predicate-new-name)
                            (code-for-one-id/new-name stx predicate-id (syntax (-> any/c boolean?)) #f)]
                           [(constructor-code constructor-new-name)
                            (code-for-one-id/new-name
                             stx
                             constructor-id
                             (build-constructor-contract stx
                                                         field-contract-ids 
                                                         predicate-id)
                             #f)]
                           [(field-contracts ...) field-contracts]
                           [(field-contract-ids ...) field-contract-ids])
               (with-syntax ([(rev-selector-new-names ...) (reverse (syntax->list (syntax (selector-new-names ...))))]
                             [(rev-mutator-new-names ...) (reverse (syntax->list (syntax (mutator-new-names ...))))])
                 (with-syntax ([struct-code 
                                (with-syntax ([id-rename (a:mangle-id provide-stx 
                                                                      "provide/contract-struct-expandsion-info-id"
                                                                      struct-name)]
                                              [struct-name struct-name]
                                              [struct:struct-name struct:struct-name]
                                              [super-id (if (boolean? super-id)
                                                            super-id
                                                            (with-syntax ([super-id super-id])
                                                              (syntax ((syntax-local-certifier) #'super-id))))])
                                  (syntax (begin
                                            (provide (rename id-rename struct-name))
                                            (define-syntax id-rename
                                              (list-immutable ((syntax-local-certifier) #'struct:struct-name)
                                                              ((syntax-local-certifier) #'constructor-new-name)
                                                              ((syntax-local-certifier) #'predicate-new-name)
                                                              (list-immutable ((syntax-local-certifier) #'rev-selector-new-names) ...
                                                                              ((syntax-local-certifier) #'rev-selector-old-names) ...)
                                                              (list-immutable ((syntax-local-certifier) #'rev-mutator-new-names) ...
                                                                              ((syntax-local-certifier) #'rev-mutator-old-names) ...)
                                                              super-id)))))]
                               [struct:struct-name struct:struct-name])
                   (syntax/loc stx
                     (begin
                       struct-code
                       (define field-contract-ids field-contracts) ...
                       selector-codes ...
                       mutator-codes ...
                       predicate-code
                       constructor-code
                       (provide struct:struct-name))))))))
 
         ;; map/count : (X Y int -> Z) (listof X) (listof Y) -> (listof Z)
         #;
         (define (map/count f l1 l2)
           (let loop ([l1 l1]
                      [l2 l2]
                      [i 0])
             (cond
               [(and (null? l1) (null? l2)) '()]
               [(or (null? l1) (null? l2)) (error 'map/count "mismatched lists")]
               [else (cons (f (car l1) (car l2) i)
                           (loop (cdr l1)
                                 (cdr l2)
                                 (+ i 1)))])))
         
         (define (map/count f . ls)
           (let loop ([ls ls]
                      [i 0])
             (cond
               [(andmap null? ls) '()]
               [(ormap null? ls) (error 'map/count "mismatched lists")]
               [else (cons (apply f (append (map car ls) (list i)))
                           (loop (map cdr ls)
                                 (+ i 1)))])))

	 ;; andmap/count : (X Y int -> Z) (listof X) (listof Y) -> (listof Z)
         (define (andmap/count f l1)
           (let loop ([l1 l1]
                      [i 0])
             (cond
               [(null? l1) #t]
               [else (and (f (car l1) i)
			  (loop (cdr l1)
				(+ i 1)))])))
         
         ;; extract-parent-struct-info : syntax -> (union #f (list syntax syntax (listof syntax) ...))
         (define (extract-parent-struct-info stx)
           (syntax-case stx ()
             [(a b)
              (syntax-local-value 
               (syntax b)
               (lambda ()
                 (raise-syntax-error 'provide/contract
                                     "expected a struct name" 
                                     provide-stx
                                     (syntax b))))]
             [a #f]))
         
         ;; extract-struct-info : syntax -> (union #f (list syntax syntax (listof syntax) ...))
         (define (extract-struct-info stx)
           (let ([id (syntax-case stx ()
                       [(a b) (syntax a)]
                       [_ stx])])
             (syntax-local-value 
              id
              (lambda ()
                (raise-syntax-error 'provide/contract
                                    "expected a struct name" 
                                    provide-stx
                                    id)))))
         
         ;; build-constructor-contract : syntax (listof syntax) syntax -> syntax
         (define (build-constructor-contract stx field-contract-ids predicate-id)
           (with-syntax ([(field-contract-ids ...) field-contract-ids]
                         [predicate-id predicate-id])
             (syntax/loc stx
               (field-contract-ids 
                ...
                . -> . 
                (let ([predicate-id (lambda (x) (predicate-id x))]) predicate-id)))))
         
         ;; build-selector-contract : syntax syntax -> syntax
         ;; constructs the contract for a selector
         (define (build-selector-contract struct-name predicate-id field-contract-id)
           (with-syntax ([field-contract-id field-contract-id]
                         [predicate-id predicate-id])
             (syntax ((let ([predicate-id (lambda (x) (predicate-id x))]) predicate-id)
                      . -> .
                      field-contract-id))))
         
         ;; build-mutator-contract : syntax syntax -> syntax
         ;; constructs the contract for a selector
         (define (build-mutator-contract struct-name predicate-id field-contract-id)
           (with-syntax ([field-contract-id field-contract-id]
                         [predicate-id predicate-id])
             (syntax ((let ([predicate-id (lambda (x) (predicate-id x))]) predicate-id)
                      field-contract-id
                      . -> .
                      void?))))
         
         ;; code-for-one-id : syntax syntax syntax (union syntax #f) -> syntax
         ;; given the syntax for an identifier and a contract,
         ;; builds a begin expression for the entire contract and provide
         ;; the first syntax object is used for source locations
         (define (code-for-one-id stx id ctrct user-rename-id)
           (with-syntax ([(code id) (code-for-one-id/new-name stx id ctrct user-rename-id)])
             (syntax code)))
         
         ;; code-for-one-id/new-name : syntax syntax syntax (union syntax #f) -> (values syntax syntax)
         ;; given the syntax for an identifier and a contract,
         ;; builds a begin expression for the entire contract and provide
         ;; the first syntax object is used for source locations
         (define (code-for-one-id/new-name stx id ctrct user-rename-id)
           (with-syntax ([id-rename (a:mangle-id provide-stx "provide/contract-id" id)]
                         [contract-id (a:mangle-id provide-stx "provide/contract-contract-id" id)]
                         [pos-module-source (a:mangle-id provide-stx "provide/contract-pos-module-source" id)]
                         [pos-stx (datum->syntax-object provide-stx 'here)]
                         [id id]
                         [ctrct ctrct]
                         [external-name (or user-rename-id id)])
             (with-syntax ([code
                            (syntax/loc stx
                              (begin
                                (provide (rename id-rename external-name))
                                
                                ;; unbound id check
                                (if #f id)
                                
                                (define pos-module-source (module-source-as-symbol #'pos-stx))
                                (define contract-id (let ([id ctrct]) id))
                                (define-syntax id-rename
				  (make-provide/contract-transformer (quote-syntax contract-id)
								     (quote-syntax id)
								     (quote-syntax pos-module-source)))))])
               (syntax (code id-rename)))))
         
         (with-syntax ([(bodies ...) (code-for-each-clause (syntax->list (syntax (p/c-ele ...))))])
           (syntax 
            (begin
              bodies ...))))]))
  
  
  ;                                                      
  ;                                                      
  ;                                                      
  ;                                                      
  ;                                                      
  ;                          ;                       ;   
  ;    ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
  ;   ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
  ;  ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
  ;  ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
  ;  ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
  ;   ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
  ;    ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
  ;                                                      
  ;                                                      
  ;                                                      
  
  ;; contract = (make-contract sexp
  ;;                           (sym
  ;;                            sym
  ;;                            (union syntax #f)
  ;;                            string
  ;;                            ->
  ;;                            (alpha -> alpha)))
  ;; the first arg to make-contract builds the name of the contract. The
  ;; path records how the violation occurs
  ;;
  ;; generic contract container; 
  ;; the first arg to proc is a symbol representing the name of the positive blame
  ;; the second arg to proc is the symbol representing the name of the negative blame
  ;; the third argument to proc is the src-info.
  ;; the fourth argumet is a textual representation of the original contract
  ;;
  ;; the argument to the result function is the value to test.
  ;; (the result function is the projection)
  ;;  
  (define-values (make-flat-contract
                  flat-contract-predicate
                  flat-contract?
                  
                  make-contract
                  contract-name
                  contract-proc
                  contract?)
    (let ()
      (define-struct contract (name proc))
      (define-struct (flat-contract contract) (predicate))
      (values make-flat-contract
              flat-contract-predicate
              flat-contract?
              
              make-contract
              contract-name
              contract-proc
              contract?)))

  (define (test-proc/flat-contract f x)
    (if (flat-contract? f)
        ((flat-contract-predicate f) x)
        (f x)))
  
  (define (proc/ctc->ctc f)
    (if (contract? f)
        f
        (flat-named-contract
         (or (object-name f)
             (string->symbol (format "contract:~e" f)))
         f)))
    
  
  
  ;; build-compound-type-name : (union contract symbol) ... -> (-> sexp)
  (define (build-compound-type-name . fs)
    (let loop ([subs fs]
               [i 0])
      (cond
        [(null? subs)
         '()]
        [else (let ([sub (car subs)])
                (cond
                  [(contract? sub)
                   (let ([mk-sub-name (contract-name sub)])
                     `(,mk-sub-name ,@(loop (cdr subs) (+ i 1))))]
                  [else `(,sub ,@(loop (cdr subs) i))]))])))
  
  (define (flat-contract predicate)
    (unless (and (procedure? predicate)
                 (procedure-arity-includes? predicate 1))
      (error 'flat-contract
             "expected procedure of one argument as argument, given ~e"
             predicate))
    (let ([pname (object-name predicate)])
      (if pname
          (flat-named-contract pname predicate)
          (flat-named-contract '??? predicate))))
  
  (define (flat-named-contract name predicate)
    (unless (and (procedure? predicate)
                 (procedure-arity-includes? predicate 1))
      (error 'flat-named-contract
             "expected procedure of one argument as second argument, given: ~e, fst arg ~e"
             predicate name))
    (build-flat-contract name predicate))
  
  (define (build-flat-contract name predicate)
    (make-flat-contract
     name
     (lambda (pos neg src-info orig-str) 
       (lambda (val)
         (if (predicate val)
             val
             (raise-contract-error
              src-info
              pos
              neg
              orig-str
              "expected <~a>, given: ~e"
              name
              val))))
     predicate))

  (define-syntax (-contract stx)
    (syntax-case stx ()
      [(_ a-contract to-check pos-blame-e neg-blame-e)
       (with-syntax ([src-loc (syntax/loc stx here)])
         (syntax/loc stx
           (contract/proc a-contract to-check pos-blame-e neg-blame-e (quote-syntax src-loc))))]
      [(_ a-contract-e to-check pos-blame-e neg-blame-e src-info-e)
       (syntax/loc stx
         (contract/proc a-contract-e to-check pos-blame-e neg-blame-e src-info-e))]))
  
  (define (contract/proc a-contract-raw name pos-blame neg-blame src-info)
    (unless (or (contract? a-contract-raw)
                (and (procedure? a-contract-raw)
                     (procedure-arity-includes? a-contract-raw 1)))
      (error 'contract "expected a contract or a procedure of arity 1 as first argument, given: ~e, other args ~e ~e ~e ~e" 
             a-contract-raw
             name
             pos-blame
             neg-blame
             src-info))
    (let ([a-contract (if (contract? a-contract-raw)
                          a-contract-raw
                          (flat-contract a-contract-raw))])
      (unless (and (symbol? neg-blame)
                   (symbol? pos-blame))
        (error 'contract
               "expected symbols as names for assigning blame, given: ~e and ~e, other args ~e ~e ~e"
               neg-blame pos-blame
               a-contract-raw 
               name
               src-info))
      (unless (syntax? src-info)
        (error 'contract "expected syntax as last argument, given: ~e, other args ~e ~e ~e ~e"
               src-info
               neg-blame 
               pos-blame
               a-contract-raw
               name))
      (((contract-proc a-contract) pos-blame neg-blame src-info (contract-name a-contract))
       name)))
    
  (define-values (make-exn:fail:contract2 
                  exn:fail:contract2?
                  exn:fail:contract2-srclocs)
    (let-values ([(exn:fail:contract2 
                   make-exn:fail:contract2 
                   exn:fail:contract2?
                   get
                   set)
                  (parameterize ([current-inspector (make-inspector)])
                    (make-struct-type 'exn:fail:contract2
                                      struct:exn:fail
                                      1
                                      0
                                      #f
                                      (list (cons prop:exn:srclocs (lambda (x) (exn:fail:contract2-srclocs x))))))])
      (values
       make-exn:fail:contract2 
       exn:fail:contract2?
       (lambda (x) (get x 0)))))
  
  ;; raise-contract-error : (union syntax #f) symbol symbol string string args ... -> alpha
  ;; doesn't return
  (define (raise-contract-error src-info to-blame other-party contract-sexp fmt . args)
    (let ([blame-src (src-info-as-string src-info)]
          [formatted-contract-sexp
           (let ([one-line (format "~s" contract-sexp)])
             (if (< (string-length one-line) 30)
                 (string-append one-line " ")
                 (let ([sp (open-output-string)])
                   (newline sp)
                   (parameterize ([pretty-print-print-line print-contract-liner]
                                  [pretty-print-columns 50])
                     (pretty-print contract-sexp sp))
                   (get-output-string sp))))]
	  [specific-blame
	   (let ([datum (syntax-object->datum src-info)])
	     (if (symbol? datum)
		 (format " on ~a" datum)
                 ""))])
      (raise
       (make-exn:fail:contract2
        (string->immutable-string
         (string-append (format "~a~a broke the contract ~ait had with ~a~a; "
                                blame-src
                                to-blame
                                formatted-contract-sexp
                                other-party
                                specific-blame)
                        (apply format fmt args)))
        (current-continuation-marks)
        (if src-info
            (list (make-srcloc 
                   (syntax-source src-info)
                   (syntax-line src-info)
                   (syntax-column src-info)
                   (syntax-position src-info)
                   (syntax-span src-info)))
            '())))))
  
  (define print-contract-liner
    (let ([default (pretty-print-print-line)])
      (λ (line port ol cols)
        (+ (default line port ol cols)
           (if line
               (begin (display "  " port)
                      2)
               0)))))
  
  ;; src-info-as-string : (union syntax #f) -> string
  (define (src-info-as-string src-info)
    (if (syntax? src-info)
        (let ([src-loc-str (build-src-loc-string src-info)])
          (if src-loc-str
              (string-append src-loc-str  ": ")
              ""))
        ""))
    
  (define-syntax (recursive-contract stx)
    (syntax-case stx ()
      [(_ arg)
       (syntax (recursive-contract/proc '(recursive-contract arg) (delay (check-contract arg))))]))
  
  (define (recursive-contract/proc name delayed-contract)
    (make-contract name
                   (λ (pos neg src str)
                     (let ([proc (contract-proc (force delayed-contract))])
                       (λ (val)
                         ((proc pos neg src str) val))))))
  
  (define (check-contract ctc)
    (unless (contract? ctc)
      (error 'recursive-contract "expected a contract, got ~e" ctc))
    ctc)
                   
;                                                                                   
;                                                                                   
;                                                                                   
;                                                                                   
;                                                                                   
;           ;                                    ;                       ;          
;            ;;            ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;;   ;;;  
;              ;;         ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;    ;     
;                ;;      ;      ;     ;  ;   ;   ;    ;       ;  ;       ;    ;;    
;   ;;;;;;       ;;      ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;     ;;   
;              ;;        ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;       ;  
;            ;;           ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;       ;  
;           ;              ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;;  ;;;   
;                                                                                   
;                                                                                   
;                                                                                   

  
  (define-syntax (any stx)
    (raise-syntax-error 'any "Use any out of an arrow contract" stx))

  (define-syntax-set (-> ->* ->d ->d* ->r ->pp ->pp-rest case-> object-contract opt-> opt->*)
    
    (define (->/proc stx) (make-/proc #f ->/h stx))
    (define (->*/proc stx) (make-/proc #f ->*/h stx))
    (define (->d/proc stx) (make-/proc #f ->d/h stx))
    (define (->d*/proc stx) (make-/proc #f ->d*/h stx))
    (define (->r/proc stx) (make-/proc #f ->r/h stx))
    (define (->pp/proc stx) (make-/proc #f ->pp/h stx))
    (define (->pp-rest/proc stx) (make-/proc #f ->pp-rest/h stx))
        
    (define (obj->/proc stx) (make-/proc #t ->/h stx))
    (define (obj->*/proc stx) (make-/proc #t ->*/h stx))
    (define (obj->d/proc stx) (make-/proc #t ->d/h stx))
    (define (obj->d*/proc stx) (make-/proc #t ->d*/h stx))
    (define (obj->r/proc stx) (make-/proc #t ->r/h stx))
    (define (obj->pp/proc stx) (make-/proc #t ->pp/h stx))
    (define (obj->pp-rest/proc stx) (make-/proc #t ->pp-rest/h stx))
    
    (define (case->/proc stx) (make-case->/proc #f stx))
    (define (obj-case->/proc stx) (make-case->/proc #t stx))

    (define (obj-opt->/proc stx) (make-opt->/proc #t stx))
    (define (obj-opt->*/proc stx) (make-opt->*/proc #t stx))
    (define (opt->/proc stx) (make-opt->/proc #f stx))
    (define (opt->*/proc stx) (make-opt->*/proc #f stx))
  
    ;; make-/proc : boolean
    ;;              (syntax -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))) 
    ;;              syntax
    ;;           -> (syntax -> syntax)
    (define (make-/proc method-proc? /h stx)
      (let-values ([(arguments-check build-proj check-val wrapper) (/h method-proc? stx)])
        (let ([outer-args (syntax (val pos-blame neg-blame src-info orig-str name-id))])
          (with-syntax ([inner-check (check-val outer-args)]
                        [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                        [(val-args body) (wrapper outer-args)])
            (with-syntax ([inner-lambda
                           (set-inferred-name-from
                            stx
                            (syntax/loc stx (lambda val-args body)))])
              (let ([inner-lambda-w/err-check
                     (syntax
                      (lambda (val)
                        inner-check
                        inner-lambda))])
                (with-syntax ([proj-code (build-proj outer-args inner-lambda-w/err-check)])
                  (arguments-check
                   outer-args
                   (syntax/loc stx
                     (make-contract
                      name-id
                      (lambda (pos-blame neg-blame src-info orig-str)
                        proj-code)))))))))))
    
    (define (make-case->/proc method-proc? stx)
      (syntax-case stx ()
        [(_ cases ...)
         (let-values ([(arguments-check build-projs check-val wrapper)
                       (case->/h method-proc? stx (syntax->list (syntax (cases ...))))])
           (let ([outer-args (syntax (val pos-blame neg-blame src-info orig-str name-id))])
             (with-syntax ([(inner-check ...) (check-val outer-args)]
                           [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                           [(body ...) (wrapper outer-args)])
               (with-syntax ([inner-lambda 
                              (set-inferred-name-from
                               stx
                               (syntax/loc stx (case-lambda body ...)))])
                 (let ([inner-lambda-w/err-check
                        (syntax
                         (lambda (val)
                           inner-check ...
                           inner-lambda))])
                   (with-syntax ([proj-code (build-projs outer-args inner-lambda-w/err-check)])
                     (arguments-check
                      outer-args
                      (syntax/loc stx
                        (make-contract
                         (apply build-compound-type-name 'case-> name-id)
                         (lambda (pos-blame neg-blame src-info orig-str)
                           proj-code))))))))))]))
    
    (define (make-opt->/proc method-proc? stx)
      (syntax-case stx (any)
        [(_ (reqs ...) (opts ...) any)
         (make-opt->*/proc method-proc? (syntax (opt->* (reqs ...) (opts ...) any)))]
        [(_ (reqs ...) (opts ...) res)
         (make-opt->*/proc method-proc? (syntax (opt->* (reqs ...) (opts ...) (res))))]))
  
    (define (make-opt->*/proc method-proc? stx)
      (syntax-case stx (any)
        [(_ (reqs ...) (opts ...) any)
         (let* ([req-vs (generate-temporaries (syntax->list (syntax (reqs ...))))]
                [opt-vs (generate-temporaries (syntax->list (syntax (opts ...))))]
                [cses
                 (reverse
                  (let loop ([opt-vs (reverse opt-vs)])
                    (cond
                      [(null? opt-vs) (list req-vs)]
                      [else (cons (append req-vs (reverse opt-vs))
                                  (loop (cdr opt-vs)))])))])
           (with-syntax ([(req-vs ...) req-vs]
                         [(opt-vs ...) opt-vs]
                         [((case-doms ...) ...) cses])
             (with-syntax ([expanded-case->
                            (make-case->/proc
                             method-proc?
                             (syntax (case-> (-> case-doms ... any) ...)))])
               (syntax/loc stx
                 (let ([req-vs reqs] ...
                       [opt-vs opts] ...)
                   expanded-case->)))))]
        [(_ (reqs ...) (opts ...) (ress ...))
         (let* ([res-vs (generate-temporaries (syntax->list (syntax (ress ...))))]
                [req-vs (generate-temporaries (syntax->list (syntax (reqs ...))))]
                [opt-vs (generate-temporaries (syntax->list (syntax (opts ...))))]
                [cses
                 (reverse
                  (let loop ([opt-vs (reverse opt-vs)])
                    (cond
                      [(null? opt-vs) (list req-vs)]
                      [else (cons (append req-vs (reverse opt-vs))
                                  (loop (cdr opt-vs)))])))])
           (with-syntax ([(res-vs ...) res-vs]
                         [(req-vs ...) req-vs]
                         [(opt-vs ...) opt-vs]
                         [((case-doms ...) ...) cses])
             (with-syntax ([(single-case-result ...)
                            (let* ([ress-lst (syntax->list (syntax (ress ...)))]
                                   [only-one?
                                    (and (pair? ress-lst)
                                         (null? (cdr ress-lst)))])
                              (map
                               (if only-one?
                                   (lambda (x) (car (syntax->list (syntax (res-vs ...)))))
                                   (lambda (x) (syntax (values res-vs ...))))
                               cses))])
               (with-syntax ([expanded-case->
                              (make-case->/proc
                               method-proc?
                               (syntax (case-> (-> case-doms ... single-case-result) ...)))])
                 (set-inferred-name-from
                  stx
                  (syntax/loc stx
                    (let ([res-vs ress] 
                          ...
                          [req-vs reqs]
                          ...
                          [opt-vs opts]
                          ...)
                      expanded-case->)))))))]))

    ;; exactract-argument-lists : syntax -> (listof syntax)
    (define (extract-argument-lists stx)
      (map (lambda (x)
             (syntax-case x ()
               [(arg-list body) (syntax arg-list)]))
           (syntax->list stx)))
    
    ;; ensure-cases-disjoint : syntax syntax[list] -> void
    (define (ensure-cases-disjoint stx cases)
      (let ([individual-cases null]
            [dot-min #f])
        (for-each (lambda (case)
                    (let ([this-case (get-case case)])
                      (cond
                        [(number? this-case) 
                         (cond
                           [(member this-case individual-cases)
                            (raise-syntax-error
                             'case-> 
                             (format "found multiple cases with ~a arguments" this-case)
                             stx)]
                           [(and dot-min (dot-min . <= . this-case))
                            (raise-syntax-error 
                             'case-> 
                             (format "found overlapping cases (~a+ followed by ~a)" dot-min this-case)
                             stx)]
                           [else (set! individual-cases (cons this-case individual-cases))])]
                        [(pair? this-case)
                         (let ([new-dot-min (car this-case)])
                           (cond
                             [dot-min
                              (if (dot-min . <= . new-dot-min)
                                  (raise-syntax-error
                                   'case->
                                   (format "found overlapping cases (~a+ followed by ~a+)" dot-min new-dot-min)
                                   stx)
                                  (set! dot-min new-dot-min))]
                             [else
                              (set! dot-min new-dot-min)]))])))
                  cases)))

    ;; get-case : syntax -> (union number (cons number 'more))
    (define (get-case stx)
      (let ([ilist (syntax-object->datum stx)])
        (if (list? ilist)
            (length ilist)
            (cons 
             (let loop ([i ilist])
               (cond
                 [(pair? i) (+ 1 (loop (cdr i)))]
                 [else 0]))
             'more))))

    
    ;; case->/h : boolean
    ;;            syntax
    ;;            (listof syntax) 
    ;;         -> (values (syntax -> syntax)
    ;;                    (syntax -> syntax)
    ;;                    (syntax syntax -> syntax) 
    ;;                    (syntax -> syntax))
    ;; like the other /h functions, but composes the wrapper functions
    ;; together and combines the cases of the case-lambda into a single list.
    (define (case->/h method-proc? orig-stx cases)
      (let loop ([cases cases]
                 [name-ids '()])
        (cond
          [(null? cases) (values (lambda (outer-args body)
                                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                                 [body body]
                                                 [(name-ids ...) (reverse name-ids)])
                                     (syntax
                                      (let ([name-id (list name-ids ...)])
                                        body))))
                                 (lambda (x y) y)
                                 (lambda (args) (syntax ()))
                                 (lambda (args) (syntax ())))]
          [else
           (let ([/h (select/h (car cases) 'case-> orig-stx)]
                 [new-id (car (generate-temporaries (syntax (case->name-id))))])
             (let-values ([(arguments-checks build-projs check-vals wrappers)
                           (loop (cdr cases) (cons new-id name-ids))]
                          [(arguments-check build-proj check-val wrapper)
                           (/h method-proc? (car cases))])
               (values
                (lambda (outer-args x) 
                  (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                [new-id new-id])
                    (arguments-check 
                     (syntax (val pos-blame neg-blame src-info orig-str new-id)) 
                     (arguments-checks 
                      outer-args
                      x))))
                (lambda (args inner)
                  (build-projs
                   args
                   (build-proj
                    args
                    inner)))
                (lambda (args)
                  (with-syntax ([checks (check-vals args)]
                                [check (check-val args)])
                    (syntax (check . checks))))
                (lambda (args)
                  (with-syntax ([case (wrapper args)]
                                [cases (wrappers args)])
                    (syntax (case . cases)))))))])))
    
    (define (object-contract/proc stx)
      
      ;; name : syntax
      ;; ctc-stx : syntax[evals to a contract]
      ;; mtd-arg-stx : syntax[list of arg-specs] (ie, for use in a case-lambda)
      (define-struct mtd (name ctc-stx mtd-arg-stx))
      
      ;; name : syntax
      ;; ctc-stx : syntax[evals to a contract]
      (define-struct fld (name ctc-stx))
      
      ;; expand-field/mtd-spec : stx -> (union mtd fld)
      (define (expand-field/mtd-spec f/m-stx)
        (syntax-case f/m-stx (field)
          [(field field-name ctc)
           (identifier? (syntax field-name))
           (make-fld (syntax field-name) (syntax ctc))]
          [(field field-name ctc)
           (raise-syntax-error 'object-contract "expected name of field" stx (syntax field-name))]
          [(mtd-name ctc)
           (identifier? (syntax mtd-name))
           (let-values ([(ctc-stx proc-stx) (expand-mtd-contract (syntax ctc))])
             (make-mtd (syntax mtd-name)
                       ctc-stx
                       proc-stx))]
          [(mtd-name ctc)
           (raise-syntax-error 'object-contract "expected name of method" stx (syntax mtd-name))]
          [_ (raise-syntax-error 'object-contract "expected field or method clause" stx f/m-stx)]))
      
      ;; expand-mtd-contract : syntax -> (values syntax[expanded ctc] syntax[mtd-arg])
      (define (expand-mtd-contract mtd-stx)
        (syntax-case mtd-stx (case-> opt-> opt->*)
          [(case-> cases ...)
           (let loop ([cases (syntax->list (syntax (cases ...)))]
                      [ctc-stxs null]
                      [args-stxs null])
             (cond
               [(null? cases) 
                (values
                 (with-syntax ([(x ...) (reverse ctc-stxs)])
                   (obj-case->/proc (syntax (case-> x ...))))
                 (with-syntax ([(x ...) (apply append (map syntax->list (reverse args-stxs)))])
                   (syntax (x ...))))]
               [else
                (let-values ([(trans ctc-stx mtd-args) (expand-mtd-arrow (car cases))])
                  (loop (cdr cases)
                        (cons ctc-stx ctc-stxs)
                        (cons mtd-args args-stxs)))]))]
          [(opt->* (req-contracts ...) (opt-contracts ...) (res-contracts ...))
           (values
            (obj-opt->*/proc (syntax (opt->* (any/c req-contracts ...) (opt-contracts ...) (res-contracts ...))))
            (generate-opt->vars (syntax (req-contracts ...))
                                (syntax (opt-contracts ...))))]
          [(opt->* (req-contracts ...) (opt-contracts ...) any)
           (values
            (obj-opt->*/proc (syntax (opt->* (any/c req-contracts ...) (opt-contracts ...) any)))
            (generate-opt->vars (syntax (req-contracts ...))
                                (syntax (opt-contracts ...))))]
          [(opt-> (req-contracts ...) (opt-contracts ...) res-contract) 
           (values
            (obj-opt->/proc (syntax (opt-> (any/c req-contracts ...) (opt-contracts ...) res-contract)))
            (generate-opt->vars (syntax (req-contracts ...))
                                (syntax (opt-contracts ...))))]
          [else (let-values ([(x y z) (expand-mtd-arrow mtd-stx)])
                  (values (x y) z))]))
      
      ;; generate-opt->vars : syntax[requried contracts] syntax[optional contracts] -> syntax[list of arg specs]
      (define (generate-opt->vars req-stx opt-stx)
        (with-syntax ([(req-vars ...) (generate-temporaries req-stx)]
                      [(ths) (generate-temporaries (syntax (ths)))])
          (let loop ([opt-vars (generate-temporaries opt-stx)])
            (cond
              [(null? opt-vars) (list (syntax (ths req-vars ...)))]
              [else (with-syntax ([(opt-vars ...) opt-vars]
                                  [(rests ...) (loop (cdr opt-vars))])
                      (syntax ((ths req-vars ... opt-vars ...)
                               rests ...)))]))))
      
      ;; expand-mtd-arrow : stx -> (values (syntax[ctc] -> syntax[expanded ctc]) syntax[ctc] syntax[mtd-arg])
      (define (expand-mtd-arrow mtd-stx)
        (syntax-case mtd-stx (-> ->* ->d ->d* ->r ->pp ->pp-rest)
          [(->) (raise-syntax-error 'object-contract "-> must have arguments" stx mtd-stx)]
          [(-> args ...)
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (args ...)))])
             (values obj->/proc
                     (syntax (-> any/c args ...))
                     (syntax ((arg-vars ...)))))]
          [(->* (doms ...) (rngs ...))
           (with-syntax ([(args-vars ...) (generate-temporaries (syntax (doms ...)))]
                         [(this-var) (generate-temporaries (syntax (this-var)))])
             (values obj->*/proc
                     (syntax (->* (any/c doms ...) (rngs ...)))
                     (syntax ((this-var args-vars ...)))))]
          [(->* (doms ...) rst (rngs ...))
           (with-syntax ([(args-vars ...) (generate-temporaries (syntax (doms ...)))]
                         [(rst-var) (generate-temporaries (syntax (rst)))]
                         [(this-var) (generate-temporaries (syntax (this-var)))])
             (values obj->*/proc
                     (syntax (->* (any/c doms ...) rst (rngs ...)))
                     (syntax ((this-var args-vars ... . rst-var)))))]
          [(->* x ...)
           (raise-syntax-error 'object-contract "malformed ->*" stx mtd-stx)]
          [(->d) (raise-syntax-error 'object-contract "->d must have arguments" stx mtd-stx)]
          [(->d doms ... rng-proc)
           (let ([doms-val (syntax->list (syntax (doms ...)))])
             (values
              obj->d/proc
              (with-syntax ([(arg-vars ...) (generate-temporaries doms-val)]
                            [arity-count (length doms-val)])
                (syntax 
                 (->d any/c doms ... 
                      (let ([f rng-proc])
			(check->* f arity-count)
                        (lambda (_this-var arg-vars ...)
                          (f arg-vars ...))))))
              (with-syntax ([(args-vars ...) (generate-temporaries doms-val)])
                (syntax ((this-var args-vars ...))))))]
          [(->d* (doms ...) rng-proc)
           (values
            obj->d*/proc
            (let ([doms-val (syntax->list (syntax (doms ...)))])
              (with-syntax ([(arg-vars ...) (generate-temporaries doms-val)]
                            [arity-count (length doms-val)])
                (syntax (->d* (any/c doms ...)
                              (let ([f rng-proc])
				(check->* f arity-count)
                                (lambda (_this-var arg-vars ...)
                                  (f arg-vars ...)))))))
            (with-syntax ([(args-vars ...) (generate-temporaries (syntax (doms ...)))]
                          [(this-var) (generate-temporaries (syntax (this-var)))])
              (syntax ((this-var args-vars ...)))))]
          [(->d* (doms ...) rst-ctc rng-proc)
           (let ([doms-val (syntax->list (syntax (doms ...)))])
             (values
              obj->d*/proc
              (with-syntax ([(arg-vars ...) (generate-temporaries doms-val)]
                            [(rest-var) (generate-temporaries (syntax (rst-ctc)))]
                            [arity-count (length doms-val)])
                (syntax (->d* (any/c doms ...)
                              rst-ctc
                              (let ([f rng-proc])
				(check->*/more f arity-count)
                                (lambda (_this-var arg-vars ... . rest-var)
                                  (apply f arg-vars ... rest-var))))))
              (with-syntax ([(args-vars ...) (generate-temporaries (syntax (doms ...)))]
                            [(rst-var) (generate-temporaries (syntax (rst-ctc)))]
                            [(this-var) (generate-temporaries (syntax (this-var)))])
                (syntax ((this-var args-vars ... . rst-var))))))]
          [(->d* x ...)
           (raise-syntax-error 'object-contract "malformed ->d* method contract" stx mtd-stx)]
          
          [(->r ([x dom] ...) rng)
           (andmap identifier? (syntax->list (syntax (x ...))))
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (x ...)))])
             (values
              obj->r/proc
              (syntax (->r ([_this any/c] [x dom] ...) rng))
              (syntax ((_this arg-vars ...)))))]
          
          [(->r ([x dom] ...) rest-x rest-dom rng)
           (andmap identifier? (syntax->list (syntax (x ...))))
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (x ...)))])
             (values
              obj->r/proc
              (syntax (->r ([_this any/c] [x dom] ...) rest-x rest-dom rng))
              (syntax ((_this arg-vars ... . rest-var)))))]
          
          [(->r . x)
           (raise-syntax-error 'object-contract "malformed ->r declaration")]
          [(->pp ([x dom] ...) . other-stuff)
           (andmap identifier? (syntax->list (syntax (x ...))))
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (x ...)))])
             (values
              obj->pp/proc
              (syntax (->pp ([_this any/c] [x dom] ...) . other-stuff))
              (syntax ((_this arg-vars ...)))))]
          [(->pp . x)
           (raise-syntax-error 'object-contract "malformed ->pp declaration")]
          [(->pp-rest ([x dom] ...) rest-id . other-stuff)
           (and (identifier? (syntax id))
                (andmap identifier? (syntax->list (syntax (x ...)))))
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (x ...)))])
             (values
              obj->pp-rest/proc
              (syntax (->pp ([_this any/c] [x dom] ...) rest-id . other-stuff))
              (syntax ((_this arg-vars ... . rest-id)))))]
          [(->pp-rest . x)
           (raise-syntax-error 'object-contract "malformed ->pp-rest declaration")]
          [else (raise-syntax-error 'object-contract "unknown method contract syntax" stx mtd-stx)]))
      
      ;; build-methods-stx : syntax[list of lambda arg specs] -> syntax[method realized as proc]
      (define (build-methods-stx mtds)
        (let loop ([arg-spec-stxss (map mtd-mtd-arg-stx mtds)]
                   [names (map mtd-name mtds)]
                   [i 0])
          (cond
            [(null? arg-spec-stxss) null]
            [else (let ([arg-spec-stxs (car arg-spec-stxss)])
                    (with-syntax ([(cases ...)
                                   (map (lambda (arg-spec-stx)
                                          (with-syntax ([i i])
                                            (syntax-case arg-spec-stx ()
                                              [(this rest-ids ...)
                                               (syntax
                                                ((this rest-ids ...)
                                                 ((field-ref this i) (wrapper-object-wrapped this) rest-ids ...)))]
                                              [else
                                               (let-values ([(this rest-ids last-var)
                                                             (let ([lst (syntax->improper-list arg-spec-stx)])
                                                               (values (car lst)
                                                                       (all-but-last (cdr lst))
                                                                       (cdr (last-pair lst))))])
                                                 (with-syntax ([this this]
                                                               [(rest-ids ...) rest-ids]
                                                               [last-var last-var])
                                                   (syntax
                                                    ((this rest-ids ... . last-var)
                                                     (apply (field-ref this i)
                                                            (wrapper-object-wrapped this)
                                                            rest-ids ...
                                                            last-var)))))])))
                                        (syntax->list arg-spec-stxs))]
                                  [name (string->symbol (format "~a method" (syntax-object->datum (car names))))])
                      (with-syntax ([proc (syntax-property (syntax (case-lambda cases ...)) 'method-arity-error #t)])
                        (cons (syntax (lambda (field-ref) (let ([name proc]) name)))
                              (loop (cdr arg-spec-stxss)
                                    (cdr names)
                                    (+ i 1))))))])))
      
      (define (syntax->improper-list stx)
        (define (se->il se)
          (cond
            [(pair? se) (sp->il se)]
            [else se]))
        (define (stx->il stx)
          (se->il (syntax-e stx)))
        (define (sp->il p)
          (cond
            [(null? (cdr p)) p]
            [(pair? (cdr p)) (cons (car p) (sp->il (cdr p)))]
            [(syntax? (cdr p)) 
             (let ([un (syntax-e (cdr p))])
               (if (pair? un)
                   (cons (car p) (sp->il un))
                   p))]))
        (stx->il stx))
      
      (syntax-case stx ()
        [(_ field/mtd-specs ...)
         (let* ([mtd/flds (map expand-field/mtd-spec (syntax->list (syntax (field/mtd-specs ...))))]
		[mtds (filter mtd? mtd/flds)]
		[flds (filter fld? mtd/flds)])
           (with-syntax ([(method-ctc-stx ...) (map mtd-ctc-stx mtds)]
                         [(method-name ...) (map mtd-name mtds)]
                         [(method-ctc-var ...) (generate-temporaries mtds)]
                         [(method-var ...) (generate-temporaries mtds)]
                         [(method/app-var ...) (generate-temporaries mtds)]
                         [(methods ...) (build-methods-stx mtds)]
                         
                         [(field-ctc-stx ...) (map fld-ctc-stx flds)]
                         [(field-name ...) (map fld-name flds)]
                         [(field-ctc-var ...) (generate-temporaries flds)]
                         [(field-var ...) (generate-temporaries flds)]
                         [(field/app-var ...) (generate-temporaries flds)])
             (syntax
              (let ([method-ctc-var method-ctc-stx] 
                    ...
                    [field-ctc-var (coerce-contract object-contract field-ctc-stx)]
                    ...)
                (let ([method-var (contract-proc method-ctc-var)] 
                      ...
                      [field-var (contract-proc field-ctc-var)] 
                      ...)
                  (make-contract
                   `(object-contract 
                     ,(build-compound-type-name 'method-name method-ctc-var) ...
                     ,(build-compound-type-name 'field 'field-name field-ctc-var) ...)
                   (lambda (pos-blame neg-blame src-info orig-str)
                     (let ([method/app-var (method-var pos-blame neg-blame src-info orig-str)] 
                           ...
                           [field/app-var (field-var pos-blame neg-blame src-info orig-str)]
                           ...)
                       (let ([cls (make-wrapper-class 'wrapper-class 
                                                      '(method-name ...)
                                                      (list methods ...)
                                                      '(field-name ...))]
                             [field-names-list '(field-name ...)])
                         (lambda (val)
			   (check-object val src-info pos-blame neg-blame orig-str)
                           (let ([val-mtd-names
                                  (interface->method-names
                                   (object-interface
                                    val))])
                             (void)
			     (check-method 'method-name val-mtd-names src-info pos-blame neg-blame orig-str)
                             ...)
                           
                           (unless (field-bound? field-name val)
			     (field-error 'field-name src-info pos-blame neg-blame orig-str)) ...
                           
                           (let ([vtable (extract-vtable val)]
                                 [method-ht (extract-method-ht val)])
                             (make-object cls
                               val
                               (method/app-var (vector-ref vtable (hash-table-get method-ht 'method-name))) ...
                               (field/app-var (get-field field-name val)) ...
                               ))))))))))))]))

    ;; ensure-no-duplicates : syntax (listof syntax[identifier]) -> void
    (define (ensure-no-duplicates stx form-name names)
      (let ([ht (make-hash-table)])
        (for-each (lambda (name)
                    (let ([key (syntax-e name)])
                      (when (hash-table-get ht key (lambda () #f))
                        (raise-syntax-error form-name
                                            "duplicate method name"
                                            stx
                                            name))
                      (hash-table-put! ht key #t)))
                  names)))
    
    ;; method-specifier? : syntax -> boolean
    ;; returns #t if x is the syntax for a valid method specifier
    (define (method-specifier? x)
      (or (eq? 'public (syntax-e x))
          (eq? 'override (syntax-e x))))
    
    ;; make-object-wrapper-method : syntax syntax[identifier] syntax[identifier] syntax -> syntax
    ;; constructs a wrapper method that checks the pre and post-condition, and
    ;; calls the original object's method
    (define (make-object-wrapper-method outer-args method-name contract-var contract-stx)
      (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                    [method-name method-name]
                    [method-name-string (symbol->string (syntax-e method-name))]
                    [contract-var contract-var])
        (syntax/loc contract-stx
          (define/public (method-name . args)
            (let ([other-method (lambda x (send/apply val method-name x))]
                  [method-specific-src-info 
                   (if (identifier? src-info)
                       (datum->syntax-object
                        src-info
                        (string->symbol
                         (string-append
                          (symbol->string (syntax-e src-info))
                          " method "
                          method-name-string)))
                       src-info)])
              (apply (contract-var
                      other-method
                      pos-blame
                      neg-blame
                      method-specific-src-info)
                     args))))))

    ;; make-class-wrapper-method : syntax syntax[identifier] syntax[identifier] syntax -> syntax
    ;; constructs a wrapper method that checks the pre and post-condition, and
    ;; calls the super method inbetween.
    (define (make-class-wrapper-method outer-args method-name contract-var contract-stx)
      (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                    [super-method-name (prefix-super method-name)]
                    [method-name method-name]
                    [method-name-string (symbol->string (syntax-e method-name))]
                    [contract-var contract-var])
        (syntax/loc contract-stx
          (define/override method-name
            (lambda args
              (let* ([super-method (lambda x (super-method-name . x))]
                     [method-specific-src-info 
                      (if (identifier? src-info)
                          (datum->syntax-object
                           src-info
                           (string->symbol
                            (string-append
                             (symbol->string (syntax-e src-info))
                             " method "
                             method-name-string)))
                          src-info)]
                     [super-contract (and super-contracts-ht
                                          (hash-table-get super-contracts-ht
                                                          'method-name
                                                          (lambda () #f)))]
                     [wrapped-method (contract-var
                                      super-method
                                      pos-blame
                                      neg-blame
                                      method-specific-src-info)])
                (apply wrapped-method args)))))))
    
    ;; prefix-super : syntax[identifier] -> syntax[identifier]
    ;; adds super- to the front of the identifier
    (define (prefix-super stx)
      (datum->syntax-object
       #'here
       (string->symbol
        (format 
         "super-~a"
         (syntax-object->datum
          stx)))))
    
    ;; method-name->contract-method-name : syntax[identifier] -> syntax[identifier]
    ;; given the syntax for a method name, constructs the name of a method
    ;; that returns the super's contract for the original method.
    (define (method-name->contract-method-name stx)
      (datum->syntax-object
       #'here
       (string->symbol
        (format 
         "ACK_DONT_GUESS_ME-super-contract-~a"
         (syntax-object->datum
          stx)))))
    
    ;; Each of the /h functions builds four pieces of syntax:
    ;;  - [arguments-check]
    ;;    code that binds the contract values to names and
    ;;    does error checking for the contract specs
    ;;    (were the arguments all contracts?)
    ;;  - [build-proj]
    ;;    code that partially applies the input contracts to build projections
    ;;  - [check-val]
    ;;    code that does error checking on the contract'd value itself
    ;;    (is it a function of the right arity?)
    ;;  - [wrapper]
    ;;    a piece of syntax that has the arguments to the wrapper
    ;;    and the body of the wrapper.
    ;; the first function accepts a body expression and wraps
    ;;    the body expression with checks. In addition, it
    ;;    adds a let that binds the contract exprssions to names
    ;;    the results of the other functions mention these names.
    ;; the second and third function's input syntax should be four
    ;;    names: val, pos-blame, neg-blame, src-info, orig-str, name-id
    ;; the fourth function returns a syntax list with two elements,
    ;;    the argument list (to be used as the first arg to lambda,
    ;;    or as a case-lambda clause) and the body of the function.
    ;; They are combined into a lambda for the -> ->* ->d ->d* macros,
    ;; and combined into a case-lambda for the case-> macro.
    
    ;; ->/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->/h method-proc? stx)
      (syntax-case stx ()
        [(_) (raise-syntax-error '-> "expected at least one argument" stx)]
        [(_ dom ... rng)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
           (with-syntax ([(name-dom-contract-x ...)
                          (if method-proc?
                              (cdr
                               (syntax->list
                                (syntax (dom-contract-x ...))))
                              (syntax (dom-contract-x ...)))])
             (syntax-case* (syntax rng) (any values) module-or-top-identifier=?
               [any 
                (values
                 (lambda (outer-args body)
                   (with-syntax ([body body]
                                 [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      (let ([dom-contract-x (coerce-contract -> dom)] ...)
                        (let ([dom-x (contract-proc dom-contract-x)] ...)
                          (let ([name-id (build-compound-type-name '-> name-dom-contract-x ... 'any)])
                            body))))))
                 
                 (lambda (outer-args inner-lambda)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                 [inner-lambda inner-lambda])
                     (syntax
                      (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                        inner-lambda))))
                 
                 (lambda (outer-args)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
		      (check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
                 
                 (lambda (outer-args)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      ((arg-x ...)
                       (val (dom-projection-x arg-x) ...))))))]
               [(values rng ...)
                (with-syntax ([(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                              [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                              [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                              [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                              [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                              [(res-x ...) (generate-temporaries (syntax (rng ...)))])
                  (values
                   (lambda (outer-args body)
                     (with-syntax ([body body]
                                   [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
                        (let ([dom-contract-x (coerce-contract -> dom)] ...
                              [rng-contract-x (coerce-contract -> rng)] ...)
                          (let ([dom-x (contract-proc dom-contract-x)] ...
                                [rng-x (contract-proc rng-contract-x)] ...)
                            (let ([name-id 
                                   (build-compound-type-name 
                                    '-> 
                                    name-dom-contract-x ...
                                    (build-compound-type-name 'values rng-contract-x ...))])
                              body))))))
                   
                   (lambda (outer-args inner-lambda)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                   [inner-lambda inner-lambda])
                       (syntax
                        (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                              [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)] ...)
                          inner-lambda))))
                   
                   (lambda (outer-args)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
			(check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
                   
                   (lambda (outer-args)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
                        ((arg-x ...)
                         (let-values ([(res-x ...) (val (dom-projection-x arg-x) ...)])
                           (values (rng-projection-x
                                    res-x)
                                   ...))))))))]
               [rng
                (with-syntax ([(rng-x) (generate-temporaries (syntax (rng)))]
                              [(rng-contact-x) (generate-temporaries (syntax (rng)))]
                              [(rng-projection-x) (generate-temporaries (syntax (rng)))]
                              [(rng-ant-x) (generate-temporaries (syntax (rng)))]
                              [(res-x) (generate-temporaries (syntax (rng)))])
                  (values
                   (lambda (outer-args body)
                     (with-syntax ([body body]
                                   [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
                        (let ([dom-contract-x (coerce-contract -> dom)] ...
                              [rng-contract-x (coerce-contract -> rng)])
                          (let ([dom-x (contract-proc dom-contract-x)] ...
                                [rng-x (contract-proc rng-contract-x)])
                            (let ([name-id (build-compound-type-name '-> name-dom-contract-x ... rng-contract-x)])
                              body))))))
                   
                   (lambda (outer-args inner-lambda)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                   [inner-lambda inner-lambda])
                       (syntax
                        (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                              [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)])
                          inner-lambda))))
                   
                   (lambda (outer-args)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
			(check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
                   
                   (lambda (outer-args)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
                        ((arg-x ...)
                         (let ([res-x (val (dom-projection-x arg-x) ...)])
                           (rng-projection-x res-x))))))))])))]))
    
    ;; ->*/h : boolean stx -> (values (syntax -> syntax) (syntax syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->*/h method-proc? stx)
      (syntax-case stx (any)
        [(_ (dom ...) (rng ...))
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       
                       [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                       [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(res-x ...) (generate-temporaries (syntax (rng ...)))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...
                       [rng-contract-x (coerce-contract ->* rng)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [rng-x (contract-proc rng-contract-x)] ...)
                     (let ([name-id  (build-compound-type-name
                                      '->*
                                      (build-compound-type-name name-dom-contract-x ...)
                                      (build-compound-type-name rng-contract-x ...))])
                       body))))))
            
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)] ...)
                   inner-lambda))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 (check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ...)
                  (let-values ([(res-x ...) (val (dom-projection-x arg-x) ...)])
                    (values (rng-projection-x
                             res-x)
                            ...))))))))]
        [(_ (dom ...) any)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...)
                     (let ([name-id (build-compound-type-name
                                     '->* 
                                     (build-compound-type-name name-dom-contract-x ...)
                                     'any)])
                       body))))))
            
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                   inner-lambda))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 (check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ...)
                  (val (dom-projection-x arg-x) ...)))))))]
        [(_ (dom ...) rest (rng ...))
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [dom-rest-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-rest-contract-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-rest-projection-x (car (generate-temporaries (list (syntax rest))))]
                       [arg-rest-x (car (generate-temporaries (list (syntax rest))))]
                       
                       [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                       [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(res-x ...) (generate-temporaries (syntax (rng ...)))]
                       [arity (length (syntax->list (syntax (dom ...))))])
           (values
            (lambda (outer-args body)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [body body]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...
                       [dom-rest-contract-x (coerce-contract ->* rest)]
                       [rng-contract-x (coerce-contract ->* rng)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [dom-rest-x (contract-proc dom-rest-contract-x)]
                         [rng-x (contract-proc rng-contract-x)] ...)
                     (let ([name-id 
                            (build-compound-type-name 
                             '->*
                             (build-compound-type-name dom-contract-x ...)
                             dom-rest-contract-x
                             (build-compound-type-name rng-contract-x ...))])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [dom-rest-projection-x (dom-rest-x neg-blame pos-blame src-info orig-str)]
                       [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)] ...)
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 (check-procedure/more val dom-length src-info pos-blame neg-blame orig-str))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ... . arg-rest-x)
                  (let-values ([(res-x ...)
                                (apply
                                 val
                                 (dom-projection-x arg-x)
                                 ...
                                 (dom-rest-projection-x arg-rest-x))])
                    (values (rng-projection-x res-x) ...))))))))]
	[(_ (dom ...) rest any)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [dom-rest-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-rest-contract-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-projection-rest-x (car (generate-temporaries (list (syntax rest))))]
                       [arg-rest-x (car (generate-temporaries (list (syntax rest))))]
                       
                       [arity (length (syntax->list (syntax (dom ...))))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...
                       [dom-rest-contract-x (coerce-contract ->* rest)])
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [dom-rest-x (contract-proc dom-rest-contract-x)])
                     (let ([name-id (build-compound-type-name
                                     '->* 
                                     (build-compound-type-name name-dom-contract-x ...)
                                     dom-rest-contract-x
                                     'any)])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [dom-projection-rest-x (dom-rest-x neg-blame pos-blame src-info orig-str)])
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 ;; CHECK: previously, this test didn't use `procedure-arity' and compare to `dom-length'
		 (check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ... . arg-rest-x)
                  (apply
		   val
		   (dom-projection-x arg-x)
		   ...
		   (dom-projection-rest-x arg-rest-x))))))))]))

    ;; ->d/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->d/h method-proc? stx)
      (syntax-case stx ()
        [(_) (raise-syntax-error '->d "expected at least one argument" stx)]
        [(_ dom ... rng)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       [arity (length (syntax->list (syntax (dom ...))))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->d dom)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [rng-x rng])
		     (check-rng-procedure '->d rng-x arity)
                     (let ([name-id (build-compound-type-name '->d name-dom-contract-x ... '(... ...))])
                       
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 (check-procedure val arity src-info pos-blame neg-blame orig-str))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ...)
                  (let ([arg-x (dom-projection-x arg-x)] ...) 
                    (let ([rng-contract (rng-x arg-x ...)])
                      (((coerce/select-contract ->d rng-contract)
                        pos-blame
                        neg-blame
                        src-info
                        orig-str)
                       (val arg-x ...))))))))))]))
    
    ;; ->d*/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->d*/h method-proc? stx)
      (syntax-case stx ()
        [(_ (dom ...) rng-mk)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->d* dom)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [rng-mk-x rng-mk])
		     (check-rng-procedure '->d* rng-mk-x dom-length)
                     (let ([name-id (build-compound-type-name
                                     '->d* 
                                     (build-compound-type-name name-dom-contract-x ...)
                                     '(... ...))])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 (check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ...)
                  (call-with-values
                   (lambda () (rng-mk-x arg-x ...))
                   (lambda rng-contracts
                     (call-with-values
                      (lambda ()
                        (val (dom-projection-x arg-x) ...))
                      (lambda results
			(check-rng-lengths results rng-contracts)
                        (apply 
                         values
                         (map (lambda (rng-contract result)
                                (((coerce/select-contract ->d* rng-contract)
                                  pos-blame
                                  neg-blame
                                  src-info
                                  orig-str)
                                 result))
                              rng-contracts
                              results))))))))))))]
        [(_ (dom ...) rest rng-mk)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-rest-x) (generate-temporaries (syntax (rest)))]
                       [(dom-rest-contract-x) (generate-temporaries (syntax (rest)))]
                       [(dom-rest-projection-x) (generate-temporaries (syntax (rest)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       [arity (length (syntax->list (syntax (dom ...))))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->d* dom)] ...
                       [dom-rest-contract-x (coerce-contract ->d* rest)])
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [dom-rest-x (contract-proc dom-rest-contract-x)]
                         [rng-mk-x rng-mk])
		     (check-rng-procedure/more rng-mk-x arity)
                     (let ([name-id (build-compound-type-name 
                                     '->d*
                                     (build-compound-type-name name-dom-contract-x ...)
                                     dom-rest-contract-x
                                     '(... ...))])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [dom-rest-projection-x (dom-rest-x neg-blame pos-blame src-info orig-str)])
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 ;; CHECK: old check use "and more", but error message didn't
		 (check-procedure/more val arity src-info pos-blame neg-blame orig-str))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ... . rest-arg-x)
                  (call-with-values
                   (lambda ()
                     (apply rng-mk-x arg-x ... rest-arg-x))
                   (lambda rng-contracts
                     (call-with-values
                      (lambda ()
                        (apply 
                         val
                         (dom-projection-x arg-x)
                         ...
                         (dom-rest-projection-x rest-arg-x)))
                      (lambda results
			(check-rng-lengths results rng-contracts)
                        (apply 
                         values
                         (map (lambda (rng-contract result)
                                (((coerce/select-contract ->d* rng-contract)
                                  pos-blame
                                  neg-blame
                                  src-info
                                  orig-str)
                                 result))
                              rng-contracts
                              results))))))))))))]))

    ;; ->r/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->r/h method-proc? stx)
      (syntax-case stx ()
        [(_ ([x dom] ...) rng)
         (syntax-case* (syntax rng) (any values) module-or-top-identifier=?
           [any 
            (->r-pp/h method-proc? '->r (syntax (->r ([x dom] ...) #t any)))]
           [(values . args)
            (->r-pp/h method-proc? '->r (syntax (->r ([x dom] ...) #t rng #t)))]
           [rng
            (->r-pp/h method-proc? '->r (syntax (->r ([x dom] ...) #t rng unused-id #t)))]
           [_
            (raise-syntax-error '->r "unknown result contract spec" stx (syntax rng))])]
        
        [(_ ([x dom] ...) rest-x rest-dom rng)
         (syntax-case* (syntax rng) (values any) module-or-top-identifier=?
           [any 
            (->r-pp-rest/h method-proc? '->r (syntax (->r ([x dom] ...) rest-x rest-dom #t any)))]
           [(values . whatever)
            (->r-pp-rest/h method-proc? '->r (syntax (->r ([x dom] ...) rest-x rest-dom #t rng #t)))]
           [_
            (->r-pp-rest/h method-proc? '->r (syntax (->r ([x dom] ...) rest-x rest-dom #t rng unused-id #t)))])]))
    
    ;; ->pp/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->pp/h method-proc? stx) (->r-pp/h method-proc? '->pp stx))
    
    ;; ->pp/h : boolean symbol stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->r-pp/h method-proc? name stx)
      (syntax-case stx ()
        [(_ ([x dom] ...) pre-expr . result-stuff)
         (and (andmap identifier? (syntax->list (syntax (x ...))))
              (not (check-duplicate-identifier (syntax->list (syntax (x ...))))))
         (with-syntax ([stx-name name])
           (with-syntax ([(dom-id ...) (generate-temporaries (syntax (dom ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))]
                         [name-stx
                          (with-syntax ([(name-xs ...) (if method-proc?
                                                           (cdr (syntax->list (syntax (x ...))))
                                                           (syntax (x ...)))])
                            (syntax 
                             (build-compound-type-name 'stx-name
                                                       (build-compound-type-name
                                                        (build-compound-type-name 'name-xs '(... ...))
                                                        ...)
                                                       '(... ...))))])
             (values
              (lambda (outer-args body)
                (with-syntax ([body body]
                              [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax
                   (let ([name-id name-stx])
                     body))))
              (lambda (outer-args inner-lambda) inner-lambda)
              (lambda (outer-args)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                              [kind-of-thing (if method-proc? 'method 'procedure)])
                  (syntax
                   (begin 
                     (check-procedure/kind val arity 'kind-of-thing src-info pos-blame neg-blame orig-str)))))
              (lambda (outer-args)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax-case* (syntax result-stuff) (any values) module-or-top-identifier=?
                    [(any)
                     (syntax
                      ((x ...)
                       (begin
                         (check-pre-expr->pp/h pre-expr src-info pos-blame neg-blame orig-str)
                         (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                               ...)
                           (val (dom-id x) ...)))))]
                    [((values (rng-ids rng-ctc) ...) post-expr)
                     (and (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                          (not (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))))
                     (with-syntax ([(rng-ids-x ...) (generate-temporaries (syntax (rng-ids ...)))])
                       (syntax
                        ((x ...)
                         (begin
                           (check-pre-expr->pp/h pre-expr src-info pos-blame neg-blame orig-str)
                           (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                                 ...)
                             (let-values ([(rng-ids ...) (val (dom-id x) ...)])
                               (check-post-expr->pp/h post-expr src-info pos-blame neg-blame orig-str)
                               (let ([rng-ids-x ((coerce/select-contract stx-name rng-ctc)
                                                 pos-blame neg-blame src-info orig-str)] ...)
                                 (values (rng-ids-x rng-ids) ...))))))))]
                    [((values (rng-ids rng-ctc) ...) post-expr)
                     (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                     (let ([dup (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))])
                       (raise-syntax-error name "duplicate identifier" stx dup))]
                    [((values (rng-ids rng-ctc) ...) post-expr)
                     (for-each (lambda (rng-id) 
                                 (unless (identifier? rng-id)
                                   (raise-syntax-error name "expected identifier" stx rng-id)))
                               (syntax->list (syntax (rng-ids ...))))]
                    [((values . x) . junk)
                     (raise-syntax-error name "malformed multiple values result" stx (syntax (values . x)))]
                    [(rng res-id post-expr)
                     (syntax
                      ((x ...)
                       (begin
                         (check-pre-expr->pp/h pre-expr src-info pos-blame neg-blame orig-str)
                         (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                               ...
                               [rng-id ((coerce/select-contract stx-name rng) pos-blame neg-blame src-info orig-str)])
                           (let ([res-id (rng-id (val (dom-id x) ...))])
                             (check-post-expr->pp/h post-expr src-info pos-blame neg-blame orig-str)
                             res-id)))))]
                    [_ 
                     (raise-syntax-error name "unknown result specification" stx (syntax result-stuff))]))))))]
        [(_ ([x dom] ...) pre-expr . result-stuff)
         (andmap identifier? (syntax->list (syntax (x ...))))
         (raise-syntax-error 
          name
          "duplicate identifier"
          stx
          (check-duplicate-identifier (syntax->list (syntax (x ...)))))]
        [(_ ([x dom] ...) pre-expr . result-stuff)
         (for-each (lambda (x) (unless (identifier? x) (raise-syntax-error name "expected identifier" stx x)))
                   (syntax->list (syntax (x ...))))]
        [(_ (x ...) pre-expr . result-stuff)
         (for-each (lambda (x)
                     (syntax-case x ()
                       [(x y) (identifier? (syntax x)) (void)]
                       [bad (raise-syntax-error name "expected identifier and contract" stx (syntax bad))]))
                   (syntax->list (syntax (x ...))))]
        [(_ x dom pre-expr . result-stuff)
         (raise-syntax-error name "expected list of identifiers and expression pairs" stx (syntax x))]))
        
    ;; ->pp-rest/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->pp-rest/h method-proc? stx) (->r-pp-rest/h method-proc? '->pp-rest stx))
    
    ;; ->r-pp-rest/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->r-pp-rest/h method-proc? name stx)
      (syntax-case stx ()
        [(_ ([x dom] ...) rest-x rest-dom pre-expr . result-stuff)
         (and (identifier? (syntax rest-x))
              (andmap identifier? (syntax->list (syntax (x ...))))
              (not (check-duplicate-identifier (cons (syntax rest-x) (syntax->list (syntax (x ...)))))))
         (with-syntax ([stx-name name])
           (with-syntax ([(dom-id ...) (generate-temporaries (syntax (dom ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))]
                         [name-stx 
                          (with-syntax ([(name-xs ...) (if method-proc?
                                                           (cdr (syntax->list (syntax (x ...))))
                                                           (syntax (x ...)))])
                            (syntax 
                             (build-compound-type-name 'stx-name
                                                       `(,(build-compound-type-name 'name-xs '(... ...)) ...)
                                                       'rest-x
                                                       '(... ...)
                                                       '(... ...))))])
             (values
              (lambda (outer-args body)
                (with-syntax ([body body]
                              [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax
                   (let ([name-id name-stx])
                     body))))
              (lambda (outer-args inner-lambda) inner-lambda)
              (lambda (outer-args)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                              [kind-of-thing (if method-proc? 'method 'procedure)])
                  (syntax
                   (begin 
                     (check-procedure/more/kind val arity 'kind-of-thing src-info pos-blame neg-blame orig-str)))))
              (lambda (outer-args)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax-case* (syntax result-stuff) (values any) module-or-top-identifier=?
                    [(any)
                     (syntax
                      ((x ... . rest-x)
                       (begin
                         (check-pre-expr->pp/h pre-expr src-info pos-blame neg-blame orig-str)
                         (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                               ...
                               [rest-id ((coerce/select-contract stx-name rest-dom) neg-blame pos-blame src-info orig-str)])
                           (apply val (dom-id x) ... (rest-id rest-x))))))]
                    [(any . x)
                     (raise-syntax-error name "cannot have anything after any" stx (syntax result-stuff))]
                    [((values (rng-ids rng-ctc) ...) post-expr)
                     (and (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                          (not (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))))
                     (with-syntax ([(rng-ids-x ...) (generate-temporaries (syntax (rng-ids ...)))])
                       (syntax
                        ((x ... . rest-x)
                         (begin
                           (check-pre-expr->pp/h pre-expr src-info pos-blame neg-blame orig-str)
                           (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                                 ...
                                 [rest-id ((coerce/select-contract stx-name rest-dom) neg-blame pos-blame src-info orig-str)])
                             (let-values ([(rng-ids ...) (apply val (dom-id x) ... (rest-id rest-x))])
                               (check-post-expr->pp/h post-expr src-info pos-blame neg-blame orig-str)
                               (let ([rng-ids-x ((coerce/select-contract stx-name rng-ctc)
                                                 pos-blame neg-blame src-info orig-str)] ...)
                                 (values (rng-ids-x rng-ids) ...))))))))]
                    [((values (rng-ids rng-ctc) ...) . whatever)
                     (and (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                          (not (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))))
                     (raise-syntax-error name "expected exactly on post-expression at the end" stx)]
                    [((values (rng-ids rng-ctc) ...) . whatever)
                     (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                     (let ([dup (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))])
                       (raise-syntax-error name "duplicate identifier" stx dup))]
                    [((values (rng-ids rng-ctc) ...) . whatever)
                     (for-each (lambda (rng-id) 
                                 (unless (identifier? rng-id)
                                   (raise-syntax-error name "expected identifier" stx rng-id)))
                               (syntax->list (syntax (rng-ids ...))))]
                    [((values . x) . whatever)
                     (raise-syntax-error name "malformed multiple values result" stx (syntax (values . x)))]
                    [(rng res-id post-expr)
                     (identifier? (syntax res-id))
                     (syntax
                      ((x ... . rest-x)
                       (begin
                         (check-pre-expr->pp/h pre-expr src-info pos-blame neg-blame orig-str)
                         (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                               ...
                               [rest-id ((coerce/select-contract stx-name rest-dom) neg-blame pos-blame src-info orig-str)]
                               [rng-id ((coerce/select-contract stx-name rng) pos-blame neg-blame src-info orig-str)])
                           (let ([res-id (rng-id (apply val (dom-id x) ... (rest-id rest-x)))])
                             (check-post-expr->pp/h post-expr src-info pos-blame neg-blame orig-str)
                             res-id)))))]
                    [(rng res-id post-expr)
                     (not (identifier? (syntax res-id)))
                     (raise-syntax-error name "expected an identifier" stx (syntax res-id))]
                    [_
                     (raise-syntax-error name "malformed result sepecification" stx (syntax result-stuff))]))))))]
        [(_ ([x dom] ...) rest-x rest-dom pre-expr . result-stuff)
         (not (identifier? (syntax rest-x)))
         (raise-syntax-error name "expected identifier" stx (syntax rest-x))]
        [(_ ([x dom] ...) rest-x rest-dom rng . result-stuff)
         (and (identifier? (syntax rest-x))
              (andmap identifier? (cons (syntax rest-x) (syntax->list (syntax (x ...))))))
         (raise-syntax-error 
          name
          "duplicate identifier"
          stx
          (check-duplicate-identifier (syntax->list (syntax (x ...)))))]

        [(_ ([x dom] ...) rest-x rest-dom rng . result-stuff)
         (for-each (lambda (x) (unless (identifier? x) (raise-syntax-error name "expected identifier" stx x)))
                   (cons
                    (syntax rest-x)
                    (syntax->list (syntax (x ...)))))]
        [(_ x dom rest-x rest-dom rng . result-stuff)
         (raise-syntax-error name "expected list of identifiers and expression pairs" stx (syntax x))]))
    
    ;; select/h : syntax -> /h-function
    (define (select/h stx err-name ctxt-stx)
      (syntax-case stx (-> ->* ->d ->d* ->r ->pp ->pp-rest)
        [(-> . args) ->/h]
        [(->* . args) ->*/h]
        [(->d . args) ->d/h]
        [(->d* . args) ->d*/h]
        [(->r . args) ->r/h]
        [(->pp . args) ->pp/h]
        [(->pp-rest . args) ->pp-rest/h]
        [(xxx . args) (raise-syntax-error err-name "unknown arrow constructor" ctxt-stx (syntax xxx))]
        [_ (raise-syntax-error err-name "malformed arrow clause" ctxt-stx stx)]))
    
    
    ;; set-inferred-name-from : syntax syntax -> syntax
    (define (set-inferred-name-from with-name to-be-named)
      (let ([name (syntax-local-infer-name with-name)])
        (cond
          [(identifier? name)
           (with-syntax ([rhs (syntax-property to-be-named 'inferred-name (syntax-e name))]
                         [name (syntax-e name)])
             (syntax (let ([name rhs]) name)))]
          [(symbol? name)
           (with-syntax ([rhs (syntax-property to-be-named 'inferred-name name)]
                         [name name])
             (syntax (let ([name rhs]) name)))]
          [else to-be-named])))
    
    ;; (cons X (listof X)) -> (listof X)
    ;; returns the elements of `l', minus the last element
    ;; special case: if l is an improper list, it leaves off
    ;; the contents of the last cdr (ie, making a proper list
    ;; out of the input), so (all-but-last '(1 2 . 3)) = '(1 2)
    (define (all-but-last l)
      (cond
        [(null? l) (error 'all-but-last "bad input")]
        [(not (pair? l)) '()]
        [(null? (cdr l)) null]
        [(pair? (cdr l)) (cons (car l) (all-but-last (cdr l)))]
        [else (list (car l))]))
    
    ;; generate-indicies : syntax[list] -> (cons number (listof number))
    ;; given a syntax list of length `n', returns a list containing
    ;; the number n followed by th numbers from 0 to n-1
    (define (generate-indicies stx)
      (let ([n (length (syntax->list stx))])
        (cons n
              (let loop ([i n])
                (cond
                  [(zero? i) null]
                  [else (cons (- n i)
                              (loop (- i 1)))]))))))

  ;; procedure-accepts-and-more? : procedure number -> boolean
  ;; returns #t if val accepts dom-length arguments and
  ;; any number of arguments more than dom-length. 
  ;; returns #f otherwise.
  (define (procedure-accepts-and-more? val dom-length)
    (let ([arity (procedure-arity val)])
      (cond
        [(number? arity) #f]
        [(arity-at-least? arity)
         (<= (arity-at-least-value arity) dom-length)]
        [else
         (let ([min-at-least (let loop ([ars arity]
                                        [acc #f])
                               (cond
                                 [(null? ars) acc]
                                 [else (let ([ar (car ars)])
                                         (cond
                                           [(arity-at-least? ar)
                                            (if (and acc
                                                     (< acc (arity-at-least-value ar)))
                                                (loop (cdr ars) acc)
                                                (loop (cdr ars) (arity-at-least-value ar)))]
                                           [(number? ar)
                                            (loop (cdr ars) acc)]))]))])
           (and min-at-least
                (begin
                  (let loop ([counts (quicksort (filter number? arity) >=)])
                    (unless (null? counts)
                      (let ([count (car counts)])
                        (cond
                          [(= (+ count 1) min-at-least)
                           (set! min-at-least count)
                           (loop (cdr counts))]
                          [(< count min-at-least)
                           (void)]
                          [else (loop (cdr counts))]))))
                  (<= min-at-least dom-length))))])))

  ;; ----------------------------------------
  ;; Checks and error functions used in macro expansions
  
  (define (check->* f arity-count)
    (unless (procedure? f)
      (error 'object-contract "expected last argument of ->d* to be a procedure, got ~e" f))
    (unless (procedure-arity-includes? f arity-count)
      (error 'object-contract 
	     "expected last argument of ->d* to be a procedure that accepts ~a arguments, got ~e"
	     arity-count
	     f)))

  (define (check->*/more f arity-count)
    (unless (procedure? f)
      (error 'object-contract "expected last argument of ->d* to be a procedure, got ~e" f))
    (unless (procedure-accepts-and-more? f arity-count)
      (error 'object-contract 
	     "expected last argument of ->d* to be a procedure that accepts ~a arguments and arbitrarily many more, got ~e"
	     arity-count
	     f)))


  (define (check-pre-expr->pp/h pre-expr src-info pos-blame neg-blame orig-str)
    (unless pre-expr
      (raise-contract-error src-info
                            neg-blame
                            pos-blame
                            orig-str
                            "pre-condition expression failure")))
  
  (define (check-post-expr->pp/h post-expr src-info pos-blame neg-blame orig-str)
    (unless post-expr
      (raise-contract-error src-info
                            pos-blame
                            neg-blame
                            orig-str
                            "post-condition expression failure")))
  
  (define (check-procedure val dom-length src-info pos-blame neg-blame orig-str)
    (unless (and (procedure? val)
		 (procedure-arity-includes? val dom-length))
      (raise-contract-error
       src-info
       pos-blame
       neg-blame
       orig-str
       "expected a procedure that accepts ~a arguments, given: ~e"
       dom-length
       val)))

  (define (check-procedure/kind val arity kind-of-thing src-info pos-blame neg-blame orig-str)
    (unless (procedure? val)
      (raise-contract-error src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected a procedure, got ~e"
			    val))
    (unless (procedure-arity-includes? val arity)
      (raise-contract-error src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected a ~a of arity ~a (not arity ~a), got  ~e"
			    kind-of-thing
			    arity
			    (procedure-arity val)
			    val)))

  (define (check-procedure/more/kind val arity kind-of-thing src-info pos-blame neg-blame orig-str)
    (unless (procedure? val)
      (raise-contract-error src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected a procedure, got ~e"
			    val))
    (unless (procedure-accepts-and-more? val arity)
      (raise-contract-error src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected a ~a that accepts ~a arguments and aribtrarily more (not arity ~a), got  ~e"
			    kind-of-thing
			    arity
			    (procedure-arity val)
			    val)))

  (define (check-procedure/more val dom-length src-info pos-blame neg-blame orig-str)
    (unless (and (procedure? val)
		 (procedure-accepts-and-more? val dom-length))
      (raise-contract-error
       src-info
       pos-blame
       neg-blame
       orig-str
       "expected a procedure that accepts ~a arguments and any number of arguments larger than ~a, given: ~e"
       dom-length
       dom-length
       val)))


  (define (check-rng-procedure who rng-x arity)
    (unless (and (procedure? rng-x)
		 (procedure-arity-includes? rng-x arity))
      (error who "expected range position to be a procedure that accepts ~a arguments, given: ~e"
	     arity
	     rng-x)))

  (define (check-rng-procedure/more rng-mk-x arity)
    (unless (and (procedure? rng-mk-x)
		 (procedure-accepts-and-more? rng-mk-x arity))
      (error '->d* "expected range position to be a procedure that accepts ~a arguments and arbitrarily many more, given: ~e"
	     arity 
	     rng-mk-x)))

  (define (check-rng-lengths results rng-contracts)
    (unless (= (length results) (length rng-contracts))
      (error '->d* 
	     "expected range contract contructor and function to have the same number of values, given: ~a and ~a respectively" 
	     (length results) (length rng-contracts))))

  (define (check-object val src-info pos-blame neg-blame orig-str)
    (unless (object? val)
      (raise-contract-error src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected an object, got ~e"
			    val)))

  (define (check-method method-name val-mtd-names src-info pos-blame neg-blame orig-str)
    (unless (memq method-name val-mtd-names)
      (raise-contract-error src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected an object with method ~s"
			    method-name)))

  (define (field-error field-name src-info pos-blame neg-blame orig-str)
    (raise-contract-error src-info
			  pos-blame
			  neg-blame
			  orig-str
			  "expected an object with field ~s"
			  field-name))
                                                
  #|

  test cases for procedure-accepts-and-more?

  (and (procedure-accepts-and-more? (lambda (x . y) 1) 3)
       (procedure-accepts-and-more? (lambda (x . y) 1) 2)
       (procedure-accepts-and-more? (lambda (x . y) 1) 1)
       (not (procedure-accepts-and-more? (lambda (x . y) 1) 0))
       
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 3)
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 2)
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 1)
       (not (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 0))
       
       (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 2)
       (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 1)
       (not (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 0)))
  
  |#
    
    
  ;; coerce/select-contract : id (union contract? procedure-arity-1) -> contract-proc
  ;; contract-proc = sym sym stx -> alpha -> alpha
  ;; returns the procedure for the contract after extracting it from the
  ;; struct. Coerces the argument to a flat contract if it is procedure, but first.
  (define-syntax (coerce/select-contract stx)
    (syntax-case stx ()
      [(_ name val)
       (syntax (coerce/select-contract/proc 'name val))]))

  (define (coerce/select-contract/proc name x)
    (cond
     [(contract? x)
      (contract-proc x)]
     [(and (procedure? x) (procedure-arity-includes? x 1))
      (contract-proc (flat-contract x))]
     [else
      (error name 
	     "expected contract or procedure of arity 1, got ~e"
	     x)]))
  
  ;; coerce-contract : id (union contract? procedure-arity-1) -> contract
  ;; contract-proc = sym sym stx -> alpha -> alpha
  ;; returns the procedure for the contract after extracting it from the
  ;; struct. Coerces the argument to a flat contract if it is procedure, but first.
  (define-syntax (coerce-contract stx)
    (syntax-case stx ()
      [(_ name val)
       (syntax (coerce-contract/proc 'name val))]))

  (define (coerce-contract/proc name x)
    (cond
     [(contract? x) x]
     [(and (procedure? x) (procedure-arity-includes? x 1))
      (flat-contract x)]
     [else
      (error name 
             "expected contract or procedure of arity 1, got ~e"
              x)]))

;                                                                                                   
;                                                                                                   
;                                                                                                   
;               ;                                                                                   
;                                                                                                   
;                                                                ;                       ;          
;   ; ;;  ;;    ;    ;;;    ;;;            ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;;   ;;;  
;   ;;  ;;  ;   ;   ;      ;   ;          ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;    ;     
;   ;   ;   ;   ;   ;;    ;              ;      ;     ;  ;   ;   ;    ;       ;  ;       ;    ;;    
;   ;   ;   ;   ;    ;;   ;              ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;     ;;   
;   ;   ;   ;   ;      ;  ;              ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;       ;  
;   ;   ;   ;   ;      ;   ;   ;  ;       ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;       ;  
;   ;   ;   ;   ;   ;;;     ;;;   ;        ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;;  ;;;   
;                                                                                                   
;                                                                                                   
;                                                                                                   

  
  
  (provide any/c
           anaphoric-contracts
           flat-rec-contract
           flat-murec-contract
	   union
           and/c
	   not/c
           =/c >=/c <=/c </c >/c 
           integer-in
           exact-integer-in
	   real-in
           natural-number/c
	   string/len
           false/c
	   printable/c
           symbols
	   is-a?/c subclass?/c implementation?/c
           listof list-immutableof 
           vectorof vector-immutableof vector/c vector-immutable/c 
           cons-immutable/c cons/c list-immutable/c list/c
           box-immutable/c box/c
           promise/c
           struct/c
	   mixin-contract make-mixin-contract
           syntax/c)
  
  (define-syntax (flat-rec-contract stx)
    (syntax-case stx  ()
      [(_ name ctc ...)
       (identifier? (syntax name))
       (with-syntax ([(ctc-id ...) (generate-temporaries (syntax (ctc ...)))]
                     [(pred-id ...) (generate-temporaries (syntax (ctc ...)))])
         (syntax 
          (let* ([pred (lambda (x) (error 'flat-rec-contract "applied too soon"))]
                 [name (flat-contract (let ([name (lambda (x) (pred x))]) name))])
            (let ([ctc-id (coerce-contract flat-rec-contract ctc)] ...)
              (unless (flat-contract? ctc-id)
                (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
              ...
              (set! pred
                    (let ([pred-id (flat-contract-predicate ctc-id)] ...)
                      (lambda (x)
                        (or (pred-id x) ...))))
              name))))]
      [(_ name ctc ...)
       (raise-syntax-error 'flat-rec-contract "expected first argument to be an identifier" stx (syntax name))]))

  (define-syntax (flat-murec-contract stx)
    (syntax-case stx  ()
      [(_ ([name ctc ...] ...) body1 body ...)
       (andmap identifier? (syntax->list (syntax (name ...))))
       (with-syntax ([((ctc-id ...) ...) (map generate-temporaries
                                              (syntax->list (syntax ((ctc ...) ...))))]
                     [(pred-id ...) (generate-temporaries (syntax (name ...)))]
                     [((pred-arm-id ...) ...) (map generate-temporaries
                                               (syntax->list (syntax ((ctc ...) ...))))])
         (syntax 
          (let* ([pred-id (lambda (x) (error 'flat-murec-contract "applied too soon"))] ...
                 [name (flat-contract (let ([name (lambda (x) (pred-id x))]) name))] ...)
            (let-values ([(ctc-id ...) (values (coerce-contract flat-rec-contract ctc) ...)] ...)
              (begin
                (void)
                (unless (flat-contract? ctc-id)
                  (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
                ...) ...
              (set! pred-id
                    (let ([pred-arm-id (flat-contract-predicate ctc-id)] ...)
                      (lambda (x)
                        (or (pred-arm-id x) ...)))) ...
              body1
              body ...))))]
      [(_ ([name ctc ...] ...) body1 body ...)
       (for-each (lambda (name)
                   (unless (identifier? name)
                     (raise-syntax-error 'flat-rec-contract
                                         "expected an identifier" stx name)))
                 (syntax->list (syntax (name ...))))]
      [(_ ([name ctc ...] ...))
       (raise-syntax-error 'flat-rec-contract "expected at least one body expression" stx)]))
      
  
  (define anaphoric-contracts
    (case-lambda
      [() (make-anaphoric-contracts (make-hash-table 'weak))]
      [(x)
       (unless (eq? x 'equal)
         (error 'anaphoric-contracts "expected either no arguments, or 'equal as first argument, got ~e" x))
       (make-anaphoric-contracts (make-hash-table 'equal 'weak))]))

  (define (make-anaphoric-contracts ht)
    (values
     (flat-named-contract
      "(anaphoric-contracts,from)"
      (lambda (v)
        (hash-table-put! ht v #t)
        v))
     (flat-named-contract
      "(anaphoric-contracts,to)"
      (lambda (v)
        (hash-table-get ht v (lambda () #f))))))     
  
  (define (union . args)
    (for-each
     (lambda (x) 
       (unless (or (contract? x)
                   (and (procedure? x)
                        (procedure-arity-includes? x 1)))
         (error 'union "expected procedures of arity 1 or contracts, given: ~e" x)))
     args)
    (let-values ([(contract fc/predicates)
                  (let loop ([contract #f]
                             [fc/predicates null]
                             [args args])
                    (cond
                      [(null? args) (values contract (reverse fc/predicates))]
                      [else 
                       (let ([arg (car args)])
                         (cond
                           [(or (flat-contract? arg)
                                (not (contract? arg)))
                            (loop contract (cons arg fc/predicates) (cdr args))]
                           [contract
                            (error 'union "expected at most one non-flat contract, given ~e and ~e"
                                   contract
                                   arg)]
                           [else (loop arg fc/predicates (cdr args))]))]))])
      (let* ([flat-contracts (map (lambda (x) (if (flat-contract? x)
                                                  x
                                                  (flat-contract x)))
                                  fc/predicates)]
             [predicates (map flat-contract-predicate flat-contracts)])
        (cond
          [contract
           (let ([c-proc (contract-proc contract)])
             (make-contract
              (apply build-compound-type-name 'union contract flat-contracts)
              (lambda (pos neg src-info orig-str)
                (let ([partial-contract (c-proc pos neg src-info orig-str)])
                  (lambda (val)
                    (cond
                      [(ormap (lambda (pred) (pred val)) predicates)
                       val]
                      [else
                       (partial-contract val)]))))))]
          [else
           (build-flat-contract
            (apply build-compound-type-name 'union flat-contracts)
            (lambda (x)
              (ormap (lambda (pred) (pred x)) predicates)))]))))
  
  (define false/c
    (flat-named-contract
     'false/c
     (lambda (x) (not x))))
  
  (define any/c
    (make-flat-contract
     'any/c
     (lambda (pos neg src-info orig-str) (lambda (val) val))
     (lambda (x) #t)))

  (define (string/len n)
    (unless (number? n)
      (error 'string/len "expected a number as argument, got ~e" n))
    (flat-named-contract 
     `(string/len ,n)
     (lambda (x)
       (and (string? x)
            ((string-length x) . < . n)))))
  
  (define (symbols . ss)
    (unless ((length ss) . >= . 1)
      (error 'symbols "expected at least one argument"))
    (unless (andmap symbol? ss)
      (error 'symbols "expected symbols as arguments, given: ~a"
	     (apply string-append (map (lambda (x) (format "~e " x)) ss))))
    (flat-named-contract
     `(symbols ,@(map (lambda (x) `',x) ss))
     (lambda (x)
       (memq x ss))))
  
  (define printable/c
    (flat-named-contract
     'printable/c
     (lambda (x)
       (let printable? ([x x])
	 (or (symbol? x)
	     (string? x)
	     (bytes? x)
	     (boolean? x)
	     (char? x)
	     (null? x)
	     (number? x)
	     (regexp? x)
	     (and (pair? x)
		  (printable? (car x))
		  (printable? (cdr x)))
	     (and (vector? x)
		  (andmap printable? (vector->list x)))
	     (and (box? x)
		  (printable? (unbox x))))))))
  
  (define (=/c x)
    (flat-named-contract
     `(=/c ,x)
     (lambda (y) (and (number? y) (= y x)))))
  (define (>=/c x)
    (flat-named-contract
     `(>=/c ,x)
     (lambda (y) (and (number? y) (>= y x)))))
  (define (<=/c x)
    (flat-named-contract
     `(<=/c ,x)
     (lambda (y) (and (number? y) (<= y x)))))
  (define (</c x)
    (flat-named-contract
     `(</c ,x)
     (lambda (y) (and (number? y) (< y x)))))
  (define (>/c x)
    (flat-named-contract
     `(>/c ,x)
     (lambda (y) (and (number? y) (> y x)))))

  (define natural-number/c
    (flat-named-contract
     'natural-number/c
     (lambda (x)
       (and (number? x)
	    (integer? x)
	    (x . >= . 0)))))
  
  (define (integer-in start end)
    (unless (and (integer? start)
                 (integer? end))
      (error 'integer-in "expected two integers as arguments, got ~e and ~e" start end))
    (flat-named-contract 
     `(integer-in ,start ,end)
     (lambda (x)
       (and (integer? x)
            (<= start x end)))))
  
  (define (exact-integer-in start end)
    (unless (and (integer? start)
                 (exact? start)
                 (integer? end)
                 (exact? end))
      (error 'integer-in "expected two exact integers as arguments, got ~e and ~e" start end))
    (flat-named-contract 
     `(exact-integer-in ,start ,end)
     (lambda (x)
       (and (integer? x)
            (exact? x)
            (<= start x end)))))

  (define (real-in start end)
    (unless (and (real? start)
                 (real? end))
      (error 'real-in "expected two real numbers as arguments, got ~e and ~e" start end))
    (flat-named-contract 
     `(real-in ,start ,end)
     (lambda (x)
       (and (real? x)
            (<= start x end)))))
  
  (define (and/c . fs)
    (for-each
     (lambda (x) 
       (unless (or (contract? x)
                   (and (procedure? x)
                        (procedure-arity-includes? x 1)))
         (error 'and/c "expected procedures of arity 1 or <contract>s, given: ~e" x)))
     fs)
    (cond
      [(null? fs) any/c]
      [(andmap flat-contract/predicate? fs)
       (let* ([to-predicate
	       (lambda (x)
		 (if (flat-contract? x)
		     (flat-contract-predicate x)
		     x))]
	      [contracts (map (lambda (x) (if (contract? x) x (flat-contract x))) fs)]
              [pred
	       (let loop ([pred (to-predicate (car fs))]
			  [preds (cdr fs)])
		 (cond
		   [(null? preds) pred]
		   [else
		    (let* ([fst (to-predicate (car preds))])
		      (loop (let ([and/c-contract? (lambda (x) (and (pred x) (fst x)))])
                              and/c-contract?)
			    (cdr preds)))]))])
	 (flat-named-contract (apply build-compound-type-name 'and/c contracts) pred))]
      [else
       (let* ([contracts (map (lambda (x) (if (contract? x) x (flat-contract x))) fs)]
              [contract/procs (map contract-proc contracts)])
	 (make-contract
          (apply build-compound-type-name 'and/c contracts)
          (lambda (pos neg src-info orig-str)
            (let ([partial-contracts (map (lambda (contract/proc) (contract/proc pos neg src-info orig-str))
                                          contract/procs)])
              (let loop ([ctct (car partial-contracts)]
                         [rest (cdr partial-contracts)])
                (cond
                  [(null? rest) ctct]
                  [else 
                   (let ([fst (car rest)])
                     (loop (lambda (x) (fst (ctct x)))
                           (cdr rest)))]))))))]))

  (define (not/c f)
    (unless (flat-contract/predicate? f)
      (error 'not/c "expected a procedure of arity 1 or <flat-named-contract>, given: ~e" f))
    (build-flat-contract
     (build-compound-type-name 'not/c (proc/ctc->ctc f))
     (lambda (x) (not (test-proc/flat-contract f x)))))

  (define (is-a?/c <%>)
    (unless (or (interface? <%>)
		(class? <%>))
      (error 'is-a?/c "expected <interface> or <class>, given: ~e" <%>))
    (let ([name (object-name <%>)])
      (flat-named-contract
       (cond
         [name
          `(is-a?/c ,name)]
         [(class? <%>)
          `(is-a?/c unknown%)]
         [else `(is-a?/c unknown<%>)])
       (lambda (x) (is-a? x <%>)))))

  (define (listof p)
    (unless (flat-contract/predicate? p)
      (error 'listof "expected a flat contract or procedure of arity 1 as argument, got: ~e" p))
    (build-flat-contract
     (build-compound-type-name 'listof (proc/ctc->ctc p))
     (lambda (v)
       (and (list? v)
	    (andmap (lambda (ele) (test-proc/flat-contract p ele))
		    v)))))
  
  (define-syntax (*-immutableof stx)
    (syntax-case stx ()
      [(_ predicate? fill type-name name)
       (syntax
        (let ([predicate?-name predicate?]
              [fill-name fill])
          (lambda (input)
            (let* ([ctc (coerce-contract name input)]
                   [p (contract-proc ctc)])
              (make-contract
               (build-compound-type-name 'name ctc)
               (lambda (pos neg src-info orig-str)
                 (let ([p-app (p pos neg src-info orig-str)])
                   (lambda (val)
                     (unless (predicate?-name val)
                       (raise-contract-error
                        src-info
                        pos
                        neg
                        orig-str
                        "expected <~a>, given: ~e"
                        'type-name
                        val))
                     (fill-name p-app val)))))))))]))
  
  (define (map-immutable f lst)
    (let loop ([lst lst])
      (cond
        [(pair? lst)
         (cons-immutable (f (car lst))
                         (loop (cdr lst)))]
        [(null? lst) null])))
  
  (define (immutable-list? lst)
    (cond
      [(and (pair? lst)
            (immutable? lst))
       (immutable-list? (cdr lst))]
      [(null? lst) #t]
      [else #f]))
  
  (define list-immutableof
    (*-immutableof immutable-list? map-immutable immutable-list list-immutableof))  

  (define vector-immutableof
    (*-immutableof (lambda (x) (and (vector? x) (immutable? x)))
                   (lambda (f v) (apply vector-immutable (map f (vector->list v))))
                   immutable-vector
                   vector-immutableof))
  
  (define (vectorof p)
    (unless (flat-contract/predicate? p)
      (error 'vectorof "expected a flat contract or procedure of arity 1 as argument, got: ~e" p))
    (build-flat-contract
     (build-compound-type-name 'vectorof (proc/ctc->ctc p))
     (lambda (v)
       (and (vector? v)
	    (andmap (lambda (ele) (test-proc/flat-contract p ele))
		    (vector->list v))))))

  (define (vector/c . args)
    (unless (andmap flat-contract/predicate? args)
      (error 'vector/c "expected flat contracts as arguments, got: ~a"
             (let loop ([args args])
               (cond
                 [(null? args) ""]
                 [(null? (cdr args)) (format "~e" (car args))]
                 [else (string-append
                        (format "~e " (car args))
                        (loop (cdr args)))]))))
    (let ([largs (length args)])
      (build-flat-contract
       (apply build-compound-type-name 'vector/c (map proc/ctc->ctc args))
       (lambda (v)
         (and (vector? v)
              (= (vector-length v) largs)
              (andmap test-proc/flat-contract
                      args
                      (vector->list v)))))))
  
  (define (box/c pred)
    (unless (flat-contract/predicate? pred)
      (error 'box/c "expected a flat contract or a procedure of arity 1, got: ~e" pred))
    (build-flat-contract
     (build-compound-type-name 'box/c (proc/ctc->ctc pred))
     (lambda (x)
       (and (box? x)
	    (test-proc/flat-contract pred (unbox x))))))

  (define (cons/c hdp tlp)
    (unless (and (flat-contract/predicate? hdp)
                 (flat-contract/predicate? tlp))
      (error 'cons/c "expected two flat contracts or procedures of arity 1, got: ~e and ~e" hdp tlp))
    (build-flat-contract
     (build-compound-type-name 'cons/c (proc/ctc->ctc hdp) (proc/ctc->ctc tlp))
     (lambda (x)
       (and (pair? x)
            (test-proc/flat-contract hdp (car x))
            (test-proc/flat-contract tlp (cdr x))))))
  
  (define-syntax (*-immutable/c stx)
    (syntax-case stx ()
      [(_ predicate? constructor (arb? selectors ...) type-name name)
       (eq? #f (syntax-object->datum (syntax arb?)))
       (with-syntax ([(params ...) (generate-temporaries (syntax (selectors ...)))]
                     [(p-apps ...) (generate-temporaries (syntax (selectors ...)))]
                     [(procs ...) (generate-temporaries (syntax (selectors ...)))]
                     [(selector-names ...) (generate-temporaries (syntax (selectors ...)))])
         (syntax
          (let ([predicate?-name predicate?]
                [constructor-name constructor]
                [selector-names selectors] ...)
            (lambda (params ...)
              (let ([procs (coerce/select-contract name params)] ...)
                (make-contract
                 (build-compound-type-name 'name (proc/ctc->ctc params) ...)
                 (lambda (pos neg src-info orig-str)
                   (let ([p-apps (procs pos neg src-info orig-str)] ...)
                     (lambda (v)
                       (if (and (immutable? v)
                                (predicate?-name v))
                           (constructor-name (p-apps (selector-names v)) ...)
                           (raise-contract-error
                            src-info
                            pos
                            neg
                            orig-str
                            "expected <~a>, given: ~e"
                            'type-name
                            v)))))))))))]
      [(_ predicate? constructor (arb? selector) correct-size type-name name)
       (eq? #t (syntax-object->datum (syntax arb?)))
       (syntax
        (let ([predicate?-name predicate?]
              [constructor-name constructor]
              [selector-name selector])
          (lambda params
            (let ([procs (map (lambda (param) (coerce/select-contract name param)) params)])
              (make-contract
               (apply build-compound-type-name 'name (map proc/ctc->ctc params))
               (lambda (pos neg src-info orig-str)
                 (let ([p-apps (map (lambda (proc) (proc pos neg src-info orig-str)) procs)]
                       [count (length params)])
                   (lambda (v)
                     (if (and (immutable? v)
                              (predicate?-name v)
                              (correct-size count v))
                         (apply constructor-name 
                                (let loop ([p-apps p-apps]
                                           [i 0])
                                  (cond
                                    [(null? p-apps) null]
                                    [else (let ([p-app (car p-apps)])
                                            (cons (p-app (selector-name v i))
                                                  (loop (cdr p-apps) (+ i 1))))])))
                         (raise-contract-error
                          src-info
                          pos
                          neg
                          orig-str
                          "expected <~a>, given: ~e"
                          'type-name
                          v))))))))))]))
  
  (define cons-immutable/c (*-immutable/c pair? cons-immutable (#f car cdr) immutable-cons cons-immutable/c))
  (define box-immutable/c (*-immutable/c box? box-immutable (#f unbox) immutable-box box-immutable/c))
  (define vector-immutable/c (*-immutable/c vector?
                                            vector-immutable
                                            (#t (lambda (v i) (vector-ref v i)))
                                            (lambda (n v) (= n (vector-length v)))
                                            immutable-vector
                                            vector-immutable/c))
       
  (define (list/c . args)
    (unless (andmap flat-contract/predicate? args)
      (error 'list/c "expected flat contracts, got: ~a"
             (let loop ([args args])
               (cond
                 [(null? args) ""]
                 [(null? (cdr args)) (format "~e" (car args))]
                 [else (string-append
                        (format "~e " (car args))
                        (loop (cdr args)))]))))
    (let loop ([args args])
      (cond
	[(null? args) (flat-contract null?)]
	[else (cons/c (car args) (loop (cdr args)))])))
  
  (define (list-immutable/c . args)
    (unless (andmap (lambda (x) (or (contract? x)
                                    (and (procedure? x)
                                         (procedure-arity-includes? x 1))))
                    args)
      (error 'list/c "expected flat contracts or procedures of arity 1, got: ~a"
             (let loop ([args args]) 
               (cond
                 [(null? args) ""]
                 [(null? (cdr args)) (format "~e" (car args))]
                 [else (string-append
                        (format "~e " (car args))
                        (loop (cdr args)))]))))
    (let loop ([args args])
      (cond
	[(null? args) (flat-contract null?)]
	[else (cons-immutable/c (car args) (loop (cdr args)))])))

  (define (syntax/c ctc-in)
    (let ([ctc (coerce-contract syntax/c ctc-in)])
      (build-flat-contract
       (build-compound-type-name 'syntax/c ctc)
       (let ([pred (flat-contract-predicate ctc)])
         (lambda (val)
           (and (syntax? val)
                (pred (syntax-e val))))))))
  
  (define promise/c
    (lambda (ctc-in)
      (let* ([ctc (coerce-contract promise/c ctc-in)]
             [ctc-proc (contract-proc ctc)])
        (make-contract
         (build-compound-type-name 'promise/c ctc)
         (lambda (pos neg src-info orig-str)
           (let ([p-app (ctc-proc pos neg src-info orig-str)])
             (lambda (val)
               (unless (promise? val)
                 (raise-contract-error
                  src-info
                  pos
                  neg
                  orig-str
                  "expected <promise>, given: ~e"
                  val))
               (delay (p-app (force val))))))))))
  
  #|
   as with copy-struct in struct.ss, this first begin0
   expansion "declares" that struct/c is an expression.
   It prevents further expansion until the internal definition
   context is sorted out.
  |#
  (define-syntax (struct/c stx)
    (syntax-case stx ()
      [(_ . args) (syntax (begin0 (do-struct/c . args)))]))
  
  (define-syntax (do-struct/c stx)
    (syntax-case stx ()
      [(_ struct-name args ...)
       (and (identifier? (syntax struct-name))
            (syntax-local-value (syntax struct-name) (lambda () #f)))
       (with-syntax ([(ctc-x ...) (generate-temporaries (syntax (args ...)))]
                     [(ctc-proc-x ...) (generate-temporaries (syntax (args ...)))]
                     [(ctc-app-x ...) (generate-temporaries (syntax (args ...)))]
                     [(type-desc-id 
                       constructor-id 
                       predicate-id 
                       (selector-id ...)
                       (mutator-id ...)
                       super-id)
                      (syntax-local-value (syntax struct-name))])
         (syntax
          (let ([ctc-x (coerce-contract struct/c args)] ...)
            
            (unless predicate-id
              (error 'struct/c "could not determine predicate for ~s" 'struct-name))
            (unless (and selector-id ...)
              (error 'struct/c "could not determine selectors for ~s" 'struct-name))
            
            (unless (flat-contract? ctc-x)
              (error 'struct/c "expected flat contracts as arguments, got ~e" ctc-x))
            ...
            
            (let ([ctc-proc-x (contract-proc ctc-x)] ...)
              (make-contract
               (build-compound-type-name 'struct/c 'struct-name ctc-x ...)
               (lambda (pos neg src-info orig-str)
                 (let ([ctc-app-x (ctc-proc-x pos neg src-info orig-str)] ...)
                   (lambda (val)
                     (unless (predicate-id val)
                       (raise-contract-error
                        src-info
                        pos
                        neg
                        orig-str
                        "expected <~a>, given: ~e"
                        'struct-name
                        val))
                    (ctc-app-x (selector-id val)) ...
                     val))))))))]
      [(_ struct-name anything ...)
       (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))]))
  
  (define (flat-contract/predicate? pred)
    (or (flat-contract? pred)
        (and (procedure? pred)
             (procedure-arity-includes? pred 1))))
  
  (define (subclass?/c %)
    (unless (class? %)
      (error 'subclass?/c "expected <class>, given: ~e" %))
    (let ([name (object-name %)])
      (flat-named-contract
       `(subclass?/c ,(or name 'unknown%))
       (lambda (x) (subclass? x %)))))

  (define (implementation?/c <%>)
    (unless (interface? <%>)
      (error 'implementation?/c "expected <interface>, given: ~e" <%>))
    (let ([name (object-name <%>)])
      (flat-named-contract
       `(implementation?/c ,(or name 'unknown<%>))
       (lambda (x) (implementation? x <%>)))))

  (define mixin-contract (class? . ->d . subclass?/c))
  
  (define (make-mixin-contract . %/<%>s)
    ((and/c (flat-contract class?)
            (apply and/c (map sub/impl?/c %/<%>s)))
     . ->d .
     subclass?/c))

  (define (sub/impl?/c %/<%>)
    (cond
      [(interface? %/<%>) (implementation?/c %/<%>)]
      [(class? %/<%>) (subclass?/c %/<%>)]
      [else (error 'make-mixin-contract "unknown input ~e" %/<%>)])))
