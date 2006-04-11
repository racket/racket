#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

add struct contracts for immutable structs?

|#

(module contract mzscheme
  
  (provide (rename -contract contract)
           (rename -contract/pos contract/pos)
           (rename -contract/neg contract/neg)
           recursive-contract
           provide/contract
           define/contract)

  (require-for-syntax mzscheme
                      (lib "list.ss")
                      (lib "stx.ss" "syntax")
                      (lib "name.ss" "syntax"))
  
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "pretty.ss")
           (lib "pconvert.ss")
           "contract-arrow.ss"
           "contract-guts.ss")
  
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
                         [ctrct (syntax-property ctrct 'inferred-name id)]
                         [external-name (or user-rename-id id)]
                         [where-stx stx])
             (with-syntax ([code
                            (syntax/loc stx
                              (begin
                                (provide (rename id-rename external-name))
                                
                                ;; unbound id check
                                (if #f id)
                                
                                (define pos-module-source (module-source-as-symbol #'pos-stx))
                                (define contract-id ctrct)
                                (define-syntax id-rename
				  (make-provide/contract-transformer (quote-syntax contract-id)
								     (quote-syntax id)
								     (quote-syntax pos-module-source)))))])
               (syntax (code id-rename)))))
         
         (with-syntax ([(bodies ...) (code-for-each-clause (syntax->list (syntax (p/c-ele ...))))])
           (syntax 
            (begin
              bodies ...))))]))
  
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
  
  (define-syntax (-contract/pos stx)
    (syntax-case stx ()
      [(_ a-contract to-check blame-e)
       (with-syntax ([src-loc (syntax/loc stx here)])
         (syntax/loc stx
           (contract/one/proc contract-pos-proc a-contract to-check blame-e (quote-syntax src-loc))))]
      [(_ a-contract-e to-check blame-e src-info-e)
       (syntax/loc stx
         (contract/one/proc contract-pos-proc a-contract-e to-check blame-e src-info-e))]))
  
  (define-syntax (-contract/neg stx)
    (syntax-case stx ()
      [(_ a-contract to-check blame-e)
       (with-syntax ([src-loc (syntax/loc stx here)])
         (syntax/loc stx
           (contract/one/proc contract-neg-proc a-contract to-check blame-e (quote-syntax src-loc))))]
      [(_ a-contract-e to-check blame-e src-info-e)
       (syntax/loc stx
         (contract/one/proc contract-neg-proc a-contract-e to-check blame-e src-info-e))]))
  
  (define (contract/one/proc contract-to-proc a-contract-raw name blame src-info)
    (unless (or (contract? a-contract-raw)
                (and (procedure? a-contract-raw)
                     (procedure-arity-includes? a-contract-raw 1)))
      (error 'contract/pos "expected a contract or a procedure of arity 1 as first argument, given: ~e, other args ~e ~e ~e" 
             a-contract-raw
             name
             blame
             src-info))
    (let ([a-contract (if (contract? a-contract-raw)
                          a-contract-raw
                          (flat-contract a-contract-raw))])
      (unless (symbol? blame)
        (error 'contract
               "expected symbol as name for assigning blame, given: ~e, other args ~e ~e ~e"
               blame
               a-contract-raw 
               name
               src-info))
      (unless (syntax? src-info)
        (error 'contract "expected syntax as last argument, given: ~e, other args ~e ~e ~e"
               src-info
               blame
               a-contract-raw
               name))
      (((contract-to-proc a-contract) blame src-info (contract-name a-contract))
       name)))
    
  (define-syntax (recursive-contract stx)
    (syntax-case stx ()
      [(_ arg)
       (syntax (make-pair-proj-contract 
                '(recursive-contract arg) 
                (λ (blame src str)
                  (let ([proc (contract-pos-proc arg)])
                    (λ (val)
                      ((proc blame src str) val))))
                (λ (blame src str)
                  (let ([proc (contract-neg-proc arg)])
                    (λ (val)
                      ((proc blame src str) val))))))]))
  
  (define (check-contract ctc)
    (unless (contract? ctc)
      (error 'recursive-contract "expected a contract, got ~e" ctc))
    ctc)

  
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

  
  
  (provide flat-rec-contract
           flat-murec-contract
	   or/c union
	   not/c
           =/c >=/c <=/c </c >/c between/c
           integer-in
           exact-integer-in
	   real-in
           natural-number/c
	   string/len
           false/c
	   printable/c
           symbols one-of/c
           listof list-immutableof 
           vectorof vector-immutableof vector/c vector-immutable/c 
           cons-immutable/c cons/c list-immutable/c list/c
           box-immutable/c box/c
           promise/c
           struct/c
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
      
  (define-syntax (union stx)
    (begin
#;
      (fprintf (current-error-port)
               "WARNING: union is deprecated, use or/c (file ~a~a)\n"
               (let ([file (syntax-source stx)])
                 (if (path? file)
                     (path->string file)
                     (format "~s" file)))
               (let ([line (syntax-line stx)])
                 (if (number? line)
                     (format ", line ~a" line)
                     "")))
      (syntax-case stx ()
        [(_ args ...) (syntax (or/c args ...))]
        [id (syntax or/c)])))
  
  (define (or/c . args)
    (for-each
     (lambda (x) 
       (unless (or (contract? x)
                   (and (procedure? x)
                        (procedure-arity-includes? x 1)))
         (error 'or/c "expected procedures of arity 1 or contracts, given: ~e" x)))
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
                            (error 'or/c "expected at most one non-flat contract, given ~e and ~e"
                                   contract
                                   arg)]
                           [else (loop arg fc/predicates (cdr args))]))]))])
      (let ([flat-contracts (map (lambda (x) (if (flat-contract? x)
                                                 x
                                                 (flat-contract x)))
                                 fc/predicates)])
        (cond
          [contract
           (make-or/c flat-contracts contract)]
          [else
           (make-flat-or/c flat-contracts)]))))

  (define-struct/prop or/c (flat-ctcs ho-ctc)
    ((pos-proj-prop (λ (ctc)
                      (let ([c-proc ((pos-proj-get (or/c-ho-ctc ctc)) (or/c-ho-ctc ctc))]
                            [predicates (map (λ (x) ((flat-get x) x))
                                             (or/c-flat-ctcs ctc))])
                        (lambda (pos src-info orig-str)
                          (let ([partial-contract (c-proc pos src-info orig-str)])
                            (lambda (val)
                              (cond
                                [(ormap (lambda (pred) (pred val)) predicates)
                                 val]
                                [else
                                 (partial-contract val)])))))))
     (neg-proj-prop 
      (λ (ctc)
        (let ([c-proc ((neg-proj-get (or/c-ho-ctc ctc)) (or/c-ho-ctc ctc))]
              [predicates (map (λ (x) ((flat-get x) x))
                               (or/c-flat-ctcs ctc))])
          (lambda (pos src-info orig-str)
            (let ([partial-contract (c-proc pos src-info orig-str)])
              (lambda (val)
                (cond
                  [(ormap (lambda (pred) (pred val)) predicates)
                   val]
                  [else
                   (partial-contract val)])))))))
     
     (name-prop (λ (ctc)
                  (apply build-compound-type-name 
                         'or/c 
                         (or/c-ho-ctc ctc)
                         (or/c-flat-ctcs ctc))))
     (stronger-prop
      (λ (this that)
        (and (or/c? that)
             (and 
              (contract-stronger? (or/c-ho-ctc this) (or/c-ho-ctc that))
              (let ([this-ctcs (or/c-flat-ctcs this)]
                    [that-ctcs (or/c-flat-ctcs that)])
                (and (= (length this-ctcs) (length that-ctcs))
                     (andmap contract-stronger?
                             this-ctcs
                             that-ctcs)))))))))
  
  (define-struct/prop flat-or/c (flat-ctcs)
    ((pos-proj-prop flat-pos-proj)
     (neg-proj-prop any-curried-proj)
     (name-prop (λ (ctc)
                  (apply build-compound-type-name 
                         'or/c 
                         (flat-or/c-flat-ctcs ctc))))
     (stronger-prop
      (λ (this that)
        (and (flat-or/c? that)
             (let ([this-ctcs (flat-or/c-flat-ctcs this)]
                   [that-ctcs (flat-or/c-flat-ctcs that)])
               (and (= (length this-ctcs) (length that-ctcs))
                    (andmap contract-stronger?
                            this-ctcs
                            that-ctcs))))))
     (flat-prop (λ (ctc) 
                  (let ([preds
                         (map (λ (x) ((flat-get x) x))
                              (flat-or/c-flat-ctcs ctc))])
                    (λ (x) (ormap (λ (p?) (p? x)) preds)))))))
  
  (define false/c
    (flat-named-contract
     'false/c
     (lambda (x) (not x))))
  
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
    (make-one-of/c ss))
  
  (define atomic-value? 
    (let ([undefined (letrec ([x x]) x)])
      (λ (x)
        (or (char? x) (symbol? x) (boolean? x)
            (null? x) (keyword? x) (number? x)
            (void? x) (eq? x undefined)))))
      
  (define (one-of/c . elems)
    (unless (andmap atomic-value? elems)
      (error 'one-of/c "expected chars, symbols, booleans, null, keywords, numbers, void, or undefined, got ~e"
             elems))
    (make-one-of/c elems))
  
  (define-struct/prop one-of/c (elems)
    ((pos-proj-prop flat-pos-proj)
     (neg-proj-prop any-curried-proj)
     (name-prop (λ (ctc) 
                  (let ([elems (one-of/c-elems ctc)])
                    `(,(cond
                         [(andmap symbol? elems)
                          'symbols]
                         [else
                          'one-of/c])
                       ,@(map print-convert elems)))))
     (stronger-prop
      (λ (this that)
        (and (one-of/c? that)
             (let ([this-elems (one-of/c-elems this)]
                   [that-elems (one-of/c-elems that)])
               (and 
                (andmap (λ (this-elem) (memv this-elem that-elems))
                        this-elems)
                #t)))))
     (flat-prop 
      (λ (ctc) 
        (let ([elems (one-of/c-elems ctc)])
          (λ (x) (memv x elems)))))))
  
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
  
  (define-struct/prop between/c (low high)
    ((pos-proj-prop flat-pos-proj)
     (neg-proj-prop any-curried-proj)
     (name-prop (λ (ctc) 
                  (let ([n (between/c-low ctc)]
                        [m (between/c-high ctc)])
                    (cond
                      [(= n -inf.0) `(<=/c ,m)]
                      [(= m +inf.0) `(>=/c ,n)]
                      [(= n m) `(=/c ,n)]
                      [else `(between/c ,n ,m)]))))
     (stronger-prop
      (λ (this that)
        (and (between/c? that)
             (<= (between/c-low that) (between/c-low this))
             (<= (between/c-high this) (between/c-high that)))))
     (flat-prop (λ (ctc) 
                  (let ([n (between/c-low ctc)]
                        [m (between/c-high ctc)])
                    (λ (x) 
                      (and (number? x)
                           (<= n x m))))))))
  (define (=/c x) (make-between/c x x))
  (define (<=/c x) (make-between/c -inf.0 x))
  (define (>=/c x) (make-between/c x +inf.0))
  (define (between/c x y) (make-between/c x y))

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
  
  (define (not/c f)
    (unless (flat-contract/predicate? f)
      (error 'not/c "expected a procedure of arity 1 or <flat-named-contract>, given: ~e" f))
    (build-flat-contract
     (build-compound-type-name 'not/c (proc/ctc->ctc f))
     (lambda (x) (not (test-proc/flat-contract f x)))))

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
                   [p-proj (contract-pos-proc ctc)]
                   [n-proj (contract-neg-proc ctc)])
              (make-pair-proj-contract
               (build-compound-type-name 'name ctc)
               (lambda (blame src-info orig-str)
                 (let ([p-app (p-proj blame src-info orig-str)])
                   (lambda (val)
                     (unless (predicate?-name val)
                       (raise-contract-error
                        val
                        src-info
                        blame
                        orig-str
                        "expected <~a>, given: ~e"
                        'type-name
                        val))
                     (fill-name p-app val))))
               (lambda (blame src-info orig-str)
                 (let ([n-app (n-proj blame src-info orig-str)])
                   (lambda (val)
                     (fill-name n-app val)))))))))]))
  
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
                     [(ctc-x ...) (generate-temporaries (syntax (selectors ...)))]
                     [(pos-procs ...) (generate-temporaries (syntax (selectors ...)))]
                     [(neg-procs ...) (generate-temporaries (syntax (selectors ...)))]
                     [(selector-names ...) (generate-temporaries (syntax (selectors ...)))])
         (syntax
          (let ([predicate?-name predicate?]
                [constructor-name constructor]
                [selector-names selectors] ...)
            (lambda (params ...)
              (let ([ctc-x (coerce-contract name params)] ...)
                (let ([pos-procs (contract-pos-proc ctc-x)]
                      ...
                      [neg-procs (contract-neg-proc ctc-x)] ...)
                  (make-pair-proj-contract
                   (build-compound-type-name 'name (proc/ctc->ctc params) ...)
                   (lambda (blame src-info orig-str)
                     (let ([p-apps (pos-procs blame src-info orig-str)] ...)
                       (lambda (v)
                         (if (and (immutable? v)
                                  (predicate?-name v))
                             (constructor-name (p-apps (selector-names v)) ...)
                             (raise-contract-error
                              v
                              src-info
                              blame
                              orig-str
                              "expected <~a>, given: ~e"
                              'type-name
                              v)))))
                   (lambda (blame src-info orig-str)
                     (let ([p-apps (neg-procs blame src-info orig-str)] ...)
                       (lambda (v)
                         (constructor-name (p-apps (selector-names v)) ...)))))))))))]
      [(_ predicate? constructor (arb? selector) correct-size type-name name)
       (eq? #t (syntax-object->datum (syntax arb?)))
       (syntax
        (let ([predicate?-name predicate?]
              [constructor-name constructor]
              [selector-name selector])
          (lambda params
            (let ([ctcs (map (lambda (param) (coerce-contract name param)) params)])
              (let ([pos-procs (map contract-pos-proc ctcs)]
                    [neg-procs (map contract-neg-proc ctcs)])
                (make-pair-proj-contract
                 (apply build-compound-type-name 'name (map proc/ctc->ctc params))
                 (lambda (blame src-info orig-str)
                   (let ([p-apps (map (lambda (proc) (proc blame src-info orig-str)) pos-procs)]
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
                            v
                            src-info
                            blame
                            orig-str
                            "expected <~a>, given: ~e"
                            'type-name
                            v)))))
                 (lambda (blame src-info orig-str)
                   (let ([p-apps (map (lambda (proc) (proc blame src-info orig-str)) neg-procs)])
                     (lambda (v)
                       (apply constructor-name 
                              (let loop ([p-apps p-apps]
                                         [i 0])
                                (cond
                                  [(null? p-apps) null]
                                  [else (let ([p-app (car p-apps)])
                                          (cons (p-app (selector-name v i))
                                                (loop (cdr p-apps) (+ i 1))))]))))))))))))]))
  
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
             [pos-ctc-proc (contract-pos-proc ctc)]
             [neg-ctc-proc (contract-neg-proc ctc)])
        (make-pair-proj-contract
         (build-compound-type-name 'promise/c ctc)
         (lambda (blame src-info orig-str)
           (let ([p-app (pos-ctc-proc blame src-info orig-str)])
             (lambda (val)
               (unless (promise? val)
                 (raise-contract-error
                  val
                  src-info
                  blame
                  'ignored
                  orig-str
                  "expected <promise>, given: ~e"
                  val))
               (delay (p-app (force val))))))
         (lambda (blame src-info orig-str)
           (let ([p-app (neg-ctc-proc blame src-info orig-str)])
             (lambda (val)
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
                     [(ctc-name-x ...) (generate-temporaries (syntax (args ...)))]
                     [(ctc-pred-x ...) (generate-temporaries (syntax (args ...)))]
                     [(ctc-app-x ...) (generate-temporaries (syntax (args ...)))]
                     [(field-numbers ...)
                      (let loop ([i 0]
                                 [l (syntax->list (syntax (args ...)))])
                        (cond
                          [(null? l) '()]
                          [else (cons i (loop (+ i 1) (cdr l)))]))]
                     [(type-desc-id 
                       constructor-id 
                       predicate-id 
                       (rev-selector-id ...)
                       (mutator-id ...)
                       super-id)
                      (syntax-local-value (syntax struct-name))])
         (with-syntax ([(selector-id ...) (reverse (syntax->list (syntax (rev-selector-id ...))))])
           (syntax
            (let ([ctc-x (coerce-contract struct/c args)] ...)
              
              (unless predicate-id
                (error 'struct/c "could not determine predicate for ~s" 'struct-name))
              (unless (and selector-id ...)
                (error 'struct/c "could not determine selectors for ~s" 'struct-name))
              
              (unless (flat-contract? ctc-x)
                (error 'struct/c "expected flat contracts as arguments, got ~e" args))
              ...
              
              (let ([ctc-pred-x (flat-contract-predicate ctc-x)] 
                    ...
                    [ctc-name-x (contract-name ctc-x)]
                    ...)
                (build-flat-contract
                 (build-compound-type-name 'struct/c 'struct-name ctc-x ...)
                 (λ (val)
                   (and (predicate-id val)
                        (ctc-pred-x (selector-id val)) ...))))))))]
      [(_ struct-name anything ...)
       (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))])))
