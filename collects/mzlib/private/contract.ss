#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

|#

(module contract mzscheme
  
  (provide (rename -contract contract)
           recursive-contract
           provide/contract
           define/contract)

  (require-for-syntax mzscheme
                      "contract-opt-guts.ss"
                      (lib "list.ss"))
  
  (require "contract-arrow.ss"
           "contract-guts.ss"
           "contract-opt.ss")
  
  (require "contract-helpers.ss")
  (require-for-syntax (prefix a: "contract-helpers.ss"))

  
  
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
     (λ (stx)
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
     (let ([saved-id-table (make-hash-table)])
       (λ (stx)
          (if (eq? 'expression (syntax-local-context))
              ;; In an expression context:
              (let ([key (syntax-local-lift-context)])
                ;; Already lifted in this lifting context?
                (unless (hash-table-get saved-id-table key #f)
                  ;; No: lift the contract creation:
                  (with-syntax ([contract-id contract-id]
                                [id id]
                                [name (datum->syntax-object #f (syntax-object->datum id) id)]
                                [pos-module-source pos-module-source])
                    (hash-table-put!
                     saved-id-table
                     key
                     (syntax-local-introduce
                      (syntax-local-lift-expression
                       #'(-contract contract-id
                                    id
                                    pos-module-source
                                    (module-source-as-symbol #'name)
                                    (quote-syntax name)))))))
                ;; Expand to a use of the lifted expression:
                (with-syntax ([saved-id (syntax-local-introduce (hash-table-get saved-id-table key))])
                  (syntax-case stx (set!)
                    [name
                     (identifier? (syntax name))
                     (syntax saved-id)]
                    [(name . more)
                     (with-syntax ([app (datum->syntax-object stx '#%app)])
                       (syntax/loc stx (app saved-id . more)))])))
              ;; In case of partial expansion for module-level and internal-defn contexts,
              ;; delay expansion until it's a good time to lift expressions:
              (quasisyntax/loc stx (#%expression #,stx)))))))
  
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
  ;; p/c-ele = (id expr) | (rename id id expr) | (struct id (id expr) ...)
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
                ;; compare raw identifiers for `struct' and `rename' just like provide does
                (syntax-case* clause (struct rename) (λ (x y) (eq? (syntax-e x) (syntax-e y))) 
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
                   (for-each (λ (field)
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
                  [all-parent-struct-count/names (get-field-counts/struct-names struct-name provide-stx)]
                  [parent-struct-count (if (null? all-parent-struct-count/names)
                                           #f
                                           (let ([pp (cdr all-parent-struct-count/names)])
                                             (if (null? pp)
                                                 #f
                                                 (car (car pp)))))]
                  [field-contract-ids (map (λ (field-name field-contract) 
                                             (if (a:known-good-contract? field-contract)
                                                 field-contract
                                                 (a:mangle-id provide-stx
                                                              "provide/contract-field-contract"
                                                              field-name
                                                              struct-name)))
                                           field-names
                                           field-contracts)]
                  [struct:struct-name
                   (datum->syntax-object
                    struct-name
                    (string->symbol
                     (string-append
                      "struct:"
                      (symbol->string (syntax-e struct-name)))))]
                  
                  [-struct:struct-name
                   (datum->syntax-object
                    struct-name
                    (string->symbol
                     (string-append
                      "-struct:"
                      (symbol->string (syntax-e struct-name)))))]
                  
                  [is-new-id?
                   (λ (index)
                     (or (not parent-struct-count)
                         (parent-struct-count . <= . index)))])
             
             (let ([unknown-info
                    (λ (what names)
                      (raise-syntax-error
                       'provide/contract
                       (format "cannot determine ~a, found ~s" what names)
                       provide-stx
                       struct-name))]
		   [is-id-ok?
		    (λ (id i)
                      (if (or (not parent-struct-count)
			      (parent-struct-count . <= . i))
			  id
			  #t))])
               
               (unless constructor-id (unknown-info "constructor" constructor-id))
               (unless predicate-id (unknown-info "predicate" predicate-id))
               (unless (andmap/count is-id-ok? selector-ids)
		 (unknown-info "selectors"
			       (map (λ (x) (if (syntax? x)
                                               (syntax-object->datum x)
                                               x))
                                    selector-ids)))
               (unless (andmap/count is-id-ok? mutator-ids)
		 (unknown-info "mutators"
			       (map (λ (x) (if (syntax? x)
                                               (syntax-object->datum x)
                                               x))
				    mutator-ids))))
             
             (unless (equal? (length selector-ids)
                             (length field-contract-ids))
               (raise-syntax-error 'provide/contract
                                   (format "found ~a field~a in struct, but ~a contract~a"
                                           (length selector-ids)
                                           (if (= 1 (length selector-ids)) "" "s")
                                           (length field-contract-ids)
                                           (if (= 1 (length field-contract-ids)) "" "s"))
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
             
             ;; make sure the field names are right.
             (let* ([relative-counts (let loop ([c (map car all-parent-struct-count/names)])
                                       (cond
                                         [(null? c) null]
                                         [(null? (cdr c)) c]
                                         [else (cons (- (car c) (cadr c))
                                                     (loop (cdr c)))]))]
                    [names (map cdr all-parent-struct-count/names)]
                    [maker-name (format "~a" (syntax-e constructor-id))]
                    [struct-name (substring maker-name 5 (string-length maker-name))])
               (let loop ([count (car relative-counts)]
                          [name (car names)]
                          [counts (cdr relative-counts)]
                          [names (cdr names)]
                          [selector-strs (reverse (map (λ (x) (format "~a" (syntax-e x))) selector-ids))]
                          [field-names (reverse field-names)])
                 (cond
                   [(or (null? selector-strs) (null? field-names)) 
                    (void)]
                   [(zero? count) 
                    (loop (car counts) (car names) (cdr counts) (cdr names) 
                          selector-strs
                          field-names)]
                   [else
                    (let* ([selector-str (car selector-strs)]
                           [field-name (car field-names)]
                           [field-name-should-be
                            (substring selector-str 
                                       (+ (string-length name) 1)
                                       (string-length selector-str))]
                           [field-name-is (format "~a" (syntax-e field-name))])
                      (unless (equal? field-name-should-be field-name-is)
                        (raise-syntax-error 'provide/contract
                                            (format "expected field name to be ~a, but found ~a"
                                                    field-name-should-be
                                                    field-name-is)
                                            provide-stx
                                            field-name))
                      (loop (- count 1)
                            name
                            counts
                            names
                            (cdr selector-strs)
                            (cdr field-names)))])))
             
             (with-syntax ([((selector-codes selector-new-names) ...)
                            (filter
                             (λ (x) x)
                             (map/count (λ (selector-id field-contract-id index)
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
                              (λ (x) x)
                              (map/count (λ (selector-id index)
                                           (if (not (is-new-id? index))
                                               selector-id
                                               #f))
                                         selector-ids)))]
                           [((mutator-codes mutator-new-names) ...)
                            (filter
                             (λ (x) x)
                             (map/count (λ (mutator-id field-contract-id index)
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
                              (λ (x) x)
                              (map/count (λ (mutator-id index)
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
                             #f
                             #t)]
                           
                           [(field-contract-id-definitions ...)
                            (filter values (map (λ (field-contract-id field-contract)
                                                  (if (a:known-good-contract? field-contract)
                                                      #f
                                                      (with-syntax ([field-contract-id field-contract-id]
                                                                    [field-contract field-contract])
                                                        #'(define field-contract-id (verify-contract field-contract)))))
                                                field-contract-ids
                                                field-contracts))]
                           [(field-contracts ...) field-contracts]
                           [(field-contract-ids ...) field-contract-ids])
               
               (with-syntax ([(rev-selector-new-names ...) (reverse (syntax->list (syntax (selector-new-names ...))))]
                             [(rev-mutator-new-names ...) (reverse (syntax->list (syntax (mutator-new-names ...))))])
                 (with-syntax ([struct-code 
                                (with-syntax ([id-rename (a:mangle-id provide-stx 
                                                                      "provide/contract-struct-expandsion-info-id"
                                                                      struct-name)]
                                              [struct-name struct-name]
                                              [-struct:struct-name -struct:struct-name]
                                              [super-id (if (boolean? super-id)
                                                            super-id
                                                            (with-syntax ([super-id super-id])
                                                              (syntax ((syntax-local-certifier) #'super-id))))])
                                  (syntax (begin
                                            (provide (rename id-rename struct-name))
                                            (define-syntax id-rename
                                              (let ([slc (syntax-local-certifier)])
                                                (list-immutable (slc #'-struct:struct-name)
                                                                (slc #'constructor-new-name)
                                                                (slc #'predicate-new-name)
                                                                (list-immutable (slc #'rev-selector-new-names) ...
                                                                                (slc #'rev-selector-old-names) ...)
                                                                (list-immutable (slc #'rev-mutator-new-names) ...
                                                                                (slc #'rev-mutator-old-names) ...)
                                                                super-id))))))]
                               [struct:struct-name struct:struct-name]
                               [-struct:struct-name -struct:struct-name]
                               [struct-name struct-name]
                               [(selector-ids ...) selector-ids])
                   (syntax/loc stx
                     (begin
                       struct-code
                       field-contract-id-definitions ...
                       selector-codes ...
                       mutator-codes ...
                       predicate-code
                       constructor-code
                       
                       ;; expanding out the body of the `make-pc-struct-type' function
                       ;; directly here in the expansion makes this very expensive at compile time
                       ;; when there are a lot of provide/contract clause using structs
                       (define -struct:struct-name 
                         (make-pc-struct-type 'struct-name struct:struct-name field-contract-ids ...))
                       (provide (rename -struct:struct-name struct:struct-name)))))))))
          
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
         
         ;; get-field-counts/struct-names : syntax syntax -> (listof (cons symbol number))
         ;; returns a list of numbers corresponding to the numbers of fields for each of the parent structs
         (define (get-field-counts/struct-names struct-name provide-stx)
           (let loop ([parent-info-id struct-name])
             (let ([parent-info 
                    (and (identifier? parent-info-id)
                         (syntax-local-value parent-info-id (λ () #f)))])
               (cond
                 [(boolean? parent-info) null]
                 [else
                  (let ([fields (list-ref parent-info 3)]
                        [constructor (list-ref parent-info 1)])
                    (cond
                      [(and (not (null? fields))
                            (not (car (last-pair fields))))
                       (raise-syntax-error 
                        'provide/contract
                        "cannot determine the number of fields in super struct"
                        provide-stx
                        struct-name)]
                      [else
                       (cons (cons (length fields) (constructor->struct-name provide-stx constructor))
                             (loop (list-ref parent-info 5)))]))]))))
         
         (define (constructor->struct-name orig-stx stx)
           (and stx
                (let ([m (regexp-match #rx"^make-(.*)$" (format "~a" (syntax-e stx)))])
                  (cond
                    [m (cadr m)]
                    [else (raise-syntax-error 'contract.ss
                                              "unable to cope with a struct maker whose name doesn't begin with `make-'"
                                              orig-stx)]))))
                       
                  
         
         ;; extract-parent-struct-info : syntax -> (union #f (list syntax syntax (listof syntax) ...))
         (define (extract-parent-struct-info stx)
           (syntax-case stx ()
             [(a b)
              (syntax-local-value 
               (syntax b)
               (λ ()
                 (raise-syntax-error 'provide/contract
                                     "expected a struct name" 
                                     provide-stx
                                     (syntax b))))
              (syntax b)]
             [a #f]))
         
         ;; extract-struct-info : syntax -> (union #f (list syntax syntax (listof syntax) ...))
         (define (extract-struct-info stx)
           (let ([id (syntax-case stx ()
                       [(a b) (syntax a)]
                       [_ stx])])
             (syntax-local-value 
              id
              (λ ()
                (raise-syntax-error 'provide/contract
                                    "expected a struct name" 
                                    provide-stx
                                    id)))))
         
         ;; build-constructor-contract : syntax (listof syntax) syntax -> syntax
         (define (build-constructor-contract stx field-contract-ids predicate-id)
           (with-syntax ([(field-contract-ids ...) field-contract-ids]
                         [predicate-id predicate-id])
             (syntax/loc stx
               (-> field-contract-ids ...
                   predicate-id))))
         
         ;; build-selector-contract : syntax syntax -> syntax
         ;; constructs the contract for a selector
         (define (build-selector-contract struct-name predicate-id field-contract-id)
           (with-syntax ([field-contract-id field-contract-id]
                         [predicate-id predicate-id])
             (syntax (-> predicate-id field-contract-id))))
         
         ;; build-mutator-contract : syntax syntax -> syntax
         ;; constructs the contract for a selector
         (define (build-mutator-contract struct-name predicate-id field-contract-id)
           (with-syntax ([field-contract-id field-contract-id]
                         [predicate-id predicate-id])
             (syntax (-> predicate-id
                         field-contract-id
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
         (define code-for-one-id/new-name
           (case-lambda
             [(stx id ctrct user-rename-id) 
              (code-for-one-id/new-name stx id ctrct user-rename-id #f)]
             [(stx id ctrct user-rename-id mangle-for-maker?)
              (let ([no-need-to-check-ctrct? (a:known-good-contract? ctrct)])
                (with-syntax ([id-rename ((if mangle-for-maker? 
                                              a:mangle-id-for-maker
                                              a:mangle-id)
                                          provide-stx
                                          "provide/contract-id" 
                                          (or user-rename-id id))]
                              [contract-id (if no-need-to-check-ctrct?
                                               ctrct
                                               (a:mangle-id provide-stx
                                                            "provide/contract-contract-id" 
                                                            (or user-rename-id id)))]
                              [pos-module-source (a:mangle-id provide-stx 
                                                              "provide/contract-pos-module-source"
                                                              (or user-rename-id id))]
                              [pos-stx (datum->syntax-object id 'here)]
                              [id id]
                              [ctrct (syntax-property ctrct 'inferred-name id)]
                              [external-name (or user-rename-id id)]
                              [where-stx stx])
                  (with-syntax ([code
                                 (quasisyntax/loc stx
                                   (begin
                                     (define pos-module-source (module-source-as-symbol #'pos-stx))
                                     
                                     #,@(if no-need-to-check-ctrct?
                                            (list)
                                            (list #'(define contract-id (verify-contract ctrct))))
                                     (define-syntax id-rename
                                       (make-provide/contract-transformer (quote-syntax contract-id)
                                                                          (quote-syntax id)
                                                                          (quote-syntax pos-module-source)))
                                     
                                     (provide (rename id-rename external-name))))])
                    
                    (syntax-local-lift-module-end-declaration
                     #'(begin 
                         (-contract contract-id id pos-module-source 'ignored #'id)
                         (void)))
                    
                    (syntax (code id-rename)))))]))
         
         (with-syntax ([(bodies ...) (code-for-each-clause (syntax->list (syntax (p/c-ele ...))))])
           (syntax 
            (begin
              bodies ...))))]))
  
  (define-syntax (verify-contract stx)
    (syntax-case stx ()
      [(_ x) (a:known-good-contract? #'x) #'x]
      [(_ x) #'(verify-contract/proc x)]))
  
  (define (verify-contract/proc x)
    (unless (or (contract? x)
                (and (procedure? x)
                     (procedure-arity-includes? x 1)))
      (error 'provide/contract "expected a contract or a procedure of arity one, got ~e" x))
    x)
  
  
  (define (make-pc-struct-type struct-name struct:struct-name . ctcs)
    (let-values ([(struct:struct-name _make _pred _get _set)
                  (make-struct-type struct-name
                                    struct:struct-name
                                    0 ;; init
                                    0 ;; auto
                                    #f  ;; auto-v
                                    '() ;; props
                                    #f  ;; inspector
                                    #f ;; proc-spec
                                    '()  ;; immutable-k-list
                                    (λ args
                                      (let ([vals (let loop ([args args])
                                                    (cond
                                                      [(null? args) null]
                                                      [(null? (cdr args)) null]
                                                      [else (cons (car args) (loop (cdr args)))]))])
                                        (apply values
                                               (map (λ (ctc val)
                                                      (-contract ctc
                                                                 val
                                                                 'not-enough-info-for-blame
                                                                 'not-enough-info-for-blame))
                                                    ctcs
                                                    vals)))))])
      struct:struct-name))
  
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
         (begin
           (contract/proc a-contract-e to-check pos-blame-e neg-blame-e src-info-e)))]))
  
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
  
  (define-syntax (recursive-contract stx)
    (syntax-case stx ()
      [(_ arg)
       (syntax (make-proj-contract 
                '(recursive-contract arg) 
                (λ (pos-blame neg-blame src str)
                  (let ([proc (contract-proc arg)])
                    (λ (val)
                      ((proc pos-blame neg-blame src str) val))))
                #f))]))
  
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
	   real-in
           natural-number/c
	   string/len
           false/c
	   printable/c
           symbols one-of/c
           listof list-immutableof 
           vectorof vector-immutableof vector/c vector-immutable/c 
           cons-immutable/c cons/c list-immutable/c list/c
           listof-unsafe cons-unsafe/c list-unsafe/c
           box-immutable/c box/c
           promise/c
           struct/c
	   syntax/c
           
           check-between/c
           check-unary-between/c
           parameter/c)
  
  (define-syntax (flat-rec-contract stx)
    (syntax-case stx  ()
      [(_ name ctc ...)
       (identifier? (syntax name))
       (with-syntax ([(ctc-id ...) (generate-temporaries (syntax (ctc ...)))]
                     [(pred-id ...) (generate-temporaries (syntax (ctc ...)))])
         (syntax 
          (let* ([pred (λ (x) (error 'flat-rec-contract "applied too soon"))]
                 [name (flat-contract (let ([name (λ (x) (pred x))]) name))])
            (let ([ctc-id (coerce-contract 'flat-rec-contract ctc)] ...)
              (unless (flat-contract? ctc-id)
                (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
              ...
              (set! pred
                    (let ([pred-id (flat-contract-predicate ctc-id)] ...)
                      (λ (x)
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
          (let* ([pred-id (λ (x) (error 'flat-murec-contract "applied too soon"))] ...
                 [name (flat-contract (let ([name (λ (x) (pred-id x))]) name))] ...)
            (let-values ([(ctc-id ...) (values (coerce-contract 'flat-rec-contract ctc) ...)] ...)
              (begin
                (void)
                (unless (flat-contract? ctc-id)
                  (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
                ...) ...
              (set! pred-id
                    (let ([pred-arm-id (flat-contract-predicate ctc-id)] ...)
                      (λ (x)
                        (or (pred-arm-id x) ...)))) ...
              body1
              body ...))))]
      [(_ ([name ctc ...] ...) body1 body ...)
       (for-each (λ (name)
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
  
  (define or/c
    (case-lambda 
      [() (make-none/c '(or/c))]
      [args
       (for-each
        (λ (x) 
          (unless (or (contract? x)
                      (and (procedure? x)
                           (procedure-arity-includes? x 1)))
            (error 'or/c "expected procedures of arity 1 or contracts, given: ~e" x)))
        args)
       (let-values ([(ho-contracts fc/predicates)
                     (let loop ([ho-contracts '()]
                                [fc/predicates null]
                                [args args])
                       (cond
                         [(null? args) (values ho-contracts (reverse fc/predicates))]
                         [else 
                          (let ([arg (car args)])
                            (cond
                              [(and (contract? arg)
                                    (not (flat-contract? arg)))
                               (loop (cons arg ho-contracts) fc/predicates (cdr args))]
                              [else
                               (loop ho-contracts (cons arg fc/predicates) (cdr args))]))]))])
         (let ([flat-contracts (map (λ (x) (if (flat-contract? x)
                                               x
                                               (flat-contract x)))
                                    fc/predicates)]
               [pred 
                (cond
                  [(null? fc/predicates) not]
                  [else
                   (let loop ([fst (car fc/predicates)]
                              [rst (cdr fc/predicates)])
                     (let ([fst-pred (if (flat-contract? fst)
                                         ((flat-get fst) fst)
                                         fst)])
                       (cond
                         [(null? rst) fst-pred]
                         [else 
                          (let ([r (loop (car rst) (cdr rst))])
                            (λ (x) (or (fst-pred x) (r x))))])))])])
           (cond
             [(null? ho-contracts)
              (make-flat-or/c pred flat-contracts)]
             [(null? (cdr ho-contracts))
              (make-or/c pred flat-contracts (car ho-contracts))]
             [else
              (make-multi-or/c flat-contracts ho-contracts)])))]))

  (define-struct/prop or/c (pred flat-ctcs ho-ctc)
    ((proj-prop (λ (ctc)
                  (let ([c-proc ((proj-get (or/c-ho-ctc ctc)) (or/c-ho-ctc ctc))]
                        [pred (or/c-pred ctc)])
                    (λ (pos-blame neg-blame src-info orig-str)
                      (let ([partial-contract (c-proc pos-blame neg-blame src-info orig-str)])
                        (λ (val)
                          (cond
                            [(pred val) val]
                            [else
                             (partial-contract val)])))))))
     
     (name-prop (λ (ctc)
                  (apply build-compound-type-name 
                         'or/c 
                         (or/c-ho-ctc ctc)
                         (or/c-flat-ctcs ctc))))     
     (first-order-prop
      (λ (ctc)
        (let ([flats (map (λ (x) ((flat-get x) x)) (or/c-flat-ctcs ctc))]
              [ho ((first-order-get (or/c-ho-ctc ctc)) (or/c-ho-ctc ctc))])
          (λ (x)
            (or (ho x)
                (ormap (λ (f) (f x)) flats))))))
     
     (stronger-prop
      (λ (this that)
        (and (or/c? that)
             (contract-stronger? (or/c-ho-ctc this) (or/c-ho-ctc that))
             (let ([this-ctcs (or/c-flat-ctcs this)]
                   [that-ctcs (or/c-flat-ctcs that)])
               (and (= (length this-ctcs) (length that-ctcs))
                    (andmap contract-stronger?
                            this-ctcs
                            that-ctcs))))))))
  
  (define (multi-or/c-proj ctc)
    (let* ([ho-contracts (multi-or/c-ho-ctcs ctc)]
           [c-procs (map (λ (x) ((proj-get x) x)) ho-contracts)]
           [first-order-checks (map (λ (x) ((first-order-get x) x)) ho-contracts)]
           [predicates (map (λ (x) ((flat-get x) x))
                            (multi-or/c-flat-ctcs ctc))])
      (λ (pos-blame neg-blame src-info orig-str)
        (let ([partial-contracts (map (λ (c-proc) (c-proc pos-blame neg-blame src-info orig-str)) c-procs)])
          (λ (val)
            (cond
              [(ormap (λ (pred) (pred val)) predicates)
               val]
              [else
               (let loop ([checks first-order-checks]
                          [procs partial-contracts]
                          [contracts ho-contracts]
                          [candidate-proc #f]
                          [candidate-contract #f])
                 (cond
                   [(null? checks)
                    (if candidate-proc
                        (candidate-proc val)
                        (raise-contract-error val src-info pos-blame orig-str 
                                              "none of the branches of the or/c matched"))]
                   [((car checks) val)
                    (if candidate-proc
                        (error 'or/c "two arguments, ~s and ~s, might both match ~s"
                               (contract-name candidate-contract)
                               (contract-name (car contracts))
                               val)
                        (loop (cdr checks)
                              (cdr procs)
                              (cdr contracts)
                              (car procs)
                              (car contracts)))]
                   [else
                    (loop (cdr checks)
                          (cdr procs)
                          (cdr contracts)
                          candidate-proc
                          candidate-contract)]))]))))))
  
  (define-struct/prop multi-or/c (flat-ctcs ho-ctcs)
    ((proj-prop multi-or/c-proj)
     (name-prop (λ (ctc)
                  (apply build-compound-type-name 
                         'or/c 
                         (append
                          (multi-or/c-flat-ctcs ctc)
                          (reverse (multi-or/c-ho-ctcs ctc))))))
     (first-order-prop
      (λ (ctc)
        (let ([flats (map (λ (x) ((flat-get x) x)) (multi-or/c-flat-ctcs ctc))]
              [hos (map (λ (x) ((first-order-get x) x)) (multi-or/c-ho-ctcs ctc))])
          (λ (x)
            (or (ormap (λ (f) (f x)) hos)
                (ormap (λ (f) (f x)) flats))))))
     
     (stronger-prop
      (λ (this that)
        (and (multi-or/c? that)
             (let ([this-ctcs (multi-or/c-ho-ctcs this)]
                   [that-ctcs (multi-or/c-ho-ctcs that)])
               (and (= (length this-ctcs) (length that-ctcs))
                    (andmap contract-stronger?
                            this-ctcs
                            that-ctcs)))
             (let ([this-ctcs (multi-or/c-flat-ctcs this)]
                   [that-ctcs (multi-or/c-flat-ctcs that)])
               (and (= (length this-ctcs) (length that-ctcs))
                    (andmap contract-stronger?
                            this-ctcs
                            that-ctcs))))))))
  
  (define-struct/prop flat-or/c (pred flat-ctcs)
    ((proj-prop flat-proj)
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
     (flat-prop (λ (ctc) (flat-or/c-pred ctc)))))
    
  ;;
  ;; or/c opter
  ;;
  (define/opter (or/c opt/i opt/info stx)
    ;; FIXME code duplication
    (define (opt/or-unknown uctc)
      (let* ((lift-var (car (generate-temporaries (syntax (lift)))))
             (partial-var (car (generate-temporaries (syntax (partial))))))
        (values
         (with-syntax ((partial-var partial-var)
                       (lift-var lift-var)
                       (uctc uctc)
                       (val (opt/info-val opt/info)))
           (syntax (partial-var val)))
         (list (cons lift-var 
                     ;; FIXME needs to get the contract name somehow
                     (with-syntax ((uctc uctc))
                       (syntax (coerce-contract 'opt/c uctc)))))
         null
         (list (cons
                partial-var
                (with-syntax ((lift-var lift-var)
                              (pos (opt/info-pos opt/info))
                              (neg (opt/info-neg opt/info))
                              (src-info (opt/info-src-info opt/info))
                              (orig-str (opt/info-orig-str opt/info)))
                  (syntax (((proj-get lift-var) lift-var) pos neg src-info orig-str)))))
         #f
         lift-var
         (list #f)
         null)))

    (define (opt/or-ctc ps)
      (let ((lift-from-hos null)
            (superlift-from-hos null)
            (partial-from-hos null))
        (let-values ([(opt-ps lift-ps superlift-ps partial-ps stronger-ribs hos ho-ctc)
                      (let loop ([ps ps]
                                 [next-ps null]
                                 [lift-ps null]
                                 [superlift-ps null]
                                 [partial-ps null]
                                 [stronger-ribs null]
                                 [hos null]
                                 [ho-ctc #f])
                        (cond
                          [(null? ps) (values next-ps
                                              lift-ps
                                              superlift-ps
                                              partial-ps
                                              stronger-ribs
                                              (reverse hos)
                                              ho-ctc)]
                          [else
                           (let-values ([(next lift superlift partial flat _ this-stronger-ribs)
                                         (opt/i opt/info (car ps))])
                             (if flat
                                 (loop (cdr ps)
                                       (cons flat next-ps)
                                       (append lift-ps lift)
                                       (append superlift-ps superlift)
                                       (append partial-ps partial)
                                       (append this-stronger-ribs stronger-ribs)
                                       hos
                                       ho-ctc)
                                 (if (< (length hos) 1)
                                     (loop (cdr ps)
                                           next-ps
                                           (append lift-ps lift)
                                           (append superlift-ps superlift)
                                           (append partial-ps partial)
                                           (append this-stronger-ribs stronger-ribs)
                                           (cons (car ps) hos)
                                           next)
                                     (loop (cdr ps)
                                           next-ps
                                           lift-ps
                                           superlift-ps
                                           partial-ps
                                           stronger-ribs
                                           (cons (car ps) hos)
                                           ho-ctc))))]))])
          (with-syntax ((next-ps
                         (with-syntax (((opt-p ...) (reverse opt-ps)))
                           (syntax (or opt-p ...)))))
            (values
             (cond
               [(null? hos) 
                (with-syntax ([val (opt/info-val opt/info)]
                              [pos (opt/info-pos opt/info)]
                              [src-info (opt/info-src-info opt/info)]
                              [orig-str (opt/info-orig-str opt/info)])
                  (syntax
                   (if next-ps 
                       val
                       (raise-contract-error val src-info pos orig-str 
                                             "none of the branches of the or/c matched"))))]
               [(= (length hos) 1) (with-syntax ((ho-ctc ho-ctc))
                                     (syntax
                                      (if next-ps val ho-ctc)))]
               ;; FIXME something's not right with this case.
               [(> (length hos) 1)
                (let-values ([(next-hos lift-hos superlift-hos partial-hos _ __ stronger-hos stronger-vars-hos)
                              (opt/or-unknown stx)])
                  (set! lift-from-hos lift-hos)
                  (set! superlift-from-hos superlift-hos)
                  (set! partial-from-hos partial-hos)
                  (with-syntax ((next-hos next-hos))
                    (syntax
                     (if next-ps val next-hos))))])
             (append lift-ps lift-from-hos)
             (append superlift-ps superlift-from-hos)
             (append partial-ps partial-from-hos)
             (if (null? hos) (syntax next-ps) #f)
             #f
             stronger-ribs)))))
    
    (syntax-case stx (or/c)
      [(or/c p ...)
       (opt/or-ctc (syntax->list (syntax (p ...))))]))
  
  (define false/c
    (flat-named-contract
     'false/c
     (λ (x) (not x))))
  
  (define (string/len n)
    (unless (number? n)
      (error 'string/len "expected a number as argument, got ~e" n))
    (flat-named-contract 
     `(string/len ,n)
     (λ (x)
       (and (string? x)
            ((string-length x) . < . n)))))
  
  (define (symbols . ss)
    (unless ((length ss) . >= . 1)
      (error 'symbols "expected at least one argument"))
    (unless (andmap symbol? ss)
      (error 'symbols "expected symbols as arguments, given: ~a"
	     (apply string-append (map (λ (x) (format "~e " x)) ss))))
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

  (define (one-of-pc x)
    (cond
      [(symbol? x)
       `',x]
      [(null? x)
       ''()]
      [(void? x)
       '(void)]
      [(or (char? x) 
           (boolean? x)
           (keyword? x)
           (number? x))
       x]
      [(eq? x (letrec ([x x]) x))
       '(letrec ([x x]) x)]
      [else (error 'one-of-pc "undef ~s" x)]))

  
  (define-struct/prop one-of/c (elems)
    ((proj-prop flat-proj)
     (name-prop (λ (ctc) 
                  (let ([elems (one-of/c-elems ctc)])
                    `(,(cond
                         [(andmap symbol? elems)
                          'symbols]
                         [else
                          'one-of/c])
                      ,@(map one-of-pc elems)))))
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
     (λ (x)
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
    ((proj-prop flat-proj)
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
  
  (define-syntax (check-unary-between/c stx)
    (syntax-case stx ()
      [(_ 'sym x-exp)
       (identifier? #'sym)
       #'(let ([x x-exp])
           (unless (real? x)
             (error 'sym "expected a real number, got ~e" x)))]))
  
  (define (=/c x) 
    (check-unary-between/c '=/c x)
    (make-between/c x x))
  (define (<=/c x) 
    (check-unary-between/c '<=/c x)
    (make-between/c -inf.0 x))
  (define (>=/c x)
    (check-unary-between/c '>=/c x)
    (make-between/c x +inf.0))
  (define (check-between/c x y)
    (unless (number? x)
      (error 'between/c "expected a number as first argument, got ~e, other arg ~e" x y))
    (unless (number? y)
      (error 'between/c "expected a number as second argument, got ~e, other arg ~e" y x)))
  (define (between/c x y)
    (check-between/c x y)
    (make-between/c x y))
  
  ;;
  ;; between/c opter helper
  ;;

  
  
  ;;
  ;; between/c opters
  ;;
  ;; note that the checkers are used by both optimized and normal contracts.
  ;;
  (define/opter (between/c opt/i opt/info stx)
    (syntax-case stx (between/c)
      [(between/c low high) 
       (let*-values ([(lift-low lifts1) (lift/binding #'low 'between-low empty-lifts)]
                     [(lift-high lifts2) (lift/binding #'high 'between-high lifts1)])
         (with-syntax ([n lift-low]
                       [m lift-high])
           (let ([lifts3 (lift/effect #'(check-between/c n m) lifts2)])
             (with-syntax ((val (opt/info-val opt/info))
                           (ctc (opt/info-contract opt/info))
                           (pos (opt/info-pos opt/info))
                           (src-info (opt/info-src-info opt/info))
                           (orig-str (opt/info-orig-str opt/info))
                           (this (opt/info-this opt/info))
                           (that (opt/info-that opt/info)))
               (values
                (syntax (if (and (number? val) (<= n val m)) 
                            val
                            (raise-contract-error
                             val
                             src-info
                             pos
                             orig-str
                             "expected <~a>, given: ~e"
                             ((name-get ctc) ctc)
                             val)))
                lifts3
                null
                null
                (syntax (and (number? val) (<= n val m)))
                #f
                (list (new-stronger-var
                       lift-low
                       (λ (this that)
                         (with-syntax ([this this]
                                       [that that])
                           (syntax (<= that this)))))
                      (new-stronger-var
                       lift-high
                       (λ (this that)
                         (with-syntax ([this this]
                                       [that that])
                           (syntax (<= this that)))))))))))]))
    
  (define-for-syntax (single-comparison-opter opt/info stx check-arg comparison arg)
    (with-syntax ([comparison comparison])
      (let*-values ([(lift-low lifts2) (lift/binding arg 'single-comparison-val empty-lifts)])
        (with-syntax ([m lift-low])
          (let ([lifts3 (lift/effect (check-arg #'m) lifts2)])
            (with-syntax ((val (opt/info-val opt/info))
                          (ctc (opt/info-contract opt/info))
                          (pos (opt/info-pos opt/info))
                          (src-info (opt/info-src-info opt/info))
                          (orig-str (opt/info-orig-str opt/info))
                          (this (opt/info-this opt/info))
                          (that (opt/info-that opt/info)))
              (values
               (syntax 
                (if (and (number? val) (comparison val m)) 
                    val
                    (raise-contract-error
                     val
                     src-info
                     pos
                     orig-str
                     "expected <~a>, given: ~e"
                     ((name-get ctc) ctc)
                     val)))
               lifts3
               null
               null
               (syntax (and (number? val) (comparison val m)))
               #f
               (list (new-stronger-var
                      lift-low
                      (λ (this that)
                        (with-syntax ([this this]
                                      [that that])
                          (syntax (comparison this that)))))))))))))
  
  (define/opter (>=/c opt/i opt/info stx)
    (syntax-case stx (>=/c)
      [(>=/c low)
       (single-comparison-opter 
        opt/info
        stx
        (λ (m) (with-syntax ([m m])
                 #'(check-unary-between/c '>=/c m)))
        #'>=
        #'low)]))
  
  (define/opter (<=/c opt/i opt/info stx)
    (syntax-case stx (<=/c)
      [(<=/c high)
       (single-comparison-opter 
        opt/info
        stx
        (λ (m) (with-syntax ([m m])
                 #'(check-unary-between/c '<=/c m)))
        #'<=
        #'high)]))
  
  (define/opter (>/c opt/i opt/info stx)
    (syntax-case stx (>/c)
      [(>/c low)
       (single-comparison-opter 
        opt/info
        stx
        (λ (m) (with-syntax ([m m])
                 #'(check-unary-between/c '>/c m)))
        #'>
        #'low)]))
  
  (define/opter (</c opt/i opt/info stx)
    (syntax-case stx (</c)
      [(</c high)
       (single-comparison-opter 
        opt/info
        stx
        (λ (m) (with-syntax ([m m])
                 #'(check-unary-between/c '</c m)))
        #'<
        #'high)]))

  (define (</c x)
    (flat-named-contract
     `(</c ,x)
     (λ (y) (and (number? y) (< y x)))))
  (define (>/c x)
    (flat-named-contract
     `(>/c ,x)
     (λ (y) (and (number? y) (> y x)))))

  (define natural-number/c
    (flat-named-contract
     'natural-number/c
     (λ (x)
       (and (number? x)
	    (integer? x)
	    (x . >= . 0)))))
  
  (define (integer-in start end)
    (unless (and (integer? start)
                 (exact? start)
                 (integer? end)
                 (exact? end))
      (error 'integer-in "expected two exact integers as arguments, got ~e and ~e" start end))
    (flat-named-contract 
     `(integer-in ,start ,end)
     (λ (x)
       (and (integer? x)
            (exact? x)
            (<= start x end)))))

  (define (real-in start end)
    (unless (and (real? start)
                 (real? end))
      (error 'real-in "expected two real numbers as arguments, got ~e and ~e" start end))
    (flat-named-contract 
     `(real-in ,start ,end)
     (λ (x)
       (and (real? x)
            (<= start x end)))))
  
  (define (not/c f)
    (unless (flat-contract/predicate? f)
      (error 'not/c "expected a procedure of arity 1 or <flat-named-contract>, given: ~e" f))
    (build-flat-contract
     (build-compound-type-name 'not/c (proc/ctc->ctc f))
     (λ (x) (not (test-proc/flat-contract f x)))))

  (define (listof p)
    (unless (flat-contract/predicate? p)
      (error 'listof "expected a flat contract or procedure of arity 1 as argument, got: ~e" p))
    (build-flat-contract
     (build-compound-type-name 'listof (proc/ctc->ctc p))
     (λ (v)
       (and (list? v)
	    (andmap (λ (ele) (test-proc/flat-contract p ele))
		    v)))))
  
  (define-syntax (*-immutableof stx)
    (syntax-case stx ()
      [(_ predicate? fill type-name name)
       (identifier? (syntax predicate?))
       (syntax
        (let ([fill-name fill])
          (λ (input)
            (let* ([ctc (coerce-contract 'name input)]
                   [proj (contract-proc ctc)])
              (make-proj-contract
               (build-compound-type-name 'name ctc)
               (λ (pos-blame neg-blame src-info orig-str)
                 (let ([p-app (proj pos-blame neg-blame src-info orig-str)])
                   (λ (val)
                     (unless (predicate? val)
                       (raise-contract-error
                        val
                        src-info
                        pos-blame
                        orig-str
                        "expected <~a>, given: ~e"
                        'type-name
                        val))
                     (fill-name p-app val))))
               predicate?)))))]))
  
  (define (map-immutable f lst)
    (let loop ([lst lst])
      (cond
        [(pair? lst)
         (cons-immutable (f (car lst))
                         (loop (cdr lst)))]
        [(null? lst) null])))
  
  (define (immutable-list? val)
    (let loop ([v val])
      (or (and (pair? v)
               (immutable? v)
               (loop (cdr v)))
          (null? v))))
  
  (define list-immutableof
    (*-immutableof immutable-list? map-immutable immutable-list list-immutableof))
  
  (define listof-unsafe
    (*-immutableof list? map list listof-unsafe))

  (define (immutable-vector? val) (and (immutable? val) (vector? val)))
  
  (define vector-immutableof
    (*-immutableof immutable-vector?
                   (λ (f v) (apply vector-immutable (map f (vector->list v))))
                   immutable-vector
                   vector-immutableof))
  
  (define (vectorof p)
    (unless (flat-contract/predicate? p)
      (error 'vectorof "expected a flat contract or procedure of arity 1 as argument, got: ~e" p))
    (build-flat-contract
     (build-compound-type-name 'vectorof (proc/ctc->ctc p))
     (λ (v)
       (and (vector? v)
	    (andmap (λ (ele) (test-proc/flat-contract p ele))
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
       (λ (v)
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
     (λ (x)
       (and (box? x)
	    (test-proc/flat-contract pred (unbox x))))))

  (define (cons/c hdp tlp)
    (unless (and (flat-contract/predicate? hdp)
                 (flat-contract/predicate? tlp))
      (error 'cons/c "expected two flat contracts or procedures of arity 1, got: ~e and ~e" hdp tlp))
    (build-flat-contract
     (build-compound-type-name 'cons/c (proc/ctc->ctc hdp) (proc/ctc->ctc tlp))
     (λ (x)
       (and (pair? x)
            (test-proc/flat-contract hdp (car x))
            (test-proc/flat-contract tlp (cdr x))))))
  
  ;;
  ;; cons/c opter
  ;;
  (define/opter (cons/c opt/i opt/info stx)
    (define (opt/cons-ctc hdp tlp)
      (let-values ([(next-hdp lifts-hdp superlifts-hdp partials-hdp flat-hdp unknown-hdp stronger-ribs-hd)
                    (opt/i opt/info hdp)]
                   [(next-tlp lifts-tlp superlifts-tlp partials-tlp flat-tlp unknown-tlp stronger-ribs-tl)
                    (opt/i opt/info tlp)]
                   [(error-check) (car (generate-temporaries (syntax (error-check))))])
        (with-syntax ((next (with-syntax ((flat-hdp flat-hdp)
                                          (flat-tlp flat-tlp)
                                          (val (opt/info-val opt/info)))
                              (syntax
                               (and (pair? val)
                                    (let ((val (car val))) flat-hdp)
                                    (let ((val (cdr val))) flat-tlp))))))
          (values
           (with-syntax ((val (opt/info-val opt/info))
                         (ctc (opt/info-contract opt/info))
                         (pos (opt/info-pos opt/info))
                         (src-info (opt/info-src-info opt/info))
                         (orig-str (opt/info-orig-str opt/info)))
             (syntax (if next
                         val
                         (raise-contract-error
                          val
                          src-info
                          pos
                          orig-str
                          "expected <~a>, given: ~e"
                          ((name-get ctc) ctc)
                          val))))
           (append
            lifts-hdp lifts-tlp
            (list (cons error-check
                        (with-syntax ((hdp hdp)
                                      (tlp tlp)
                                      (check (with-syntax ((flat-hdp
                                                            (cond
                                                              [unknown-hdp
                                                               (with-syntax ((ctc unknown-hdp))
                                                                 (syntax (flat-contract/predicate? ctc)))]
                                                              [else (if flat-hdp #'#t #'#f)]))
                                                           (flat-tlp
                                                            (cond
                                                              [unknown-tlp
                                                               (with-syntax ((ctc unknown-tlp))
                                                                 (syntax (flat-contract/predicate? ctc)))]
                                                              [else (if flat-tlp #'#t #'#f)])))
                                               (syntax (and flat-hdp flat-tlp)))))
                          (syntax
                           (unless check
                             (error 'cons/c "expected two flat contracts or procedures of arity 1, got: ~e and ~e"
                                    hdp tlp)))))))
           (append superlifts-hdp superlifts-tlp)
           (append partials-hdp partials-tlp)
           (syntax (if next #t #f))
           #f
           (append stronger-ribs-hd stronger-ribs-tl)))))
    
    (syntax-case stx (cons/c)
      [(cons/c hdp tlp)
       (opt/cons-ctc #'hdp #'tlp)]))
  
  (define-syntax (*-immutable/c stx)
    (syntax-case stx ()
      [(_ predicate? constructor (arb? selectors ...) type-name name)
       #'(*-immutable/c predicate? constructor (arb? selectors ...) type-name name #t)]
      [(_ predicate? constructor (arb? selectors ...) type-name name test-immutable?)
       (and (eq? #f (syntax-object->datum (syntax arb?)))
            (boolean? (syntax-object->datum #'test-immutable?)))
       (let ([test-immutable? (syntax-object->datum #'test-immutable?)])
         (with-syntax ([(params ...) (generate-temporaries (syntax (selectors ...)))]
                       [(p-apps ...) (generate-temporaries (syntax (selectors ...)))]
                       [(ctc-x ...) (generate-temporaries (syntax (selectors ...)))]
                       [(procs ...) (generate-temporaries (syntax (selectors ...)))]
                       [(selector-names ...) (generate-temporaries (syntax (selectors ...)))])
           #`(let ([predicate?-name predicate?]
                   [constructor-name constructor]
                   [selector-names selectors] ...)
               (λ (params ...)
                 (let ([ctc-x (coerce-contract 'name params)] ...)
                   (let ([procs (contract-proc ctc-x)] ...)
                     (make-proj-contract
                      (build-compound-type-name 'name (proc/ctc->ctc params) ...)
                      (λ (pos-blame neg-blame src-info orig-str)
                        (let ([p-apps (procs pos-blame neg-blame src-info orig-str)] ...)
                          (λ (v)
                            (if #,(if test-immutable?
                                      #'(and (predicate?-name v)
                                             (immutable? v))
                                      #'(predicate?-name v))
                                (constructor-name (p-apps (selector-names v)) ...)
                                (raise-contract-error
                                 v
                                 src-info
                                 pos-blame
                                 orig-str
                                 #,(if test-immutable?
                                       "expected immutable <~a>, given: ~e"
                                       "expected <~a>, given: ~e")
                                 'type-name
                                 v)))))
                      #f)))))))]
      [(_ predicate? constructor (arb? selector) correct-size type-name name)
       (eq? #t (syntax-object->datum (syntax arb?)))
       (syntax
        (let ([predicate?-name predicate?]
              [constructor-name constructor]
              [selector-name selector])
          (λ params
            (let ([ctcs (map (λ (param) (coerce-contract 'name param)) params)])
              (let ([procs (map contract-proc ctcs)])
                (make-proj-contract
                 (apply build-compound-type-name 'name (map proc/ctc->ctc params))
                 (λ (pos-blame neg-blame src-info orig-str)
                   (let ([p-apps (map (λ (proc) (proc pos-blame neg-blame src-info orig-str)) procs)]
                         [count (length params)])
                     (λ (v)
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
                            pos-blame
                            orig-str
                            "expected <~a>, given: ~e"
                            'type-name
                            v)))))
                 #f))))))]))
  
  (define cons-immutable/c (*-immutable/c pair? cons-immutable (#f car cdr) immutable-cons cons-immutable/c))
  (define cons-unsafe/c (*-immutable/c pair? cons (#f car cdr) cons cons-unsafe/c #f))
  (define box-immutable/c (*-immutable/c box? box-immutable (#f unbox) immutable-box box-immutable/c))
  (define vector-immutable/c (*-immutable/c vector?
                                            vector-immutable
                                            (#t (λ (v i) (vector-ref v i)))
                                            (λ (n v) (= n (vector-length v)))
                                            immutable-vector
                                            vector-immutable/c))
  
  ;;
  ;; cons-immutable/c opter
  ;;
  (define/opter (cons-immutable/c opt/i opt/info stx)
    (define (opt/cons-immutable-ctc hdp tlp)
      (let-values ([(next-hdp lifts-hdp superlifts-hdp partials-hdp flat-hdp unknown-hdp stronger-ribs-hd)
                    (opt/i opt/info hdp)]
                   [(next-tlp lifts-tlp superlifts-tlp partials-tlp flat-tlp unknown-tlp stronger-ribs-tl)
                    (opt/i opt/info tlp)])
        (with-syntax ((check (with-syntax ((val (opt/info-val opt/info)))
                               (syntax (and (immutable? val) (pair? val))))))
          (values
           (with-syntax ((val (opt/info-val opt/info))
                         (ctc (opt/info-contract opt/info))
                         (pos (opt/info-pos opt/info))
                         (src-info (opt/info-src-info opt/info))
                         (orig-str (opt/info-orig-str opt/info))
                         (next-hdp next-hdp)
                         (next-tlp next-tlp))
             (syntax (if check
                         (cons-immutable (let ((val (car val))) next-hdp)
                                         (let ((val (cdr val))) next-tlp))
                         (raise-contract-error
                          val
                          src-info
                          pos
                          orig-str
                          "expected <~a>, given: ~e"
                          ((name-get ctc) ctc)
                          val))))        
           (append lifts-hdp lifts-tlp) 
           (append superlifts-hdp superlifts-tlp)
           (append partials-hdp partials-tlp)
           (if (and flat-hdp flat-tlp)
               (with-syntax ((val (opt/info-val opt/info))
                             (flat-hdp flat-hdp)
                             (flat-tlp flat-tlp))
                 (syntax (if (and check
                                  (let ((val (car val))) flat-hdp)
                                  (let ((val (cdr val))) flat-tlp)) #t #f)))
               #f)
           #f
           (append stronger-ribs-hd stronger-ribs-tl)))))
    
    (syntax-case stx (cons-immutable/c)
      [(cons-immutable/c hdp tlp) (opt/cons-immutable-ctc #'hdp #'tlp)]))
       
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
    (unless (andmap (λ (x) (or (contract? x)
                                    (and (procedure? x)
                                         (procedure-arity-includes? x 1))))
                    args)
      (error 'list/c "expected contracts or procedures of arity 1, got: ~a"
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
  
  (define (list-unsafe/c . args)
    (unless (andmap (λ (x) (or (contract? x)
                               (and (procedure? x)
                                    (procedure-arity-includes? x 1))))
                    args)
      (error 'list/c "expected contracts or procedures of arity 1, got: ~a"
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
	[else (cons-unsafe/c (car args) (loop (cdr args)))])))

  (define (syntax/c ctc-in)
    (let ([ctc (coerce-contract 'syntax/c ctc-in)])
      (build-flat-contract
       (build-compound-type-name 'syntax/c ctc)
       (let ([pred (flat-contract-predicate ctc)])
         (λ (val)
           (and (syntax? val)
                (pred (syntax-e val))))))))
  
  (define promise/c
    (λ (ctc-in)
      (let* ([ctc (coerce-contract 'promise/c ctc-in)]
             [ctc-proc (contract-proc ctc)])
        (make-proj-contract
         (build-compound-type-name 'promise/c ctc)
         (λ (pos-blame neg-blame src-info orig-str)
           (let ([p-app (ctc-proc pos-blame neg-blame src-info orig-str)])
             (λ (val)
               (unless (promise? val)
                 (raise-contract-error
                  val
                  src-info
                  pos-blame
                  'ignored
                  orig-str
                  "expected <promise>, given: ~e"
                  val))
               (delay (p-app (force val))))))
         promise?))))
  
  #|
   as with copy-struct in struct.ss, this first begin0
   expansion "declares" that struct/c is an expression.
   It prevents further expansion until the internal definition
   context is sorted out.
  |#
  (define-syntax (struct/c stx)
    (syntax-case stx ()
      [(_ . args) 
       (with-syntax ([x (syntax/loc stx (do-struct/c . args))])
         (syntax/loc stx (begin0 x)))]))
  
  (define-syntax (do-struct/c stx)
    (syntax-case stx ()
      [(_ struct-name args ...)
       (and (identifier? (syntax struct-name))
            (syntax-local-value (syntax struct-name) (λ () #f)))
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
         (unless (= (length (syntax->list (syntax (rev-selector-id ...))))
                    (length (syntax->list (syntax (args ...)))))
           (raise-syntax-error 'struct/c 
                               (format "expected ~a contracts because struct ~a has ~a fields"
                                       (length (syntax->list (syntax (rev-selector-id ...))))
                                       (syntax-e #'struct-name)
                                       (length (syntax->list (syntax (rev-selector-id ...)))))
                               stx))
         (with-syntax ([(selector-id ...) (reverse (syntax->list (syntax (rev-selector-id ...))))])
           (syntax
            (let ([ctc-x (coerce-contract 'struct/c args)] ...)
              
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
       (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))]))
  
  
  (define (parameter/c x)
    (make-parameter/c (coerce-contract 'parameter/c x)))
  
  (define-struct/prop parameter/c (ctc)
    ((proj-prop (λ (ctc)
                  (let ([c-proc ((proj-get (parameter/c-ctc ctc)) (parameter/c-ctc ctc))])
                    (λ (pos-blame neg-blame src-info orig-str)
                      (let ([partial-neg-contract (c-proc neg-blame pos-blame src-info orig-str)]
                            [partial-pos-contract (c-proc pos-blame neg-blame src-info orig-str)])
                        (λ (val)
                          (cond
                            [(parameter? val)
                             (make-derived-parameter 
                              val 
                              partial-neg-contract
                              partial-pos-contract)]
                            [else
                             (raise-contract-error val src-info pos-blame orig-str 
                                                   "expected a parameter")])))))))
     (name-prop (λ (ctc) (build-compound-type-name 'parameter/c (parameter/c-ctc ctc))))
     (first-order-prop
      (λ (ctc)
        (let ([tst ((first-order-get (parameter/c-ctc ctc)) (parameter/c-ctc ctc))])
          (λ (x)
            (and (parameter? x)
                 (tst (x)))))))
     
     (stronger-prop
      (λ (this that)
        ;; must be invariant (because the library doesn't currently split out pos/neg contracts
        ;; which could be tested individually ....)
        (and (parameter/c? that)
             (contract-stronger? (parameter/c-ctc this) 
                                 (parameter/c-ctc that))
             (contract-stronger? (parameter/c-ctc that) 
                                 (parameter/c-ctc this)))))))
  
  )
