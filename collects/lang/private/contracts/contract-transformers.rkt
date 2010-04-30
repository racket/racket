(module contract-transformers mzscheme
  (require-for-template mzscheme
			"contracts-helpers.ss" 
			"beginner-contracts.ss" 
			"intermediate-contracts.ss" 
			"advanced-contracts.ss")

  (provide (all-defined))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; COMMON STUFF- Translators
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; We really should compare with module-identifier=", 
  ;; but importing the right identifiers here is a pain.
  ;; As it happens, the teaching languages disable shadowing,
  ;; so we can safely use symbol equality.
  (define (contract-id=? a b)
    (eq? (syntax-e a) (syntax-e b)))

  ;; a translator is a function that translates syntax for contracts
  ;; (like:  number, (number -> string), (number -> (number -> boolean)) )
  ;; and converts it into the necesary function calls to enforce those contracts

  ;; translate-arrow-contract : syntax (syntax -> syntax) -> syntax
  ;; parses contracts for (domain ... -> range) type contracts
  ;; the first argument is the syntax object to parse
  ;; the second argument is translator that should be used for recursive calls
  (define translate-arrow-contract
    (lambda (stx translator)
      (syntax-case stx (->)
	[(domain ... -> range)
	 (with-syntax 
	     ([(parsed-domain ...) (map translator (syntax-e (syntax (domain ...))))]
	      [parsed-range (translator (syntax range))]
	      [ret stx])
	   (syntax (->-contract (list parsed-domain ...) parsed-range #'ret)))]
	[else-stx (raise-syntax-error 'contracts "unknown contract" (syntax else-stx))])))

  ;; beginners cant use higher order contracts, so only allow func contracts in the top level
  (define beginner-translate-contract
    (case-lambda
     [(stx) (beginner-translate-contract stx beginner-translate-flat-contract)]
     [(stx recur)
      (syntax-case stx (->)
	[(domain ... -> range) (translate-arrow-contract stx recur)]
	[_else-stx (beginner-translate-flat-contract stx recur)])]))


  ;; syntax definitions for beginner language contracts (from beginner-contracts.scm)
  (define beginner-translate-flat-contract
    (case-lambda 
     [(stx) (beginner-translate-flat-contract stx beginner-translate-flat-contract)]
     [(stx recur)
      
      (syntax-case* stx (-> add1 quote cons number any integer 
			    exact-number inexact-number posn boolean 
			    true false 
			    string empty symbol list)
		    contract-id=?
	[(cons a b) (with-syntax ([car-contract (recur (syntax a))]
				  [cdr-contract (recur (syntax b))]
				  [ret stx])
		      (syntax/loc stx (cons-contract car-contract cdr-contract #'ret)))]
	[(list a ...) (with-syntax ([(translated ...) (map recur (syntax-e (syntax/loc stx (a ...))))]
				    [ret stx])
			(syntax/loc stx (args-contract (list translated ...) #'ret)))]
	[(add1 a) (with-syntax ([translated (recur (syntax/loc stx a))]
				[ret stx])
		    (syntax/loc stx (add1-contract translated #'ret)))]
	[empty (with-syntax ([ret stx]) 
		 (syntax/loc stx (empty-contract #'ret)))]
	[(quote n) (with-syntax ([ret stx]) 
		     (syntax/loc stx (build-flat-contract (lambda (x) (eq? x 'n)) 'n #'ret)))]
	[number (with-syntax ([ret stx]) 
		  (syntax/loc stx (number-contract #'ret)))]
	[any (with-syntax ([ret stx]) 
	       (syntax/loc stx (any-contract #'ret)))]
	[symbol (with-syntax ([ret stx]) 
		  (syntax/loc stx (symbol-contract #'ret)))]
	[integer (with-syntax ([ret stx]) 
		   (syntax/loc stx (integer-contract #'ret)))]
	[exact-number (with-syntax ([ret stx]) 
			(syntax/loc stx (exact-number-contract #'ret)))]
	[inexact-number (with-syntax ([ret stx]) 
			  (syntax/loc stx (inexact-number-contract #'ret)))]
	[boolean (with-syntax ([ret stx]) 
		   (syntax/loc stx (boolean-contract #'ret)))]
	[true (with-syntax ([ret stx]) 
		(syntax/loc stx (true-contract #'ret)))]
	[false (with-syntax ([ret stx]) 
		 (syntax/loc stx (false-contract #'ret)))]
	[string(with-syntax ([ret stx]) 
		 (syntax/loc stx (string-contract #'ret)))]
	[posn (with-syntax ([ret stx]) 
		(syntax/loc stx (posn-contract #'ret)))]
	[(domain ... -> range) 
	 (raise-syntax-error 'contracts "functions in the beginner language can't take other functions as input" stx)]
	[(make-struct e1 e2 ...)
	 (pair? (regexp-match "make-(.*)" (symbol->string (syntax-object->datum (syntax make-struct)))))
	 (let ([make (regexp-match "make-(.*)" (symbol->string (syntax-object->datum (syntax make-struct))))])
	   (let ([struct (datum->syntax-object stx (string->symbol (cadr make)))])
	     (if (define-struct? struct)
		 (with-syntax ([pred (get-predicate-from-struct struct)]
			       [(accessors ...) (reverse (get-accessors-from-struct struct))]
			       [name (syntax-object->datum struct)]
			       [(translated ...) (map recur (syntax-e (syntax (e1 e2 ...))))]
			       [ret stx])                  ; we wrap with lambdas so that beginner doesnt complain about using them without a (
		   (syntax/loc stx (struct-contract 'name (lambda (x) (pred x)) (list (lambda (x) (accessors x)) ...) (list translated ...) #'ret)))
		 (raise-syntax-error 'contracts (format "unknown structure type: ~e" 
							(syntax-object->datum struct)) stx))))]
	
	[-> (raise-syntax-error 'contracts "found a lone '->', check your parentheses!" stx)]
	
	[name
	 (number? (syntax-object->datum (syntax name)))
	 (with-syntax ([num (syntax-object->datum (syntax name))]
		       [ret stx])
	   (syntax/loc stx (build-flat-contract (lambda (x) (eq? num x)) num #'ret)))]
	
	[name 
	 (define-data? (syntax name)) ; things from a define data
	 (with-syntax ([cnt (get-cnt-from-dd (syntax name))])
	   (syntax/loc stx cnt))]
	
	[name
	 (define-struct? (syntax name)) ; generic structs
	 (with-syntax ([pred (get-predicate-from-struct (syntax name))]
		       [type-name (get-name-from-struct (syntax name))]
		       [ret stx])
	   (syntax/loc stx (build-flat-contract (lambda (x) (pred x)) 'type-name #'ret)))]
	
	[name (raise-syntax-error 'contracts "unknown contract" (syntax name))])]))

  ;; stx definitions for intermediate language contracts (intermediate-contracts.scm)
  (define intermediate-translate-contract
    (case-lambda 
     [(stx) (intermediate-translate-contract stx intermediate-translate-contract)]
     [(stx recur)
      (syntax-case* stx (-> listof quote vectorof boxof)
		    contract-id=?
	[(listof type) (with-syntax ([ret stx]
				     [trans-type (recur (syntax type))])  
			 (syntax/loc stx (listof-contract trans-type #'ret)))]
	[(vectorof type) (with-syntax ([ret stx]
				       [trans-type (recur (syntax type))]) 
			   (syntax/loc stx (vectorof-contract trans-type #'ret)))]
	[(boxof type) (with-syntax ([ret stx]
				    [trans-type (recur (syntax type))]) 
			(syntax/loc stx (boxof-contract trans-type #'ret)))]
	[(quote n) (with-syntax ([ret stx]) 
		     (syntax/loc stx (build-flat-contract (lambda (x) (eq? x 'n)) 'n #'ret)))]
	[(domain ... -> range) (translate-arrow-contract stx intermediate-translate-contract)]
	[else-stx (beginner-translate-contract (syntax else-stx) intermediate-translate-contract)])]))

  ;; stx definitions for advanced language contracts (advanced-contracts.scm)
  (define advanced-translate-contract
    (case-lambda 
     [(stx) (advanced-translate-contract stx advanced-translate-contract)]
     [(stx recur) 
      (syntax-case* stx (-> void) contract-id=?
	[void (with-syntax ([ret stx]) 
		(syntax/loc stx (void-contract #'ret)))]
	[(domain ... -> range) (translate-arrow-contract stx advanced-translate-contract)]
	[else-stx (intermediate-translate-contract (syntax else-stx))])]))

  ;; helper functions for these 

  ;;get-predicate-from-struct : stx -> stx
  ;;returns the predicate for the given stx, which needs to be an object defined via define-struct/define-data
  (define (get-predicate-from-struct stx) 
    (caddr (syntax-local-value stx)))

  ;;get-cnt-from-dd : stx -> stx
  ;;returns the syntax contract that corresponds to the current define-data object
  (define (get-cnt-from-dd stx)
    (if (define-data? stx)
	(caddr (syntax-local-value stx))))

  ;;get-name-from-struct : stx -> stx
  ;;returns the name for the given stx, which needs to be an object defined via define-struct/define-data
  (define (get-name-from-struct stx) 
    (car (syntax-local-value stx)))

  ;;get-accessors-from-struct : stx -> list of stx
  ;;returns the name for the given stx, which needs to be an object defined via define-struct/define-data
  (define (get-accessors-from-struct stx) 
    (cadddr (syntax-local-value stx)))

  ;;define-struct? : stx -> boolean
					; was this thing defined in a define-struct? 
  (define (define-struct? stx)
    (and (identifier? stx) (pair? (syntax-local-value stx (lambda () #f)))))

  ;;define-data? : stx -> boolean
					; was this thing defined in a define-data? 
  (define (define-data? stx)
    (and (identifier? stx)
	 (let ([data (syntax-local-value stx (lambda () #f))])
	   (and data (pair? data) (eq? (cadr data) 'define-data)))))

  )
