(module contracts-helpers mzscheme
  
  (require "hilighters.ss")
  (require mzlib/etc)
           
  (provide (all-defined))
  
  #| 
contract : 
  enforcer: function (x -> x) that enforces the contract
  hilighter: a function that given a path to a contract violation, 
             returns the content of the contract with the expression 
             denoted by the path hilighted.
  source,line,column,pos,span: source location data for where the contract was defined
                               correspond to same fields in defining syntax object
|#
  (define-struct contract (enforcer hilighter source line column position span))
  
  #|
flat-contract : contract
  predicate: a predicate that can be used to verify this contract.
|#
  (define-struct (flat-contract contract) (predicate))
  
  ; build-flat-contract: short cut for making flat-contracts
  ; (any -> bool) symbol syntax -> flat-contract
  ; returns contract that enforces given predicate
  (define build-flat-contract
    (lambda (predicate name stx)
      (letrec ([me (make-flat-contract
                    
                    (lambda (x) 
                      (if (predicate x) 
                          x 
                          (contract-error x me '())))
                    
                    (mk-flat-hilighter name)
                    
                    (syntax-source stx)
                    (syntax-line stx)
                    (syntax-column stx)
                    (syntax-position stx)
                    (syntax-span stx)
                    
                    predicate)])
        me)))
  
  ;; ->-contract: list-of-contracts contract syntax -> contract
  ;; translates domain and range contracts into a single contract
  ;; that represents the contract of a function that takes in values that 
  ;; satisfy domain contract and returns values that satisfies range contract
  ;; the domain contract is defined by the list,
  (define ->-contract
    (lambda (domain-contracts-list range-contract stx)
      (letrec ([me (make-contract
                    (lambda (f)
                      (cond [(not (procedure? f))
                             (error 'contracts (format "~e is not a function" f))]
                            [(procedure-arity-includes? f (length domain-contracts-list))
                             (apply-contract f domain-contracts-list range-contract me stx)]
                            [else
                             (parameterize ([print-struct true])
                               (error 'contracts (format "~e expects ~e arguments, not ~e" 
                                                         f 
                                                         (procedure-arity f) 
                                                         (length domain-contracts-list))))]))
                   
                    (let ([dom-hilighter-list (map (lambda (x) (contract-hilighter x)) domain-contracts-list)]) 
                      (mk-arrow-hilighter dom-hilighter-list (contract-hilighter range-contract)))
                    
                    (syntax-source stx)
                    (syntax-line stx)
                    (syntax-column stx)
                    (syntax-position stx)
                    (syntax-span stx))])
        
        me)))
  
  ;; args-contract: (contract ...) stx -> contract
  ;; takes in a list of contracts, and returns a contract that checks for a
  ;; list (a b c ...) where each member satisfies the corresponding contract in the input
  ;; useful for function arguments
  (define (args-contract cnt-list stx)
    (letrec ([me (make-flat-contract
                  (lambda (values) 
                    (if (pair? values)
                        (verify-contracts me cnt-list values)
                        (contract-error values me '())))
                  (mk-list-hilighter (map (lambda (c) (contract-hilighter c)) cnt-list))
                  (syntax-source stx)
                  (syntax-line stx)
                  (syntax-column stx)
                  (syntax-position stx)
                  (syntax-span stx)
                  
                  (lambda (values)
                    (if (not (andmap flat-contract? cnt-list))
                        (error 'args-contract 
                               "not all subcontracts of ~e are flat contracts, so predicate is undefined"
                               ((contract-hilighter me) #f))
                        (and (list? values) 
                             (andmap (lambda (c x) ((flat-contract-predicate c) x)) cnt-list values)))))])
      me))
  
  
  ;; apply-contract: (any ... -> any) list of contracts contract -> (any ... -> any)
  ;; returns a function that identical to the given one, except that it enforces contracts
  ;; since functions can take in multiple values, the full domain contract is given as a list of 
  ;; contracts.
  (define (apply-contract function domain-contracts-list range-contract func-cnt stx)
    (let ([dom-list-contract (args-contract domain-contracts-list stx)]
          [range-composer 
           (lambda (values)
             (lambda (error) 
               (let ([name (object-name function)])
                 (if (regexp-match #rx"[0-9]+:[0-9]+" (symbol->string name)) 
                     ; cant infer a good name (higher order things)
                     (format "function defined on line ~e (called with: ~a) failed the assertion ~s"
                             name 
                             (format-list-with-spaces values)
                             error)
                     ; have func name
                     (format "function ~e (called with: ~a) failed the assertion ~s"
                             name
                             (format-list-with-spaces values)
                             error)))))]
          [domain-composer
           (lambda (values)
             (lambda (error)
               (let ([name (object-name function)])
                 (if (regexp-match #rx"[0-9]+:[0-9]+" (symbol->string name)) 
                     (format "the arguments to the function defined on line ~e (~e) failed the assertion ~s"
                             name values error)
                     (format "function ~e's arguments ~a failed the assertion ~s" 
                             name
                             (format-list-with-spaces values)
                             error)))))])
      
      (lambda values
        (let* ([checked-values (catch-contract-error (domain-composer values)
                                                     'car
                                                     func-cnt
                                                     ((contract-enforcer dom-list-contract) values))]
               [return-value (apply function checked-values)]
               [range-enforcer (contract-enforcer range-contract)])
          (catch-contract-error (range-composer values) 'cdr func-cnt (range-enforcer return-value))))))
  
  ;; format-list-with-spaces : (listof any) -> string
  (define (format-list-with-spaces args)
    (cond
      [(null? args) ""]
      [else 
       (apply
        string-append
        (let loop ([fst (car args)]
                   [rst (cdr args)])
          (cond
            [(null? rst) (list (format "~e" fst))]
            [else (cons (format "~e " fst)
                        (loop (car rst)
                              (cdr rst)))])))]))
              
  
  ; verify-contracts: contract contracts values
  ; contracts and values are two lists of equal length
  ; uses the hilighter from the first argument. enforcer for args-contract
  (define (verify-contracts cnt cnt-list values)
    (if (not (eq? (length cnt-list) (length values)))
        (error 'verify-contracts 
               (format "~e and ~e dont have same length (~e and ~e)" 
                       cnt-list
                       values
                       (length cnt-list)
                       (length values))))
    (let loop ([path-to-add (cons 'car '())]
               [curr-cnt cnt-list]
               [curr-val values]
               [ret-val '()])
      
      (if (or (null? curr-val) (null? curr-cnt))
          (reverse ret-val)   
          
          (let ([curr-ret (catch-contract-error path-to-add
                                                cnt
                                                ((contract-enforcer (car curr-cnt)) (car curr-val)))])  
            (loop (cons 'cdr path-to-add)
                  (cdr curr-cnt)
                  (cdr curr-val)
                  (cons curr-ret ret-val))))))
  
  ; cons-contract: contract contract -> contract
  ; given two contracts, returns a contract that accepts lists where the car satisfies the first
  ; and the cdr satisfies the second
  (define (cons-contract car-contract cdr-contract stx)
    (if (not (and (flat-contract? car-contract) (flat-contract? cdr-contract)))
        (error 'contracts "(cons <a> <b>) type contracts can only take flat contracts")
        (letrec ([cons-hilighter (mk-cons-hilighter (contract-hilighter car-contract) (contract-hilighter cdr-contract))]
                 [me (make-flat-contract 
                      (lambda (l)
                        
                        (if (pair? l) 
                            (let ([composer (lambda (h) (format "~s didnt satisfy the contract ~s" l h))]) 
                              (catch-contract-error composer 'car me ((contract-enforcer car-contract) (car l)))
                              (catch-contract-error composer 'cdr me ((contract-enforcer cdr-contract) (cdr l)))
                              l)
                            (contract-error l me '())))
                      
                      cons-hilighter
                      
                      (syntax-source stx)
                      (syntax-line stx)
                      (syntax-column stx)
                      (syntax-position stx)
                      (syntax-span stx)
                      
                      (lambda (l) (and (pair? l) ((flat-contract-predicate car-contract) (car l)) 
                                       ((flat-contract-predicate cdr-contract) (cdr l)))))])
          me)))
  
  
  ;; add1-contract: flat-contract syntax -> flat-contract
  ;; implements checker for (add1 <cnt>) style contracts
  (define (add1-contract cnt stx)
    (if (flat-contract? cnt) 
        (letrec ([me (make-flat-contract
                      
                      (lambda (x) 
                        (if (and (number? x) (>= x 0))
                            (catch-contract-error 'car me ((contract-enforcer cnt) (- x 1)))
                            (contract-error x me '())))
                      
                      (mk-add1-hilighter (contract-hilighter cnt))
                      
                      (syntax-source stx)
                      (syntax-line stx)
                      (syntax-column stx)
                      (syntax-position stx)
                      (syntax-span stx)
                      
                      (lambda (x) (and (number? x) (>= x 0) ((flat-contract-predicate cnt) (- x 1)))))])
          me)
        (error 'contracts "add1 can only be used with flat contracts")))
  
  ; struct contract: symbol (any -> boolean) (struct -> any) flat-contracts -> flat-contract
  ; returns a contract that enforces the contract (make-<blah> cnt cnt cnt ...)
  (define (struct-contract name predicate accessors field-contracts stx)
    (letrec ([hilighter (mk-struct-hilighter name (map contract-hilighter field-contracts))]
             [not-struct-composer 
              (lambda (value) 
                (lambda (error)
                  (format "~e is not a structure of type ~e, so didnt satisfy contract ~e" value name error)))]
             [me (make-flat-contract
                  (lambda (x) (if (predicate x) 
                                  (let ([field-values (map (lambda (accessor) (accessor x)) accessors)]
                                        [field-composer (lambda (error) 
                                                          (format "the fields of structure ~e did not satisfy the assertion ~e" 
                                                                  x error))])
                                    (catch-contract-error field-composer #f me 
                                                          (verify-contracts me field-contracts field-values)))
                                  (contract-error x me '() #f (not-struct-composer x))))
                  hilighter
                  (syntax-source stx)
                  (syntax-line stx)
                  (syntax-column stx)
                  (syntax-position stx)
                  (syntax-span stx)
                  (lambda (x) 
                    (and (predicate x) 
                         (andmap 
                          (lambda (a b) 
                            (and (flat-contract? b) 
                                 ((flat-contract-predicate b) (a x)))) 
                          accessors field-contracts))))])
      me))
  
  ; define-data-enforcer: contract any listof contracts -> any
  (define (define-data-enforcer me value list-of-cnts)
    (if ((flat-contract-predicate me) value)
        value
        (define-data-error me value list-of-cnts)))
  
  ;; define-data-error: define-data-contract any list-of-flat-contracts -> void
  ;; raises the contract exception with a good error message
  (define (define-data-error me value list-of-cnts)
    (let loop ([contract-list list-of-cnts]
               [max-depth 0]
               [best-cnt #f])
      
      (if (null? contract-list) 
          (if (or (eq? max-depth 0) (not best-cnt)) ; all contracts failed at top level, so just give standard report
              (contract-error value me '())
              (define-data-report me value best-cnt))) ; there was a contract that failed deeper than others, 
      
      ; give this as the failed part
      (let ([current-depth (get-error-depth (car contract-list) value)])
        (if (> current-depth max-depth)
            (loop (cdr contract-list)
                  current-depth
                  (car contract-list))
            
            (loop (cdr contract-list)
                  max-depth
                  best-cnt)))))
  
  ;; get-error-depth: flat-contract any -> number
  ;; returns how 'deep' the contract checker had to go to find a violation
  ;; used as a guess as to which contract was the best match for a define-data
  (define (get-error-depth cnt value)
    (with-handlers ([exn:contract-violation?
                     (lambda (e)
                       (length (exn:contract-violation-trace e)))])
      ((contract-enforcer cnt) value)
      (error 'define-data 
             (format "major internal error: ~e didnt fail ~e, but define-data-error said it did."
                     value
                     ((contract-hilighter cnt) #f)))))
  
  
  ;; define-data-report: define-data-contract any flat-contract -> void
  ;; generates the correct error report for a define data. 
  ;; the first argument is the define-data contract being checked,
  ;; the second is the value being checked
  ;; and the third is the contract (one of the flats that was used in the
  ;; define-data) that will be reported as the best failure match
  (define (define-data-report me value best-cnt)
    (with-handlers ([exn:contract-violation?
                     (lambda (e)
                       (raise
                        (make-exn:contract-violation
                         (format "contract violation: ~e is not a ~e [failed part: ~e]"
                                 value
                                 ((contract-hilighter me) '())
                                 ((contract-hilighter best-cnt) (exn:contract-violation-path e)))
                         (current-continuation-marks)
                         value
                         '()
                         me 
                         (exn:contract-violation-trace e))))])
      
      ((contract-enforcer best-cnt) value))
    
    (error 'define-data 
           (format "major internal error: ~e didnt fail ~e, but define-data-error said it did."
                   value
                   ((contract-hilighter best-cnt) #f))))
  
  ;;;;;;;;;;;;;;;;;;  ERROR HANDLING
  
  ; exn:contract-violation
  ; exception used for contract violations.
  ; full fields: 
  ; - message
  ; - continuation-marks
  ; - value
  ; - path: a list of 'car 'cdr that describes which component of the contract failed
  ; - failed-cnt: the contract that failed
  ; - trace: a list of exn:contract-violation that describe the whole path of contract exceptions thrown
  (define-struct (exn:contract-violation exn) (value path failed-cnt trace))
  
  ; contract-error-display-handler: outputs of the information from a contract exception
  (define contract-error-display-handler 
    (lambda (msg e)
      (if (exn:contract-violation? e)
          
          (let ([failed (exn:contract-violation-failed-cnt e)])
            (begin 
              (printf "~s (source: ~e, line: ~e, col: ~e, pos: ~e, span: ~e)\n" 
                      (exn-message e)
                      (contract-source failed)
                      (contract-line failed)
                      (contract-column failed)
                      (contract-position failed)
                      (contract-span failed))

              (printf "current path: ~e\n" (exn:contract-violation-path e))) 

              (unless (null? (exn:contract-violation-trace e))
                (printf "trace:\n")
                (map (lambda (x) 
                       (printf "\t~s ~s\n" (exn-message x) (exn:contract-violation-path x)))
                     (exn:contract-violation-trace e)))))
          
          (display msg)))
  
  ; contract-error : value failed-contract path [exn:contract-violation message-composer] -> void
  ; produces a nice error message.
  ; if exn-to-pass is given, its tacked on in the front of the new exception's trace. if not given, trace is emptied
  ; a message composer is a function that takes in an S-Expression that represents the failed contract
  ; and returns a string that will be used as the error message
  (define contract-error
    (opt-lambda (value cnt path [exn-to-pass #f] [message-composer #f])
       (let ([cnt-hilighted ((contract-hilighter cnt) path)])
         (raise (make-exn:contract-violation
                 (if message-composer
                   (format "contract violation: ~a" (message-composer cnt-hilighted))
                   (format "contract violation: ~e didnt satisfy the contract ~e" value cnt-hilighted))
                 (current-continuation-marks)
                 value
                 path
                 cnt
                 (if (and (boolean? exn-to-pass) (not exn-to-pass))
                     '()
                     (cons exn-to-pass (exn:contract-violation-trace exn-to-pass))))))))
  
  
  ; shorthand for catching a contract-violation and passing up a new exception with modified paths and hilighters
  ; (catch-contract-error ('car|'cdr) hilighter expr1 expr2 ...)
  (define-syntax catch-contract-error
    (lambda (stx)
      (syntax-case stx ()
        [(_ path-to-add cnt e1)
         (syntax 
          (catch-contract-error #f path-to-add cnt e1))]
        
        ;; this version takes a message-composer, as in contract-error
        [(_ message-composer path-to-add cnt e1)
         (syntax 
          (with-handlers 
              ([exn:contract-violation? 
                (lambda (e) 
                  (let ([val (exn:contract-violation-value e)]
                        [new-path (cond
                                    [(not path-to-add) (exn:contract-violation-path e)]
                                    [(pair? path-to-add) (append path-to-add (exn:contract-violation-path e))]
                                    [else (cons path-to-add (exn:contract-violation-path e))])])
                    (contract-error val cnt new-path e message-composer)))])
            e1))]))))
