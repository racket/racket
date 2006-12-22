(module render-test-list-impl mzscheme
  
  (require (lib "stx.ss" "syntax"))
  
  (require "match-error.ss"
           "match-helper.ss"
           "test-structure.scm"
           "coupling-and-binding.scm"
           "update-counts.scm"
           "update-binding-counts.scm"
           "reorder-tests.scm"
           "match-expander-struct.ss"
           "render-helpers.ss")
  
  (require "render-sigs.ss"
           (lib "unit.ss"))
  
  (require-for-syntax "match-helper.ss"
                      "match-expander-struct.ss"
                      "test-no-order.ss")
  
  (require-for-template mzscheme
			"match-error.ss"
			"test-no-order.ss"
                        "match-helper.ss")
  
  (provide render-test-list@)
  
  
  
  
  (define-unit render-test-list@
    (import ddk-handlers^ getbindings^)
    (export render-test-list^) 
    
    ;; some convenient syntax for make-reg-test and make-shape-test
    (define make-test-gen
      (case-lambda
        [(constructor test ae emitter) (make-test-gen constructor test ae emitter ae)]
        [(constructor test ae emitter ae2)
         (constructor test ae
                      (lambda (ks kf let-bound)
                        (lambda (sf bv)
                          (emit emitter ae2 let-bound sf bv kf ks))))]))
    
    (define (reg-test . args) (apply make-test-gen make-reg-test args))
    (define (shape-test . args) (apply make-test-gen make-shape-test args))
    
    ;; produce a matcher for the empty list
    (define (emit-null ae)  
      (list (reg-test `(null? ,(syntax-object->datum ae)) 
                      ae (lambda (exp) #`(null? #,exp)))))
    
    ;; generic helper for producing set/get matchers
    (define-syntax (set/get-matcher stx)    
      (syntax-case stx (set! get!)
        [(_ set!/get! ae p arg set/get-func) #`(set/get-matcher set!/get! ae p let-bound arg set/get-func)]
        [(_ set!/get! ae p let-bound arg set/get-func)              
         (with-syntax ([sym (syntax-case  #'set!/get! (set! get!) ['set! #''set!-pat] ['get! #''get!-pat])])
           #`(syntax-case arg ()
               [(ident)
                (identifier? #'ident)
                (list (make-act
                       sym
                       ae
                       (lambda (ks kf let-bound)
                         (lambda (sf bv)
                           (ks sf (cons (cons #'ident
                                              set/get-func)
                                        bv))))))]
               [() (match:syntax-err p
                                     (format "there should be an identifier after ~a in pattern" set!/get!))]
               [(_) (match:syntax-err p
                                      (format " ~a followed by something that is not an identifier" set!/get!))]
               [(_ (... ...))
                (match:syntax-err p
                                  (format "there should be only one identifier after ~a in pattern" set!/get!))]
               [_ (match:syntax-err p
                                    (format "invalid ~a pattern syntax" set!/get!))]))]))
    
    
    ;; expand the regexp-matcher into an (and) with string?
    (define (regexp-matcher ae stx pred cert)
      (render-test-list #`(and (? string?) #,pred) ae cert stx))
    
    
    ;;!(function or-gen
    ;;         (form (or-gen exp orpatlist sf bv ks kf let-bound)
    ;;               ->
    ;;               syntax)
    ;;         (contract (syntax list list list (list list -> syntax) 
    ;;                    (list list -> syntax) list)
    ;;                   ->
    ;;                   syntax))
    ;; The function or-gen is very similar to the function gen except
    ;; that it is called when an or pattern is compiled.  An or
    ;; pattern is essentially the same as a match pattern with several
    ;; clauses.  The key differences are that it exists within a
    ;; larger pattern and the state of compilation has information
    ;; that will help optimaize its compilation.  And the success of
    ;; any pattern results in the same outcome.
    (define (or-gen exp orpatlist sf bv ks kf let-bound cert stx)
      (define rendered-list
        (map
         (lambda (pat) 
           (cons (render-test-list pat exp cert stx)
                 (lambda (fail let-bound)
                   (lambda (sf bv)
                     (let ((bv (map
                                (lambda (bind)
                                  (cons (car bind)
                                        (subst-bindings (cdr bind) 
                                                        let-bound)))
                                bv)))
                       (ks sf bv))))))
         orpatlist))
      (update-counts rendered-list)
      (update-binding-counts rendered-list)
      ((meta-couple (reorder-all-lists rendered-list) kf let-bound bv) sf bv))
    
    
    ;;!(function render-test-list
    ;;          (form (render-test-list p ae stx) -> test-list)
    ;;          (contract (syntax syntax syntax) -> list))
    ;; This is the most important function of the entire compiler.
    ;; This is where the functionality of each pattern is implemented.
    ;; This function maps out how each pattern is compiled.  While it
    ;; only returns a list of tests, the comp field of those tests
    ;; contains a function which inturn knows enough to compile the
    ;; pattern.
    ;; <p>This is implemented in what Wright terms as mock-continuation-passing
    ;; style.  The functions that create the syntax for a match success and failure
    ;; are passed forward
    ;; but they are always called in emit.  This is extremely effective for
    ;; handling the different structures that are matched.  This way we can
    ;; specify ahead of time how the rest of the elements of a list or vector
    ;; should be handled.  Otherwise we would have to pass more information
    ;; forward in the argument list of next and then test for it later and
    ;; then take the appropriate action.  To understand this better take a
    ;; look at how proper and improper lists are handled.
    (define/opt (render-test-list p ae cert [stx #'here])
      (define ae-datum (syntax-object->datum ae))
      (syntax-case*
          p
        (_ list quote quasiquote vector box ? app and or not struct set! var
           list-rest get! ... ___ unquote unquote-splicing cons
           list-no-order hash-table regexp pregexp cons) stx-equal?
        
        ;; this is how we extend match
        [(expander args ...)
         (and (identifier? #'expander) 
              (match-expander? (syntax-local-value (cert #'expander) (lambda () #f))))
         (let* ([expander (syntax-local-value (cert #'expander))]
                [transformer (match-expander-plt-match-xform expander)])
           (if (not transformer)
               (match:syntax-err #'expander
                                 "This expander only works with standard match.")
               (let ([introducer (make-syntax-introducer)]
                     [certifier (match-expander-certifier expander)])
                 (render-test-list 
                  (introducer (transformer (introducer p)))
                  ae 
                  (lambda (id)
                    (certifier (cert id) #f introducer))
                  stx))))]
        
        ;; underscore is reserved to match anything and bind nothing
        (_ '()) ;(ks sf bv let-bound))
        
        ;; for variable patterns, we do bindings, and check if we've seen this variable before
        ((var pt)
         (identifier? (syntax pt))
         (list (make-act `bind-var-pat
                         ae
                         (lambda (ks kf let-bound)
                           (lambda (sf bv)                           
                             (cond [(ormap (lambda (x)
                                             (if (bound-identifier=? #'pt (car x))
                                                 (cdr x) 
                                                 #f)) 
                                           bv)
                                    => (lambda (bound-exp)
                                         (emit (lambda (exp)
                                                 #`((match-equality-test) #,exp #,(subst-bindings bound-exp let-bound)))
                                               ae
                                               let-bound
                                               sf bv kf ks))]
                                   [else
                                    (ks sf (cons (cons (syntax pt) ae) bv))]))))))
        
        ;; Recognize the empty list
        ((list) (emit-null ae))          
        
        ;; This recognizes constants such strings
        [pt
         (constant-data? (syntax-e #'pt))
         (list
          (reg-test 
           `(equal? ,ae-datum
                    ,(syntax-object->datum (syntax pt)))
           ae (lambda (exp) #`(equal? #,exp pt))))]
        
        ;(pt
        ; (stx-? regexp? (syntax pt))
        ; (render-test-list (syntax/loc p (regex pt)) ae stx))
        
        ;; match a quoted datum
        ;; this is very similar to the previous pattern, except for the second argument to equal?
        [(quote item)
         (list 
          (reg-test
           `(equal? ,ae-datum
                    ,(syntax-object->datum p))
           ae (lambda (exp) #`(equal? #,exp #,p))))]
        
        ;; check for predicate patterns
        ;; could we check to see if a predicate is a procedure here?
        [(? pred?)
         (list (reg-test 
                `(,(syntax-object->datum #'pred?)
                   ,ae-datum)
                ae (lambda (exp) #`(#,(cert #'pred?) #,exp))))]
        
        ;; app patterns just apply their operation. 
        ((app op pat)
         (render-test-list #'pat #`(#,(cert #'op) #,ae) cert stx))
        
        [(and . pats) (apply 
                       append
                       (map (lambda (pat) (render-test-list pat ae cert stx))
                            (syntax->list #'pats)))]
        
        ((or . pats)
         (list (make-act
                'or-pat ;`(or-pat ,ae-datum)
                ae
                (lambda (ks kf let-bound)
                  (lambda (sf bv)
                    (or-gen ae (syntax-e #'pats)
                            sf bv ks kf let-bound
                            cert stx))))))
        
        
        ((not pat)
         (list (make-act
                'not-pat ;`(not-pat ,ae-datum)
                ae
                (lambda (ks kf let-bound)
                  (lambda (sf bv)
                    ;; swap success and fail
                    (next-outer #'pat ae sf bv let-bound ks kf cert))))))
        
        ;; could try to catch syntax local value error and rethrow syntax error      
        ((list-no-order pats ...)
         (if (stx-null? (syntax (pats ...)))
             (render-test-list #'(list) ae cert stx)
             (let* ((pat-list (syntax->list (syntax (pats ...))))
                    (ddk-list (ddk-in-list? pat-list))
                    (ddk (ddk-only-at-end-of-list? pat-list)))
               (if (or (not ddk-list)
                       (and ddk-list ddk))
                   (let* ((bound (getbindings (append-if-necc 'list
                                                              (syntax (pats ...)))
                                              cert))
                          (bind-map
                           (map (lambda (x)
                                  (cons x #`#,(gensym (syntax-object->datum x))))
                                bound)))
                     
                     (list
                      (shape-test
                       `(list? ,ae-datum)
                       ae (lambda (exp) #`(list? #,exp)))
                      (make-act
                       'list-no-order
                       ae
                       (lambda (ks kf let-bound)
                         (lambda (sf bv)
                           (let ((last-test
                                  (if ddk
                                      (let ((pl (cdr (reverse pat-list))))
                                        (begin
                                          (set! pat-list (reverse (cdr pl)))
                                          (create-test-func (car pl)
                                                            sf
                                                            let-bound
                                                            bind-map
                                                            #t
                                                            cert)))
                                      #f)))
                             #`(let #,(map (lambda (b)
                                             #`(#,(cdr b) '()))
                                           bind-map)
                                 (let ((last-test #,last-test)
                                       (test-list
                                        (list
                                         #,@(map (lambda (p)
                                                   (create-test-func
                                                    p
                                                    sf
                                                    let-bound
                                                    bind-map
                                                    #f
                                                    cert))
                                                 pat-list))))
                                   (if (match:test-no-order test-list
                                                            #,ae
                                                            last-test
                                                            #,ddk)
                                       #,(ks sf (append bind-map bv))
                                       #,(kf sf bv))))))))))
                   (match:syntax-err
                    p
                    (string-append "dot dot k can only appear at "
                                   "the end of unordered match patterns"))))))
        
        ((hash-table pats ...)
         ;; must check the structure
         #;(proper-hash-table-pattern? (syntax->list (syntax (pats ...))))
           (list
            (shape-test
             `(hash-table? ,ae-datum)
             ae (lambda (exp) #`(hash-table? #,exp)))
            
            (let ([mod-pat
                   (lambda (pat)
                     (syntax-case* pat (var) stx-equal?
                       [(var id) pat]
                       [(keypat valpat) (syntax/loc pat (list keypat valpat))]
                       [_ pat]))])
              (make-act
               'hash-table-pat
               ae
               (lambda (ks kf let-bound)
                 (lambda (sf bv)
                   (let ((hash-name (gensym 'hash)))
                     #`(let ((#,hash-name
                                (hash-table-map #,(subst-bindings ae
                                                                  let-bound)
                                                (lambda (k v) (list k v)))))
                         #,(next-outer #`(list-no-order #,@(syntax-map mod-pat #'(pats ...)))
                                       #`#,hash-name
                                       sf
                                       ;; these tests have to be true
                                       ;;(append (list
                                       ;;         '(pair? exp)
                                       ;;         '(pair? (cdr exp))
                                       ;;         '(null? (cdr (cdr exp))))
                                       ;;        sf)
                                       bv
                                       let-bound
                                       kf
                                       ks
				       cert)))))))))
        
        ((struct struct-name (fields ...))
         (identifier? (syntax struct-name))
         (let*-values ([(field-pats) (syntax->list (syntax (fields ...)))]
                       [(num-of-fields) (length field-pats)]
                       [(pred accessors mutators parental-chain)
                        (struct-pred-accessors-mutators (cert #'struct-name))]
                       ;; check that we have the right number of fields
                       [(dif) (- (length accessors) num-of-fields)])
           (unless (zero? dif)
             (match:syntax-err
              p
              (string-append
               (if (> dif 0) "not enough " "too many ")
               "fields for structure in pattern")))
           (cons
            (shape-test
             `(struct-pred ,(syntax-object->datum pred)
                           ,(map syntax-object->datum parental-chain)
                           ,ae-datum)
             ae (lambda (exp) #`(struct-pred #,pred #,parental-chain #,exp)))
            (apply
             append
             (map
              (lambda (cur-pat cur-mutator cur-accessor) 
                (syntax-case cur-pat (set! get!)
                  [(set! . rest)
                   (unless cur-mutator (match:syntax-err cur-pat "Cannot use set! pattern with immutable fields"))
                   (set/get-matcher 'set! ae p #'rest
                                    #`(lambda (y)
                                        (#,cur-mutator #,ae y)))]
                  [(get! . rest)
                   (set/get-matcher 'get! ae p #'rest
                                    #`(lambda ()
                                        (#,cur-accessor #,ae)))]
                  [_ (render-test-list 
                      cur-pat
                      (quasisyntax/loc cur-pat (#,cur-accessor #,ae))
                      cert
                      stx)]))
              field-pats mutators accessors)))))
        
        ;; syntax checking
        ((struct ident ...)
         (match:syntax-err
          p
          (if (zero? (length (syntax-e (syntax (ident ...)))))
              (format "~a~n~a~n~a"
                      "a structure pattern must have the name "
                      "of a defined structure followed by a list of patterns "
                      "to match each field of that structure")
              "syntax error in structure pattern")))
        ;; use a helper macro to match set/get patterns.  
        ;; we give it the whole rest so that it can do error-checking and reporting
        [(set! . rest)
         (set/get-matcher 'set! ae p let-bound (syntax rest)
                          (setter ae p let-bound))]
        [(get! . rest)
         (set/get-matcher 'get! ae p let-bound (syntax rest)
                          (getter ae p let-bound))]
        
        ;; list pattern with ooo or ook
        ((list pat dot-dot-k pat-rest ...)
         (and (not (or (memq (syntax-e (syntax pat))
                             '(unquote unquote-splicing ... ___))
                       (stx-dot-dot-k? (syntax pat))))
              (stx-dot-dot-k? (syntax dot-dot-k)))
         (list
          (shape-test
           `(list? ,ae-datum)
           ae (lambda (exp) #`(list? #,exp)))
          (make-act
           'list-ddk-pat
           ae
           (lambda (ks kf let-bound)
             (if (stx-null? (syntax (pat-rest ...)))
                 (handle-end-ddk-list ae kf ks
                                      (syntax pat)
                                      (syntax dot-dot-k)
                                      let-bound
                                      cert)
                 (handle-inner-ddk-list ae kf ks
                                        (syntax pat)
                                        (syntax dot-dot-k)
                                        (append-if-necc 'list
                                                        (syntax (pat-rest ...)))
                                        let-bound
                                        cert))))))
        
        ;; list-rest pattern with a ooo or ook pattern
        ((list-rest pat dot-dot-k pat-rest ...)
         (and (not (or (memq (syntax-e (syntax pat))
                             '(unquote unquote-splicing ... ___))
                       (stx-dot-dot-k? (syntax pat))
                       (stx-null? (syntax (pat-rest ...)))))
              (stx-dot-dot-k? (syntax dot-dot-k)))
         (list
          (shape-test
           `(pair? ,ae-datum)
           ae (lambda (exp) #`(pair? #,exp)))
          (make-act
           'list-ddk-pat
           ae
           (lambda (ks kf let-bound)
             (handle-inner-ddk-list
              ae kf ks
              (syntax pat)
              (syntax dot-dot-k)
              (if (= 1 (length
                        (syntax->list (syntax (pat-rest ...)))))
                  (stx-car (syntax (pat-rest ...)))
                  (append-if-necc  'list-rest
                                   (syntax (pat-rest ...))))
              let-bound
              cert)))))
        
        ;; list-rest pattern for improper lists
        ;; handle proper and improper lists      
        ((list-rest car-pat cdr-pat) ;pattern ;(pat1 pats ...)
         (not (or (memq (syntax-e (syntax car-pat))
                        '(unquote unquote-splicing))
                  (stx-dot-dot-k? (syntax car-pat))))
         (cons
          (shape-test
           `(pair? ,ae-datum)
           ae (lambda (exp) #`(pair? #,exp)))
          (append
           (render-test-list (syntax car-pat)
                             (quasisyntax/loc (syntax car-pat) (car #,ae))
                             cert
                             stx) ;(add-a e)
           (render-test-list
            (syntax cdr-pat)
            #`(cdr #,ae)
            cert
            stx))))
        
        ;; list-rest pattern
        ((list-rest car-pat cdr-pat ...) ;pattern ;(pat1 pats ...)
         (not (or (memq (syntax-e (syntax car-pat))
                        '(unquote unquote-splicing))
                  (stx-dot-dot-k? (syntax car-pat))))
         (cons
          (shape-test
           `(pair? ,ae-datum)
           ae (lambda (exp) #`(pair? #,exp)))
          (append
           (render-test-list (syntax car-pat)
                             #`(car #,ae)
                             cert
                             stx) ;(add-a e)
           (render-test-list
            (append-if-necc 'list-rest (syntax (cdr-pat ...)))
            #`(cdr #,ae)
            cert
            stx))))
        
        ;; general list pattern
        ((list car-pat cdr-pat ...) ;pattern ;(pat1 pats ...)
         (not (or (memq (syntax-e (syntax car-pat))
                        '(unquote unquote-splicing))
                  (stx-dot-dot-k? (syntax car-pat))))
         (cons
          (shape-test
           `(pair? ,ae-datum)
           ae (lambda (exp) #`(pair? #,exp)))
          (append
           (render-test-list (syntax car-pat)
                             #`(car #,ae)
                             cert
                             stx) ;(add-a e)
           (if (stx-null? (syntax (cdr-pat ...)))
               (list
                (shape-test
                 `(null? (cdr ,ae-datum))
                 ae (lambda (exp) #`(null? #,exp)) #`(cdr #,ae)))
               (render-test-list
                (append-if-necc 'list (syntax (cdr-pat ...)))
                #`(cdr #,ae)
                cert
                stx)))))
        
        ;; vector pattern with ooo or ook at end
        ((vector pats ...)
         (ddk-only-at-end-of-list? (syntax-e (syntax (pats ...))))
         (list
          (shape-test
           `(vector? ,ae-datum)
           ae (lambda (exp) #`(vector? #,exp)))
          (make-act
           'vec-ddk-pat
           ae
           (lambda (ks kf let-bound)
             (handle-ddk-vector ae kf ks
                                #'#(pats ...)
                                let-bound
                                cert)))))
        
        ;; vector pattern with ooo or ook, but not at end
        [(vector pats ...)
         (let* ((temp (syntax-e (syntax (pats ...))))
                (len (length temp)))
           (and (>= len 2)
                (ddk-in-list? temp)))
         ;; make this contains ddk with no ddks consecutive
         ;;(stx-dot-dot-k? (vector-ref temp (sub1 len))))))
         (list
          (shape-test
           `(vector? ,ae-datum)
           ae (lambda (exp) #`(vector? #,exp)))
          ;; we have to look at the first pattern and see if a ddk follows it
          ;; if so handle that case else handle the pattern
          (make-act
           'vec-ddk-pat
           ae
           (lambda (ks kf let-bound)
             (handle-ddk-vector-inner ae kf ks
                                      #'#(pats ...)
                                      let-bound
                                      cert))))]
        
        ;; plain old vector pattern
        [(vector pats ...)
         (let* ([syntax-vec (list->vector (syntax->list (syntax (pats ...))))]
                [vlen (vector-length syntax-vec)])
           (list*
            (shape-test
             `(vector? ,ae-datum) ae
             (lambda (exp) #`(vector? #,exp)))
            (shape-test
             `(equal? (vector-length ,ae-datum) ,vlen)
             ae (lambda (exp) #`(equal? (vector-length #,exp) #,vlen)))
            (let vloop ((n 0))
              (if (= n vlen)
                  '()
                  (append
                   (render-test-list
                    (vector-ref syntax-vec n)
                    #`(vector-ref #,ae #,n)
                    cert
                    stx)
                   (vloop (+ 1 n)))))))]
        
        [(box pat)
         (cons
          (shape-test
           `(box? ,ae-datum)
           ae (lambda (exp) #`(box? #,exp)))
          (render-test-list
           #'pat #`(unbox #,ae) cert stx))]
        
        ;; This pattern wasn't a valid form.
        [got-too-far
         (match:syntax-err
          #'got-too-far
          "syntax error in pattern")]))
    
    ;; end of render-test-list@
    )
  
  )