(load-relative "loadtest.rktl")

(require (for-syntax syntax/parse racket/syntax syntax/stx)
         syntax/id-set
         (prefix-in gen:set- racket/set))
  
(Section 'id-set)

(begin-for-syntax
  (define-syntax-rule (mk-set-op-ids #:prefix prefix-str #:names x ...) 
    (list (format-id #'x (string-append prefix-str "~a") #'x) ...)))
;; set ops whose names have the "set-" prefix
(define-for-syntax PREFIXED-SET-OPS
  (mk-set-op-ids
   #:prefix "set-"
   #:names 
     empty? member? count map for-each copy copy-clear >list >stream first
     rest add remove clear union intersect subtract symmetric-difference
     add! remove! clear! union! intersect! subtract! symmetric-difference!))
;; set ops whose names don't follow the "set-" prefix
(define-for-syntax OTHER-SET-OPS
  (mk-set-op-ids #:prefix "" #:names set=? subset? proper-subset?))

;; usage: (define-id-set-tests #:type type
;;                             #:interface intfc)
;; where: 
;;   type: the type of id-set, eg free or bound
;;   intfc: prefix to attach to set functions (eg gen:set or free-id or bound-id)
(define-syntax (define-id-set-tests stx)
  (syntax-parse stx
    [(_ #:type type #:interface intfc)
     #:with mk-immutable-id-set (format-id #'type "immutable-~a-id-set" #'type)
     #:with mk-mutable-id-set (format-id #'type "mutable-~a-id-set" #'type)
     #:with generic-id-set? (format-id #'type "~a-id-set?" #'type)
     #:with immutable-id-set? (format-id #'type "immutable-~a" #'generic-id-set?)
     #:with mutable-id-set? (format-id #'type "mutable-~a" #'generic-id-set?)
     #:with identifier=? (format-id #'type "~a-identifier=?" #'type)
     #:with id-set=? (format-id #'type "~a-id-set=?" #'type)
     #:with id-set-empty? (format-id #'type "~a-id-set-empty?" #'type)
     ;; ops that are parameterized in the tests use upcase convention
     ;; handle in-set specially
     #:with IN-SET (if (free-identifier=? #'gen:set #'intfc)
                       #'gen:set-in-set
                       (format-id #'intfc "in-~a-set" #'intfc))
     #:with set-ops (append PREFIXED-SET-OPS OTHER-SET-OPS)
     #:with set-op-names (stx-map
                          (compose
                           (λ (symb) (datum->syntax #'here symb))
                           string->symbol
                           string-upcase
                           symbol->string
                           syntax->datum)
                          #'set-ops)
     #:with (set-op-fn-name ...) 
            (stx-map (λ (f) (format-id f "~a-~a" #'intfc f)) #'set-ops)
     #'(let-values ([set-op-names (values set-op-fn-name ...)]) ;; define set fn names
         ;; -------------------------------------------------------------------
         ;; mutable/immutable combination Tests

         ;; defines tests for different combinations of compatible set types
         ;; (eg mutable and immutable id sets), using #:constructor as the 
         ;; "base" set type (ie the first argument to the set fns)
         ;; - this macro must be locally defined to capture appropriate set fn names
         (define-syntax (define-id-set-combo-tests stx)
           (syntax-parse stx
             [(_ #:constructor mk-id-set)
              #:with id-set? (format-id #'mk-id-set "~a?" #'mk-id-set)
              #'(begin
                  (define EMPTY (mk-id-set))
                  (define ABC (mk-id-set (list #'a #'b #'c)))
                  (define ABCD (mk-id-set (list #'a #'b #'c #'d)))
                  
                  (test #t generic-id-set? EMPTY)
                  (test #t generic-id-set? ABC)
                  (test #t generic-id-set? ABCD)
                  (test #t id-set? EMPTY)
                  (test #t id-set? ABC)
                  (test #t id-set? ABCD)

                  (test #t SET-EMPTY? EMPTY)
                  (test #f SET-EMPTY? ABC)
                  (test #f SET-EMPTY? ABCD)
                  
                  (test 0 SET-COUNT EMPTY)
                  (test 3 SET-COUNT ABC)
                  (test 4 SET-COUNT ABCD)
                  
                  (test #t SET-MEMBER? ABC #'a)
                  (test #t SET-MEMBER? ABC #'b)
                  (test #t SET-MEMBER? ABC #'c)
                  (test #f SET-MEMBER? ABC #'d)
                  (test #t SET-MEMBER? ABCD #'a)
                  (test #t SET-MEMBER? ABCD #'b)
                  (test #t SET-MEMBER? ABCD #'c)
                  (test #t SET-MEMBER? ABCD #'d)
                  (test #t SET-MEMBER? (mk-id-set (list #'x)) #'x)
                  (test #f SET-MEMBER? (let ([x 1]) (mk-id-set (list #'x))) #'x)
                  (test #f SET-MEMBER? (let ([x 1]) (mk-id-set (list #'x))) 
                           (let ([x 1]) #'x))
         
                  ;; explicit in-*-id-set sequence iterator
                  (test #t SET=? (mk-id-set (SET->LIST ABC))
                                 (mk-id-set (for/list ([v (IN-SET ABC)]) v)))
                  (test #t SET=? (mk-id-set (SET->LIST ABCD))
                        (let ([seq (IN-SET ABCD)])
                          (mk-id-set (for/list ([v seq]) v))))
                  (test #t sequence? (IN-SET ABCD))
                  (test #f stream? (IN-SET ABCD))
                  (test #t stream? (SET->STREAM ABCD))
                  (test #t sequence? (SET->STREAM ABCD)) 
                  (test #t SET=? 
                        (mk-id-set (SET->LIST ABCD))
                        (let ([seq (SET->STREAM ABCD)])
                          (mk-id-set (for/list ([v seq]) v))))
                  (test #t values 
                          (let ([noset #t])
                            (for ([v (IN-SET (mk-id-set))]) (set! noset #f))
                            noset))

                  ;; id-set used as implicit sequence
                  (test #t SET=? 
                        (mk-id-set (SET->LIST ABC))
                        (mk-id-set (for/list ([v ABC]) v)))
                  (test #t SET=?
                        (mk-id-set (SET->LIST ABCD))
                        (let ([seq ABCD]) (mk-id-set (for/list ([v seq]) v))))
                  (test #t sequence? ABCD)
                  (test #t values (let ([noset #t])
                                    (for ([v (mk-id-set)]) (set! noset #f))
                                    noset))

                  (test #t SET=? ABCD (SET-COPY ABCD))
                  (test #t eq? ABCD ABCD)
                  (test #f eq? ABCD (SET-COPY ABCD))
                  (test #t id-set-empty? (SET-COPY-CLEAR ABCD))
                  (test #f eq? EMPTY (SET-COPY-CLEAR ABCD))
                  (test #t id-set? (SET-COPY ABCD))
                  (test #t id-set? (SET-COPY-CLEAR ABCD))
         
                  ;; test gen:equal+hash
                  (test #t equal? 
                        (mk-id-set (SET->LIST ABC))
                        (mk-id-set (for/list ([v ABC]) v)))
                  (test #t equal? 
                        (mk-id-set (SET->LIST ABCD))
                        (let ([seq ABCD]) (mk-id-set (for/list ([v seq]) v))))
                  (test #t equal? ABCD (SET-COPY ABCD))
                  (test #t equal? (equal-hash-code ABC) (equal-hash-code ABC))
                  (test #t equal?
                        (equal-secondary-hash-code ABC)
                        (equal-secondary-hash-code ABC))
                  (test #t equal? 
                        (equal-hash-code (mk-id-set (SET->LIST ABC)))
                        (equal-hash-code (mk-id-set (for/list ([v ABC]) v))))
                  (test #t equal? 
                        (equal-hash-code (mk-id-set (SET->LIST ABCD)))
                        (equal-hash-code 
                         (let ([seq ABCD]) (mk-id-set (for/list ([v seq]) v)))))
                  (test #t equal? 
                        (equal-hash-code ABCD)
                        (equal-hash-code (SET-COPY ABCD)))
                  (test #t equal? 
                        (equal-secondary-hash-code (mk-id-set (SET->LIST ABC)))
                        (equal-secondary-hash-code
                         (mk-id-set (for/list ([v ABC]) v))))
                  (test #t equal? 
                        (equal-secondary-hash-code(mk-id-set (SET->LIST ABCD)))
                        (equal-secondary-hash-code
                         (let ([seq ABCD]) (mk-id-set (for/list ([v seq]) v)))))
                  (test #t equal? 
                        (equal-secondary-hash-code ABCD)
                        (equal-secondary-hash-code (SET-COPY ABCD)))
                  
                  ;; set union
                  (let ()
                    (define EMPTY/MUTABLE (mk-mutable-id-set null))
                    (define EMPTY/IMMUTABLE (mk-immutable-id-set null))
                    (define ABC-LIST (list #'a #'b #'c))
                    (define ABC/MUTABLE (mk-mutable-id-set ABC-LIST))
                    (define ABC/IMMUTABLE (mk-immutable-id-set ABC-LIST))
                    (test 3 SET-COUNT (SET-UNION ABC/IMMUTABLE))
                    (test #t SET-EMPTY? (SET-UNION EMPTY/IMMUTABLE))
                    (test 4 SET-COUNT (SET-UNION EMPTY/IMMUTABLE ABCD))
                    (test 4 SET-COUNT (SET-UNION ABC/IMMUTABLE ABCD))
                    (test 3 SET-COUNT (SET-UNION ABC/IMMUTABLE EMPTY))
                    (define IMMUTABLE/UNION/3 
                      (SET-UNION ABC/IMMUTABLE
                                 ABCD
                                 (mk-id-set (list #'d #'e #'f))))
                    (test 6 SET-COUNT IMMUTABLE/UNION/3)
                    (test #t SET-MEMBER? IMMUTABLE/UNION/3 #'d)
                    (test #t SET-MEMBER? IMMUTABLE/UNION/3 #'e)
                    (test #t SET-MEMBER? IMMUTABLE/UNION/3 #'f)
                    
                    (SET-UNION! ABC/MUTABLE)
                    (test 3 SET-COUNT ABC/MUTABLE)
                    (SET-UNION! EMPTY/MUTABLE)
                    (test #t SET-EMPTY? EMPTY/MUTABLE)
                    (SET-UNION! EMPTY/MUTABLE ABCD)
                    (test 4 SET-COUNT EMPTY/MUTABLE)
                    (SET-UNION! ABC/MUTABLE ABCD)
                    (test 4 SET-COUNT ABC/MUTABLE)
                    (SET-UNION! ABC/MUTABLE EMPTY)
                    (test 4 SET-COUNT ABC/MUTABLE)
                    (define MUTABLE/UNION/3 (mk-mutable-id-set (list #'a #'b #'c)))
                    (SET-UNION! MUTABLE/UNION/3
                                ABCD
                                (mk-id-set (list #'d #'e #'f)))
                    (test 6 SET-COUNT MUTABLE/UNION/3)
                    (test #t SET-MEMBER? MUTABLE/UNION/3 #'d)
                    (test #t SET-MEMBER? MUTABLE/UNION/3 #'e)
                    (test #t SET-MEMBER? MUTABLE/UNION/3 #'f)
                    (void))
         
                  ;; set intersect
                  (let ()
                    (define EMPTY/MUTABLE (mk-mutable-id-set null))
                    (define EMPTY/IMMUTABLE (mk-immutable-id-set null))
                    (define ABC-LIST (list #'a #'b #'c))
                    (define ABC/MUTABLE (mk-mutable-id-set ABC-LIST))
                    (define ABC/IMMUTABLE (mk-immutable-id-set ABC-LIST))
                    (test 3 SET-COUNT (SET-INTERSECT ABC/IMMUTABLE))
                    (test #t SET-EMPTY? (SET-INTERSECT EMPTY/IMMUTABLE))
                    (test 0 SET-COUNT (SET-INTERSECT EMPTY/IMMUTABLE ABCD))
                    (test 3 SET-COUNT (SET-INTERSECT ABC/IMMUTABLE ABCD))
                    (test 0 SET-COUNT (SET-INTERSECT ABC/IMMUTABLE EMPTY))
                    (define IMMUTABLE/INTERSECT/3 
                      (SET-INTERSECT ABC/IMMUTABLE
                                     ABCD
                                     (mk-id-set (list #'b #'c))))
                    (test 2 SET-COUNT IMMUTABLE/INTERSECT/3)
                    (test #f SET-MEMBER? IMMUTABLE/INTERSECT/3 #'a)
                    (test #t SET-MEMBER? IMMUTABLE/INTERSECT/3 #'b)
                    (test #t SET-MEMBER? IMMUTABLE/INTERSECT/3 #'c)
                    
                    (SET-INTERSECT! ABC/MUTABLE)
                    (test 3 SET-COUNT ABC/MUTABLE)
                    (SET-INTERSECT! EMPTY/MUTABLE)
                    (test #t SET-EMPTY? EMPTY/MUTABLE)
                    (SET-INTERSECT! EMPTY/MUTABLE ABCD)
                    (test 0 SET-COUNT EMPTY/MUTABLE)
                    (SET-INTERSECT! ABC/MUTABLE ABCD)
                    (test 3 SET-COUNT ABC/MUTABLE)
                    (test #t SET-MEMBER? ABC/MUTABLE #'a)
                    (test #t SET-MEMBER? ABC/MUTABLE #'b)
                    (test #t SET-MEMBER? ABC/MUTABLE #'c)
                    (test #f SET-MEMBER? ABC/MUTABLE #'d)
                    (test #t mutable-id-set? ABC/MUTABLE)
                    (test #t SET-EMPTY? EMPTY)
                    (SET-INTERSECT! ABC/MUTABLE EMPTY)
                    (test 0 SET-COUNT ABC/MUTABLE)
                    (define MUTABLE/INTERSECT/3 (mk-mutable-id-set (list #'a #'b #'c)))
                    (SET-INTERSECT! MUTABLE/INTERSECT/3
                                    ABCD
                                    (mk-id-set (list #'a #'b)))
                    (test 2 SET-COUNT MUTABLE/INTERSECT/3)
                    (test #t SET-MEMBER? MUTABLE/INTERSECT/3 #'a)
                    (test #t SET-MEMBER? MUTABLE/INTERSECT/3 #'b)
                    (test #f SET-MEMBER? MUTABLE/INTERSECT/3 #'c)
                    
                    (void))
         
                  ;; set subtract
                  (let ()
                    (define EMPTY/MUTABLE (mk-mutable-id-set null))
                    (define EMPTY/IMMUTABLE (mk-immutable-id-set null))
                    (define ABCDE-LIST (list #'a #'b #'c #'d #'e))
                    (define ABCDE/MUTABLE (mk-mutable-id-set ABCDE-LIST))
                    (define ABCDE/IMMUTABLE (mk-immutable-id-set ABCDE-LIST))
                    (test 5 SET-COUNT (SET-SUBTRACT ABCDE/IMMUTABLE))
                    (test #t SET-EMPTY? (SET-SUBTRACT EMPTY/IMMUTABLE))
                    (test 0 SET-COUNT (SET-SUBTRACT EMPTY/IMMUTABLE ABCD))
                    (test 1 SET-COUNT (SET-SUBTRACT ABCDE/IMMUTABLE ABCD))
                    (test 5 SET-COUNT (SET-SUBTRACT ABCDE/IMMUTABLE EMPTY))
                    (define IMMUTABLE/SUBTRACT/3
                      (SET-SUBTRACT ABCDE/IMMUTABLE
                                    ABC
                                    (mk-id-set (list #'a #'b #'e))))
                    (test 1 SET-COUNT IMMUTABLE/SUBTRACT/3)
                    (test #f SET-MEMBER? IMMUTABLE/SUBTRACT/3 #'a)
                    (test #f SET-MEMBER? IMMUTABLE/SUBTRACT/3 #'b)
                    (test #t SET-MEMBER? IMMUTABLE/SUBTRACT/3 #'d)
                    (test #f SET-MEMBER? IMMUTABLE/SUBTRACT/3 #'e)
                    
                    (SET-SUBTRACT! ABCDE/MUTABLE)
                    (test 5 SET-COUNT ABCDE/MUTABLE)
                    (SET-SUBTRACT! EMPTY/MUTABLE)
                    (test #t SET-EMPTY? EMPTY/MUTABLE)
                    (SET-SUBTRACT! EMPTY/MUTABLE ABCD)
                    (test 0 SET-COUNT EMPTY/MUTABLE)
                    (SET-SUBTRACT! ABCDE/MUTABLE ABC)
                    (test 2 SET-COUNT ABCDE/MUTABLE)
                    (test #f SET-MEMBER? ABCDE/MUTABLE #'a)
                    (test #f SET-MEMBER? ABCDE/MUTABLE #'b)
                    (test #f SET-MEMBER? ABCDE/MUTABLE #'c)
                    (test #t SET-MEMBER? ABCDE/MUTABLE #'d)
                    (test #t SET-MEMBER? ABCDE/MUTABLE #'e)
                    (test #t mutable-id-set? ABCDE/MUTABLE)
                    (test #t SET-EMPTY? EMPTY)
                    (set! ABCDE/MUTABLE (mk-mutable-id-set ABCDE-LIST))
                    (SET-SUBTRACT! ABCDE/MUTABLE EMPTY)
                    (test 5 SET-COUNT ABCDE/MUTABLE)
                    (define MUTABLE/SUBTRACT/3
                      (mk-mutable-id-set (list #'a #'b #'c #'d #'e)))
                    (SET-SUBTRACT! MUTABLE/SUBTRACT/3
                                   ABC
                                   (mk-id-set (list #'a #'b #'e)))
                    (test 1 SET-COUNT MUTABLE/SUBTRACT/3)
                    (test #f SET-MEMBER? MUTABLE/SUBTRACT/3 #'a)
                    (test #f SET-MEMBER? MUTABLE/SUBTRACT/3 #'b)
                    (test #t SET-MEMBER? MUTABLE/SUBTRACT/3 #'d)
                    (test #f SET-MEMBER? MUTABLE/SUBTRACT/3 #'e)
                    
                    (void))
         
                  ;; set symmetric difference
                  (let ()
                    (define EMPTY/MUTABLE (mk-mutable-id-set null))
                    (define EMPTY/IMMUTABLE (mk-immutable-id-set null))
                    (define ABCDE-LIST (list #'a #'b #'c #'d #'e))
                    (define ABCDE/MUTABLE (mk-mutable-id-set ABCDE-LIST))
                    (define ABCDE/IMMUTABLE (mk-immutable-id-set ABCDE-LIST))
                    
                    (test 5 SET-COUNT (SET-SYMMETRIC-DIFFERENCE ABCDE/IMMUTABLE))
                    (test #t SET-EMPTY? (SET-SYMMETRIC-DIFFERENCE EMPTY/IMMUTABLE))
                    (test 4 SET-COUNT (SET-SYMMETRIC-DIFFERENCE EMPTY/IMMUTABLE ABCD))
                    (test #t SET=? 
                          (SET-SYMMETRIC-DIFFERENCE EMPTY/IMMUTABLE ABCD) ABCD)
                    (test 1 SET-COUNT (SET-SYMMETRIC-DIFFERENCE ABCDE/IMMUTABLE ABCD))
                    (test 5 SET-COUNT (SET-SYMMETRIC-DIFFERENCE ABCDE/IMMUTABLE EMPTY))
                    (define IMMUTABLE/DIFFERENCE/3
                      (SET-SYMMETRIC-DIFFERENCE ABCDE/IMMUTABLE
                                                ABC
                                                (mk-id-set (list #'a #'b #'e))))
                    (test 3 SET-COUNT IMMUTABLE/DIFFERENCE/3)
                    (test #t SET-MEMBER? IMMUTABLE/DIFFERENCE/3 #'a)
                    (test #t SET-MEMBER? IMMUTABLE/DIFFERENCE/3 #'b)
                    (test #f SET-MEMBER? IMMUTABLE/DIFFERENCE/3 #'c)
                    (test #t SET-MEMBER? IMMUTABLE/DIFFERENCE/3 #'d)
                    (test #f SET-MEMBER? IMMUTABLE/DIFFERENCE/3 #'e)
                    
                    (SET-SYMMETRIC-DIFFERENCE! ABCDE/MUTABLE)
                    (test 5 SET-COUNT ABCDE/MUTABLE)
                    (SET-SYMMETRIC-DIFFERENCE! EMPTY/MUTABLE)
                    (test #t SET-EMPTY? EMPTY/MUTABLE)
                    (SET-SYMMETRIC-DIFFERENCE! EMPTY/MUTABLE ABCD)
                    (test 4 SET-COUNT EMPTY/MUTABLE)
                    (test #t SET=? EMPTY/MUTABLE ABCD)
                    (SET-SYMMETRIC-DIFFERENCE! ABCDE/MUTABLE ABC)
                    (test 2 SET-COUNT ABCDE/MUTABLE)
                    (test #f SET-MEMBER? ABCDE/MUTABLE #'a)
                    (test #f SET-MEMBER? ABCDE/MUTABLE #'b)
                    (test #f SET-MEMBER? ABCDE/MUTABLE #'c)
                    (test #t SET-MEMBER? ABCDE/MUTABLE #'d)
                    (test #t SET-MEMBER? ABCDE/MUTABLE #'e)
                    (test #t mutable-id-set? ABCDE/MUTABLE)
                    (test #t SET-EMPTY? EMPTY)
                    (set! ABCDE/MUTABLE (mk-mutable-id-set ABCDE-LIST))
                    (SET-SYMMETRIC-DIFFERENCE! ABCDE/MUTABLE EMPTY)
                    (test 5 SET-COUNT ABCDE/MUTABLE)
                    (define MUTABLE/DIFFERENCE/3 (mk-mutable-id-set (list #'a #'b #'c #'d #'e)))
                    (SET-SYMMETRIC-DIFFERENCE! MUTABLE/DIFFERENCE/3
                                               ABC
                                               (mk-id-set (list #'a #'b #'e)))
                    (test 3 SET-COUNT MUTABLE/DIFFERENCE/3)
                    (test #t SET-MEMBER? MUTABLE/DIFFERENCE/3 #'a)
                    (test #t SET-MEMBER? MUTABLE/DIFFERENCE/3 #'b)
                    (test #f SET-MEMBER? MUTABLE/DIFFERENCE/3 #'c)
                    (test #t SET-MEMBER? MUTABLE/DIFFERENCE/3 #'d)
                    (test #f SET-MEMBER? MUTABLE/DIFFERENCE/3 #'e)
                    
                    (void))
         
                  ;; Test subset:
                  (let ()
                    (test #t SUBSET? EMPTY EMPTY)
                    (test #t SUBSET? ABC ABC)
                    (test #t SUBSET? ABCD ABCD)
                    (test #f PROPER-SUBSET? EMPTY EMPTY)
                    (test #f PROPER-SUBSET? ABC ABC)
                    (test #f PROPER-SUBSET? ABCD ABCD)
                    (test #t SUBSET? EMPTY ABC)
                    (test #t SUBSET? EMPTY ABCD)
                    (test #t SUBSET? ABC ABCD)
                    (test #f SUBSET? ABCD ABC)
                    (test #f SUBSET? ABCD EMPTY)
                    (test #f SUBSET? ABC EMPTY)
                    (test #t PROPER-SUBSET? EMPTY ABC)
                    (test #t PROPER-SUBSET? EMPTY ABCD)
                    (test #t PROPER-SUBSET? ABC ABCD)
                    (test #f PROPER-SUBSET? ABCD ABC)
                    (test #f PROPER-SUBSET? ABCD EMPTY)
                    (test #f PROPER-SUBSET? ABC EMPTY)
                    
                    (define EMPTY/MUTABLE (mk-mutable-id-set null))
                    (define EMPTY/IMMUTABLE (mk-immutable-id-set null))
                    (define AB-LIST (list #'a #'b))
                    (define ABC-LIST (list #'a #'b #'c))
                    (define ABCDE-LIST (list #'a #'b #'c #'d #'e))
                    (define AB/MUTABLE (mk-mutable-id-set AB-LIST))
                    (define AB/IMMUTABLE (mk-immutable-id-set AB-LIST))
                    (define ABC/MUTABLE (mk-mutable-id-set ABC-LIST))
                    (define ABC/IMMUTABLE (mk-immutable-id-set ABC-LIST))
                    (define ABCDE/MUTABLE (mk-mutable-id-set ABCDE-LIST))
                    (define ABCDE/IMMUTABLE (mk-immutable-id-set ABCDE-LIST))

                    (test #t SUBSET? EMPTY EMPTY/MUTABLE)
                    (test #t SUBSET? EMPTY EMPTY/IMMUTABLE)
                    (test #f PROPER-SUBSET? EMPTY EMPTY/MUTABLE)
                    (test #f PROPER-SUBSET? EMPTY EMPTY/IMMUTABLE)
                    
                    (test #t SUBSET? ABC ABCDE/MUTABLE)
                    (test #t SUBSET? ABC ABCDE/MUTABLE)
                    (test #t PROPER-SUBSET? ABC ABCDE/MUTABLE)
                    (test #t PROPER-SUBSET? ABC ABCDE/MUTABLE)
                    (test #t SUBSET? ABC ABCDE/IMMUTABLE)
                    (test #t SUBSET? ABC ABCDE/IMMUTABLE)
                    (test #t PROPER-SUBSET? ABC ABCDE/IMMUTABLE)
                    (test #t PROPER-SUBSET? ABC ABCDE/IMMUTABLE)
                    
                    (test #t SUBSET? ABC ABC/MUTABLE)
                    (test #t SUBSET? ABC ABC/MUTABLE)
                    (test #f PROPER-SUBSET? ABC ABC/MUTABLE)
                    (test #f PROPER-SUBSET? ABC ABC/MUTABLE)
                    (test #t SUBSET? ABC ABC/IMMUTABLE)
                    (test #t SUBSET? ABC ABC/IMMUTABLE)
                    (test #f PROPER-SUBSET? ABC ABC/IMMUTABLE)
                    (test #f PROPER-SUBSET? ABC ABC/IMMUTABLE)

                    (test #f SUBSET? ABC AB/MUTABLE)
                    (test #f SUBSET? ABC AB/MUTABLE)
                    (test #f PROPER-SUBSET? ABC AB/MUTABLE)
                    (test #f PROPER-SUBSET? ABC AB/MUTABLE)
                    (test #f SUBSET? ABC AB/IMMUTABLE)
                    (test #f SUBSET? ABC AB/IMMUTABLE)
                    (test #f PROPER-SUBSET? ABC AB/IMMUTABLE)
                    (test #f PROPER-SUBSET? ABC AB/IMMUTABLE)
                    
                    (void))
         
                  ;; id-set and id-set-for-each
                  (test #t null? (SET-MAP EMPTY (λ (x) x)))
                  (test #t SET=? ABC (mk-id-set (SET-MAP ABC (λ (x) x))))
                  (test #t SET=? ABCD (mk-id-set (SET-MAP ABCD (λ (x) x))))
                  (test #t SET=? 
                        ABC 
                        (mk-id-set 
                         ;; drop #'d
                         (SET-MAP ABCD (λ (id) (if (identifier=? #'d id) #'a id)))))
                  (let ([new-set (mk-mutable-id-set null)])
                    (SET-FOR-EACH ABC (λ (id) (SET-ADD! new-set id)))
                    (test #t SET=? ABC new-set))
                  (let ([new-set (mk-immutable-id-set null)])
                    (SET-FOR-EACH 
                     ABCD
                     (λ (id) (set! new-set (SET-ADD new-set id))))
                    (test #t SET=? ABCD new-set)))]))
         
         ;; invoke macro to define immutable-mutable combo tests
         (define-id-set-combo-tests #:constructor mk-immutable-id-set)
         (define-id-set-combo-tests #:constructor mk-mutable-id-set)
         
         ;; ----------------------------------------------------------------------------
         ;; immutable-only/mutable-only id set tests
         (define EMPTY/MUTABLE (mk-mutable-id-set))
         (define NONEMPTY/MUTABLE (mk-mutable-id-set (list #'a #'b #'c)))
         (define EMPTY/IMMUTABLE (mk-immutable-id-set))
         (define NONEMPTY/IMMUTABLE (mk-immutable-id-set (list #'a #'b #'c)))

         (test #t mutable-id-set? EMPTY/MUTABLE)
         (test #f mutable-id-set? EMPTY/IMMUTABLE)
         (test #t mutable-id-set? NONEMPTY/MUTABLE)
         (test #f mutable-id-set? NONEMPTY/IMMUTABLE)
         (test #f immutable-id-set? EMPTY/MUTABLE)
         (test #t immutable-id-set? EMPTY/IMMUTABLE)
         (test #f immutable-id-set? NONEMPTY/MUTABLE)
         (test #t immutable-id-set? NONEMPTY/IMMUTABLE)
         
         (test #t SET=? EMPTY/MUTABLE EMPTY/IMMUTABLE)
         (test #t SET=? NONEMPTY/MUTABLE NONEMPTY/IMMUTABLE)
         (test #f SET=? EMPTY/MUTABLE NONEMPTY/MUTABLE)
         (test #f SET=? EMPTY/IMMUTABLE NONEMPTY/IMMUTABLE)
         (test #f SET=? EMPTY/MUTABLE NONEMPTY/IMMUTABLE)

         ;; -------------------------------------------------------------------
         ;; immutable-only id set tests

         (let ([s (mk-immutable-id-set (list #'a #'b #'c))])
           ;; fns not implemented for immutable id sets
           (err/rt-test (SET-ADD! s #'z) exn:fail?)
           (err/rt-test (SET-REMOVE! s #'z) exn:fail?)
           (err/rt-test (SET-CLEAR! s) exn:fail?)
           (err/rt-test (SET-UNION! s) exn:fail?)
           (err/rt-test (SET-INTERSECTION! s) exn:fail?)
           (err/rt-test (SET-SUBTRACT! s) exn:fail?)
           (err/rt-test (SET-SYMMETRIC-DIFFERENCE! s) exn:fail?)

           (test #t SET=? 
                 s 
                 (SET-ADD 
                  (SET-ADD 
                   (SET-ADD (mk-immutable-id-set) #'b) #'a) #'c))
           (test #f SET=?
                 s
                 (let ([a 1])
                   (SET-ADD 
                    (SET-ADD 
                     (SET-ADD (mk-immutable-id-set) #'b) #'a) #'c)))

           (test #t SET-MEMBER? (SET-ADD s #'d) #'c)
           (test #t SET-MEMBER? (SET-ADD s #'d) #'d)
           (test #f SET-MEMBER? (SET-ADD s #'d) #'e)
           (test #f SET-MEMBER? (SET-ADD s (let ([d 1]) #'d)) #'d)
           (test #f SET-MEMBER? (SET-ADD s #'d) (let ([d 1]) #'d))
           
           (test #t SET-MEMBER? (SET-REMOVE s #'a) #'b)
           (test #f SET-MEMBER? (SET-REMOVE s #'b) #'b)
           (test #t SET-MEMBER? (SET-REMOVE s (let ([c 1]) #'c)) #'c)
           
           (test #t identifier=? (SET-FIRST s) (SET-FIRST s))
           (test #t SET=? (SET-REMOVE s (SET-FIRST s)) 
                 (SET-REST s))
           
           ;; tests for gen:stream interface
           (test #t stream? s)
           (test #t free-identifier=? (stream-first s) (stream-first s))
           (test #t SET=? (SET-REMOVE s (stream-first s)) 
                 (stream-rest s))
           (test #t stream-empty? EMPTY/IMMUTABLE)
           (test #f stream-empty? NONEMPTY/IMMUTABLE)
  
           (void))

         ;; -------------------------------------------------------------------
         ;; mutable-only id set tests

         (let ([ms1 (mk-mutable-id-set (list #'a #'b #'c))]
               [ms2 (mk-mutable-id-set)])
           ;; fns not implemented for mutable id sets
           (err/rt-test (SET-ADD ms1 #'z) exn:fail?)
           (err/rt-test (SET-REMOVE ms1 #'z) exn:fail?)
           (err/rt-test (SET-REST ms1) exn:fail?)
           (err/rt-test (SET-CLEAR ms1) exn:fail?)
           (err/rt-test (SET-UNION ms1) exn:fail?)
           (err/rt-test (SET-INTERSECTION ms1) exn:fail?)
           (err/rt-test (SET-SUBTRACT ms1) exn:fail?)
           (err/rt-test (SET-SYMMETRIC-DIFFERENCE ms1) exn:fail?)
           
           ;; mutable sets are not streams
           (test #f stream? ms1)
           (err/rt-test (stream-empty? ms1) exn:fail?)
           (err/rt-test (stream-first ms1) exn:fail?)
           (err/rt-test (stream-rest ms1) exn:fail?)

           (SET-ADD! ms2 #'b)
           (SET-ADD! ms2 #'a)
           (SET-ADD! ms2 #'c)
           (test #t SET=? ms1 ms2)
           (define ms3 (mk-mutable-id-set))
           (let ([a 1])
             (SET-ADD! ms3 #'b)
             (SET-ADD! ms3 #'a)
             (SET-ADD! ms3 #'c))
           (test #f SET=? ms1 ms3)

           (define ms4 (mk-mutable-id-set (list #'a #'b #'c)))
           (test #t SET-MEMBER? ms4 #'c)
           (SET-ADD! ms4 #'d)
           (test #t SET-MEMBER? ms4 #'c)
           (test #t SET-MEMBER? ms4 #'d)
           (SET-ADD! ms4 #'d)
           (test #t SET-MEMBER? ms4 #'d)
           (test #f SET-MEMBER? ms4 #'e)
           (SET-ADD! ms4 (let ([e 1]) #'e))
           (test #f SET-MEMBER? ms4 #'e)
           (test #f SET-MEMBER? ms4 (let ([d 1]) #'d))
  
           (SET-REMOVE! ms4 #'a)
           (test #t SET-MEMBER? ms4 #'b)
           (SET-REMOVE! ms4 #'b)
           (test #f SET-MEMBER? ms4 #'b)
           (SET-REMOVE! ms4 (let ([c 1]) #'c))
           (test #t SET-MEMBER? ms4 #'c)
           
           (test #t free-identifier=? (SET-FIRST ms1) (SET-FIRST ms1))
           (SET-REMOVE! ms1 #'a)
           (test #t SET=? ms1 (mk-mutable-id-set (list #'b #'c)))
           
           (SET-CLEAR! ms1)
           (test #t SET-EMPTY? ms1)
  
           (void))

         )]))

;; contract tests -------------------------------------------------------------
(test #t contract? (id-set/c identifier?))
(test #t contract? (id-set/c identifier? #:idsettype 'free))
(test #t contract? (id-set/c identifier? #:idsettype 'bound 
                                         #:mutability 'mutable))
(test #t contract? (free-id-set/c (λ (id) (free-identifier=? #'a id))))
(test #t contract? (free-id-set/c (λ (id) (free-identifier=? #'a id)) 
                                  #:mutability 'immutable))
(test #t contract? (bound-id-set/c (λ (id) (bound-identifier=? #'b id))))
(test #t contract? (bound-id-set/c (λ (id) (bound-identifier=? #'b id)) 
                                  #:mutability 'mutable))

(test #t chaperone-contract? (id-set/c identifier?))
(test #f impersonator-contract? (id-set/c identifier?))
(test #t flat-contract? (id-set/c identifier?))
(test #f flat-contract? (free-id-set/c identifier? #:mutability 'mutable))
(test #t flat-contract? (bound-id-set/c identifier? #:mutability 'immutable))

(let ()
  
  ;; - these contract testing util fns are copied from id-table-test
  ;; TODO: move them into a separate file
  (define (app-ctc ctc value)
    (contract ctc value 'positive 'negative))
  
  (define (positive-error? exn)
    (and exn:fail:contract?
         (regexp-match? "blaming: positive" (exn-message exn))))
  (define (negative-error? exn)
    (and exn:fail:contract?
         (regexp-match? "blaming: negative" (exn-message exn))))
  
  (define-syntax-rule (test/blame-pos e)
    (thunk-error-test (lambda () e) #'e positive-error?))
  (define-syntax-rule (test/blame-neg e)
    (thunk-error-test (lambda () e) #'e negative-error?))
  
  (define EMPTY/FREE/MUTABLE (mutable-free-id-set null))
  (define EMPTY/BOUND/MUTABLE (mutable-bound-id-set null))
  (define EMPTY/BOUND/IMMUTABLE (immutable-bound-id-set null))
  (test/blame-pos 
   (app-ctc (id-set/c identifier? #:idsettype 'bound #:mutability 'mutable)
            EMPTY/FREE/MUTABLE))
  (test/blame-pos (app-ctc (id-set/c identifier? #:idsettype 'free)
                           EMPTY/FREE/MUTABLE)) ; default is immutable
  (test/blame-pos (app-ctc (id-set/c any/c #:mutability 'immutable)
                           EMPTY/FREE/MUTABLE))
  (test/blame-pos (app-ctc (free-id-set/c any/c #:mutability 'immutable)
                           EMPTY/FREE/MUTABLE))
  (test/blame-pos (app-ctc (bound-id-set/c any/c #:mutability 'mutable)
                           EMPTY/FREE/MUTABLE))
  (test/blame-pos (app-ctc (bound-id-set/c any/c)
                           EMPTY/BOUND/MUTABLE)) ; default is immutable
  (test/blame-pos (app-ctc (id-set/c identifier? #:idsettype 'free)
                           EMPTY/BOUND/IMMUTABLE))
  (test/blame-pos (app-ctc (id-set/c any/c #:mutability 'mutable)
                           EMPTY/BOUND/IMMUTABLE))
  (test/blame-pos (app-ctc (bound-id-set/c any/c #:mutability 'mutable)
                           EMPTY/BOUND/IMMUTABLE))
  (test/blame-pos (app-ctc (free-id-set/c any/c) EMPTY/BOUND/IMMUTABLE))
  
  (define (not-free-a? id) (not (free-identifier=? id #'a)))
  (define (not-bound-b? id) (not (bound-identifier=? id #'b)))
  (define ABC/FREE (immutable-free-id-set (list #'a #'b #'c)))
  (define ABC/BOUND (immutable-bound-id-set (list #'a #'b #'c)))
  (test/blame-pos (app-ctc (free-id-set/c not-free-a?) ABC/FREE))
  (test/blame-pos (app-ctc (bound-id-set/c not-bound-b?) ABC/BOUND))
  (define EMPTY/BOUND/CTC 
    (app-ctc (bound-id-set/c not-bound-b? #:mutability 'mutable) 
             (mutable-bound-id-set null)))
  (define EMPTY/FREE/CTC 
    (app-ctc (free-id-set/c not-free-a? #:mutability 'mutable) 
             (mutable-free-id-set null)))
  (bound-id-set-add! EMPTY/BOUND/CTC #'a)
  (free-id-set-add! EMPTY/FREE/CTC #'b)
  (test/blame-neg (bound-id-set-add! EMPTY/BOUND/CTC #'b))
  (test/blame-neg (free-id-set-add! EMPTY/FREE/CTC #'a))
  (test/blame-neg (bound-id-set-union! EMPTY/BOUND/CTC (mutable-bound-id-set (list #'b))))
  (test/blame-neg (bound-id-set-union! EMPTY/BOUND/CTC (immutable-bound-id-set (list #'b))))
  (test/blame-neg (free-id-set-union! EMPTY/FREE/CTC (mutable-free-id-set (list #'a))))
  (test/blame-neg (free-id-set-union! EMPTY/FREE/CTC (immutable-free-id-set (list #'a))))
  (test/blame-neg (bound-id-set-symmetric-difference!
                   EMPTY/BOUND/CTC (mutable-bound-id-set (list #'b))))
  (test/blame-neg (bound-id-set-symmetric-difference!
                   EMPTY/BOUND/CTC (immutable-bound-id-set (list #'b))))
  (test/blame-neg (free-id-set-symmetric-difference!
                   EMPTY/FREE/CTC (mutable-free-id-set (list #'a))))
  (test/blame-neg (free-id-set-symmetric-difference!
                   EMPTY/FREE/CTC (immutable-free-id-set (list #'a))))
  )



;; ----------------------------------------------------------------------------
;; run test suite instances
(define-id-set-tests #:type free #:interface gen:set)
(define-id-set-tests #:type free #:interface free-id)
(define-id-set-tests #:type bound #:interface gen:set)
(define-id-set-tests #:type bound #:interface bound-id)

(report-errs)
