(load-relative "loadtest.rktl")

(require (for-syntax syntax/parse racket/syntax syntax/stx)
         syntax/id-set
         (prefix-in gen:set- racket/set))
  
(Section 'id-set)

(begin-for-syntax
  (define-syntax-rule (mk-set-ops-stx #:prefix prefix-str #:names x ...) 
    (list (format-id #'here (string-append prefix-str "~a") (syntax x)) ...)))
;; set ops whose names have the "set-" prefix
(define-for-syntax PREFIXED-SET-OPS
  (mk-set-ops-stx 
   #:prefix "set-"
   #:names 
     empty? member? count map for-each copy copy-clear >list >stream first
     rest add remove clear union intersect subtract symmetric-difference
     add! remove! clear! union! intersect! subtract! symmetric-difference!))
;; set ops whose names don't follow the "set-" prefix
(define-for-syntax OTHER-SET-OPS
  (mk-set-ops-stx #:prefix "" #:names set=? subset? proper-subset?))

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
     ;; handle in-set specially
     #:with IN-SET (if (free-identifier=? #'gen:set #'intfc)
                       #'gen:set-in-set
                       (format-id #'intfc "in-~a-set" #'intfc))
     #:with set-ops (append PREFIXED-SET-OPS OTHER-SET-OPS)
     ;; ops that are parameterized in the tests use upcase convention
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

;; ----------------------------------------------------------------------------
;; run test suite instances
(define-id-set-tests #:type free #:interface gen:set)
(define-id-set-tests #:type free #:interface free-id)


;(test #t generic-set? (set))
;(test #t set-empty? (set))
;(test #t generic-set? (set 1 2 3))
;(test #f set-empty? (set 1 2 3))
;(test #t generic-set? (seteq))
;(test #t set-empty? (seteq))
;(test #t generic-set? (seteq 1 2 3))
;(test #f set-empty? (seteq 1 2 3))
;(test #t generic-set? (seteqv))
;(test #t set-empty? (seteqv))
;(test #t generic-set? (seteqv 1 2 3))
;(test #f set-empty? (seteqv 1 2 3))
;(test #t generic-set? (mutable-set))
;(test #t set-empty? (mutable-set))
;(test #t generic-set? (mutable-set 1 2 3))
;(test #f set-empty? (mutable-set 1 2 3))
;(test #t generic-set? (mutable-seteq))
;(test #t set-empty? (mutable-seteq))
;(test #t generic-set? (mutable-seteq 1 2 3))
;(test #f set-empty? (mutable-seteq 1 2 3))
;(test #t generic-set? (mutable-seteqv))
;(test #t set-empty? (mutable-seteqv))
;(test #t generic-set? (mutable-seteqv 1 2 3))
;(test #f set-empty? (mutable-seteqv 1 2 3))
;(test #t generic-set? (list))
;(test #t set-empty? (list))
;(test #t generic-set? (list 1 2 3))
;(test #f set-empty? (list 1 2 3))
;
;(test #f set-eq? (set 1 2 3))
;(test #f set-eqv? (set 1 2 3))
;(test #t set-equal? (set 1 2 3))
;(test #t set-eq? (seteq 1 2 3))
;(test #f set-eqv? (seteq 1 2 3))
;(test #f set-equal? (seteq 1 2 3))
;(test #f set-eq? (seteqv 1 2 3))
;(test #t set-eqv? (seteqv 1 2 3))
;(test #f set-equal? (seteqv 1 2 3))
;(test #f set-eq? (mutable-set 1 2 3))
;(test #f set-eqv? (mutable-set 1 2 3))
;(test #t set-equal? (mutable-set 1 2 3))
;(test #t set-eq? (mutable-seteq 1 2 3))
;(test #f set-eqv? (mutable-seteq 1 2 3))
;(test #f set-equal? (mutable-seteq 1 2 3))
;(test #f set-eq? (mutable-seteqv 1 2 3))
;(test #t set-eqv? (mutable-seteqv 1 2 3));  (test s set-intersect s)
;  (test (set 3) set-intersect s (set 5 4 3 6))
;  (test (set 3) set-intersect (set 5 4 3 6) s)
;  (test (seteq 3) set-intersect (seteq 5 4 3 6) (seteq 1 2 3))
;  (test (seteqv 3) set-intersect (seteqv 5 4 3 6) (seteqv 1 2 3))
;  (test (set 3 2) set-intersect s (set 5 2 3))
;  (test (seteq 3 2) set-intersect (seteq 1 2 3) (seteq 5 2 3))
;  (test (set 2) set-intersect s (set 5 2 3) (set 2 20 200))
;  (test (seteq 2) set-intersect (seteq 1 2 3) (seteq 5 2 3) (seteq 2 20 200))
;(test #f set-equal? (mutable-seteqv 1 2 3))
;(test #f set-eq? (list 1 2 3))
;(test #f set-eqv? (list 1 2 3))
;(test #f set-equal? (list 1 2 3))
;
;(test 3 set-count (set (string #\a) "b" "c" (string #\a)))
;(test 4 set-count (seteqv (string #\a) "b" "c" (string #\a)))
;(test 4 set-count (seteq (string #\a) "b" "c" (string #\a)))
;(test 3 set-count (mutable-set (string #\a) "b" "c" (string #\a)))
;(test 4 set-count (mutable-seteqv (string #\a) "b" "c" (string #\a)))
;(test 4 set-count (mutable-seteq (string #\a) "b" "c" (string #\a)))
;(test 4 set-count (list (string #\a) "b" "c" (string #\a)))
;
;(test #t set-member? (set 1 2 3) 1)
;(test #t set-member? (set 1 2 3) 2);  (test 3 set-count (set-union s))
;  (test 6 set-count (set-union s (set 3 4 5 6)))
;  (test 6 set-count (set-union (set 3 4 5 6) s))
;  (test 8 set-count (set-union (set 3 4 5 6) s (set 1 10 100)))
;
;  (test (seteq 1 2 3) set-union (seteq 1 2) (seteq 3))
;  (test (seteqv 1 2 3) set-union (seteqv 1 2) (seteqv 3))

;(test #t set-member? (set 1 2 3) 3)
;(test #f set-member? (set 1 2 3) 4)
;
;(test #t set-member? (seteq 1 2 3) 1)
;(test #t set-member? (seteq 1 2 3) 2)
;(test #t set-member? (seteq 1 2 3) 3)
;(test #f set-member? (seteq 1 2 3) 4)
;
;(test #t set-member? (seteqv 1 2 3) 1)
;(test #t set-member? (seteqv 1 2 3) 2)
;(test #t set-member? (seteqv 1 2 3) 3)
;(test #f set-member? (seteqv 1 2 3) 4)
;
;(test #t set-member? (mutable-set 1 2 3) 1)
;(test #t set-member? (mutable-set 1 2 3) 2)
;(test #t set-member? (mutable-set 1 2 3) 3)
;(test #f set-member? (mutable-set 1 2 3) 4)
;
;(test #t set-member? (mutable-seteq 1 2 3) 1)
;(test #t set-member? (mutable-seteq 1 2 3) 2)
;(test #t set-member? (mutable-seteq 1 2 3) 3)
;(test #f set-member? (mutable-seteq 1 2 3) 4)
;
;(test #t set-member? (mutable-seteqv 1 2 3) 1)
;(test #t set-member? (mutable-seteqv 1 2 3) 2)
;(test #t set-member? (mutable-seteqv 1 2 3) 3)
;(test #f set-member? (mutable-seteqv 1 2 3) 4)
;
;(test #t set-member? (list 1 2 3) 1)
;(test #t set-member? (list 1 2 3) 2)
;(test #t set-member? (list 1 2 3) 3)
;(test #f set-member? (list 1 2 3) 4)
;
;(test #t stream? (set 1 2 3))
;(test (set-first (set 1 2 3)) set-first (set 1 2 3))
;(test (set-remove (set 1 2 3) (set-first (set 1 2 3))) set-rest (set 1 2 3))
;
;(test #t stream? (seteq 1 2 3))
;(test (set-first (seteq 1 2 3)) set-first (seteq 1 2 3))
;(test (set-remove (seteq 1 2 3) (set-first (seteq 1 2 3))) set-rest (seteq 1 2 3))
;
;(test #t stream? (seteqv 1 2 3))
;(test (set-first (seteqv 1 2 3)) set-first (seteqv 1 2 3))
;(test (set-remove (seteqv 1 2 3) (set-first (seteqv 1 2 3))) set-rest (seteqv 1 2 3))
;
;(test #f stream? (mutable-set 1 2 3))
;(test (set-first (mutable-set 1 2 3)) set-first (mutable-set 1 2 3))
;
;(test #f stream? (mutable-seteq 1 2 3))
;(test (set-first (mutable-seteq 1 2 3)) set-first (mutable-seteq 1 2 3))
;
;(test #f stream? (mutable-seteqv 1 2 3))
;(test (set-first (mutable-seteqv 1 2 3)) set-first (mutable-seteqv 1 2 3))
;
;(test (set-first (list 1 2 3)) set-first (list 1 2 3))
;(test (set-remove (list 1 2 3) (set-first (list 1 2 3))) set-rest (list 1 2 3))
;
;(test (sort (set-union '(1 2) '(2 3)) <)
;      'set-union/list
;      '(1 2 3))
;
;(test (sort (set-intersect '(1 2) '(2 3)) <)
;      'set-intersect/list
;      '(2))
;
;(test (sort (set-subtract '(1 2) '(2 3)) <)
;      'set-subtract/list
;      '(1))
;
;(test (sort (set-symmetric-difference '(1 2) '(2 3)) <)
;      'set-symmetric-difference/list
;      '(1 3))
;
;(let ([s (set 1 2 3)])
;  (test #t equal? s (set-add (set-add (set-add (set) 1) 2) 3))
;  (test #t equal? (seteq 1 2 3) (seteq 1 2 3))
;  (test #t equal? (seteq 1 2 3) (seteq 3 2 1))
;  (test #t equal? (seteqv 1 2 3) (seteqv 1 2 3))
;  (test #f equal? s (seteq 1 2 3))
;  (test #f equal? s (seteqv 1 2 3))
;  (test #f equal? (seteq 1 2 3) (seteqv 1 2 3))
;
;  (test #t set-member? (set-add s 5) 3)
;  (test #t set-member? (set-add s 5) 5)
;  (test #f set-member? (set-add s 5) 4)
;
;  (test #t set-member? (set-remove s 5) 3)
;  (test #f set-member? (set-remove s 3) 3)
;
;  (test #t subset? (set 1 3) s)
;  (test #t subset? (set 1 2 3) s)
;  (test #f subset? (set 1 4) s)
;  (test #t subset? (set) s)
;
;  (test 3 set-count (set-union s))
;  (test 6 set-count (set-union s (set 3 4 5 6)))
;  (test 6 set-count (set-union (set 3 4 5 6) s))
;  (test 8 set-count (set-union (set 3 4 5 6) s (set 1 10 100)))
;
;  (test (seteq 1 2 3) set-union (seteq 1 2) (seteq 3))
;  (test (seteqv 1 2 3) set-union (seteqv 1 2) (seteqv 3))
;
;  (test s set-intersect s)
;  (test (set 3) set-intersect s (set 5 4 3 6))
;  (test (set 3) set-intersect (set 5 4 3 6) s)
;  (test (seteq 3) set-intersect (seteq 5 4 3 6) (seteq 1 2 3))
;  (test (seteqv 3) set-intersect (seteqv 5 4 3 6) (seteqv 1 2 3))
;  (test (set 3 2) set-intersect s (set 5 2 3))
;  (test (seteq 3 2) set-intersect (seteq 1 2 3) (seteq 5 2 3))
;  (test (set 2) set-intersect s (set 5 2 3) (set 2 20 200))
;  (test (seteq 2) set-intersect (seteq 1 2 3) (seteq 5 2 3) (seteq 2 20 200))
;
;  (test s set-subtract s)
;  (test (set) set-subtract s s)
;  (test s set-subtract s (set 100))
;  (test (set 1 3) set-subtract s (set 2 100))
;  (test (seteq 100) set-subtract (seteq 2 100) (seteq 1 2 3))
;  (test (seteq 9 100) set-subtract (seteq 2 100 1000 9) (seteq 1 2 3) (seteq 1000 5))
;
;  (let ([try-mismatch (lambda (set-op)
;                        (err/rt-test (set-op (seteqv 1 2) (set 3)))
;                        (err/rt-test (set-op (seteqv 1 2) (seteq 3)))
;                        (err/rt-test (set-op (set 1 2) (seteq 3)))
;                        (err/rt-test (set-op (set 1 2) (set 4) (seteq 3)))
;                        (err/rt-test (set-op (set 1 2) (seteq 3) (set 4)))
;                        (err/rt-test (set-op (seteq 3) (set 1 2) (set 4))))])
;    (try-mismatch set-union)
;    (try-mismatch set-intersect)
;    (try-mismatch set-subtract))
;
;  (test #t andmap negative? (set-map s -))
;  (test 3 length (set-map s +))
;
;  (let ([v 0])
;    (set-for-each s (lambda (n) (set! v (+ v n))))
;    (test 6 values v))
;
;  (test '(1 2 3) sort (for/list ([v s]) v) <)
;  (test '(1 2 3) sort (for/list ([v (in-set s)]) v) <)
;  (test '(1 2 3) sort (let ([seq (in-set s)]) (for/list ([v seq]) v)) <)
;  ;; Optimized
;  (test '(1) sort (for/list ([v (in-set (set 1))]) v) <)
;  (test #t values (let ([noset #t])
;                    (for ([v (in-set (set))]) (set! noset #f))
;                    noset))
;        
;
;  (void))
;
;(let ()
;
;  (define (str=? x y rec) (string=? x y))
;  (define (str-hc1 x rec) (string-length x))
;  (define (str-hc2 x rec) (rec (string-ref x 0)))
;
;  (define-custom-set-types string-set #:elem? string? str=? str-hc1 str-hc2)
;
;  (define (strset . strs) (make-immutable-string-set strs))
;  (define (mutable-strset . strs) (make-mutable-string-set strs))
;  (define (weak-strset . strs) (make-weak-string-set strs))
;
;  ;; Tests for the different set types:
;
;  (define (t mset-A mset-B mset-C set-A set-B set-C)
;
;    (define (t1 ms s subs0 just-elems just-supers <? f)
;
;      ;; Construct sets for comparison:
;
;      (define subs (sort subs0 <?))
;      (define elems (sort (append subs just-elems) <?))
;      (define supers (sort (append elems just-supers) <?))
;      (define not-subs (sort (append just-elems just-supers) <?))
;      (define msA (apply mset-A elems))
;      (define msB (apply mset-B elems))
;      (define msC (apply mset-C elems))
;      (define sA (apply set-A elems))
;      (define sB (apply set-B elems))
;      (define sC (apply set-C elems))
;      (define ms-sub (apply mset-A subs))
;      (define ms-super (apply mset-A supers))
;      (define ms-not-sub (apply mset-A not-subs))
;      (define s-sub (apply set-A subs))
;      (define s-super (apply set-A supers))
;      (define s-not-sub (apply set-A not-subs))
;
;      ;; For weak hash tables, to make the results more predictable:
;      (collect-garbage)
;
;      ;; Test contents:
;
;      (define mcontents (sort (set->list ms) <?))
;      (test #true equal? mcontents elems)
;      (test (null? just-elems) equal? mcontents subs)
;      (test (null? just-supers) equal? mcontents supers)
;      (test (and (null? subs) (null? just-supers)) equal? mcontents not-subs)
;
;      (define contents (sort (set->list s) <?))
;      (test #true equal? contents elems)
;      (test (null? just-elems) equal? contents subs)
;      (test (null? just-supers) equal? contents supers)
;      (test (and (null? subs) (null? just-supers)) equal? contents not-subs)
;
;      ;; Test equality:
;
;      (test #true equal? ms msA)
;      (test #false equal? ms msB)
;      (test #false equal? ms msC)
;      (test #false equal? ms sA)
;      (test #false equal? ms sB)
;      (test #false equal? ms sC)
;      (test #true equal? ms ms)
;      (test (null? just-elems) equal? ms ms-sub)
;      (test (null? just-supers) equal? ms ms-super)
;      (test (and (null? subs) (null? just-supers)) equal? ms ms-not-sub)
;
;      (test #false equal? s msA)
;      (test #false equal? s msB)
;      (test #false equal? s msC)
;      (test #true equal? s sA)
;      (test #false equal? s sB)
;      (test #false equal? s sC)
;      (test #true equal? s s)
;      (test (null? just-elems) equal? s s-sub)
;      (test (null? just-supers) equal? s s-super)
;      (test (and (null? subs) (null? just-supers)) equal? s s-not-sub)
;
;      ;; Test membership:
;
;      (for ([elem (in-list elems)])
;        (test #true set-member? ms elem)
;        (test #true set-member? s elem)
;        (test #false set-member? (set-clear s) elem)
;        (test #false set-member? (set-copy-clear ms) elem)
;        (test #false set-member? (set-copy-clear s) elem))
;
;      (for ([elem (in-list just-supers)])
;        (test #false set-member? ms elem)
;        (test #false set-member? s elem))
;
;      ;; Test set equality:
;
;      (test #true set=? ms ms)
;
;      (test #true set=? ms msA)
;      (test (null? just-elems) set=? ms ms-sub)
;      (test (null? just-supers) set=? ms ms-super)
;      (test (and (null? subs) (null? just-supers)) set=? ms ms-not-sub)
;
;      (test #true set=? ms sA)
;      (test (null? just-elems) set=? ms s-sub)
;      (test (null? just-supers) set=? ms s-super)
;      (test (and (null? subs) (null? just-supers)) set=? ms s-not-sub)
;
;      (err/rt-test (set=? ms msB))
;      (err/rt-test (set=? ms msC))
;      (err/rt-test (set=? ms sB))
;      (err/rt-test (set=? ms sC))
;
;      (test #true set=? s s)
;
;      (test #true set=? s msA)
;      (test (null? just-elems) set=? s ms-sub)
;      (test (null? just-supers) set=? s ms-super)
;      (test (and (null? subs) (null? just-supers)) set=? s ms-not-sub)
;
;      (test #true set=? s sA)
;      (test (null? just-elems) set=? s s-sub)
;      (test (null? just-supers) set=? s s-super)
;      (test (and (null? subs) (null? just-supers)) set=? s s-not-sub)
;
;      (err/rt-test (set=? s msB))
;      (err/rt-test (set=? s msC))
;      (err/rt-test (set=? s sB))
;      (err/rt-test (set=? s sC))
;
;      ;; Test subset:
;
;      (test #true subset? ms ms)
;
;      (test #true subset? ms msA)
;      (test (null? just-elems) subset? ms ms-sub)
;      (test #true subset? ms ms-super)
;      (test (null? subs) subset? ms ms-not-sub)
;
;      (test #true subset? ms sA)
;      (test (null? just-elems) subset? ms s-sub)
;      (test #true subset? ms s-super)
;      (test (null? subs) subset? ms s-not-sub)
;
;      (err/rt-test (subset? ms msB))
;      (err/rt-test (subset? ms msC))
;      (err/rt-test (subset? ms sB))
;      (err/rt-test (subset? ms sC))
;
;      (test #true subset? s s)
;
;      (test #true subset? s msA)
;      (test (null? just-elems) subset? s ms-sub)
;      (test #true subset? s ms-super)
;      (test (null? subs) subset? s ms-not-sub)
;
;      (test #true subset? s sA)
;      (test (null? just-elems) subset? s s-sub)
;      (test #true subset? s s-super)
;      (test (null? subs) subset? s s-not-sub)
;
;      (err/rt-test (subset? s msB))
;      (err/rt-test (subset? s msC))
;      (err/rt-test (subset? s sB))
;      (err/rt-test (subset? s sC))
;
;      ;; Test proper subset:
;
;      (test #false proper-subset? ms ms)
;
;      (test #false proper-subset? ms msA)
;      (test #false proper-subset? ms ms-sub)
;      (test #true proper-subset? ms ms-super)
;      (test (and (null? subs) (pair? just-supers)) proper-subset? ms ms-not-sub)
;
;      (test #false proper-subset? ms sA)
;      (test #false proper-subset? ms s-sub)
;      (test #true proper-subset? ms s-super)
;      (test (and (null? subs) (pair? just-supers)) proper-subset? ms s-not-sub)
;
;      (err/rt-test (proper-subset? ms msB))
;      (err/rt-test (proper-subset? ms msC))
;      (err/rt-test (proper-subset? ms sB))
;      (err/rt-test (proper-subset? ms sC))
;
;      (test #false proper-subset? s s)
;
;      (test #false proper-subset? s msA)
;      (test #false proper-subset? s ms-sub)
;      (test #true proper-subset? s ms-super)
;      (test (and (null? subs) (pair? just-supers)) proper-subset? s ms-not-sub)
;
;      (test #false proper-subset? s sA)
;      (test #false proper-subset? s s-sub)
;      (test #true proper-subset? s s-super)
;      (test (and (null? subs) (pair? just-supers)) proper-subset? s s-not-sub)
;
;      (err/rt-test (proper-subset? s msB))
;      (err/rt-test (proper-subset? s msC))
;      (err/rt-test (proper-subset? s sB))
;      (err/rt-test (proper-subset? s sC))
;
;      ;; Test iteration:
;
;      (define sorted (sort elems <?))
;
;      (test (map f sorted) 'set-map/mutable (sort (set-map ms f) <?))
;      (test (map f sorted) 'set-map/immutable (sort (set-map s f) <?))
;
;      (test sorted
;            'set-for-each/mutable
;            (sort
;             (let ([xs '()])
;               (set-for-each ms (lambda (x) (set! xs (cons x xs))))
;               xs)
;             <?))
;      (test sorted
;            'set-for-each/immutable
;            (sort
;             (let ([xs '()])
;               (set-for-each s (lambda (x) (set! xs (cons x xs))))
;               xs)
;             <?))
;
;      (test sorted 'in-set/mutable (sort (for/list ([x (in-set ms)]) x) <?))
;      (test sorted 'in-set/immutable (sort (for/list ([x (in-set s)]) x) <?))
;
;      (test sorted 'in-set/proc/mutable (sort (sequence->list (in-set ms)) <?))
;      (test sorted 'in-set/proc/immutable (sort (sequence->list (in-set s)) <?))
;
;      (test sorted 'set->list/mutable (sort (set->list ms) <?))
;      (test sorted 'set->list/immutable (sort (set->list s) <?))
;
;      (void))
;
;    ;; Test instances:
;
;    ;; Using string constants stored in variables:
;    ;; - allows us to hash them via equal, eqv, and eq
;    ;; - allows us to keep them around in weak tables
;    (define x1 (string-copy "one"))
;    (define x2 (string-copy "two"))
;    (define x3 (string-copy "three"))
;    (define x4 (string-copy "four"))
;
;    (define ms (mset-A x1 x2 x3))
;    (define s0 (set-A x1 x2 x3))
;    (t1 ms s0 (list x1 x2) (list x3) (list x4) string<? string-upcase)
;
;    (define msc (set-copy ms))
;    (t1 msc s0 (list x1 x2) (list x3) (list x4) string<? string-upcase)
;
;    (set-remove! ms x3)
;    (define s1 (set-remove s0 x3))
;    (t1 ms s1 (list x1) (list x2) (list x3 x4) string<? string-upcase)
;
;    ;; Ensure the copy hasn't changed.
;    (t1 msc s0 (list x1 x2) (list x3) (list x4) string<? string-upcase)
;
;    (set-add! ms x4)
;    (define s2 (set-add s1 x4))
;    (t1 ms s2 (list x1) (list x2 x4) (list x3) string<? string-upcase)
;
;    (set-clear! ms)
;    (define s3 (set-clear s2))
;    (t1 ms s3 (list) (list) (list x1 x2 x3 x4) string<? string-upcase)
;
;    (set-union! ms (mset-A x1 x2) (mset-A x2 x3))
;    (define s4 (set-union s3 (set-A x1 x2) (set-A x2 x3)))
;    (t1 ms s4 (list x2) (list x1 x3) (list x4) string<? string-upcase)
;
;    (set-intersect! ms (mset-A x1 x2) (mset-A x2 x3))
;    (define s5 (set-intersect s4 (set-A x1 x2) (set-A x2 x3)))
;    (t1 ms s5 (list x2) (list) (list x1 x3 x4) string<? string-upcase)
;    (t1 ms s5 (list) (list x2) (list x1 x3 x4) string<? string-upcase)
;
;    (set-symmetric-difference! ms (mset-A x1 x2) (mset-A x2 x3))
;    (define s6 (set-symmetric-difference s5 (set-A x1 x2) (set-A x2 x3)))
;    (t1 ms s6 (list x1 x3) (list x2) (list x4) string<? string-upcase)
;
;    (set-subtract! ms (mset-A x1 x4) (mset-A x2 x4))
;    (define s7 (set-subtract s6 (set-A x1 x4) (set-A x2 x4)))
;    (t1 ms s7 (list x3) (list) (list x1 x2 x4) string<? string-upcase)
;    (t1 ms s7 (list) (list x3) (list x1 x2 x4) string<? string-upcase)
;
;    ;; need to do something to keep these from being garbage collected
;    (test "one" string-copy x1)
;    (test "two" string-copy x2)
;    (test "three" string-copy x3)
;    (test "four" string-copy x4))
;
;  (t mutable-set mutable-seteqv mutable-seteq set seteqv seteq)
;  (t mutable-seteqv mutable-seteq mutable-set seteqv seteq set)
;  (t mutable-seteq mutable-set mutable-seteqv seteq set seteqv)
;  (t weak-set weak-seteqv weak-seteq set seteqv seteq)
;  (t weak-seteqv weak-seteq weak-set seteqv seteq set)
;  (t weak-seteq weak-set weak-seteqv seteq set seteqv)
;  (t weak-set mutable-seteqv mutable-seteq set seteqv seteq)
;  (t mutable-set weak-seteqv weak-seteq set seteqv seteq)
;  (t mutable-strset mutable-set weak-set strset set seteqv)
;  (t weak-strset mutable-seteqv weak-seteq strset seteqv seteq))
;
;(test "#<set: 1>" 
;      'print-set1
;      (let ([sp (open-output-string)])
;        (write (set 1) sp)
;        (get-output-string sp)))
;
;(test "#<seteq: 1>" 
;      'print-set1
;      (let ([sp (open-output-string)])
;        (write (seteq 1) sp)
;        (get-output-string sp)))
;
;(test "#<seteqv: 1>" 
;      'print-set1
;      (let ([sp (open-output-string)])
;        (write (seteqv 1) sp)
;        (get-output-string sp)))
;
;;; ----------------------------------------
;
;(test (set 1 2 3) 'for/set (for/set ([i '(0 1 2)]) (add1 i)))
;
;(test (set 1 2 3) 'for/set (for/set ([i '(0 1 2 3 4)]) 
;                                    #:break (= i 3)
;                                    (add1 i)))
;
;(test (set 1 2 3) 'for/set (for/set ([i '(0 1 2 3 4)]) 
;                                    #:final (= i 2)
;                                    (add1 i)))
;
;(test (mutable-set 1 2 3)
;      'for/mutable-set
;      (for/mutable-set ([i '(0 1 2)]) (add1 i)))
;
;(test (mutable-set 1 2 3)
;      'for/mutable-set
;      (for/mutable-set ([i '(0 1 2 3 4)]) 
;                       #:break (= i 3)
;                       (add1 i)))
;
;(test (mutable-set 1 2 3)
;      'for/mutable-set
;      (for/mutable-set ([i '(0 1 2 3 4)]) 
;                       #:final (= i 2)
;                       (add1 i)))
;
;;; ----------------------------------------
;
;(err/rt-test (set/c '(not a contract)))
;(err/rt-test (set/c any/c #:cmp 'not-a-comparison))
;(err/rt-test (set/c any/c #:kind 'not-a-kind-of-set))
;(err/rt-test (set/c (-> integer? string?) #:cmp 'eq))
;(err/rt-test (set/c (-> integer? string?) #:cmp 'eqv))

(report-errs)
