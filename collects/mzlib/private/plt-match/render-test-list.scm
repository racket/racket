;; This library is used by match.ss

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
(define (render-test-list p ae stx)
  (include "special-generators.scm")
  (include "ddk-handlers.scm")
  (include "getter-setter.scm")
  (include "emit-assm.scm")
  (include "parse-quasi.scm")
  (include "pattern-predicates.scm")

  (define (append-if-necc sym stx)
    (syntax-case stx ()
      (() (syntax (list)))
      ((a ...)
       (quasisyntax/loc stx (#,sym a ...)))
      (p (syntax p))))
  (syntax-case*
   p
   (_ list quote quasiquote vector box ? app and or not struct set! var
      list-rest get! ... ___ unquote unquote-splicing
      list-no-order hash-table regexp pregexp) stx-equal?
      (_ '()) ;(ks sf bv let-bound))
      (pt
       (and (identifier? (syntax pt))
            (pattern-var? (syntax-object->datum (syntax pt)))
            (not (stx-dot-dot-k? (syntax pt))))
       (render-test-list (syntax/loc stx (var pt)) ae stx))
      ((var pt)
       (identifier? (syntax pt))
       (list (make-act `bind-var-pat
                       ae
                       (lambda (ks kf let-bound)
                         (lambda (sf bv)
                           (let ((raw-id (syntax-object->datum (syntax pt))))
                             (cond ((ormap (lambda (x)
                                             (if (equal? raw-id (syntax-object->datum (car x)))
                                                 (cdr x) #f)) bv)
                                    => (lambda (bound-exp)
                                         (emit (lambda (exp)
                                                 (quasisyntax/loc
                                                  p
                                                  (equal? #,exp #,(subst-bindings bound-exp let-bound))))
                                               ae
                                               let-bound
                                               sf bv kf ks)))
                                   (else
                                    (ks sf (cons (cons (syntax pt) ae) bv))))))))))

      ((list)
       (list
        (make-reg-test
         `(null? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           (lambda (sf bv)
             (emit (lambda (exp)
                     (quasisyntax/loc p (null? #,exp)))
                   ae
                   let-bound
                   sf
                   bv
                   kf
                   ks))))))
      ('()
       (list
        (make-reg-test
         `(null? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           (lambda (sf bv)
             (emit (lambda (exp)
                     (quasisyntax/loc p (null? #,exp)))
                   ae
                   let-bound
                   sf
                   bv
                   kf
                   ks)))))
       )

      (pt
       ;; could convert the syntax once
       (or (stx-? string? (syntax pt))
           (stx-? boolean? (syntax pt))
           (stx-? char? (syntax pt))
           (stx-? number? (syntax pt)))
       (list
        (make-reg-test
         `(equal? ,(syntax-object->datum ae)
                  ,(syntax-object->datum (syntax pt)))
         ae
         (lambda (ks kf let-bound)
           (lambda (sf bv)
             (emit (lambda (exp) (quasisyntax/loc p (equal? #,exp pt)))
                   ae
                   let-bound
                   sf bv kf ks))))))

      ;(pt
      ; (stx-? regexp? (syntax pt))
      ; (render-test-list (syntax/loc p (regex pt)) ae stx))

      ((quote _)
       (list
        (make-reg-test
         `(equal? ,(syntax-object->datum ae)
                  ,(syntax-object->datum p))
         ae
         (lambda (ks kf let-bound)
           (lambda (sf bv)
             (emit (lambda (exp) (quasisyntax/loc p (equal? #,exp #,p)))
                   ae
                   let-bound
                   sf bv kf ks))))))
      (`quasi-pat
       (render-test-list (parse-quasi (syntax quasi-pat)) ae stx))
      ('item
       (list (make-reg-test
              `(equal? ,(syntax-object->datum ae)
                       ,(syntax-object->datum p))
              ae
              (lambda (ks kf let-bound)
                (lambda (sf bv)
                  (emit (lambda (exp) (quasisyntax/loc p (equal? #,exp #,p)))
                        ae
                        let-bound
                        sf bv kf ks))))))
      ;;('(items ...)
      ;;(emit (quasisyntax/loc p (equal? #,e #,p)) sf bv kf ks))
      ((? pred? pat1 pats ...)
       (render-test-list (syntax (and (? pred?) pat1 pats ...)) ae stx))
      ;; could we check to see if a predicate is a procedure here?
      ((? pred?)
       (list (make-reg-test
              `(,(syntax-object->datum (syntax pred?))
                ,(syntax-object->datum ae))
              ae
              (lambda (ks kf let-bound)
                (lambda (sf bv)
                  (emit (lambda (exp) (quasisyntax/loc p (pred? #,exp)))
                        ae
                        let-bound
                        sf bv kf ks))))))
      ;; syntax checking
      ((? pred? ...)
       (match:syntax-err
        p
        (if (zero? (length (syntax-e (syntax (pred? ...)))))
            "a predicate pattern must have a predicate following the ?"
            "syntax error in predicate pattern")))

      ((regexp reg-exp)
       (render-test-list (quasisyntax/loc 
                          p
                          (and (? string?)
                               (? (lambda (x) (regexp-match reg-exp x)))))
                         ae 
                         stx))
      ((pregexp reg-exp)
       (render-test-list (quasisyntax/loc
                          p
                          (and (? string?)
                               (? (lambda (x) (pregexp-match-with-error 
                                               reg-exp x)))))
                         ae 
                         stx))

      ((regexp reg-exp pat)
       (render-test-list (quasisyntax/loc
                          p
                          (and (? string?)
                               (app (lambda (x) (regexp-match reg-exp x)) pat)))
                         ae 
                         stx))

      ((pregexp reg-exp pat)
       (render-test-list (quasisyntax/loc
                          p
                          (and (? string?)
                               (app (lambda (x) (pregexp-match-with-error 
                                                   reg-exp x)) pat)))
                         ae 
                         stx))

      ((app op pat)
       (render-test-list (syntax pat)
                         (quasisyntax/loc p (op #,ae))
                         stx))
      ;; syntax checking
      ((app op ...)
       (match:syntax-err
        p
        (if (zero? (length (syntax-e (syntax (op ...)))))
            "an operation pattern must have a procedure following the app"
            "there should be one pattern following the operator")))
      ((and pats ...)
       (let loop
           ((p (syntax (pats ...))))
         (syntax-case p ()
           (() '()) ;(ks seensofar boundvars let-bound))
           ((pat1 pats ...)
            (append (render-test-list (syntax pat1) ae stx)
                    (loop (syntax (pats ...))))))))

      ((or pats ...)
       (list (make-act
              'or-pat ;`(or-pat ,(syntax-object->datum ae))
              ae
              (lambda (ks kf let-bound)
                (lambda (sf bv)
                  (or-gen ae (syntax-e (syntax (pats ...)))
                          stx sf bv ks kf let-bound))))))

                                        ; backtracking or

                                        ;       ((or pats ...)
                                        ;        (let* ((pat-list (syntax->list (syntax (pats ...)))))
                                        ;          (let* ((bound (getbindings (stx-car (syntax (pats ...)))))
                                        ;                 (bind-map
                                        ;                  (map (lambda (x)
                                        ;                         (cons x
                                        ;                               #`#,(gensym (syntax-object->datum x))))
                                        ;                       bound))
                                        ;                 (id (begin (set! or-id (add1 or-id)) (sub1 or-id))))
                                        ;            (write id)(newline)
                                        ;            (write (syntax-object->datum (syntax (pats ...))))(newline)
                                        ;            (list
                                        ;             (make-act
                                        ;              'or-pat
                                        ;              ae
                                        ;              (lambda (ks kf let-bound)
                                        ;                (lambda (sf bv)
                                        ;                  (write id)(newline)
                                        ;                  (if (stx-null? (syntax (pats ...)))
                                        ;                      (kf sf bv)
                                        ;                      #`(let #,(map (lambda (b)
                                        ;                                      #`(#,(cdr b) '()))
                                        ;                                    bind-map)
                                        ;                          (if (or
                                        ;                               #,@(map (lambda (p)
                                        ;                                         #`(#,(create-test-func
                                        ;                                               p
                                        ;                                               sf
                                        ;                                               let-bound
                                        ;                                               bind-map
                                        ;                                               #f) #,(subst-bindings ae
                                        ;                                                            let-bound)))
                                        ;                                       pat-list))
                                        ;                              #,(ks sf (append bind-map bv))
                                        ;                              #,(kf sf bv)))))))))))

      ((not pat)
       (list (make-act
              'not-pat ;`(not-pat ,(syntax-object->datum ae))
              ae
              (lambda (ks kf let-bound)
                (lambda (sf bv)
                  ;; swap success and fail
                  (next-outer (syntax pat) ae sf bv let-bound ks kf))))))
      ;; could try to catch syntax local value error and rethrow syntax error

      ((list-no-order pats ...)
       (if (stx-null? (syntax (pats ...)))
           (render-test-list (syntax/loc p (list)) ae stx)
           (let* ((pat-list (syntax->list (syntax (pats ...))))
                  (ddk-list (ddk-in-list? pat-list))
                  (ddk (ddk-only-at-end-of-list? pat-list)))
             (if (or (not ddk-list)
                     (and ddk-list ddk))
                 (let* ((bound (getbindings (append-if-necc 'list
                                                            (syntax (pats ...)))))
                        (bind-map
                         (map (lambda (x)
                                (cons x
                                      #`#,(gensym (syntax-object->datum x))))
                              bound)))

                   (list
                    (make-shape-test
                     `(list? ,(syntax-object->datum ae))
                     ae
                     (lambda (ks kf let-bound)
                       (lambda (sf bv)
                         (emit (lambda (exp)
                                 (quasisyntax/loc stx (list? #,exp)))
                               ae
                               let-bound
                               sf
                               bv
                               kf
                               ks))))
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
                                                          #t)))
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
                                                  #f))
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
       (proper-hash-table-pattern? (syntax->list (syntax (pats ...))))
       (list
        (make-shape-test
         `(hash-table? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           (lambda (sf bv)
             (emit (lambda (exp) (quasisyntax/loc stx (hash-table? #,exp)))
                   ae
                   let-bound
                   sf
                   bv
                   kf
                   ks))))
        (let ((mod-pat
               (lambda (pat)
                 (syntax-case pat ()
                   ((key value) (syntax (list key value)))
                   (ddk
                    (stx-dot-dot-k? (syntax ddk))
                    (syntax ddk))
                   (id
                    (and (identifier? (syntax id))
                         (pattern-var? (syntax-object->datum (syntax id)))
                         (not (stx-dot-dot-k? (syntax id))))
                    (syntax id))
                   (p (match:syntax-err
                       (syntax/loc stx p)
                       "poorly formed hash-table pattern"))))))
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
                     #,(next-outer #`(list-no-order #,@(map mod-pat (syntax->list (syntax (pats ...)))))
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
                                   ks)))))))))

      ((hash-table pats ...)
       (match:syntax-err
        p
        "improperly formed hash table pattern"))

      ((struct struct-name (fields ...))
       (identifier? (syntax struct-name))
       (let ((num-of-fields (stx-length (syntax (fields ...)))))
         (let-values (((pred accessors mutators parental-chain)
                       (struct-pred-accessors-mutators
                        (syntax struct-name)
                        (lambda ()
                          (match:syntax-err
                           (syntax struct-name)
                           "not a defined structure")))))
           (let ((dif (- (length accessors) num-of-fields)))
             (if (not (zero? dif))
                 (match:syntax-err
                  p
                  (string-append
                   (if (> dif 0) "not enough " "too many ")
                   "fields for structure in pattern"))
                 (cons
                  (make-shape-test
                   `(struct-pred ,(syntax-object->datum pred)
                                 ,(map syntax-object->datum parental-chain)
                                 ,(syntax-object->datum ae))
                   ae
                   (lambda (ks kf let-bound)
                     (lambda (sf bv)
                       (emit (lambda (exp)
                               (quasisyntax/loc stx (struct-pred #,pred #,parental-chain #,exp)))
                             ae
                             let-bound
                             sf
                             bv
                             kf
                             ks))))
                  (let ((field-pats (syntax->list (syntax (fields ...)))))
                    (let rloop ((n 0))
                      (if (= n num-of-fields)
                          '()
                          (append
                           (let ((cur-pat (list-ref field-pats n)))
                             (syntax-case cur-pat (set! get!)
                               ((set! setter-name)
                                (list (make-act
                                       'set!-pat
                                       ae
                                       (lambda (ks kf let-bound)
                                         (lambda (sf bv)
                                           (ks sf
                                               (cons (cons (syntax setter-name)
                                                           #`(lambda (y)
                                                               (#,(list-ref
                                                                   mutators
                                                                   n)
                                                                #,ae y)))
                                                     bv)))))))
                               ((get! getter-name)
                                (list (make-act
                                       'get!-pat
                                       ae
                                       (lambda (ks kf let-bound)
                                         (lambda (sf bv)
                                           (ks sf
                                               (cons (cons (syntax getter-name)
                                                           #`(lambda ()
                                                               (#,(list-ref
                                                                   accessors
                                                                   n)
                                                                #,ae)))
                                                     bv)))))))
                               (_
                                (render-test-list
                                 cur-pat
                                 (quasisyntax/loc
                                  stx
                                  (#,(list-ref accessors n) #,ae))
                                 stx))))
                           (rloop (+ 1 n))))))))))))
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
      ((set! ident)
       (identifier? (syntax ident))
       (list (make-act
              'set!-pat
              ae
              (lambda (ks kf let-bound)
                (lambda (sf bv)
                  (ks sf (cons (cons (syntax ident)
                                     (setter ae p let-bound))
                               bv)))))))
      ;; syntax checking
      ((set! ident ...)
       (let ((x (length (syntax-e (syntax (ident ...))))))
         (match:syntax-err
          p
          (if (= x 1)
              "there should be an identifier after set! in pattern"
              (string-append "there should "
                             (if (zero? x) "" "only ")
                             "be one identifier after set! in pattern")))))
      ((get! ident)
       (identifier? (syntax ident))
       (list (make-act
              'get!-pat
              ae
              (lambda (ks kf let-bound)
                (lambda (sf bv)
                  (ks sf (cons (cons (syntax ident)
                                     (getter ae p let-bound))
                               bv)))))))
      ((get! ident ...)
       (let ((x (length (syntax-e (syntax (ident ...))))))
         (match:syntax-err
          p
          (if (= x 1)
              "there should be an identifier after get! in pattern"
              (string-append "there should "
                             (if (zero? x) "" "only ")
                             "be one identifier after get! in pattern")))))

      ((list pat dot-dot-k pat-rest ...)
       (and (not (or (memq (syntax-e (syntax pat))
                           '(unquote unquote-splicing ... ___))
                     (stx-dot-dot-k? (syntax pat))))
            (stx-dot-dot-k? (syntax dot-dot-k)))
       (list
        (make-shape-test
         `(list? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           ;;(write 'here)(write let-bound)(newline)
           (lambda (sf bv)
             (emit
              (lambda (exp) (quasisyntax/loc stx (list? #,exp)))
              ae
              let-bound
              sf
              bv
              kf
              ks))))
        (make-act
         'list-ddk-pat
         ae
         (lambda (ks kf let-bound)
           (if (stx-null? (syntax (pat-rest ...)))
               (handle-end-ddk-list ae kf ks
                                    (syntax pat)
                                    (syntax dot-dot-k)
                                    stx let-bound)
               (handle-inner-ddk-list ae kf ks
                                      (syntax pat)
                                      (syntax dot-dot-k)
                                      (append-if-necc 'list
                                                      (syntax (pat-rest ...)))
                                      stx
                                      let-bound))))))

      ((list-rest pat dot-dot-k pat-rest ...)
       (and (not (or (memq (syntax-e (syntax pat))
                           '(unquote unquote-splicing ... ___))
                     (stx-dot-dot-k? (syntax pat))
                     (stx-null? (syntax (pat-rest ...)))))
            (stx-dot-dot-k? (syntax dot-dot-k)))
       (list
        (make-shape-test
         `(pair? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           ;;(write 'here)(write let-bound)(newline)
           (lambda (sf bv)
             (emit
              (lambda (exp) (quasisyntax/loc stx (pair? #,exp)))
              ae
              let-bound
              sf
              bv
              kf
              ks))))
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
            stx
            let-bound)))))

      ;; handle proper and improper lists

      ((list-rest car-pat cdr-pat) ;pattern ;(pat1 pats ...)
       (not (or (memq (syntax-e (syntax car-pat))
                      '(unquote unquote-splicing))
                (stx-dot-dot-k? (syntax car-pat))))
       (cons
        (make-shape-test
         `(pair? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           ;;(write 'here)(write let-bound)(newline)
           (lambda (sf bv)
             (emit
              (lambda (exp) (quasisyntax/loc stx (pair? #,exp)))
              ae
              let-bound
              sf
              bv
              kf
              ks))))
        (append
         (render-test-list (syntax car-pat)
                           (quasisyntax/loc (syntax car-pat) (car #,ae))
                           stx) ;(add-a e)
         (render-test-list
          (syntax cdr-pat)
          (quasisyntax/loc (syntax cdr-pat) (cdr #,ae))
          stx))))

      ((list-rest car-pat cdr-pat ...) ;pattern ;(pat1 pats ...)
       (not (or (memq (syntax-e (syntax car-pat))
                      '(unquote unquote-splicing))
                (stx-dot-dot-k? (syntax car-pat))))
       (cons
        (make-shape-test
         `(pair? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           ;;(write 'here)(write let-bound)(newline)
           (lambda (sf bv)
             (emit
              (lambda (exp) (quasisyntax/loc stx (pair? #,exp)))
              ae
              let-bound
              sf
              bv
              kf
              ks))))
        (append
         (render-test-list (syntax car-pat)
                           (quasisyntax/loc (syntax car-pat) (car #,ae))
                           stx) ;(add-a e)
         (render-test-list
          (append-if-necc 'list-rest (syntax (cdr-pat ...)))
          (quasisyntax/loc (syntax (cdr-pat ...)) (cdr #,ae))
          stx))))

      ((list car-pat cdr-pat ...) ;pattern ;(pat1 pats ...)
       (not (or (memq (syntax-e (syntax car-pat))
                      '(unquote unquote-splicing))
                (stx-dot-dot-k? (syntax car-pat))))
       (cons
        (make-shape-test
         `(pair? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           ;;(write 'here)(write let-bound)(newline)
           (lambda (sf bv)
             (emit
              (lambda (exp) (quasisyntax/loc stx (pair? #,exp)))
              ae
              let-bound
              sf
              bv
              kf
              ks))))
        (append
         (render-test-list (syntax car-pat)
                           (quasisyntax/loc (syntax car-pat) (car #,ae))
                           stx) ;(add-a e)
         (if (stx-null? (syntax (cdr-pat ...)))
             (list
              (make-shape-test
               `(null? (cdr ,(syntax-object->datum ae)))
               ae
               (lambda (ks kf let-bound)
                 (lambda (sf bv)
                   (emit (lambda (exp)
                           (quasisyntax/loc p (null? #,exp)))
                         (quasisyntax/loc (syntax (cdr-pat ...)) (cdr #,ae));ae
                         let-bound
                         sf
                         bv
                         kf
                         ks)))))
             (render-test-list
              (append-if-necc 'list (syntax (cdr-pat ...)))
              (quasisyntax/loc (syntax (cdr-pat ...)) (cdr #,ae))
              stx)))))

      ((vector pats ...)
       (ddk-only-at-end-of-list? (syntax-e (syntax (pats ...))))
       (list
        (make-shape-test
         `(vector? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           (lambda (sf bv)
             (emit (lambda (exp) (quasisyntax/loc stx (vector? #,exp)))
                   ae
                   let-bound
                   sf
                   bv
                   kf
                   ks))))
        (make-act
         'vec-ddk-pat
         ae
         (lambda (ks kf let-bound)
           (handle-ddk-vector ae kf ks
                              (syntax/loc p #(pats ...))
                              stx let-bound)))))

      ((vector pats ...)
       (let* ((temp (syntax-e (syntax (pats ...))))
              (len (length temp)))
         (and (>= len 2)
              (ddk-in-list? temp)))
       ;; make this contains ddk with no ddks consecutive
       ;;(stx-dot-dot-k? (vector-ref temp (sub1 len))))))
       (list
        (make-shape-test
         `(vector? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           (lambda (sf bv)
             (emit (lambda (exp) (quasisyntax/loc stx (vector? #,exp)))
                   ae
                   let-bound
                   sf
                   bv
                   kf
                   ks))))
        ;; we have to look at the first pattern and see if a ddk follows it
        ;; if so handle that case else handle the pattern
        (make-act
         'vec-ddk-pat
         ae
         (lambda (ks kf let-bound)
           (handle-ddk-vector-inner ae kf ks
                                    (syntax/loc p #(pats ...))
                                    stx let-bound)))))

      ((vector pats ...)
       (let* ((syntax-vec (list->vector (syntax->list (syntax (pats ...)))))
              (vlen (vector-length syntax-vec)))
         (cons
          (make-shape-test
           `(vector? ,(syntax-object->datum ae))
           ae
           (lambda (ks kf let-bound)
             (lambda (sf bv)
               (emit
                (lambda (exp) (quasisyntax/loc stx (vector? #,exp)))
                ae
                let-bound
                sf bv kf ks))))
          (cons
           (make-shape-test
            `(equal? (vector-length ,(syntax-object->datum ae)) ,vlen)
            ae
            (lambda (ks kf let-bound)
              (lambda (sf bv)
                (emit
                 (lambda (exp) (quasisyntax/loc
                                stx
                                (equal? (vector-length #,exp) #,vlen)))
                 ae
                 let-bound
                 sf bv kf ks))))
           (let vloop ((n 0))
             (if (= n vlen)
                 '()
                 (append
                  (render-test-list
                   (vector-ref syntax-vec n)
                   (quasisyntax/loc stx (vector-ref #,ae #,n))
                   stx)
                  (vloop (+ 1 n)))))))))

      ((box pat)
       (cons
        (make-shape-test
         `(box? ,(syntax-object->datum ae))
         ae
         (lambda (ks kf let-bound)
           (lambda (sf bv)
             (emit
              (lambda (exp) (quasisyntax/loc stx (box? #,exp)))
              ae
              let-bound
              sf bv kf ks))))
        (render-test-list
         (syntax pat)
         (quasisyntax/loc stx (unbox #,ae))
         stx)))
      (got-too-far
       (match:syntax-err
        (syntax/loc stx got-too-far)
        "syntax error in pattern"))))
