;; This library is used by match.ss
(module render-test-list mzscheme
  (provide render-test-list)
  
  (require (lib "etc.ss"))
  (require (lib "stx.ss" "syntax"))    
  (require (rename (lib "1.ss" "srfi") map-append append-map ))
  
  (require "match-error.ss"
           "match-helper.ss"
           "test-structure.scm"
           "coupling-and-binding.scm"
           "update-counts.scm"
           "update-binding-counts.scm"
           "reorder-tests.scm"
           "match-expander-struct.ss"

           ;; the following are only used by render-test-list
           "render-helpers.ss")

  (require-for-syntax "match-helper.ss"
                      "match-expander-struct.ss")
  
  (require-for-template
   mzscheme
   "match-expander-struct.ss"
   "match-error.ss"
   "match-helper.ss"
   "test-structure.scm"
   "coupling-and-binding.scm"
   "update-counts.scm"
   "update-binding-counts.scm"
   "reorder-tests.scm"
   
   ;; the following are only used by render-test-list
   "render-helpers.ss")
  
   ;; BEGIN SPECIAL-GENERATORS.SCM

      ;;!(function or-gen
    ;;         (form (or-gen exp orpatlist stx sf bv ks kf let-bound)
    ;;               ->
    ;;               syntax)
    ;;         (contract (syntax list syntax list list (list list -> syntax) 
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
    (define or-gen
      (lambda (exp orpatlist stx sf bv ks kf let-bound)
        (let ((rendered-list
               (map
                (lambda (pat) 
                  (cons (render-test-list pat exp stx)
                        (lambda (fail let-bound)
                          (lambda (sf bv)
                            (let ((bv (map
                                       (lambda (bind)
                                         (cons (car bind)
                                               (subst-bindings (cdr bind) 
                                                               let-bound)))
                                       bv)))
                              (ks sf bv))))))
                orpatlist)))
          (update-counts rendered-list)
          (update-binding-counts rendered-list)
          (let* ((rendered-list 
                  (reorder-all-lists rendered-list)
                  )
                 (output ((meta-couple rendered-list kf let-bound bv) sf bv)))
            output))))
    
    ;;!(function next-outer
    ;;          (form (next-outer p ae sf bv let-bound kf ks syntax bool)
    ;;                ->
    ;;                syntax)
    ;;          (contract (syntax syntax list list list (list list -> syntax)
    ;;                     (list list -> syntax) syntax bool)
    ;;                    ->
    ;;                    syntax))
    ;; The function next-outer is basically a throw-back to the next
    ;; function of the original match compiler.  It compiles a pattern
    ;; or sub-pattern of a clause and does not yield a list of
    ;; partially compiled test structs.  This function is called
    ;; inside of test constructs that cannot be eliminated because of
    ;; a related presence in the test-so-far list.  So, instead of
    ;; partially compiling patterns this function fully compiles patterns.
    (define next-outer
      (opt-lambda (p
                   ae ;; this is the actual expression
                   sf
                   bv
                   let-bound
                   kf
                   ks
                   [stx (syntax '())]
                   [opt #f])
        (next-outer-helper p ae sf bv let-bound 
                           (lambda (x) kf) (lambda (a b) ks) stx opt)))
    
    ;;!(function next-outer-helper
    ;;          (form (next-outer p ae sf bv let-bound kf-func ks-func syntax bool)
    ;;                ->
    ;;                syntax)
    ;;          (contract (syntax syntax list list list (list list -> syntax)
    ;;                     (list list -> syntax) syntax bool)
    ;;                    ->
    ;;                    syntax))
    ;; The function next-outer-helper contains the meat of next-outer
    ;; and allows the programmer to pass higher order functions
    ;; ks-func and kf-func that will be given compile time imformation
    ;; about let-bindings etc. which in turn will allow the programmer
    ;; to take advantage of this info.
    (define next-outer-helper
      (opt-lambda (p
                   ae ;; this is the actual expression
                   sf
                   bv
                   let-bound
                   kf-func
                   ks-func
                   [stx (syntax '())]
                   [opt #f])
        ;; right now this does not bind new variables
        (let ((rendered-list (render-test-list p ae stx)))
          ;; no need to reorder lists although I suspect that it may be
          ;; better to put shape tests first
          (update-binding-count rendered-list)
          ((couple-tests rendered-list ks-func kf-func let-bound) sf bv))))

      ;;!(function create-test-func
    ;;          (form (create-test-func p sf let-bound bind-map last-test)
    ;;                ->
    ;;                syntax)
    ;;          (contract (syntax list list a-list bool) -> syntax))
    ;; This function creates a runtime function that is used as an
    ;; individual test in a list of tests for the list-no-order
    ;; pattern.
    ;; <pre>
    ;; bindmap - a-list of bindings mapped to their expressions
    ;; last-test - a boolean value that indicates whether this function
    ;; is collecting one value or a list of values.</pre>
    (define (create-test-func p sf let-bound bind-map last-test)
      (quasisyntax/loc
          p
        (lambda (exp)
          #,(next-outer-helper 
             p #'exp sf '() let-bound
             (lambda (let-bound)
               (lambda (sf bv)
                 #'#f))
             (lambda (fail let-bound)
               (lambda (sf bv)
                 #`(begin
                     #,@(map (lambda (bind)
                               (let ((binding-name (get-bind-val (car bind) bind-map))
                                     (exp-to-bind 
                                      (subst-bindings (cdr bind) let-bound)))
                                 (if last-test
                                     #`(set! #,binding-name
                                             (cons #,exp-to-bind #,binding-name))
                                     #`(set! #,binding-name
                                             #,exp-to-bind))))
                             bv)
                     #t)))))))
  
  ;;!(function getbindings
    ;;          (form (getbindings pat-syntax) -> list)
    ;;          (contract syntax -> list))
    ;; This function given a pattern returns a list of pattern
    ;; variable names which are found in the pattern.
    (define (getbindings pat-syntax)
      (let/cc out
        (next-outer
         pat-syntax
         (quote-syntax dummy)
         '()
         '()
         '()
         (lambda (sf bv) #'(dummy-symbol))
         (lambda (sf bv) (out (map car bv))))))
 
     ;; END SPECIAL-GENERATORS.SCM
  
  ;; BEGIN DDK

     
  ;; END DDK


    
    
    
    
    ;; BEGIN DDK-HANDLERS.SCM
    
    
 
    
    
    ;;!(function handle-end-ddk-list
    ;;          (form (handle-end-ddk-list ae kf ks pat
    ;;                                     dot-dot-k stx
    ;;                                     let-bound)
    ;;                ->
    ;;                ((list list) -> syntax))
    ;;          (contract (syntax
    ;;                     ((list list) -> syntax)
    ;;                     ((list list) -> syntax)
    ;;                     syntax
    ;;                     syntax
    ;;                     syntax
    ;;                     list)
    ;;                    ->
    ;;                    ((list list) -> syntax)))
    ;; This returns a function which generates the code for
    ;; a pattern that ends with a ddk. This function is only applied to the
    ;; last pattern and the ddk.
    ;; Args:
    ;; ae - the expression being matched
    ;; kf - a failure function
    ;; ks - a success function
    ;; pat - the pattern to be matched repeatedly
    ;; dot-dot-k - the ddk pattern
    ;; stx - the source stx for error purposes
    ;; let-bound - a list of let bindings
    (define handle-end-ddk-list
      (lambda (ae kf ks pat dot-dot-k stx let-bound)
        (lambda (sf bv)
          (let* ((k (stx-dot-dot-k? dot-dot-k))
                 (ksucc (lambda (sf bv) 
                          (let ((bound (getbindings pat)))
                            (if (syntax? bound)
                                (kf sf bv)
                                (syntax-case pat (_)
                                  (_ (ks sf bv))
                                  (the-pat
                                   (null? bound)
                                   (with-syntax ((exp-sym (syntax exp-sym)))
                                     (let* ((ptst (next-outer
                                                   pat
                                                   (syntax exp-sym)
                                                   sf
                                                   bv
                                                   let-bound
                                                   (lambda (sf bv) (syntax #f))
                                                   (lambda (sf bv) (syntax #t))))
                                            (tst (syntax-case ptst ()
                                                   ((pred eta)
                                                    (and (identifier?
                                                          (syntax pred))
                                                         ;free-identifier=?
                                                         (stx-equal?
                                                          (syntax eta)
                                                          (syntax exp-sym)))
                                                    (syntax pred))
                                                   (whatever
                                                    (quasisyntax/loc 
                                                        stx 
                                                      (lambda (exp-sym)
                                                        #,ptst))))))
                                       (assm (quasisyntax/loc 
                                                 stx 
                                               (andmap #,tst 
                                                       #,(subst-bindings ae let-bound)))
                                             (kf sf bv)
                                             (ks sf bv)))))
                                  (id
                                   (and (identifier? (syntax id))
                                        (stx-equal? (syntax id)
                                                    (car bound)))
                                   (next-outer (syntax id) ae sf bv let-bound kf ks))
                                  (the-pat
                                   (let ((binding-list-names
                                          (map (lambda (x)
                                                 (datum->syntax-object
                                                  (quote-syntax here)
                                                  (symbol-append
                                                   (gensym (syntax-object->datum x))
                                                   '-bindings)))
                                               bound))
                                         (loop-name (quasisyntax/loc 
                                                        (syntax the-pat) 
                                                      #,(gensym 'loop)))
                                         (exp-name (quasisyntax/loc 
                                                       (syntax the-pat) 
                                                     #,(gensym 'exp))))
                                     (quasisyntax/loc
                                         stx
                                       (let #,loop-name
                                         ((#,exp-name #,(subst-bindings ae let-bound))
                                          #,@(map
                                              (lambda (x)
                                                (quasisyntax/loc
                                                    stx
                                                  (#,x '())))
                                              binding-list-names))
                                         (if (null? #,exp-name)
                                             #,(ks sf
                                                   (append
                                                    (map cons
                                                         bound
                                                         (map
                                                          (lambda (x)
                                                            (quasisyntax/loc 
                                                                stx 
                                                              (reverse #,x)))
                                                          binding-list-names))
                                                    bv))
                                             #,(next-outer (syntax the-pat)
                                                           (quasisyntax/loc 
                                                               (syntax the-pat) 
                                                             (car #,exp-name))
                                                           sf
                                                           bv  ;; we always start
                                                           ;; over with the old
                                                           ;; bindings
                                                           let-bound
                                                           kf
                                                           (lambda (sf bv)
                                                             (quasisyntax/loc
                                                                 stx
                                                               (#,loop-name
                                                                  (cdr #,exp-name)
                                                                  #,@(map
                                                                      (lambda
                                                                          (b-var
                                                                           bindings-var)
                                                                        (quasisyntax/loc
                                                                            stx
                                                                          (cons
                                                                           #,(get-bind-val
                                                                              b-var
                                                                              bv)
                                                                           #,bindings-var)))
                                                                      bound binding-list-names))))))))))))))))
            (case k
              ((0) (ksucc sf bv))
              ((1) (emit (lambda (exp) (quasisyntax/loc stx (pair? #,exp)))
                         ae
                         let-bound
                         sf bv kf ksucc))
              (else (emit (lambda (exp) (quasisyntax/loc stx (>= (length #,exp) #,k)))
                          ae
                          let-bound
                          sf bv kf ksucc)))))))
    
    ;;!(function handle-inner-ddk-list
    ;;          (form (handle-inner-ddk-list ae kf ks pat
    ;;                                     dot-dot-k pat-rest stx
    ;;                                     let-bound)
    ;;                ->
    ;;                ((list list) -> syntax))
    ;;          (contract (syntax
    ;;                     ((list list) -> syntax)
    ;;                     ((list list) -> syntax)
    ;;                     syntax
    ;;                     syntax
    ;;                     syntax
    ;;                     syntax
    ;;                     list)
    ;;                    ->
    ;;                    ((list list) -> syntax)))
    ;; This returns a function which generates the code for a list
    ;; pattern that contains with a ddk that occurs before the end of
    ;; the list. This code is extremely similar to the code in
    ;; handle-end-ddk-list but there are enough differences to warrant
    ;; having a separate method for readability.
    ;; Args:
    ;; ae - the expression being matched
    ;; kf - a failure function
    ;; ks - a success function
    ;; pat - the pattern that preceeds the ddk
    ;; dot-dot-k - the ddk pattern
    ;; pat-rest - the rest of the list pattern that occurs after the ddk
    ;; stx - the source stx for error purposes
    ;; let-bound - a list of let bindings
    (define handle-inner-ddk-list
      (lambda (ae kf ks pat dot-dot-k pat-rest stx let-bound)
        (lambda (sf bv)
          (let* ((k (stx-dot-dot-k? dot-dot-k)))
            (let ((bound (getbindings pat)))
              (if (syntax? bound)
                  (kf sf bv)
                  (syntax-case pat (_)
                    (_
                     (stx-null? pat-rest)
                     (ks sf bv))
                    (the-pat
                     (null? bound)
                     (with-syntax ((exp-sym (syntax exp-sym)))
                       (let* ((ptst (next-outer
                                     pat
                                     (syntax exp-sym)
                                     sf
                                     bv
                                     let-bound
                                     (lambda (sf bv) (syntax #f))
                                     (lambda (sf bv) (syntax #t))))
                              (tst (syntax-case ptst ()
                                     ((pred eta)
                                      (and (identifier?
                                            (syntax pred))
                                           ;free-identifier=?
                                           (stx-equal?
                                            (syntax eta)
                                            (syntax exp-sym)))
                                      (syntax pred))
                                     (whatever
                                      (quasisyntax/loc stx (lambda (exp-sym)
                                                             #,ptst)))))
                              (loop-name (gensym 'ddnnl))
                              (exp-name (gensym 'exp))
                              (count-name (gensym 'count)))
                         (quasisyntax/loc
                             (syntax the-pat)
                           (let #,loop-name ((#,exp-name 
                                                #,(subst-bindings ae let-bound))
                                             (#,count-name 0))
                             (if (and (not (null? #,exp-name))
                                      ;; added for improper ddk
                                      (pair? #,exp-name)
                                      (#,tst (car #,exp-name)))
                                 (#,loop-name (cdr #,exp-name) 
                                    (add1 #,count-name))
                                 ;; testing the count is not neccessary 
                                 ;; if the count is zero
                                 #,(let ((succ (next-outer
                                                pat-rest
                                                (quasisyntax/loc
                                                    (syntax the-pat) #,exp-name)
                                                sf
                                                bv
                                                let-bound
                                                kf
                                                ks)))
                                     (if (zero? k)
                                         succ
                                         (quasisyntax/loc
                                             (syntax the-pat)
                                           (if (>= #,count-name #,k)
                                               #,succ
                                               #,(kf sf bv)))))))))))
                    (the-pat
                     (let* ((binding-list-names
                             (map (lambda (x)
                                    (datum->syntax-object
                                     (quote-syntax here)
                                     (symbol-append
                                      (gensym (syntax-object->datum x))
                                      '-bindings)))
                                  bound))
                            (loop-name (quasisyntax/loc 
                                           (syntax the-pat) 
                                         #,(gensym 'loop)))
                            (exp-name (quasisyntax/loc 
                                          (syntax the-pat) 
                                        #,(gensym 'exp)))
                            (fail-name (quasisyntax/loc 
                                           (syntax the-pat) 
                                         #,(gensym 'fail)))
                            (count-name (quasisyntax/loc 
                                            (syntax the-pat) 
                                          #,(gensym 'count)))
                            (new-bv (append
                                     (map cons
                                          bound
                                          (map
                                           (lambda (x)
                                             (quasisyntax/loc stx (reverse #,x)))
                                           binding-list-names)) bv)))
                       (quasisyntax/loc
                           (syntax the-pat)
                         (let #,loop-name
                           ((#,exp-name #,(subst-bindings ae let-bound))
                            (#,count-name 0)
                            #,@(map
                                (lambda (x) (quasisyntax/loc 
                                                (syntax the-pat) 
                                              (#,x '())))
                                binding-list-names))
                           (let ((#,fail-name
                                    (lambda ()
                                      #,(let ((succ (next-outer
                                                     pat-rest
                                                     (quasisyntax/loc 
                                                         (syntax the-pat) 
                                                       #,exp-name)
                                                     sf
                                                     new-bv
                                                     let-bound
                                                     kf
                                                     ks)))
                                          (if (zero? k)
                                              succ
                                              (quasisyntax/loc 
                                                  (syntax the-pat) 
                                                (if (>= #,count-name #,k)
                                                    #,succ
                                                    #,(kf sf new-bv))))))))
                             (if (or (null? #,exp-name)
                                     (not (pair? #,exp-name)))
                                 (#,fail-name)
                                 #,(next-outer (syntax the-pat)
                                               (quasisyntax/loc 
                                                   (syntax the-pat) 
                                                 (car #,exp-name))
                                               sf
                                               bv  ;; we always start
                                               ;; over with the old
                                               ;; bindings
                                               let-bound
                                               (lambda (sf bv)
                                                 (quasisyntax/loc 
                                                     (syntax the-pat) 
                                                   (#,fail-name)))
                                               (lambda (sf bv)
                                                 (quasisyntax/loc
                                                     stx
                                                   (#,loop-name
                                                      (cdr #,exp-name)
                                                      (add1 #,count-name)
                                                      #,@(map
                                                          (lambda
                                                              (b-var
                                                               bindings-var)
                                                            (quasisyntax/loc 
                                                                stx 
                                                              (cons
                                                               #,(get-bind-val
                                                                  b-var
                                                                  bv)
                                                               #,bindings-var)))
                                                          bound
                                                          binding-list-names))))))))))))))))))
    ;;!(function handle-ddk-vector
    ;;          (form (handle-ddk-vector ae kf ks pt let-bound)
    ;;                ->
    ;;                ((list list) -> syntax))
    ;;          (contract (syntax
    ;;                     ((list list) -> syntax)
    ;;                     ((list list) -> syntax)
    ;;                     syntax
    ;;                     list)
    ;;                    ->
    ;;                    ((list list) -> syntax)))
    ;; This returns a function which generates the code for a vector
    ;; pattern that contains a ddk that occurs at the end of the
    ;; vector.
    ;; Args:
    ;; ae - the expression being matched
    ;; kf - a failure function
    ;; ks - a success function
    ;; pt - the whole vector pattern
    ;; let-bound - a list of let bindings
    (define handle-ddk-vector
      (lambda (ae kf ks pt stx let-bound)
        (let* ((vec-stx (syntax-e pt))
               (vlen (- (vector-length vec-stx) 2)) ;; length minus
               ;; the pat ...
               (k (stx-dot-dot-k? (vector-ref vec-stx (add1 vlen))))
               (minlen (+ vlen k))
               ;; get the bindings for the second to last element:
               ;; 'pat' in pat ...
               (bound (getbindings (vector-ref vec-stx vlen)))
               (exp-name (gensym 'exnm)))
          (lambda (sf bv)
            (if (syntax? bound)
                (kf sf bv)
                (quasisyntax/loc
                    pt
                  (let ((#,exp-name #,(subst-bindings ae let-bound)))
                    #,(assm (quasisyntax/loc
                                stx
                              (>= (vector-length #,exp-name) #,minlen))
                            (kf sf bv)
                            ((let vloop ((n 0))
                               (lambda (sf bv)
                                 (cond
                                   ((not (= n vlen))
                                    (next-outer
                                     (vector-ref vec-stx n)
                                     (quasisyntax/loc
                                         stx
                                       (vector-ref #,exp-name #,n))
                                     sf
                                     bv
                                     let-bound
                                     kf
                                     (vloop (+ 1 n))))
                                   ((eq? (syntax-object->datum
                                          (vector-ref vec-stx vlen))
                                         '_)
                                    (ks sf bv))
                                   (else
                                    (let* ((binding-list-names
                                            (map (lambda (x)
                                                   (datum->syntax-object
                                                    (quote-syntax here)
                                                    (symbol-append
                                                     (gensym (syntax-object->datum x))
                                                     '-bindings)))
                                                 bound))
                                           (vloop-name (gensym 'vloop))
                                           (index-name (gensym 'index)))
                                      (quasisyntax/loc
                                          stx
                                        (let #,vloop-name
                                          ((#,index-name (- (vector-length #,exp-name) 1))
                                           #,@(map (lambda (x) 
                                                     (quasisyntax/loc stx (#,x '())))
                                                   binding-list-names))
                                          (if (> #,vlen #,index-name)
                                              #,(ks sf
                                                    (append (map cons bound
                                                                 binding-list-names)
                                                            bv))
                                              #,(next-outer
                                                 (vector-ref vec-stx n)
                                                 (quasisyntax/loc
                                                     stx
                                                   (vector-ref #,exp-name #,index-name))
                                                 sf
                                                 bv ;; we alway start over
                                                 ;; with the old bindings
                                                 let-bound
                                                 kf
                                                 (lambda (sf bv)
                                                   (quasisyntax/loc
                                                       stx (#,vloop-name
                                                              (- #,index-name 1)
                                                              #,@(map
                                                                  (lambda (b-var
                                                                           bindings-var)
                                                                    (quasisyntax/loc
                                                                        stx
                                                                      (cons
                                                                       #,(get-bind-val
                                                                          b-var
                                                                          bv)
                                                                       #,bindings-var)))
                                                                  bound
                                                                  binding-list-names)))))))))))))
                             sf
                             bv)))))))))
    
    ;;!(function handle-ddk-vector-inner
    ;;          (form (handle-ddk-vector-inner ae kf ks pt let-bound)
    ;;                ->
    ;;                ((list list) -> syntax))
    ;;          (contract (syntax
    ;;                     ((list list) -> syntax)
    ;;                     ((list list) -> syntax)
    ;;                     syntax
    ;;                     list)
    ;;                    ->
    ;;                    ((list list) -> syntax)))
    ;; This returns a function which generates the code for a vector
    ;; pattern that contains a ddk that occurs before another pattern
    ;; in the list.
    ;; Args:
    ;; ae - the expression being matched
    ;; kf - a failure function
    ;; ks - a success function
    ;; pt - the whole vector pattern
    ;; let-bound - a list of let bindings
    (define handle-ddk-vector-inner
      (lambda (ae kf ks pt stx let-bound)
        (let* ((vec-stx (syntax-e pt))
               ;; vlen as an index points at the pattern before the ddk
               (vlen (- (vector-length vec-stx) 2)) ;; length minus
               ;; the pat ...
               (vec-len (vector-length vec-stx))
               (total-k (ddk-in-vec? vec-stx pt))
               ;; (k (stx-dot-dot-k? (vector-ref vec-stx (add1 vlen))))
               (minlen (+ vec-len total-k))
               (length-of-vector-name (gensym 'lv))
               (exp-name (gensym 'exnm)))
          ;; get the bindings for the second to last element:
          ;; 'pat' in pat ...
          ;;(bound (getbindings (vector-ref vec-stx vlen))))
          ;; we have to look at the first pattern and see if a ddk follows it
          ;; if so handle that case else handle the pattern
          (lambda (sf bv)
            ;; minlen here could be the lentgh plus the k's - 1 for each ddk
            (quasisyntax/loc
                pt
              (let ((#,exp-name #,(subst-bindings ae let-bound)))
                (let ((#,length-of-vector-name (vector-length #,exp-name)))
                  #,(assm (quasisyntax/loc pt (>= #,length-of-vector-name #,minlen))
                          (kf sf bv)
                          (let ((current-index-name (gensym 'curr-ind)))
                            (quasisyntax/loc
                                pt
                              (let ((#,current-index-name 0))
                                #,((let vloop ((n 0)
                                               (count-offset-name-passover 
                                                current-index-name))
                                     (lambda (sf bv)
                                       
                                       (cond
                                         ((= n vec-len) ;; at the end of the patterns
                                          (quasisyntax/loc
                                              pt
                                            (if (>= #,count-offset-name-passover
                                                    #,length-of-vector-name)
                                                #,(ks sf bv)
                                                #,(kf sf bv))))
                                         ((stx-dot-dot-k? (vector-ref vec-stx n))  
                                          ;;this could be it
                                          (match:syntax-err
                                           stx
                                           "should not get here"))
                                         ;; if the next one is not a ddk do a normal pattern match
                                         ;; on element
                                         ((or (= n (sub1 vec-len))
                                              (not (stx-dot-dot-k? (vector-ref vec-stx
                                                                               (add1 n)))))
                                          (quasisyntax/loc
                                              pt
                                            (if (= #,count-offset-name-passover
                                                   #,length-of-vector-name)
                                                #,(kf sf bv)
                                                #,(next-outer
                                                   (vector-ref vec-stx n) ;this could be it
                                                   (quasisyntax/loc
                                                       stx
                                                     (vector-ref #,exp-name #,count-offset-name-passover))
                                                   '() ;we don't want these tests to take part in future
                                                   ; elimination or to be eliminated
                                                   bv
                                                   let-bound
                                                   kf
                                                   (lambda (bsf bv)
                                                     ;(set! current-index-name #`(add1 #,current-index-name))
                                                     (let ((cindnm (gensym 'cindnm)))
                                                       (quasisyntax/loc
                                                           pt
                                                         (let ((#,cindnm (add1 #,count-offset-name-passover)))
                                                           #,((vloop (+ 1 n) cindnm) sf bv)))))))))
                                         ((and (eq? (syntax-object->datum
                                                     (vector-ref vec-stx n)) ;this could be it
                                                    '_)
                                               (>= (- vec-len n 1)
                                                   (stx-dot-dot-k? (vector-ref vec-stx (add1 n)))))
                                          (ks sf bv))
                                         (else  ;; we now know that the next pattern is a ddk
                                          (let ((bound (getbindings (vector-ref vec-stx n))))
                                            (if (syntax? bound)
                                                (kf sf bv)
                                                (let* ((k (stx-dot-dot-k? (vector-ref vec-stx (add1 n))))
                                                       (binding-list-names
                                                        (map (lambda (x)
                                                               (datum->syntax-object
                                                                (quote-syntax here)
                                                                (symbol-append
                                                                 (gensym (syntax-object->datum x))
                                                                 '-bindings)))
                                                             bound))
                                                       (vloop-name (gensym 'vloop))
                                                       (count-name (gensym 'count))
                                                       (index-name (gensym 'index)))
                                                  (quasisyntax/loc
                                                      stx
                                                    (let #,vloop-name
                                                      ((#,count-name #,count-offset-name-passover)
                                                       #,@(map (lambda (x) (quasisyntax/loc stx (#,x '())))
                                                               binding-list-names))
                                                      #,(let ((fail-name (gensym 'fail))
                                                              (count-offset-name (gensym 'count-offset))
                                                              (index-name (gensym 'index))
                                                              )
                                                          (quasisyntax/loc
                                                              pt
                                                            (let ((#,fail-name
                                                                     (lambda (#,count-offset-name #,index-name)
                                                                       #,(let ((body ((vloop (+ n 2) index-name) sf
                                                                                                                 (append (map (lambda (b bln)
                                                                                                                                (cons b
                                                                                                                                      (quasisyntax/loc
                                                                                                                                          pt
                                                                                                                                        (reverse #,bln))))
                                                                                                                              bound
                                                                                                                              binding-list-names)
                                                                                                                         bv)
                                                                                                                 )))
                                                                           (if (> k 0)
                                                                               (quasisyntax/loc
                                                                                   pt
                                                                                 (if (>= #,count-offset-name #,k)
                                                                                     #,body
                                                                                     #,(kf sf bv)))
                                                                               body)))))
                                                              (if (= #,length-of-vector-name #,count-name)
                                                                  (#,fail-name
                                                                     (- #,count-name #,count-offset-name-passover)
                                                                     #,count-name)
                                                                  #,(next-outer
                                                                     (vector-ref vec-stx n) ;this could be it
                                                                     (quasisyntax/loc
                                                                         stx
                                                                       (vector-ref #,exp-name #,count-name))
                                                                     '() ;sf
                                                                     bv ;; we alway start over
                                                                     ;; with the old bindings
                                                                     let-bound
                                                                     (lambda (sf bv)
                                                                       (quasisyntax/loc
                                                                           pt
                                                                         (#,fail-name
                                                                            (- #,count-name
                                                                               #,count-offset-name-passover)
                                                                            #,count-name)))
                                                                     (lambda (sf bv)
                                                                       (quasisyntax/loc
                                                                           stx
                                                                         (let ((arglist
                                                                                (list
                                                                                 #,@(map
                                                                                     (lambda (b-var
                                                                                              bindings-var)
                                                                                       (quasisyntax/loc
                                                                                           stx
                                                                                         (cons
                                                                                          #,(get-bind-val
                                                                                             b-var
                                                                                             bv)
                                                                                          #,bindings-var)))
                                                                                     bound
                                                                                     binding-list-names))))
                                                                           (apply
                                                                            #,vloop-name
                                                                            (add1 #,count-name)
                                                                            arglist))))))))))))))))))
                                   sf
                                   bv))))))))))))
    
    ;; END DDK-HANDLERS.SCM
    
    ;(include "ddk-handlers.scm")
    ;(include "getter-setter.scm")
    ;(include "emit-assm.scm")
    ;(include "parse-quasi.scm")
    ;(include "pattern-predicates.scm")
  
  ;; some convenient syntax for make-reg-test and make-shape-test
  (define-syntax make-test-gen
    (syntax-rules ()
      [(_ constructor test ae emitter) (make-test-gen constructor test ae emitter ae)]
      [(_ constructor test ae emitter ae2)
       (constructor test ae
                    (lambda (ks kf let-bound)
                      (lambda (sf bv)
                        (emit emitter ae2 let-bound sf bv kf ks))))]))
  
  (define-syntax reg-test
    (syntax-rules ()
      [(_ args ...) (make-test-gen make-reg-test args ...)]))
  
  (define-syntax shape-test
    (syntax-rules ()
      [(_ args ...) (make-test-gen make-shape-test args ...)]))
  
  ;; expand the regexp-matcher into an (and) with string?
  (define-syntax regexp-matcher
    (syntax-rules ()
      [(_ ae stx pred)
       (render-test-list #'(and (? string?) pred)
                         ae stx)]))
  
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
              (identifier? (syntax ident))
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
    ;(include "special-generators.scm")
    
    
    

    (syntax-case*
        p
      (_ list quote quasiquote vector box ? app and or not struct set! var
         list-rest get! ... ___ unquote unquote-splicing
         list-no-order hash-table regexp pregexp cons) stx-equal?

      ;; this is how we extend match
      [(expander args ...)
       (and (identifier? #'expander) 
            (match-expander? (syntax-local-value #'expander (lambda () #f))))
       (let ([transformer (match-expander-plt-match-xform (syntax-local-value #'expander))])                  
         (if (not transformer)
             (match:syntax-err #'expander
                               "This expander only works with standard match.")            
             (render-test-list (transformer #'(expander args ...))
                               ae stx)))]

      ;; underscore is reserved to match nothing
      (_ '()) ;(ks sf bv let-bound))
      
      ;; plain identifiers expand into (var) patterns
      (pt
       (and (pattern-var? (syntax pt))
            (not (stx-dot-dot-k? (syntax pt))))
       (render-test-list #'(var pt) ae stx))
      
      ;; for variable patterns, we do bindings, and check if we've seen this variable before
      ((var pt)
       (identifier? (syntax pt))
       (list (make-act `bind-var-pat
                       ae
                       (lambda (ks kf let-bound)
                         (lambda (sf bv)                           
                           (cond [(ormap (lambda (x)
                                           (if (stx-equal? #'pt (car x))
                                               (cdr x) #f)) bv)
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
      ('() (emit-null ae))
         

      ;; This recognizes constants such strings
      [pt
       (let ([pt (syntax-object->datum #'pt)])
         (or (string? pt)
             (boolean? pt)
             (char? pt)
             (number? pt)))
       (list
        (reg-test 
         `(equal? ,(syntax-object->datum ae)
                  ,(syntax-object->datum (syntax pt)))
         ae (lambda (exp) #`(equal? #,exp pt))))]
      
      ;(pt
      ; (stx-? regexp? (syntax pt))
      ; (render-test-list (syntax/loc p (regex pt)) ae stx))
      
       ;; match a quoted datum
       ;; this is very similar to the previous pattern, except for the second argument to equal?
      [(quote _)
       (list 
        (reg-test
         `(equal? ,(syntax-object->datum ae)
                  ,(syntax-object->datum p))
         ae (lambda (exp) #`(equal? #,exp #,p))))]
      
      ;; I do not understand this, or why it is ever matched, but removing it causes test failures
      ('item
       (list 
        (reg-test
         `(equal? ,(syntax-object->datum ae)
                  ,(syntax-object->datum p))
         ae (lambda (exp) #`(equal? #,exp #,p)))))
      
      (`quasi-pat
        (render-test-list (parse-quasi #'quasi-pat) ae stx))
      
      
      ;; check for predicate patterns
      ;; could we check to see if a predicate is a procedure here?
      ((? pred?)
       (list (reg-test 
              `(,(syntax-object->datum #'pred?)
                 ,(syntax-object->datum ae))
              ae (lambda (exp) #`(pred? #,exp)))))
      
      ;; predicate patterns with binders are redundant with and patterns
      ((? pred? pats ...)
       (render-test-list #'(and (? pred?) pats ...) ae stx))
      
      ;; syntax checking
      ((? anything ...)
       (match:syntax-err
        p
        (if (zero? (length (syntax-e #'(anything ...))))
            "a predicate pattern must have a predicate following the ?"
            "syntax error in predicate pattern")))
      
      ((regexp reg-exp)
       (regexp-matcher ae stx (? (lambda (x) (regexp-match reg-exp x)))))
      ((pregexp reg-exp) 
       (regexp-matcher ae stx (? (lambda (x) (pregexp-match-with-error reg-exp x)))))
      ((regexp reg-exp pat)
       (regexp-matcher ae stx (app (lambda (x) (regexp-match reg-exp x)) pat)))
      ((pregexp reg-exp pat)
       (regexp-matcher ae stx (app (lambda (x) (pregexp-match-with-error reg-exp x)) pat)))
      
      ;; app patterns just apply their operation.  I'm not sure why they exist.
      ((app op pat)
       (render-test-list #'pat #`(op #,ae) stx))
      
      ;; syntax checking
      ((app . op)
       (match:syntax-err
        p
        (if (zero? (length (syntax-e #'op)))
            "an operation pattern must have a procedure following the app"
            "there should be one pattern following the operator")))
      ((and . pats)
       (let loop
         ((p #'pats))
         (syntax-case p ()
           ;; empty and always succeeds
           (() '()) ;(ks seensofar boundvars let-bound))
           ((pat . rest)
            (append (render-test-list #'pat ae stx)
                    (loop #'rest))))))
      
      ((or . pats)
       (list (make-act
              'or-pat ;`(or-pat ,(syntax-object->datum ae))
              ae
              (lambda (ks kf let-bound)
                (lambda (sf bv)
                  (or-gen ae (syntax-e #'pats)
                          stx sf bv ks kf let-bound))))))
      
      
      ((not pat)
       (list (make-act
              'not-pat ;`(not-pat ,(syntax-object->datum ae))
              ae
              (lambda (ks kf let-bound)
                (lambda (sf bv)
                  ;; swap success and fail
                  (next-outer #'pat ae sf bv let-bound ks kf))))))
      
      ;; (cons a b) == (list-rest a b)
      [(cons p1 p2) (render-test-list #'(list-rest a b) ae stx)]
      
      ;; could try to catch syntax local value error and rethrow syntax error      
      ((list-no-order pats ...)
       (if (stx-null? (syntax (pats ...)))
           (render-test-list #'(list) ae stx)
           (let* ((pat-list (syntax->list (syntax (pats ...))))
                  (ddk-list (ddk-in-list? pat-list))
                  (ddk (ddk-only-at-end-of-list? pat-list)))
             (if (or (not ddk-list)
                     (and ddk-list ddk))
                 (let* ((bound (getbindings (append-if-necc 'list
                                                            (syntax (pats ...)))))
                        (bind-map
                         (map (lambda (x)
                                (cons x #`#,(gensym (syntax-object->datum x))))
                              bound)))
                   
                   (list
                    (shape-test
                     `(list? ,(syntax-object->datum ae))
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
        (shape-test
         `(hash-table? ,(syntax-object->datum ae))
         ae (lambda (exp) #`(hash-table? #,exp)))
         
        (let ((mod-pat
               (lambda (pat)
                 (syntax-case pat ()
                   ((key value) (syntax (list key value)))
                   (ddk
                    (stx-dot-dot-k? (syntax ddk))
                    (syntax ddk))
                   (id
                    (and (pattern-var? (syntax id))
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
      
      ((hash-table . pats)
       (match:syntax-err
        p
        "improperly formed hash table pattern"))
      
      ((struct struct-name (fields ...))
       (identifier? (syntax struct-name))       
       (let*-values ([(field-pats) (syntax->list (syntax (fields ...)))]
                     [(num-of-fields) (length field-pats)]
                     [(pred accessors mutators parental-chain)
                      (struct-pred-accessors-mutators #'struct-name)]
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
                         ,(syntax-object->datum ae))
           ae (lambda (exp) #`(struct-pred #,pred #,parental-chain #,exp)))
          (map-append 
             (lambda (cur-pat cur-mutator cur-accessor) 
               (syntax-case cur-pat (set! get!)
                 [(set! . rest) 
                  (set/get-matcher 'set! ae stx (syntax rest)
                                   #`(lambda (y)
                                       (#,cur-mutator #,ae y)))]
                 [(get! . rest)
                  (set/get-matcher 'get! ae stx (syntax rest)
                                   #`(lambda ()
                                       (#,cur-accessor #,ae)))]
                 [_ (render-test-list 
                     cur-pat
                     (quasisyntax/loc stx (#,cur-accessor #,ae))
                     stx)]))
             field-pats mutators accessors))))
  
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
         `(list? ,(syntax-object->datum ae))
         ae (lambda (exp) #`(list? #,exp)))
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
      
      ;; list-rest pattern with a ooo or ook pattern
      ((list-rest pat dot-dot-k pat-rest ...)
       (and (not (or (memq (syntax-e (syntax pat))
                           '(unquote unquote-splicing ... ___))
                     (stx-dot-dot-k? (syntax pat))
                     (stx-null? (syntax (pat-rest ...)))))
            (stx-dot-dot-k? (syntax dot-dot-k)))
       (list
        (shape-test
         `(pair? ,(syntax-object->datum ae))
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
            stx
            let-bound)))))
      
      ;; list-rest pattern for improper lists
      ;; handle proper and improper lists      
      ((list-rest car-pat cdr-pat) ;pattern ;(pat1 pats ...)
       (not (or (memq (syntax-e (syntax car-pat))
                      '(unquote unquote-splicing))
                (stx-dot-dot-k? (syntax car-pat))))
       (cons
        (shape-test
         `(pair? ,(syntax-object->datum ae))
         ae (lambda (exp) #`(pair? #,exp)))
        (append
         (render-test-list (syntax car-pat)
                           (quasisyntax/loc (syntax car-pat) (car #,ae))
                           stx) ;(add-a e)
         (render-test-list
          (syntax cdr-pat)
          #`(cdr #,ae)
          stx))))
      
      ;; list-rest pattern
      ((list-rest car-pat cdr-pat ...) ;pattern ;(pat1 pats ...)
       (not (or (memq (syntax-e (syntax car-pat))
                      '(unquote unquote-splicing))
                (stx-dot-dot-k? (syntax car-pat))))
       (cons
        (shape-test
         `(pair? ,(syntax-object->datum ae))
         ae (lambda (exp) #`(pair? #,exp)))
        (append
         (render-test-list (syntax car-pat)
                           #`(car #,ae)
                           stx) ;(add-a e)
         (render-test-list
          (append-if-necc 'list-rest (syntax (cdr-pat ...)))
          #`(cdr #,ae)
          stx))))
      
      ;; general list pattern
      ((list car-pat cdr-pat ...) ;pattern ;(pat1 pats ...)
       (not (or (memq (syntax-e (syntax car-pat))
                      '(unquote unquote-splicing))
                (stx-dot-dot-k? (syntax car-pat))))
       (cons
        (shape-test
         `(pair? ,(syntax-object->datum ae))
         ae (lambda (exp) #`(pair? #,exp)))
        (append
         (render-test-list (syntax car-pat)
                           #`(car #,ae)
                           stx) ;(add-a e)
         (if (stx-null? (syntax (cdr-pat ...)))
             (list
              (shape-test
               `(null? (cdr ,(syntax-object->datum ae)))
               ae (lambda (exp) #`(null? #,exp)) #`(cdr #,ae)))
             (render-test-list
              (append-if-necc 'list (syntax (cdr-pat ...)))
              #`(cdr #,ae)
              stx)))))
      
      ;; vector pattern with ooo or ook at end
      ((vector pats ...)
       (ddk-only-at-end-of-list? (syntax-e (syntax (pats ...))))
       (list
        (shape-test
         `(vector? ,(syntax-object->datum ae))
         ae (lambda (exp) #`(vector? #,exp)))
        (make-act
         'vec-ddk-pat
         ae
         (lambda (ks kf let-bound)
           (handle-ddk-vector ae kf ks
                              #'#(pats ...)
                              stx let-bound)))))
      
      ;; vector pattern with ooo or ook, but not at end
      ((vector pats ...)
       (let* ((temp (syntax-e (syntax (pats ...))))
              (len (length temp)))
         (and (>= len 2)
              (ddk-in-list? temp)))
       ;; make this contains ddk with no ddks consecutive
       ;;(stx-dot-dot-k? (vector-ref temp (sub1 len))))))
       (list
        (shape-test
         `(vector? ,(syntax-object->datum ae))
         ae (lambda (exp) #`(vector? #,exp)))
         ;; we have to look at the first pattern and see if a ddk follows it
        ;; if so handle that case else handle the pattern
        (make-act
         'vec-ddk-pat
         ae
         (lambda (ks kf let-bound)
           (handle-ddk-vector-inner ae kf ks
                                    #'#(pats ...)
                                    stx let-bound)))))
      
      ;; plain old vector pattern
      ((vector pats ...)
       (let* ((syntax-vec (list->vector (syntax->list (syntax (pats ...)))))
              (vlen (vector-length syntax-vec)))
         (list*
          (shape-test
           `(vector? ,(syntax-object->datum ae)) ae
           (lambda (exp) #`(vector? #,exp)))
          (shape-test
           `(equal? (vector-length ,(syntax-object->datum ae)) ,vlen)
           ae (lambda (exp) #`(equal? (vector-length #,exp) #,vlen)))
          (let vloop ((n 0))
            (if (= n vlen)
                '()
                (append
                 (render-test-list
                  (vector-ref syntax-vec n)
                  #`(vector-ref #,ae #,n)
                  stx)
                 (vloop (+ 1 n))))))))
      
      ((box pat)
       (cons
        (shape-test
         `(box? ,(syntax-object->datum ae))
         ae (lambda (exp) #`(box? #,exp)))
        (render-test-list
         #'pat #`(unbox #,ae) stx)))
      
      ;; This pattern wasn't a valid form.
      (got-too-far
       (match:syntax-err
        #'got-too-far
        "syntax error in pattern"))))
  )