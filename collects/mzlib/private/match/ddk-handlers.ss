(module ddk-handlers mzscheme
  
  (provide ddk-handlers@)
  
  (require "match-error.ss"
           "match-helper.ss"
           "coupling-and-binding.scm"
           "render-helpers.ss"
           "render-sigs.ss"
           (lib "stx.ss" "syntax")
           (lib "unit.ss"))
  
  (require-for-template mzscheme
			"test-no-order.ss")
  
  (define-unit ddk-handlers@ 
    (import getbindings^ render-test-list^)
    (export ddk-handlers^)
    
    ;;!(function handle-end-ddk-list
    ;;          (form (handle-end-ddk-list ae kf ks pat
    ;;                                     dot-dot-k
    ;;                                     let-bound)
    ;;                ->
    ;;                ((list list) -> syntax))
    ;;          (contract (syntax
    ;;                     ((list list) -> syntax)
    ;;                     ((list list) -> syntax)
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
    ;; let-bound - a list of let bindings
    (define ((handle-end-ddk-list ae kf ks pat dot-dot-k let-bound cert) sf bv)
      (define k (stx-dot-dot-k? dot-dot-k))
      (define (ksucc sf bv) 
        (let ([bound (getbindings pat cert)])
          (if (syntax? bound)
              (kf sf bv)
              (syntax-case pat (_)
                [_ (ks sf bv)]
                [the-pat
                 (null? bound)
                 (with-syntax ([exp-sym #'exp-sym])
                   (let* ([ptst (next-outer
                                 pat
                                 #'exp-sym
                                 sf
                                 bv
                                 let-bound
                                 (lambda (sf bv) #'#f)
                                 (lambda (sf bv) #'#t)
                                 cert)]
                          [tst (syntax-case ptst ()
                                 [(pred eta)
                                  (and (identifier? #'pred)
                                       ;free-identifier=?
                                       (stx-equal? #'eta #'exp-sym))
                                  #'pred]
                                 [_ #`(lambda (exp-sym) #,ptst)])])
                     (assm #`(andmap #,tst #,(subst-bindings ae let-bound))
                           (kf sf bv)
                           (ks sf bv))))]
                [id
                 (and (identifier? #'id) (stx-equal? #'id (car bound)))
                 (next-outer #'id ae sf bv let-bound kf ks cert)]
                [the-pat
                 (let ([binding-list-names (generate-temporaries bound)]
                       (loop-name (gensym 'loop))
                       (exp-name (gensym 'exp)))
                   #`(let #,loop-name
                       ((#,exp-name #,(subst-bindings ae let-bound))
                        #,@(map
                            (lambda (x)
                              #`(#,x '()))
                            binding-list-names))
                       (if (null? #,exp-name)
                           #,(ks sf (append (map cons bound
                                                 (map 
                                                  (lambda (x) #`(reverse #,x))
                                                  binding-list-names))
                                            bv))
                           #,(next-outer #'the-pat
                                         #`(car #,exp-name)
                                         sf
                                         bv  ;; we always start
                                         ;; over with the old
                                         ;; bindings
                                         let-bound
                                         kf
                                         (lambda (sf bv)
                                           #`(#,loop-name
                                                (cdr #,exp-name)
                                                #,@(map
                                                    (lambda
                                                        (b-var
                                                         bindings-var)
                                                      #`(cons
                                                         #,(get-bind-val
                                                            b-var
                                                            bv)
                                                         #,bindings-var))
                                                    bound binding-list-names)))
                                         cert))))]))))
      (define (new-emit f) (emit f ae let-bound sf bv kf ksucc))
      (case k
        ((0) (ksucc sf bv))
        ((1) (new-emit (lambda (exp) #`(pair? #,exp))))
        (else (new-emit (lambda (exp) #`(>= (length #,exp) #,k))))))
    
    ;;!(function handle-inner-ddk-list
    ;;          (form (handle-inner-ddk-list ae kf ks pat
    ;;                                     dot-dot-k pat-rest
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
    ;; let-bound - a list of let bindings
    (define ((handle-inner-ddk-list ae kf ks pat dot-dot-k pat-rest let-bound cert) sf bv)
      (let* ((k (stx-dot-dot-k? dot-dot-k)))
        (let ((bound (getbindings pat cert)))
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
                                 #'exp-sym
                                 sf
                                 bv
                                 let-bound
                                 (lambda (sf bv) #'#f)
                                 (lambda (sf bv) #'#t)
                                 cert))
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
                                  #`(lambda (exp-sym) #,ptst))))
                          (loop-name (gensym 'ddnnl))
                          (exp-name (gensym 'exp))
                          (count-name (gensym 'count)))
                     #`(let #,loop-name ((#,exp-name 
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
                                            #`#,exp-name
                                            sf
                                            bv
                                            let-bound
                                            kf
                                            ks
                                            cert)))
                                 (if (zero? k)
                                     succ
                                     #`(if (>= #,count-name #,k)
                                           #,succ
                                           #,(kf sf bv)))))))))
                (the-pat
                 (let* ([binding-list-names (generate-temporaries bound)]
                        (loop-name #`#,(gensym 'loop))
                        (exp-name #`#,(gensym 'exp))
                        (fail-name #`#,(gensym 'fail))
                        (count-name #`#,(gensym 'count))
                        (new-bv (append (map cons bound
                                             (map (lambda (x) #`(reverse #,x))
                                                  binding-list-names)) 
                                        bv)))
                   #`(let #,loop-name
                       ((#,exp-name #,(subst-bindings ae let-bound))
                        (#,count-name 0)
                        #,@(map
                            (lambda (x) #`(#,x '()))
                            binding-list-names))
                       (let ((#,fail-name
                                (lambda ()
                                  #,(let ((succ (next-outer
                                                 pat-rest
                                                 #`#,exp-name
                                                 sf
                                                 new-bv
                                                 let-bound
                                                 kf
                                                 ks
                                                 cert)))
                                      (if (zero? k)
                                          succ
                                          #`(if (>= #,count-name #,k)
                                                #,succ
                                                #,(kf sf new-bv)))))))
                         (if (or (null? #,exp-name)
                                 (not (pair? #,exp-name)))
                             (#,fail-name)
                             #,(next-outer #'the-pat
                                           #`(car #,exp-name)
                                           sf
                                           bv  ;; we always start
                                           ;; over with the old
                                           ;; bindings
                                           let-bound
                                           (lambda (sf bv)
                                             #`(#,fail-name))
                                           (lambda (sf bv)
                                             #`(#,loop-name
                                                  (cdr #,exp-name)
                                                  (add1 #,count-name)
                                                  #,@(map
                                                      (lambda
                                                          (b-var
                                                           bindings-var)
                                                        #`(cons
                                                           #,(get-bind-val
                                                              b-var
                                                              bv)
                                                           #,bindings-var))
                                                      bound
                                                      binding-list-names)))
                                           cert)))))))))))
    ;;!(function handle-ddk-vector
    ;;          (form (handle-ddk-vector ae kf ks let-bound)
    ;;                ->
    ;;                ((list list) -> syntax))
    ;;          (contract (syntax
    ;;                     ((list list) -> syntax)
    ;;                     ((list list) -> syntax)
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
    (define (handle-ddk-vector ae kf ks pt let-bound cert)
      (let* ((vec-stx (syntax-e pt))
             (vlen (- (vector-length vec-stx) 2)) ;; length minus
             ;; the pat ...
             (k (stx-dot-dot-k? (vector-ref vec-stx (add1 vlen))))
             (minlen (+ vlen k))
             ;; get the bindings for the second to last element:
             ;; 'pat' in pat ...
             (bound (getbindings (vector-ref vec-stx vlen) cert))
             (exp-name (gensym 'exnm)))
        (lambda (sf bv)
          (if (syntax? bound)
              (kf sf bv)
              (quasisyntax/loc
                  pt
                (let ((#,exp-name #,(subst-bindings ae let-bound)))
                  #,(assm #`(>= (vector-length #,exp-name) #,minlen)
                          (kf sf bv)
                          ((let vloop ((n 0))
                             (lambda (sf bv)
                               (cond
                                 ((not (= n vlen))
                                  (next-outer
                                   (vector-ref vec-stx n)
                                   #`(vector-ref #,exp-name #,n)
                                   sf
                                   bv
                                   let-bound
                                   kf
                                   (vloop (+ 1 n))
                                   cert))
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
                                    #`(let #,vloop-name
                                        ((#,index-name (- (vector-length #,exp-name) 1))
                                         #,@(map (lambda (x) #`(#,x '()))
                                                 binding-list-names))
                                        (if (> #,vlen #,index-name)
                                            #,(ks sf
                                                  (append (map cons bound
                                                               binding-list-names)
                                                          bv))
                                            #,(next-outer
                                               (vector-ref vec-stx n)
                                               #`(vector-ref #,exp-name #,index-name)
                                               sf
                                               bv ;; we alway start over
                                               ;; with the old bindings
                                               let-bound
                                               kf
                                               (lambda (sf bv)
                                                 #`(#,vloop-name
                                                      (- #,index-name 1)
                                                      #,@(map
                                                          (lambda (b-var
                                                                   bindings-var)
                                                            #`(cons
                                                               #,(get-bind-val
                                                                  b-var
                                                                  bv)
                                                               #,bindings-var))
                                                          bound
                                                          binding-list-names)))
                                               cert))))))))
                           sf
                           bv))))))))
    
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
    (define (handle-ddk-vector-inner ae kf ks pt let-bound cert)
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
        ;;(bound (getbindings (vector-ref vec-stx vlen) cert)))
        ;; we have to look at the first pattern and see if a ddk follows it
        ;; if so handle that case else handle the pattern
        (lambda (sf bv)
          ;; minlen here could be the lentgh plus the k's - 1 for each ddk
          #`(let ((#,exp-name #,(subst-bindings ae let-bound)))
              (let ((#,length-of-vector-name (vector-length #,exp-name)))
                #,(assm #`(>= #,length-of-vector-name #,minlen)
                        (kf sf bv)
                        (let ((current-index-name (gensym 'curr-ind)))
                          #`(let ((#,current-index-name 0))
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
                                         pt
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
                                                 #`(vector-ref #,exp-name #,count-offset-name-passover)
                                                 '() ;we don't want these tests to take part in future
                                                 ; elimination or to be eliminated
                                                 bv
                                                 let-bound
                                                 kf
                                                 (lambda (bsf bv)
                                                   ;(set! current-index-name #`(add1 #,current-index-name))
                                                   (let ((cindnm (gensym 'cindnm)))
                                                     #`(let ((#,cindnm (add1 #,count-offset-name-passover)))
                                                         #,((vloop (+ 1 n) cindnm) sf bv))))
                                                 cert))))
                                       ((and (eq? (syntax-object->datum
                                                   (vector-ref vec-stx n)) ;this could be it
                                                  '_)
                                             (>= (- vec-len n 1)
                                                 (stx-dot-dot-k? (vector-ref vec-stx (add1 n)))))
                                        (ks sf bv))
                                       (else  ;; we now know that the next pattern is a ddk
                                        (let ((bound (getbindings (vector-ref vec-stx n) cert)))
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
                                                #`(let #,vloop-name
                                                    ((#,count-name #,count-offset-name-passover)
                                                     #,@(map (lambda (x) #`(#,x '()))
                                                             binding-list-names))
                                                    #,(let ((fail-name (gensym 'fail))
                                                            (count-offset-name (gensym 'count-offset))
                                                            (index-name (gensym 'index))
                                                            )
                                                        #`(let ((#,fail-name
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
                                                                   #`(vector-ref #,exp-name #,count-name)
                                                                   '() ;sf
                                                                   bv ;; we alway start over
                                                                   ;; with the old bindings
                                                                   let-bound
                                                                   (lambda (sf bv)
                                                                     #`(#,fail-name
                                                                          (- #,count-name
                                                                             #,count-offset-name-passover)
                                                                          #,count-name))
                                                                   (lambda (sf bv)
                                                                     #`(let ((arglist
                                                                              (list
                                                                               #,@(map
                                                                                   (lambda (b-var
                                                                                            bindings-var)
                                                                                     #`(cons
                                                                                        #,(get-bind-val
                                                                                           b-var
                                                                                           bv)
                                                                                        #,bindings-var))
                                                                                   bound
                                                                                   binding-list-names))))
                                                                         (apply
                                                                          #,vloop-name
                                                                          (add1 #,count-name)
                                                                          arglist)))
                                                                   cert))))))))))))
                                 sf
                                 bv)))))))))
    
    ;; end of ddk-handlers@
    )
  
  )