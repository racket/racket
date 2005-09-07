(module annotate mzscheme
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "contract.ss")
	   (lib "list.ss")
           (lib "etc.ss")
           (lib "match.ss")
           "marks.ss"
           "shared.ss"
           "my-macros.ss"
           "xml-box.ss"
           (prefix beginner-defined: "beginner-defined.ss"))

  
  ; CONTRACTS

  
  ; PROVIDE
  (provide/contract
   [annotate
    (-> syntax?                         ; syntax to annotate
        (opt->* ((union continuation-mark-set? false/c) 
                 break-kind?)
                (list?)
                (any/c))                 ; procedure for runtime break
        boolean?                        ; track-inferred-name?
        syntax?)]                       ; results
   #;[top-level-rewrite (-> syntax? syntax?)])

;  ;;                                              ;;;;                          ;
; ;  ;                                     ;       ;                         ;
; ;     ;   ;  ; ;;;   ; ;;;    ;;;   ; ;;;;;;     ;     ;   ;  ; ;;    ;;; ;;;; ;   ;;;   ; ;;    ;;;
; ;     ;   ;  ;;   ;  ;;   ;  ;   ;  ;;   ;       ;     ;   ;  ;;  ;  ;     ;   ;  ;   ;  ;;  ;  ;
;  ;;   ;   ;  ;    ;  ;    ;  ;   ;  ;    ;       ;;;;  ;   ;  ;   ;  ;     ;   ;  ;   ;  ;   ;  ;
;    ;  ;   ;  ;    ;  ;    ;  ;   ;  ;    ;       ;     ;   ;  ;   ;  ;     ;   ;  ;   ;  ;   ;   ;;
;    ;  ;   ;  ;    ;  ;    ;  ;   ;  ;    ;       ;     ;   ;  ;   ;  ;     ;   ;  ;   ;  ;   ;     ;
; ;  ;  ;  ;;  ;;   ;  ;;   ;  ;   ;  ;    ;       ;     ;  ;;  ;   ;  ;     ;   ;  ;   ;  ;   ;     ;
;  ;;    ;; ;  ; ;;;   ; ;;;    ;;;   ;     ;;     ;      ;; ;  ;   ;   ;;;   ;; ;   ;;;   ;   ;  ;;;
;              ;       ;
;              ;       ;                                                                               
; 
  ; wrap-struct-form

;  (define (wrap-struct-form names annotated)
;    (let* ([arg-temps (build-list (length names) get-arg-var)]
;           [struct-proc-names (cdr names)]
;           [closure-records (map (lambda (proc-name) (make-closure-record
;                                                      proc-name 
;                                                      (lambda () #f)
;                                                      (eq? proc-name (car struct-proc-names))
;                                                      #f))
;                                 struct-proc-names)]
;           [proc-arg-temp-syms (cdr arg-temp-syms)]
;           [setters (map (lambda (arg-temp-sym closure-record)
;                           `(,closure-table-put! ,arg-temp-sym ,closure-record))
;                         proc-arg-temp-syms
;                         closure-records)]
;           [full-body (append setters (list `(values ,@arg-temp-syms)))])
;      `(#%let-values ((,arg-temp-syms ,annotated)) ,@full-body)))

  
  ;;;;;;;;;;
  ;;
  ;; collapse-let-values: for the purposes of the annotater, it's easier to simply collapse let's and
  ;;  let*'s into one big let*.  The lexical-binding information attached to each variable reference
  ;;  guarantees that this won't affect correctness
  ;;
  ;;;;;;;;;;

  ;; uh... apparently this isn't used.  2005-01-15, JBC
  
  #;(define (collapse-let-values stx)
    (syntax-case stx (let-values let*-values)
      [(_ (outer-binding ...) (let-values (inner-binding ...) . bodies))
       (collapse-let-values (syntax/loc stx (let*-values (outer-binding ... inner-binding ...) . bodies)))]
      [else stx]))

  ; test exprs:
  ;  (andmap (lambda (arg-list)
  ;            (let* ([stx (car arg-list)]
  ;                   [elaborated (cadr arg-list)]
  ;                   [eval-result (caddr arg-list)]
  ;                   [collapsed (collapse-let-values (expand stx))])
  ;              (printf "~a~n~a~n~a~n~a~n" (syntax-object->datum collapsed)
  ;                      elaborated
  ;                      (eval collapsed)
  ;                      eval-result)
  ;              (and (equal? (syntax-object->datum collapsed) elaborated)
  ;                   (equal? (eval collapsed) eval-result))))
  ;          (list (list #'(let ([a 3] [b 9]) (+ a b)) '(let-values ([(a) (#%datum . 3)] [(b) (#%datum . 9)]) (#%app (#%top . +) a b)) 12)
  ;                (list #'(let* ([a 9] [b a] [c b]) c) '(let*-values ([(a) (#%datum . 9)] [(b) a] [(c) b]) c) 9)
  ;                (list #'(let ([a 3] [b 9]) (let ([b 14]) b)) '(let*-values ([(a) (#%datum . 3)] [(b) (#%datum . 9)] [(b) (#%datum . 14)]) b) 14)))
;
;  ;                         ;                      ;                                ;            
;  ;                         ;                      ;                                   ;         
; ;;;;  ;;;   ; ;;;          ;   ;;;  ;   ;   ;;;   ;      ; ;;  ;;;  ;   ;   ; ; ;; ; ;;;;  ;;;  
;  ;   ;   ;  ;;   ;         ;  ;   ; ;   ;  ;   ;  ;      ;;   ;   ; ;   ;   ; ;;   ;  ;   ;   ; 
;  ;   ;   ;  ;    ;         ;  ;   ;  ; ;   ;   ;  ;      ;    ;   ;  ; ; ; ;  ;    ;  ;   ;   ; 
;  ;   ;   ;  ;    ;  ;;;;;  ;  ;;;;;  ; ;   ;;;;;  ;      ;    ;;;;;  ; ; ; ;  ;    ;  ;   ;;;;; 
;  ;   ;   ;  ;    ;         ;  ;      ; ;   ;      ;      ;    ;      ; ; ; ;  ;    ;  ;   ;     
;  ;   ;   ;  ;;   ;         ;  ;      ;;    ;      ;      ;    ;      ; ; ; ;  ;    ;  ;   ;     
;   ;;  ;;;   ; ;;;          ;   ;;;;   ;     ;;;;  ;      ;     ;;;;   ;   ;   ;    ;   ;;  ;;;; 
;             ;                                                                                   
;             ;                                                                                   
;                                                                                                 

  
  ; top-level-rewrite : (SYNTAX-OBJECT -> SYNTAX-OBJECT)
  
  ; top-level-rewrite performs several tasks; it labels variables with their types (let-bound, lambda-bound, or non-lexical),
  ; it flags if's which could come from cond's, it labels the begins in conds with 'stepper-skip annotations
  
  ; label-var-types returns a syntax object which is identical to the original except that the variable references are labeled
  ; with the syntax-property 'stepper-binding-type, which is set to either let-bound, lambda-bound, or non-lexical.
  
  (define (top-level-rewrite stx)
    (let loop ([stx stx]
               [let-bound-bindings null]
               [cond-test (lx #f)])
      (if (or (syntax-property stx 'stepper-skip-completely)
              (syntax-property stx 'stepper-define-struct-hint))
          stx
          (let* ([recur-regular 
                  (lambda (stx)
                    (loop stx let-bound-bindings (lx #f)))]
                 [recur-with-bindings
                  (lambda (exp vars)
                    (loop exp (append vars let-bound-bindings) (lx #f)))]
                 [recur-in-cond
                  (lambda (stx new-cond-test)
                    (loop stx let-bound-bindings new-cond-test))]
                 [do-let/rec
                  (lambda (stx rec?)
                    (with-syntax ([(label ((vars rhs) ...) . bodies) stx])
                      (let* ([vars-list (apply append (map syntax->list (syntax->list (syntax (vars ...)))))]
                             [labelled-vars-list (map (lambda (var-list) (map (lambda (exp) (recur-with-bindings exp vars-list))
                                                                              (syntax->list var-list)))
                                                      (syntax->list (syntax (vars ...))))]
                             [rhs-list (if rec?
                                           (map (lambda (exp) (recur-with-bindings exp vars-list)) (syntax->list #'(rhs ...)))
                                           (map recur-regular (syntax->list #'(rhs ...))))]
                             [new-bodies (map (lambda (exp) (recur-with-bindings exp vars-list)) (syntax->list #'bodies))]
                             [new-bindings (map list labelled-vars-list rhs-list)])
                        (datum->syntax-object stx `(,#'label ,new-bindings ,@new-bodies) stx stx))))]

                 ; evaluated at runtime, using 3D code:
                 [put-into-xml-table (lambda (val)
                                     (hash-table-put! finished-xml-box-table val #t)
                                     val)]
                 
                 
                 [rewritten
                  (kernel:kernel-syntax-case stx #f
                    
                    ; cond :
                    [(if test (begin then) else-stx)
                     (let ([origin (syntax-property stx 'origin)]
                           [rebuild-if
                            (lambda (new-cond-test)
                              (let* ([new-then (recur-regular (syntax then))]
                                     [rebuilt (syntax-property
                                               (rebuild-stx `(if ,(recur-regular (syntax test))
                                                                 ,new-then
                                                                 ,(recur-in-cond (syntax else-stx) new-cond-test))
                                                            stx)
                                               'stepper-hint
                                               'comes-from-cond)])
                                ; move the stepper-else mark to the if, if it's present:
                                (if (syntax-property (syntax test) 'stepper-else)
                                    (syntax-property rebuilt 'stepper-else #t)
                                    rebuilt)))])
                       (cond [(cond-test stx) ; continuing an existing 'cond'
                              (rebuild-if cond-test)]
                             [(and origin (pair? origin) (eq? (syntax-e (car origin)) 'cond)) ; starting a new 'cond'
                              (rebuild-if (lambda (test-stx) 
                                            (and (eq? (syntax-source stx) (syntax-source test-stx))
                                                 (eq? (syntax-position stx) (syntax-position test-stx)))))]
                             [else ; not from a 'cond' at all.
                              (rebuild-stx `(if ,@(map recur-regular (list (syntax test) (syntax (begin then)) (syntax else-stx)))) stx)]))]
                    [(begin body) ; else clauses of conds; ALWAYS AN ERROR CALL
                     (cond-test stx)
                     (syntax-property stx 'stepper-skip-completely #t)]
                    
                    ; wrapper on a local.  This is necessary because teach.ss expands local into a trivial let wrapping a bunch of
                    ;  internal defines, and therefore the letrec-values on which I want to hang the 'stepper-hint doesn't yet
                    ;  exist.  So we patch it up after expansion.  And we discard the outer 'let' at the same time.
                    [(let-values () expansion-of-local)
                     (eq? (syntax-property stx 'stepper-hint) 'comes-from-local)
                     (syntax-case #`expansion-of-local (letrec-values)
                       [(letrec-values (bogus-clause clause ...) . bodies)
                        (recur-regular
                         (syntax-property #`(letrec-values (clause ...) . bodies) 'stepper-hint 'comes-from-local))]
                       [else (error 'top-level-rewrite "expected a letrec-values inside a local, given: ~e" 
                                    (syntax-object->datum #`expansion-of-local))])]
                    
                    ; let/letrec :
                    [(let-values x ...) (do-let/rec stx #f)]
                    [(letrec-values x ...) (do-let/rec stx #t)]
                    [var
                     (identifier? (syntax var))
                     (syntax-property 
                      (syntax var) 
                      'stepper-binding-type
                      (if (eq? (identifier-binding (syntax var)) 'lexical)
                          (cond [(ormap (lx (bound-identifier=? _ (syntax var))) let-bound-bindings)
                                 'let-bound]
                                [else
                                 'lambda-bound])
                          'non-lexical))]
                    
                    [else
                     (let ([content (syntax-e stx)])
                       (if (pair? content)
                           (rebuild-stx (syntax-pair-map content recur-regular) stx)
                           stx))])])
            
            (if (eq? (syntax-property stx 'stepper-xml-hint) 'from-xml-box)
                (syntax-property #`(#,put-into-xml-table #,rewritten) 
                                 'stepper-skipto
                                 (list syntax-e cdr car))
                (syntax-recertify rewritten stx (current-code-inspector) #f))))))
  
                                                 
   ;                                               
  ; ;                         ;          ;         
  ; ;    ; ;;   ; ;;    ;;;  ;;;;  ;;;  ;;;;  ;;;  
  ; ;    ;;  ;  ;;  ;  ;   ;  ;   ;   ;  ;   ;   ; 
 ;   ;   ;   ;  ;   ;  ;   ;  ;       ;  ;   ;   ; 
 ;;;;;   ;   ;  ;   ;  ;   ;  ;    ;;;;  ;   ;;;;; 
 ;   ;   ;   ;  ;   ;  ;   ;  ;   ;   ;  ;   ;     
;     ;  ;   ;  ;   ;  ;   ;  ;   ;   ;  ;   ;     
;     ;  ;   ;  ;   ;   ;;;    ;;  ;;;;;  ;;  ;;;; 
                                                   
                                                   
                                                   
  
;  oh-say-can-you-see,by-the-dawn's-early-light,what-so-proudly-we-hailed,at-the-twilight's-last-gle
;  a m i n g . W h o s e b r o a d s t r i                                                         p
;  pe s a n d b r i g h t s t a r s , t hrough-the-perilous-night,o'er-the-ramparts-we-watched,were-
;  s o g a l l a n t l y s t r e a m i n g                                                         .
;  an d t h e r o c k e t ' s r e d g l are,the-bombs-bursting-in-air,gave-proof-through-the-night,,
;  t h a t o u r f l a g w a s s t i l l t                                                         h
;  er e . O h s a y , d o e s t h a t s tar-spangled-banner-yet-wave,o'er-the-land-of-the-free,and-t
;  h e h o m e o f t h e b r a v e ? . . .                                                         .
;  .. . . . . . . . . . . . . . . . . . ............................................................
;  . . . . . . . . . . . . . . . . . . . .                                                         .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;
  
  
  ; annotate takes 
  ; a) a list of syntax expressions
  ; b) a break routine to be called at breakpoints, and
  ; c) a boolean indicating whether to store inferred names.
  ;
  
  (define (annotate expr break track-inferred-names?)

    (define binding-indexer
      (let ([binding-index 0])
        (lambda ()
          (let ([temp binding-index])
            (set! binding-index (+ binding-index 1))
            temp))))
    
    (define (normal-break)
      (break (current-continuation-marks) 'normal-break))
    
    (define (result-exp-break)
      (break (current-continuation-marks) 'result-exp-break))
    
    (define (result-value-break vals-list)
      (break (current-continuation-marks) 'result-value-break vals-list))
    
    (define (expr-finished-break vals-list)
      (break (current-continuation-marks) 'expr-finished-break vals-list))
    
    (define (double-break)
      (break (current-continuation-marks) 'double-break))
    
    (define (late-let-break . interlaced-info)
      (break (current-continuation-marks) 'late-let-break interlaced-info))
    
    ; here are the possible configurations of wcm's, pre-breaks, and breaks (not including late-let & double-breaks):
    
    ; (for full-on stepper)
    ; wcm, result-break, normal-break
    ; wcm, normal-break
    
    ; wcm-pre-break-wrap : call wcm-wrap with a pre-break on the expr
    (define (wcm-pre-break-wrap debug-info expr)
      (wcm-wrap debug-info #`(begin (#,result-exp-break) #,expr)))
    
    (define (break-wrap expr)
      #`(begin (#,normal-break) #,expr))
    
    (define (double-break-wrap expr)
      #`(begin (#,double-break) #,expr))
    
    (define (late-let-break-wrap var-names lifted-gensyms expr)
      (let* ([interlaced (apply append (map list var-names lifted-gensyms))])
        #`(begin (#,late-let-break #,@interlaced) #,expr)))
    
    (define (return-value-wrap expr)
      #`(call-with-values
         (lambda () #,expr)
         (lambda args
           (#,result-value-break args)
           (apply values args))))
    
    (define (expr-finished-break-wrap expr)
      #`(call-with-values
         (lambda () #,expr)
         (lambda args (#,expr-finished-break args) (apply values args))))
    
    (define (make-define-struct-break expr)
      (lambda ()
        (break #f 'define-struct-break (list expr))))
        
    (define (top-level-annotate/inner expr source-expr defined-name)
      (let*-2vals ([(annotated dont-care)
                    (annotate/inner expr 'all #f defined-name)]
                   [top-level-wrapped #`(with-continuation-mark #,debug-key 
                                                                #,(make-top-level-mark source-expr)
                                                                #,(expr-finished-break-wrap annotated))])
        top-level-wrapped))
    
    
    
    ; annotate/inner takes 
    ; a) an expression to annotate
    ; b) a list of all bindings which this expression is tail w.r.t. 
    ;    or 'all to indicate that this expression is tail w.r.t. _all_ bindings.
    ; d) a boolean indicating whether this expression will be the r.h.s. of a reduction
    ;    (and therefore should be broken before)
    ; g) information about the binding name of the given expression.  This is used 
    ;    to associate a name with a closure mark (though this may now be redundant)
    
    ; it returns (as a 2vals)
    ; a) an annotated s-expression
    ; b) a list of varrefs for the variables which occur free in the expression
    ;
    ;(syntax-object BINDING-SET bool bool (union #f symbol (list binding symbol)) -> 
    ;          sexp (list-of z:varref))
    
    
    
    
    
                                                         ;  ;                           
                                ;          ;             ;                              
     ;;;   ; ;;   ; ;;    ;;;  ;;;;  ;;;  ;;;;  ;;;     ;   ;  ; ;;   ; ;;    ;;;   ; ;;
    ;   ;  ;;  ;  ;;  ;  ;   ;  ;   ;   ;  ;   ;   ;    ;   ;  ;;  ;  ;;  ;  ;   ;  ;;  
        ;  ;   ;  ;   ;  ;   ;  ;       ;  ;   ;   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   
     ;;;;  ;   ;  ;   ;  ;   ;  ;    ;;;;  ;   ;;;;;   ;    ;  ;   ;  ;   ;  ;;;;;  ;   
    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;   ;  ;   ;       ;    ;  ;   ;  ;   ;  ;      ;   
    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;   ;  ;   ;       ;    ;  ;   ;  ;   ;  ;      ;   
     ;;;;; ;   ;  ;   ;   ;;;    ;;  ;;;;;  ;;  ;;;;   ;    ;  ;   ;  ;   ;   ;;;;  ;   
                                                      ;                                 
                                                      ;                                 
    
    (define annotate/inner
      ;(-> syntax? binding-set? boolean? (union false/c syntax? (list/p syntax? syntax?)) (vector/p syntax? binding-set?))
      (lambda (expr tail-bound pre-break? procedure-name-info)
        
        (cond [(syntax-property expr 'stepper-skipto)                
               (let* ([free-vars-captured #f] ; this will be set!'ed
                      ;[dont-care (printf "expr: ~a\nskipto: ~a\n" expr (syntax-property expr 'stepper-skipto))]
                      ; WARNING! I depend on the order of evaluation in application arguments here:
                      [annotated (skipto-annotate
                                  (syntax-property expr 'stepper-skipto) 
                                  expr 
                                  (lambda (subterm)
                                    (let*-2vals ([(stx free-vars) (annotate/inner subterm tail-bound pre-break? procedure-name-info)])
                                      (set! free-vars-captured free-vars)
                                      stx)))])
                 (2vals (wcm-wrap
                         skipto-mark
                         annotated)
                        free-vars-captured))]

              [(syntax-property expr 'stepper-skip-completely)
               (2vals (wcm-wrap 13 expr) null)]

              [else
               (let* ([tail-recur (lambda (expr) (annotate/inner expr tail-bound #t procedure-name-info))]
                      [non-tail-recur (lambda (expr) (annotate/inner expr null #f #f))]
                      [result-recur (lambda (expr) (annotate/inner expr null #f procedure-name-info))]
                      [set!-rhs-recur (lambda (expr name) (annotate/inner expr null #f name))]
                      [let-rhs-recur (lambda (expr binding-names dyn-index-syms)
                                       (let* ([proc-name-info 
                                               (if (not (null? binding-names))
                                                   (list (car binding-names) (car dyn-index-syms))
                                                   #f)])
                                         (annotate/inner expr null #f proc-name-info)))]
                      [lambda-body-recur (lambda (expr) (annotate/inner expr 'all #t #f))]
                      ; note: no pre-break for the body of a let; it's handled by the break for the
                      ; let itself.
                      [let-body-recur (lambda (bindings)
                                        (lambda (expr) 
                                          (annotate/inner expr (binding-set-union (list tail-bound bindings)) #f procedure-name-info)))]
                      [make-debug-info-normal (lambda (free-bindings)
                                                (make-debug-info expr tail-bound free-bindings 'none #t))]
                      [make-debug-info-app (lambda (tail-bound free-bindings label)
                                             (make-debug-info expr tail-bound free-bindings label #t))]
                      [make-debug-info-let (lambda (free-bindings binding-list let-counter)
                                             (make-debug-info expr 
                                                              (binding-set-union (list tail-bound 
                                                                                       binding-list
                                                                                       (list let-counter)))
                                                              (varref-set-union (list free-bindings 
                                                                                      binding-list
                                                                                      (list let-counter))) ; NB using bindings as varrefs
                                                              'let-body
                                                              #t))]
                      [outer-wcm-wrap (if pre-break?
                                          wcm-pre-break-wrap
                                          wcm-wrap)]
                      [wcm-break-wrap (lambda (debug-info expr)
                                        (outer-wcm-wrap debug-info (break-wrap expr)))]
                      
                      [normal-bundle
                       (lambda (free-vars annotated)
                         (2vals (outer-wcm-wrap (make-debug-info-normal free-vars)
                                                annotated)
                                free-vars))]
                      
                      [lambda-clause-abstraction
                       (lambda (clause)
                         (with-syntax ([(args-stx . bodies) clause])
                           (let*-2vals ([(annotated-body free-varrefs)
                                         ; wrap bodies in explicit begin if more than 1 user-introduced (non-skipped) bodies
                                         ; NB: CAN'T HAPPEN in beginner up through int/lambda
                                         (if (> (length (filter (lambda (clause)
                                                                  (not (syntax-property clause 'stepper-skip-completely)))
                                                                (syntax->list (syntax bodies)))) 1)
                                             (lambda-body-recur (syntax (begin . bodies)))
                                             (let*-2vals ([(annotated-bodies free-var-sets)
                                                           (2vals-map lambda-body-recur (syntax->list #`bodies))])
                                               (2vals #`(begin . #,annotated-bodies) (varref-set-union free-var-sets))))]
                                        [new-free-varrefs (varref-set-remove-bindings free-varrefs
                                                                                      (arglist-flatten #'args-stx))])
                             (2vals (datum->syntax-object #'here `(,#'args-stx ,annotated-body) #'clause) new-free-varrefs))))]
                      
                      [outer-lambda-abstraction
                       (lambda (annotated-lambda free-varrefs)
                         (let*-2vals
                             ([closure-info (make-debug-info-app 'all free-varrefs 'none)]
                              [closure-name (if track-inferred-names?
                                                (cond [(syntax? procedure-name-info) procedure-name-info]
                                                      [(pair? procedure-name-info) (car procedure-name-info)]
                                                      [else #f])
                                                #f)]
                              [closure-storing-proc
                               (opt-lambda (closure debug-info [lifted-index #f])
                                 (closure-table-put! closure (make-closure-record 
                                                              closure-name
                                                              debug-info
                                                              #f
                                                              lifted-index))
                                 closure)]
                              [inferred-name-lambda
                               (if closure-name
                                   (syntax-property annotated-lambda 'inferred-name (syntax-e closure-name))
                                   annotated-lambda)]
                              [captured
                               (cond [(pair? procedure-name-info)
                                      #`(#,closure-storing-proc #,inferred-name-lambda #,closure-info 
                                          #,(cadr procedure-name-info))]
                                     [else
                                      #`(#,closure-storing-proc #,inferred-name-lambda #,closure-info)])])
                           
                           (normal-bundle free-varrefs captured)))]
                      
                      ; The let transformation is complicated.
                      ; here's a sample transformation (not including 'break's):
                      ;(let-values ([(a b c) e1] [(d e) e2]) e3)
                      ;
                      ;turns into
                      ;
                      ;(let ([counter (<dynamic-counter-call>)])
                      ;(let-values ([(a b c d e lifter-a-1 lifter-b-2 lifter-c-3 lifter-d-4 lifter-e-5 let-counter)
                      ;              (values *unevaluated* *unevaluated* *unevaluated* *unevaluated* *unevaluated*
                      ;                      counter counter counter counter counter 0)])
                      ;  (with-continuation-mark 
                      ;   key huge-value
                      ;   (begin
                      ;     (set!-values (a b c) e1)
                      ;     (set! let-counter 1)
                      ;     (set!-values (d e) e2)
                      ;     (set! let-counter 2)
                      ;     e3))))
                      ;
                      ; note that this elaboration looks exactly like the one for letrec, and that's
                      ; okay, becuase expand guarantees that reordering them will not cause capture.
                      ; this is because a bound variable answers is considered bound by a binding only when
                      ; the pair answers true to bound-identifier=?, which is determined during (the first)
                      ; expand.
                      
                      ; another irritating point: the mark and the break that must go immediately 
                      ; around the body.  Irritating because they will be instantly replaced by
                      ; the mark and the break produced by the annotated body itself. However, 
                      ; they're necessary, because the body may not contain free references to 
                      ; all of the variables defined in the let, and thus their values are not 
                      ; known otherwise.  
                      ; whoops! hold the phone.  I think I can get away with a break before, and
                      ; a mark after, so only one of each.  groovy, eh?
                      
                      [let-abstraction
                       (lambda (stx output-identifier make-init-list)
                         (with-syntax ([(_ ([(var ...) val] ...) . bodies) stx])
                           (let*-2vals
                               ([binding-sets (map syntax->list (syntax->list #'((var ...) ...)))]
                                [binding-list (apply append binding-sets)]
                                [vals (syntax->list #'(val ...))]
                                [lifted-var-sets (map (lx (map get-lifted-var _)) binding-sets)]
                                [lifted-vars (apply append lifted-var-sets)]
                                [(annotated-vals free-varref-sets-vals)
                                 (2vals-map let-rhs-recur vals binding-sets lifted-var-sets)]
                                [(annotated-body free-varrefs-body)
                                 ((let-body-recur binding-list) 
                                  (if (= (length (syntax->list (syntax bodies))) 1)
                                      (car (syntax->list (syntax bodies)))
                                      (syntax (begin . bodies))))]
                                [free-varrefs (varref-set-remove-bindings 
                                               (varref-set-union (cons free-varrefs-body
                                                                       free-varref-sets-vals)) 
                                               binding-list)])
                             
                             
                             (let* ([counter-id #`lifting-counter]
                                    [unevaluated-list (make-init-list binding-list)]
                                    [outer-initialization
                                     #`([(#,@lifted-vars #,@binding-list #,let-counter)
                                         (values #,@(append (map (lambda (dc_binding) counter-id)
                                                                 binding-list)
                                                            unevaluated-list
                                                            (list 0)))])]
                                    [counter-clauses (build-list 
                                                      (length binding-sets)
                                                      (lambda (num)
                                                        #`(set! #,let-counter #,(+ num 1))))]
                                    [set!-clauses
                                     (map (lambda (binding-set val)
                                            #`(set!-values #,binding-set #,val))
                                          binding-sets
                                          annotated-vals)] 
                                    ; time to work from the inside out again
                                    ; without renaming, this would all be much much simpler.
                                    [wrapped-begin (outer-wcm-wrap (make-debug-info-let free-varrefs
                                                                                        binding-list
                                                                                        let-counter) 
                                                                   (double-break-wrap
                                                                    #`(begin #,@(apply append (zip set!-clauses counter-clauses)) 
                                                                             #,(late-let-break-wrap binding-list
                                                                                                    lifted-vars
                                                                                                    annotated-body))))])
                               (2vals (quasisyntax/loc 
                                       expr 
                                       (let ([#,counter-id (#,binding-indexer)])
                                         (#,output-identifier #,outer-initialization #,wrapped-begin))) 
                                      free-varrefs)))))]
                      
                      ; if-abstraction: (-> syntax? syntax? (union false/c syntax?) (values syntax? varref-set?))
                      [if-abstraction
                       (lambda (test then else) 
                         (let*-2vals
                             ([(annotated-test free-varrefs-test) 
                               (non-tail-recur test)]
                              [(annotated-then free-varrefs-then) 
                               (tail-recur then)]
                              [(annotated-else free-varrefs-else)
                               (if else
                                   (tail-recur else)
                                   (2vals #f null))]
                              [free-varrefs (varref-set-union (list free-varrefs-test 
                                                                    free-varrefs-then 
                                                                    free-varrefs-else))]
                              [annotated-if 
                               #`(begin (set! #,if-temp #,annotated-test) 
                                        (#,normal-break)
                                        #,(if else
                                              (quasisyntax/loc expr (if #,if-temp #,annotated-then #,annotated-else))
                                              (quasisyntax/loc expr (if #,if-temp #,annotated-then))))]
                              [wrapped (outer-wcm-wrap (make-debug-info-app (binding-set-union (list tail-bound (list if-temp)))
                                                                            (varref-set-union (list free-varrefs (list if-temp)))
                                                                            'none)
                                                       annotated-if)])
                           (2vals
                            (with-syntax ([test-var if-temp]
                                          [wrapped-stx wrapped]
                                          [unevaluated-stx *unevaluated*])
                              (syntax/loc expr (let ([test-var unevaluated-stx]) wrapped-stx)))
                            free-varrefs)))]
                      
                      [varref-abstraction
                       (lambda (var)
                         (let*-2vals ([free-varrefs (list var)]
                                      [varref-break-wrap
                                       (lambda ()
                                         (wcm-break-wrap (make-debug-info-normal free-varrefs)
                                                         (return-value-wrap var)))]
                                      [varref-no-break-wrap
                                       (lambda ()
                                         (outer-wcm-wrap (make-debug-info-normal free-varrefs) var))]
                                      [top-level-varref-break-wrap
                                       (lambda ()
                                         (if (memq (syntax-e var) beginner-defined:must-reduce)
                                             (varref-break-wrap)
                                             (varref-no-break-wrap)))])
                           (2vals 
                            (case (syntax-property var 'stepper-binding-type)
                              ((lambda-bound macro-bound)   (varref-no-break-wrap))
                              ((let-bound)                  (varref-break-wrap))
                              ((non-lexical) ;; is it from this module or not?
                               (match (identifier-binding var)                                 
                                 (#f (top-level-varref-break-wrap))
                                 [`(,path-index-or-symbol ,dc1 ,dc2 ,dc3 ,dc4)
                                   (if (module-path-index? path-index-or-symbol)
                                       (let-values ([(module-path dc5) (module-path-index-split path-index-or-symbol)])
                                         (if module-path 
                                             ;; not a module-local variable:
                                             (top-level-varref-break-wrap)
                                             ;; a module-local-variable:
                                             (varref-break-wrap)))
                                       (top-level-varref-break-wrap))]
                                 [else (error 'annotate "unexpected value for identifier-binding: ~v" identifier-binding)])))
                            free-varrefs)))]
                      
                      [recertifier
                       (lambda (vals)
                         (let*-2vals ([(new-expr bindings) vals])
                           (2vals (syntax-recertify new-expr expr (current-code-inspector) #f)
                                  bindings)))]
                      
                      )
                 ; find the source expression and associate it with the parsed expression
                 ;             (when (and red-exprs foot-wrap?)
                 ;               (set-expr-read! expr (find-read-expr expr)))

                 
                 (recertifier
                  (kernel:kernel-syntax-case expr #f

                  [(lambda . clause)
                    (let*-2vals ([(annotated-clause free-varrefs)
                                  (lambda-clause-abstraction (syntax clause))]
                                 [annotated-lambda
                                  (with-syntax ([annotated-clause annotated-clause])
                                    (syntax/loc expr (lambda . annotated-clause)))])
                      (outer-lambda-abstraction annotated-lambda free-varrefs))]
                   
                   [(case-lambda . clauses)
                    (let*-2vals ([(annotated-cases free-varrefs-cases)
                                  (2vals-map lambda-clause-abstraction (syntax->list (syntax clauses)))]
                                 [annotated-case-lambda (with-syntax ([annotated-cases annotated-cases])
                                                          (syntax/loc expr (case-lambda . annotated-cases)))]
                                 [free-varrefs (varref-set-union free-varrefs-cases)])
                      (outer-lambda-abstraction annotated-case-lambda free-varrefs))]
                   
                   
                   
                   [(if test then else) (if-abstraction (syntax test) (syntax then) (syntax else))]
                   [(if test then) (if-abstraction (syntax test) (syntax then) #f)]
                   
                   [(begin . bodies-stx)
                    (if (null? (syntax->list (syntax bodies-stx)))
                        (normal-bundle null expr)
                        (let*-2vals 
                            ([reversed-bodies (reverse (syntax->list (syntax bodies-stx)))]
                             [last-body (car reversed-bodies)]
                             [all-but-last (reverse (cdr reversed-bodies))]
                             [(annotated-a free-varrefs-a)
                              (2vals-map non-tail-recur all-but-last)]
                             [(annotated-final free-varrefs-final)
                              (tail-recur last-body)])
                          (normal-bundle (varref-set-union (cons free-varrefs-final free-varrefs-a))
                                         (quasisyntax/loc expr (begin #,@annotated-a #,annotated-final)))))]
                   
                   [(begin0 . bodies-stx)
                    (let*-2vals
                        ([bodies (syntax->list (syntax bodies-stx))]
                         [(annotated-first free-varrefs-first)
                          (result-recur (car bodies))]
                         [(annotated-bodies free-varref-sets)
                          (2vals-map non-tail-recur (cdr bodies))])
                      (normal-bundle (varref-set-union (cons free-varrefs-first free-varref-sets))
                                     (quasisyntax/loc expr (begin0 #,annotated-first #,@annotated-bodies))))]
                   
                   [(let-values . _)
                    (let-abstraction expr 
                                     #`let-values
                                     (lambda (bindings)
                                       (map (lambda (_) *unevaluated*) bindings)))]
                   
                   [(letrec-values . _)
                    (let-abstraction expr 
                                     #`letrec-values
                                     (lambda (bindings) (map (lambda (b) #`#,b) bindings)))]
                   
                   [(set! var val)
                    (let*-2vals
                        ([(annotated-val val-free-varrefs)
                          (set!-rhs-recur (syntax val) (syntax-case (syntax var) (#%top)
                                                         [(#%top . real-var) (syntax-e (syntax real-var))]
                                                         [else (syntax var)]))])
                      (normal-bundle (varref-set-union (list (list (syntax var)) val-free-varrefs))
                                     (quasisyntax/loc expr (set! #,(syntax var) #,annotated-val))))]
                   
                   
                   [(quote _)
                    (normal-bundle null expr)]
                   
                   [(quote-syntax _)
                    (normal-bundle null expr)]
                   
                   [(with-continuation-mark key mark body)
                    ;(let*-2vals ([(annotated-key free-varrefs-key)
                    ;              (non-tail-recur (syntax key))]
                    ;             [(annotated-mark free-varrefs-mark)
                    ;              (non-tail-recur (syntax mark))]
                    ;             [(annotated-body dc_free-varrefs-body)
                    ;              (result-recur (syntax body))])
                    (error 'annotate/inner "this region of code is still under construction")
                    
                    ;                                       [annotated #`(let-values ([key-temp #,*unevaluated*]
                    ;                                             [mark-temp #,*unevaluated*]
                    ;)
                    ]
                   
                   ;                                  [foot-wrap? 
                   ;                                   (wcm-wrap debug-info annotated)])
                   ;                           free-bindings))]
                   
                   ; the app form's elaboration looks like this, where M0 etc. stand for expressions, and t0 etc
                   ; are temp identifiers that do not occur in the program:
                   ; (M0 ...)
                   ;
                   ; goes to
                   ;
                   ;(let ([t0 *unevaluated*]
                   ;      ...)
                   ;  (with-continuation-mark
                   ;   debug-key
                   ;   huge-value
                   ;   (set! t0 M0)
                   ;   ...
                   ;   (with-continuation-mark
                   ;    debug-key
                   ;    much-smaller-value
                   ;    (t0 ...))))
                   ; 
                   ; 'break's are not illustrated.  An optimization is possible when all expressions M0 ... are
                   ; varrefs.  In particular (where v0 ... are varrefs):
                   ; (v0 ...)
                   ;
                   ; goes to
                   ; 
                   ; (with-continuation-mark
                   ;  debug-key
                   ;  debug-value
                   ;  (v0 ...))
                   ;
                   ; in other words, no real elaboration occurs. Note that this doesn't work as-is for the
                   ; stepper, because there's nowhere to hang the breakpoint; you want to see the break
                   ; occur after all vars have been evaluated.  I suppose you could do (wcm ... (begin v0 ... (v0 ...)))
                   ; where the second set are not annotated ... but stepper runtime is not at a premium.
                   
                   [(#%app . terms)
                    (let*-2vals
                        ([(annotated-terms free-varrefs-terms)
                          (2vals-map non-tail-recur (syntax->list (syntax terms)))]
                         [free-varrefs (varref-set-union free-varrefs-terms)])
                      (2vals
                       (let* ([arg-temps (build-list (length annotated-terms) get-arg-var)]
                              [tagged-arg-temps (map (lambda (var) (syntax-property var 'stepper-binding-type 'stepper-temp))
                                                     arg-temps)]
                              [let-clauses #`((#,tagged-arg-temps 
                                               (values #,@(map (lambda (_) *unevaluated*) tagged-arg-temps))))]
                              [set!-list (map (lambda (arg-symbol annotated-sub-expr)
                                                #`(set! #,arg-symbol #,annotated-sub-expr))
                                              tagged-arg-temps annotated-terms)]
                              [new-tail-bound (binding-set-union (list tail-bound tagged-arg-temps))]
                              [app-debug-info (make-debug-info-app new-tail-bound tagged-arg-temps 'called)]
                              [app-term (quasisyntax/loc expr #,tagged-arg-temps)]
                              [debug-info (make-debug-info-app new-tail-bound
                                                               (varref-set-union (list free-varrefs tagged-arg-temps)) ; NB using bindings as vars
                                                               'not-yet-called)]
                              [let-body (outer-wcm-wrap debug-info #`(begin #,@set!-list
                                                                            #,(break-wrap
                                                                               (wcm-wrap
                                                                                app-debug-info
                                                                                #`(if (#,in-closure-table #,(car tagged-arg-temps))
                                                                                      #,app-term
                                                                                      #,(return-value-wrap app-term))))))])
                         #`(let-values #,let-clauses #,let-body))
                       ;)
                       free-varrefs))]   
                   
                   [(#%datum . _)
                    (normal-bundle null expr)]
                   
                   [(#%top . var-stx)
                    (varref-abstraction #`var-stx)]
                   
                   [var-stx
                    (identifier? #`var-stx)
                    (varref-abstraction #`var-stx)]
                   
                   [else 
                    (error 'annotate "unexpected syntax for expression: ~v" (syntax-object->datum expr))])))])))
    
    
    ;; annotate/top-level : syntax-> syntax
    ;; expansion of teaching level language programs produces two kinds of 
    ;; expressions: modules containing all of the code in the def'ns window, and
    ;; require statements that invoke those modules.  In the first case, we must annotate
    ;; the expressions inside the top-level module, and in the second, we should just
    ;; leave it alone.
    
    (define/contract annotate/top-level
      (syntax? . -> . syntax?)
      (lambda (expr)
        (syntax-case expr (module #%plain-module-begin let-values dynamic-wind lambda)
          [(m1 n1 l1
             (pm1 . bodies))
           #`(m1 n1 l1 (pm1 #,@(map annotate/module-top-level (syntax->list #`bodies))))]
          ; the 'require' form is used for the test harness
          [(require module-name)
           expr]
          ; the 'dynamic-require' form is used by the actual expander 
          [(let-values ([(done-already?) . rest1])
                (#%app dynamic-wind
                 void
                 (lambda () . rest2)
                 (lambda () . rest3)))
           expr]
          [else (error `annotate/top-level "unexpected top-level expression: ~a\n" (syntax-object->datum expr))])))
    
    (define/contract annotate/module-top-level
      (syntax? . -> . syntax?)
      (lambda (expr)
        (cond [(syntax-property expr 'stepper-skip-completely) expr]
              [(syntax-property expr 'stepper-define-struct-hint)
               #`(begin #,expr
                        (#,(make-define-struct-break (syntax-property expr 'stepper-define-struct-hint))))]
              [(syntax-property expr 'stepper-skipto)
               (skipto-annotate (syntax-property expr 'stepper-skipto) expr annotate/module-top-level)] 
              [else 
               (syntax-case expr (#%app call-with-values define-values define-syntaxes require require-for-syntax provide begin lambda)
                 [(define-values (new-vars ...) e)
                  (let* ([name-list (syntax->list #`(new-vars ...))]
                         [defined-name (if (and (pair? name-list) (null? (cdr name-list)))
                                           (car name-list)
                                           #f)])
                    #`(define-values (new-vars ...)
                        #,(top-level-annotate/inner (top-level-rewrite #`e) expr defined-name)))]
                 [(define-syntaxes (new-vars ...) e)
                  expr]
                 [(require specs ...)
                  expr]
                 [(require-for-syntax specs ...)
                  expr]
                 [(provide specs ...)
                  expr]
                 [(begin .  bodies)
                  #`(begin #,@(map annotate/module-top-level (syntax->list #`bodies)))]
                 [(#%app call-with-values (lambda () body) print-values)
                  #`(#%app call-with-values (lambda () #,(top-level-annotate/inner (top-level-rewrite #`body) expr #f)) print-values)]
                 [any
                  (syntax-property expr 'stepper-test-suite-hint)
                  (top-level-annotate/inner (top-level-rewrite expr) expr #f)]
                 [else
                  (top-level-annotate/inner (top-level-rewrite expr) expr #f)
                  ;; the following check can't be permitted in the presence of things like test-suite cases
                  ;; which produce arbitrary expressions at the top level.
                  #;(error `annotate/module-top-level "unexpected module-top-level expression to annotate: ~a\n" (syntax-object->datum expr))])])))
    
    ; body of local
    #;(printf "input: ~a\n" expr)
    (let* ([annotated-expr (annotate/top-level expr)])
      #;(printf "annotated: \n~a\n" (syntax-object->datum annotated-expr))
      annotated-expr)))
