; general assertions about reconstruction:
; a varref can only occur at the top of a mark-list
; a varref at the top of the mark-list must either be a top-level-variable
;  or have a value in some mark somewhere (or both).

(module reconstruct mzscheme
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "list.ss")
           (lib "etc.ss")
	   (lib "contract.ss")
           "marks.ss"
           "model-settings.ss"
           "shared.ss"
           "my-macros.ss"
           "lifting.ss")

  (provide/contract 
   [reconstruct-completed (-> mark-list? (listof any/c) render-settings? 
                              syntax?)]
   [reconstruct-current (-> mark-list? symbol? (listof any/c) render-settings?
                            (union (listof syntax?)
                                   (list/c (listof syntax?) (listof syntax?))))]
   [final-mark-list? (-> mark-list? boolean?)]
   [skip-step? (-> break-kind? (union mark-list? false/c) render-settings? boolean?)]
   [step-was-app? (-> mark-list? boolean?)])
  
  (define nothing-so-far (gensym "nothing-so-far-"))

  ; the let-glump is a structure that contains the reconstruct-time data about
  ; a let-binding; that is, the names on the left-hand-side, the expression on
  ; the right-hand side, and the values computed.
  
  (define-struct let-glump (name-set exp val-set))

  ; split-list : ('a -> boolean) (listof 'a) -> (2vals (listof 'a) (listof 'a))
  ; split-list splits a list into two lists at the first element s.t. (fn element) => true).
  ; that is, split-list yields the lists A and B such that (append A B) gives the original
  ; list, and (fn element) => false for all elements in A, and B is either empty or
  ; (fn (car B)) => true
  
 (define (split-list fn lst)
    (let loop ([remaining lst] [so-far null]) 
      (cond [(null? remaining)
             (2vals (reverse so-far) null)]
            [else
             (if (fn (car remaining))
                 (2vals (reverse so-far) remaining)
                 (loop (cdr remaining) (cons (car remaining) so-far)))])))
  
  ; test cases
  ; (test (2vals '(93 4 2) '(0 2 1)) split-list (lambda (x) (= x 0)) '(93 4 2 0 2 1))
  ; (test (2vals '(3 4 5) '()) split-list (lambda (x) (= x 0)) '(3 4 5))
        
  ; n-split-list : num ('a list) -> ('a list) ('a list)
  ; n-split-list splits a given list A into two lists B and C, such that B contains the
  ; first n elements of A, and C contains the rest.

  (define (n-split-list num lst)
    (when (> num (length lst))
      (error 'n-split-list "can't split list ~a after ~ath element; not long enough" lst num))
    (let loop ([count num] [remaining lst] [so-far null])
      (if (= count 0)
          (2vals (reverse so-far) remaining)
          (loop (- count 1) (cdr remaining) (cons (car remaining) so-far)))))
  
  ; test cases
  ; (test (2vals '(a b c) '(d e f)) n-split-list 3 '(a b c d e f))
  
  
  (define (mark-as-highlight stx)
    (syntax-property stx 'stepper-highlight #t))
                                                     ;               
                                                     ;               
 ; ;;  ;;;    ;;;   ;;;   ; ;;         ;   ;   ;;;   ;  ;   ;   ;;;  
 ;;   ;   ;  ;     ;   ;  ;;  ;        ;   ;  ;   ;  ;  ;   ;  ;   ; 
 ;    ;   ;  ;     ;   ;  ;   ;         ; ;       ;  ;  ;   ;  ;   ; 
 ;    ;;;;;  ;     ;   ;  ;   ;  ;;;;;  ; ;    ;;;;  ;  ;   ;  ;;;;; 
 ;    ;      ;     ;   ;  ;   ;         ; ;   ;   ;  ;  ;   ;  ;     
 ;    ;      ;     ;   ;  ;   ;         ;;    ;   ;  ;  ;  ;;  ;     
 ;     ;;;;   ;;;   ;;;   ;   ;          ;     ;;;;; ;   ;; ;   ;;;; 
                                                                     
                                                                     
  ; recon-value print-converts a value.  If the value is a closure, recon-value
  ; prints the name attached to the procedure, unless we're on the right-hand-side
  ; of a let, or unless there _is_ no name.
  
  (define recon-value
    (opt-lambda (val render-settings [assigned-name #f])
      (if (hash-table-get finished-xml-box-table val (lambda () #f))
          (syntax-property #`(#%datum . #,val) 'stepper-xml-value-hint 'from-xml-box)
          (let ([closure-record (closure-table-lookup val (lambda () #f))])     
            (if closure-record
                (let* ([mark (closure-record-mark closure-record)]
                       [base-name (closure-record-name closure-record)])
                  (if base-name
                      (let* ([lifted-index (closure-record-lifted-index closure-record)]
                             [name (if lifted-index
                                       (construct-lifted-name base-name lifted-index)
                                       base-name)])
                        (if (and assigned-name (free-identifier=? base-name assigned-name))
                            (recon-source-expr (mark-source mark) (list mark) null null render-settings)
                            #`#,name))
                      (recon-source-expr (mark-source mark) (list mark) null null render-settings)))
                (let* ([rendered ((render-settings-render-to-sexp render-settings) val)])
                  (if (symbol? rendered)
                      #`#,rendered
                      #`(#%datum . #,rendered))))))))
    
  (define (final-mark-list? mark-list)
    (and (not (null? mark-list)) (eq? (mark-label (car mark-list)) 'final)))


       ;      ;                                                                     ;;;
       ;                                                          ;                    ;
  ;;;  ;   ;  ;  ; ;;;         ;    ; ;    ; ;    ;          ;;; ;;;;  ;;;   ; ;;;     ;
 ;     ;  ;   ;  ;;   ;         ;  ;   ;  ;   ;  ;          ;     ;   ;   ;  ;;   ;    ;
 ;     ; ;    ;  ;    ;          ;;     ;;     ;;           ;     ;   ;   ;  ;    ;   ;
  ;;   ;;     ;  ;    ;  ;;;;;   ;;     ;;     ;;    ;;;;;   ;;   ;   ;;;;;  ;    ;  ;
    ;  ; ;    ;  ;    ;          ;;     ;;     ;;              ;  ;   ;      ;    ;  ;
    ;  ;  ;   ;  ;;   ;         ;  ;   ;  ;   ;  ;             ;  ;   ;      ;;   ;
 ;;;   ;   ;  ;  ; ;;;         ;    ; ;    ; ;    ;         ;;;    ;;  ;;;;  ; ;;;   ;
                 ;                                                           ;
                 ;                                                           ;

  (define (skip-step? break-kind mark-list render-settings)
    (case break-kind
      [(result-value-break)
       #f]
      [(result-exp-break)
       ;; skip if clauses that are the result of and/or reductions
       (let ([and/or-clauses-consumed (syntax-property (mark-source (car mark-list)) 'stepper-and/or-clauses-consumed)])
         (and and/or-clauses-consumed
              (> and/or-clauses-consumed 0)))]
      [(normal-break)
       (skip-redex-step? mark-list render-settings)]
      [(double-break)
       (not (render-settings-lifting? render-settings))]
      [(expr-finished-break define-struct-break late-let-break) #f]))
  
  (define (skip-redex-step? mark-list render-settings)
    
    (define (varref-skip-step? varref)
      (with-handlers ([exn:fail:contract:variable? (lambda (dc-exn) #f)])
        (let ([val (lookup-binding mark-list varref)])
          (equal? (syntax-object->interned-datum (recon-value val render-settings))
                  (syntax-object->interned-datum (case (syntax-property varref 'stepper-binding-type)
                                                   ([let-bound]
                                                    (binding-lifted-name mark-list varref))
                                                   ([non-lexical]
                                                    varref)
                                                   (else
                                                    (error 'varref-skip-step? "unexpected value for stepper-binding-type: ~e for variable: ~e\n"
                                                           (syntax-property varref 'stepper-binding-type)
                                                           varref))))))))
    
    (and (pair? mark-list)
         (let ([expr (mark-source (car mark-list))])
           (or (kernel:kernel-syntax-case expr #f
                  [id
                   (identifier? expr)
                   (case (syntax-property expr 'stepper-binding-type)
                       [(lambda-bound) #t]  ; don't halt for lambda-bound vars
                       [(let-bound)
                        (varref-skip-step? expr)]
                       [(non-lexical)
                        (varref-skip-step? expr)])]
                  [(#%top . id-stx)
                   (varref-skip-step? #`id-stx)]
                  [(#%app . terms)
                   ; don't halt for proper applications of constructors
                   (let ([fun-val (lookup-binding mark-list (get-arg-var 0))])
                     (and (procedure? fun-val)
                          (procedure-arity-includes? 
                           fun-val
                           (length (cdr (syntax->list (syntax terms)))))
                          (or (and (render-settings-constructor-style-printing? render-settings)
                                   (if (render-settings-abbreviate-cons-as-list? render-settings)
                                       (eq? fun-val (find-special-value 'list '(3)))    
                                       (and (eq? fun-val (find-special-value 'cons '(3 empty)))
                                            (second-arg-is-list? mark-list))))
                              ;(model-settings:special-function? 'vector fun-val)
                              (and (eq? fun-val void)
                                   (eq? (cdr (syntax->list (syntax terms))) null))
                              (struct-constructor-procedure? fun-val))))]
                 [else #f])))))
  
  (define (find-special-value name valid-args)
    (let ([expanded (kernel:kernel-syntax-case (expand (cons name valid-args)) #f
                      [(#%app fn . rest)
                       #`fn]
                      [else (error 'find-special-name "couldn't find expanded name for ~a" name)])])
      (eval expanded)))
  
  (define (second-arg-is-list? mark-list)
    (let ([arg-val (lookup-binding mark-list (get-arg-var 2))])
      (list? arg-val)))
    
;   ; static-binding-indexer (z:parsed -> integer)
;  
;  (define static-binding-indexer
;    (let* ([name-number-table (make-hash-table)]
;           [binding-number-table (make-hash-table-weak)])
;      (lambda (binding)
;        (cond [(hash-table-get binding-number-table binding (lambda () #f)) =>
;               (lambda (x) x)]
;              [else (let* ([orig-name (z:binding-orig-name binding)]
;                           [old-index (hash-table-get name-number-table orig-name (lambda () -1))]
;                           [new-index (+ old-index 1)])
;                      (hash-table-put! name-number-table orig-name new-index)
;                      (hash-table-put! binding-number-table binding new-index)
;                      new-index)]))))
  
  ; construct-lifted-name 
  ; (-> syntax? (union num? false/c) symbol?)
  
  (define/contract construct-lifted-name
    (-> syntax? number? syntax?)
    (lambda (binding dynamic-index)
      #`#,(string->symbol
           (string-append (symbol->string (syntax-e binding)) "_" 
                          (number->string dynamic-index)))))
  
  ; binding-lifted-name
  
  (define/contract binding-lifted-name
    (-> mark-list? syntax? syntax?)
    (lambda (mark-list binding)
      (construct-lifted-name binding (lookup-binding mark-list (get-lifted-var binding)))))

  
  (define (step-was-app? mark-list)
    (and (not (null? mark-list))
         (syntax-case (mark-source (car mark-list)) (#%app)
           [(#%app . rest)
            #t]
           [else
            #f])))
  
                                                                ;              ;  ;               
                                                                               ;                  
 ; ;;; ;;    ;;;    ;;;  ; ;;  ;;;       ;   ;  ; ;;  ;   ;   ; ;  ; ;;    ;;; ;  ;  ; ;;    ;; ; 
 ;;  ;;  ;  ;   ;  ;     ;;   ;   ;      ;   ;  ;;  ; ;   ;   ; ;  ;;  ;  ;   ;;  ;  ;;  ;  ;  ;; 
 ;   ;   ;      ;  ;     ;    ;   ;      ;   ;  ;   ;  ; ; ; ;  ;  ;   ;  ;    ;  ;  ;   ;  ;   ; 
 ;   ;   ;   ;;;;  ;     ;    ;   ;      ;   ;  ;   ;  ; ; ; ;  ;  ;   ;  ;    ;  ;  ;   ;  ;   ; 
 ;   ;   ;  ;   ;  ;     ;    ;   ;      ;   ;  ;   ;  ; ; ; ;  ;  ;   ;  ;    ;  ;  ;   ;  ;   ; 
 ;   ;   ;  ;   ;  ;     ;    ;   ;      ;  ;;  ;   ;  ; ; ; ;  ;  ;   ;  ;   ;;  ;  ;   ;  ;  ;; 
 ;   ;   ;   ;;;;;  ;;;  ;     ;;;        ;; ;  ;   ;   ;   ;   ;  ;   ;   ;;; ;  ;  ;   ;   ;; ; 
                                                                                                ; 
  
  ; unwind takes a syntax object with a single highlight, 
  ; and returns a list of syntax objects
  
  (define (unwind stx lift-at-highlight?)
    (macro-unwind (lift stx lift-at-highlight?)))
  
  ; unwind-no-highlight is really just macro-unwind, but with the 'right' interface that
  ; makes it more obvious what it does.
  ; [unwind-no-highlight (-> syntax? (listof syntax?))]
  
  (define (unwind-no-highlight stx)
    (macro-unwind (list stx)))
  
  ; unwind-only-highlight : syntax? -> (listof syntax?)
  (define (unwind-only-highlight stx)
    (unwind stx #t))
  
  (define (first-of-one x) 
    (unless (= (length x) 1)
      (error 'first-of-one "expected a list of length one in: ~v" x))
    (car x))
  
  (define (improper-member elt improper-list)
    (cond [(pair? improper-list)
           (or (eq? elt (car improper-list))
               (improper-member elt (cdr improper-list)))]
          [else
           (eq? elt improper-list)]))
  
  (define-syntax (noisy-and stx)
    (syntax-case stx ()
      [(_) #`#t]
      [(_ a b ...)
       (with-syntax ([inner (syntax/loc stx (noisy-and b ...))]
                     [error (syntax/loc #`a (error 'noisy-and "and clause failed"))])
       (syntax/loc stx (if a inner error)))]
      [else
       (error 'noisy-and "bad syntax for noisy-and")]))
  
  ;(->* (syntax? (listof syntax?)) 
  ;     (syntax? (listof syntax?)))
  
  (define (macro-unwind stxs)
    (local
        ((define (recur-on-pieces stx)
           (if (pair? (syntax-e stx))
                (datum->syntax-object stx (syntax-pair-map (syntax-e stx) inner) stx stx)
                stx))
         
         (define (inner stx)
           (define (fall-through)
             (kernel:kernel-syntax-case stx #f
                      [id
                       (identifier? stx)
                       (or (syntax-property stx 'stepper-lifted-name)
                           stx)]
                      [(define-values dc ...)
                       (unwind-define stx)]
                      [(#%app exp ...)
                       (recur-on-pieces #'(exp ...))]
                      [(#%datum . datum)
                       #'datum]
                      [(let-values . rest)
                       (unwind-mz-let stx)]
                      [(letrec-values . rest)
                       (unwind-mz-let stx)]
                      [else
                       (recur-on-pieces stx)]))
           
           (transfer-info 
            (if (syntax-property stx 'user-stepper-hint)
                (case (syntax-property stx 'user-stepper-hint)
                  
                  
                  [(comes-from-cond) (unwind-cond stx 
                                                  (syntax-property stx 'user-source)
                                                  (syntax-property stx 'user-position))]
                  
                  [(comes-from-and) (unwind-and/or stx
                                                   (syntax-property stx 'user-source)
                                                   (syntax-property stx 'user-position)
                                                   'and)]
                  
                  [(comes-from-or) (unwind-and/or stx
                                                  (syntax-property stx 'user-source)
                                                  (syntax-property stx 'user-position)
                                                  'or)]
                  
                  [(comes-from-local)
                   (unwind-local stx)]
                  
                  [(comes-from-recur)
                   (unwind-recur stx)]
                  
                  (else (fall-through)))
                (fall-through))
            stx))
         
         (define (transfer-highlight from to)
           (if (syntax-property from 'stepper-highlight)
               (syntax-property to 'stepper-highlight #t)
               to))
         
         (define (unwind-recur stx)
           (with-syntax ([(app-keywd letrec-term argval ...) stx]) ; if you use #%app, it gets captured here
             (with-syntax ([(new-argval ...) (map inner (syntax->list #`(argval ...)))])
               (let ([unwound (inner #`letrec-term)])
                 (syntax-case unwound (letrec lambda)
                   [(letrec ([loop-name (lambda (argname ...) . bodies)]) loop-name-2)
                    (unless (module-identifier=? #`loop-name #`loop-name-2)
                      (error "unexpected syntax for 'recur': ~v" stx))
                    (transfer-highlight unwound #`(recur loop-name ([argname new-argval] ...) . bodies))]
                   [else #`(#,unwound new-argval ...)])))))
         
         (define (unwind-define stx)
           (kernel:kernel-syntax-case stx #f
             [(define-values (name . others) body)
              (unless (null? (syntax-e #'others))
                (error 'reconstruct "reconstruct fails on multiple-values define: ~v\n" (syntax-object->datum stx)))
              (let* ([printed-name (or (syntax-property #`name 'stepper-lifted-name)
                                        (syntax-property #'name 'stepper-orig-name)
                                        #'name)]
                     [unwound-body (inner #'body)]
                     [define-type (syntax-property unwound-body 'user-stepper-define-type)]) ; see notes in internal-docs.txt
                (if define-type
                    (kernel:kernel-syntax-case unwound-body #f
                      [(lambda arglist lam-body ...)
                       (case define-type
                         [(shortened-proc-define)
                          (let ([proc-define-name (syntax-property unwound-body 'user-stepper-proc-define-name)])
                            (if (or (module-identifier=? proc-define-name #'name)
                                    (and (syntax-property #'name 'stepper-orig-name)
                                         (module-identifier=? proc-define-name (syntax-property #'name 'stepper-orig-name))))
                                #`(define (#,printed-name . arglist) lam-body ...)
                                #`(define #,printed-name #,unwound-body)))]
                         [(lambda-define)
                          #`(define #,printed-name #,unwound-body)]
                         [else (error 'unwind-define "unknown value for syntax property 'user-stepper-define-type: ~e" define-type)])]
                      [else (error 'unwind-define "expr with stepper-define-type is not a lambda: ~e" (syntax-object->datum unwound-body))])
                    #`(define #,printed-name #,unwound-body)))]
             [else (error 'unwind-define "expression is not a define-values: ~e" (syntax-object->datum stx))]))
         
         (define (unwind-mz-let stx)
           (with-syntax ([(label ([(var) rhs] ...) . bodies) stx])
             (with-syntax ([(rhs2 ...) (map inner (syntax->list #'(rhs ...)))]
                           [new-label (if (improper-member 'comes-from-let* (syntax-property stx 'user-stepper-hint))
                                          #`let*
                                          (case (syntax-e #'label)
                                            [(let-values) #'let]
                                            [(letrec-values) #'letrec]))]
                           [new-bodies (map inner (syntax->list #'bodies))])
               (syntax-case #`new-bodies (let*)           ; is this let and the nested one part of a let*?
                 [((let* bindings inner-body ...))
                  (and 
                   (improper-member 'comes-from-let* (syntax-property stx 'user-stepper-hint))
                   (eq? (syntax-property stx 'user-stepper-source)
                        (syntax-property (car (syntax->list #`new-bodies)) 'user-stepper-source))
                   (eq? (syntax-property stx 'user-stepper-position)
                        (syntax-property (car (syntax->list #`new-bodies)) 'user-stepper-position)))
                  #`(let* #,(append (syntax->list #`([var rhs2] ...)) (syntax->list #`bindings)) inner-body ...)]
                 [else
                  #`(new-label ([var rhs2] ...) . new-bodies)]))))
         
         (define (unwind-local stx)
           (kernel:kernel-syntax-case stx #f
             [(letrec-values ([vars exp] ...) body) ; at least through intermediate, define-values may not occur in local.
              (with-syntax ([defns (map inner (syntax->list #`((define-values vars exp) ...)))])
                  #`(local defns #,(inner #'body)))]
             [else (error 'unwind-local "expected a letrec-values, given: ~e" (syntax-object->datum stx))]))
         
         ;(define (unwind-quasiquote-the-cons-application stx)
         ;  (syntax-case (recur-on-pieces stx) ()
         ;    [(#%app the-cons . rest)
         ;     (syntax (cons . rest))]
         ;    [else
         ;     (error 'reconstruct "unexpected result for unwinding the-cons application")]))
         
         (define (unwind-cond-clause stx test-stx result-stx)
           (with-syntax ([new-test (if (syntax-property stx 'user-stepper-else)
                                       #`else
                                       (inner test-stx))]
                         [result (inner result-stx)])
             #`(new-test result)))
         
         (define (unwind-cond stx user-source user-position)
           (with-syntax ([clauses
                          (let loop ([stx stx])
                            (if (and (eq? user-source (syntax-property stx 'user-source))
                                     (eq? user-position (syntax-property stx 'user-position)))
                                (syntax-case stx (if begin #%app)
                                  [(if test result) ; the else clause disappears when it's a language-inserted else clause
                                   (list (unwind-cond-clause stx #`test #`result))]
                                  [(if test result else-clause)
                                   (cons (unwind-cond-clause stx #`test #`result)
                                         (loop (syntax else-clause)))]
                                  [(begin . rest) ; else clause appears momentarily in 'before,' even though it's a 'skip-completely'
                                   null]
                                  [else-stx
                                   (error 'unwind-cond "expected an if, got: ~e" (syntax-object->datum (syntax else-stx)))])
                                (error 'unwind-cond "expected a cond clause expansion, got: ~e" (syntax-object->datum stx))))])
             (syntax (cond . clauses))))
         
         (define (unwind-and/or stx user-source user-position label)
           (let ([clause-padder (case label
                                  [(and) #`true]
                                  [(or) #`false])])
             (with-syntax ([clauses
                            (append (build-list (syntax-property stx 'user-stepper-and/or-clauses-consumed) (lambda (dc) clause-padder))
                                    (let loop ([stx stx])
                                      (if (and (eq? user-source (syntax-property stx 'user-source))
                                               (eq? user-position (syntax-property stx 'user-position)))
                                          (syntax-case stx (if let-values #%datum)
                                            [(if part-1 part-2 part-3)
                                             (cons (inner (syntax part-1))
                                                   (case label
                                                     ((and)
                                                      (loop (syntax part-2)))
                                                     ((or)
                                                      (loop (syntax part-3)))
                                                     (else
                                                      (error 'unwind-and/or "unknown label ~a" label))))]
                                            [else (error 'unwind-and/or "syntax: ~a does not match and/or patterns" (syntax-object->datum stx))])
                                          null)))])
               #`(#,label . clauses)))))
      
      (map inner stxs)))
  
  
                                                                       
                                                                                                               
 ; ;;  ;;;    ;;;   ;;;   ; ;;           ;;;   ;;;   ;   ;  ; ;;  ;;;   ;;;           ;;;  ;    ;  ; ;;;   ; ;;
 ;;   ;   ;  ;     ;   ;  ;;  ;         ;     ;   ;  ;   ;  ;;   ;     ;   ;         ;   ;  ;  ;   ;;   ;  ;;  
 ;    ;   ;  ;     ;   ;  ;   ;         ;     ;   ;  ;   ;  ;    ;     ;   ;         ;   ;   ;;    ;    ;  ;   
 ;    ;;;;;  ;     ;   ;  ;   ;  ;;;;;   ;;   ;   ;  ;   ;  ;    ;     ;;;;;  ;;;;;  ;;;;;   ;;    ;    ;  ;   
 ;    ;      ;     ;   ;  ;   ;            ;  ;   ;  ;   ;  ;    ;     ;             ;       ;;    ;    ;  ;   
 ;    ;      ;     ;   ;  ;   ;            ;  ;   ;  ;  ;;  ;    ;     ;             ;      ;  ;   ;;   ;  ;   
 ;     ;;;;   ;;;   ;;;   ;   ;         ;;;    ;;;    ;; ;  ;     ;;;   ;;;;          ;;;; ;    ;  ; ;;;   ;   
                                                                                                   ;           
                                                                                                   ;           
                                                                                                               

  ; recon-source-expr 
  
  ; recon-source-expr produces the reconstructed version of a given source epxression, using the binding
  ; information contained in the binding-list.  This happens during reconstruction whenever we come upon
  ; expressions that we haven't yet evaluated. 
  
  ; NB: the variable 'dont-lookup' contains a list of variables whose bindings occur INSIDE the expression
  ; being evaluated, and hence do NOT yet have values.
  
  ; the 'use-lifted-names' vars are those bound by a let which does have lifted names.  it is used in
  ; rendering the lifting of a let or local to show the 'after' step, which should show the lifted names.

  (define/contract recon-source-expr 
     (-> syntax? mark-list? binding-set? binding-set? render-settings? syntax?)
     (lambda (expr mark-list dont-lookup use-lifted-names render-settings)
      (if (syntax-property expr 'stepper-skipto)
          (skipto-reconstruct
           (syntax-property expr 'stepper-skipto)
           expr
           (lambda (stx)
             (recon-source-expr stx mark-list dont-lookup use-lifted-names render-settings)))
          (if (syntax-property expr 'stepper-prim-name)
              (syntax-property expr 'stepper-prim-name)
              (let* ([recur (lambda (expr) (recon-source-expr expr mark-list dont-lookup use-lifted-names render-settings))]
                     [let-recur (lambda (expr bindings)
                                  (recon-source-expr expr mark-list (append bindings dont-lookup) use-lifted-names render-settings))]
                     
                     [recon-basic
                      (lambda ()
                        (with-syntax ([(label . bodies) expr])
                          #`(label #,@(map recur (filter-skipped (syntax->list (syntax bodies)))))))]
                     [recon-let/rec
                      (lambda (rec?)
                        (with-syntax ([(label  ((vars val) ...) body) expr])
                          (let* ([bindings (map syntax->list (syntax->list (syntax (vars ...))))]
                                 [binding-list (apply append bindings)]
                                 [recur-fn (if rec? 
                                               (lambda (expr) (let-recur expr binding-list))
                                               recur)]
                                 [right-sides (map recur-fn (syntax->list (syntax (val ...))))]
                                 [recon-body (let-recur (syntax body) binding-list)])
                            (with-syntax ([(recon-val ...) right-sides]
                                          [recon-body recon-body]
                                          [(new-vars ...) (map (lx (map (lx (if (ormap (lambda (binding)
                                                                                         (bound-identifier=? binding _))
                                                                                       use-lifted-names)
                                                                                (syntax-property _
                                                                                             'stepper-lifted-name
                                                                                             (binding-lifted-name mark-list _))
                                                                                _))
                                                                        _))
                                                               bindings)])
                              (syntax (label ((new-vars recon-val) ...) recon-body))))))]
                     [recon-lambda-clause
                      (lambda (clause)
                        (with-syntax ([(args . bodies-stx) clause])
                          (let* ([arglist (arglist-flatten #'args)]
                                 [bodies (map (lambda (body) (let-recur body arglist))
                                              (filter-skipped (syntax->list (syntax bodies-stx))))])
                            (cons (syntax args) bodies))))]
                     [recon (kernel:kernel-syntax-case expr #f
                              
                              ; lambda
                              [(lambda . clause-stx)
                               (let* ([clause (recon-lambda-clause (syntax clause-stx))])
                                 #`(lambda #,@clause))]
                              
                              ; case-lambda
                              [(case-lambda . clauses-stx)
                               (let* ([clauses (map recon-lambda-clause (syntax->list (syntax clauses-stx)))])
                                 #`(case-lambda #,@clauses))]
                              
                              ; if, begin, begin0
                              [(if test then else) (recon-basic)]
                              [(if test then) (recon-basic)]
                              [(begin . bodies) (recon-basic)]
                              [(begin0 . bodies) (recon-basic)]
                              
                              ; let-values, letrec-values
                              [(let-values . rest) (recon-let/rec #f)]
                              [(letrec-values . rest) (recon-let/rec #t)]
                              
                              ; set! : set! doesn't fit into this scheme. It would be a mistake to allow it to proceed.
                              
                              ; quote 
                              [(quote body) (recon-value (syntax-e (syntax body)) render-settings)]
                              
                              ; quote-syntax : like set!, the current stepper cannot handle quote-syntax
                              
                              ; with-continuation-mark
                              [(with-continuation-mark . rest) (recon-basic)]
                              
                              ; application
                              [(#%app . terms) (recon-basic)]
                              
                              ; #%datum
                              [(#%datum . datum) 
                               #`#,(recon-value (syntax-e #'datum) render-settings)]
                              
                              ; varref                        
                              [var-stx
                               (identifier? expr)
                               (let* ([var (syntax var-stx)])
                                 (cond [(eq? (identifier-binding var) 'lexical)
                                        ; has this varref's binding not been evaluated yet?
                                        ; (and this varref isn't in the list of must-lookups?)
                                        (if (and (ormap (lambda (binding)
                                                     (bound-identifier=? binding var))
                                                   dont-lookup)
                                                 (not (ormap (lambda (binding)
                                                               (bound-identifier=? binding var))
                                                             use-lifted-names)))
                                            var

                                            
                                            (case (syntax-property var 'stepper-binding-type)
                                              ((lambda-bound) 
                                               (recon-value (lookup-binding mark-list var) render-settings))
                                              ((macro-bound)
                                               ; for the moment, let-bound vars occur only in and/or :
                                               (recon-value (lookup-binding mark-list var) render-settings))
                                              ((top-level) var)
                                              ((let-bound)
                                               (syntax-property var
                                                                'stepper-lifted-name
                                                                (binding-lifted-name mark-list var)))
                                              ((stepper-temp)
                                               (error 'recon-source-expr "stepper-temp showed up in source?!?"))
                                              (else
                                               (error 'recon-source-expr "unknown 'stepper-binding-type property: ~a" 
                                                      (syntax-property var 'stepper-binding-type)))))]
                                       [else ; top-level-varref
                                        (fixup-name
                                         var)]))]
                              [(#%top . var)
                               (syntax var)]
                              
                              [else
                               (error 'recon-source "no matching clause for syntax: ~a" expr)])])
                (attach-info recon expr))))))
  
  
  ;; filter-skipped : (listof syntax?) -> (listof syntax?)
  ;; filter out any elements of the list with 'stepper-skip-completely set, except those with stepper-prim-name set. (HACK).
  (define (filter-skipped los)
    (filter (lambda (stx)
              (or (syntax-property stx 'stepper-prim-name)
                  (not (syntax-property stx 'stepper-skip-completely))))
            los))
 
  
  ;; mflatt: MAJOR HACK - work around the prefix on
  ;;         beginner name definitions
  (define (fixup-name s)
    (let ([m (regexp-match re:beginner: (symbol->string (syntax-e s)))])
      (if m
	  (datum->syntax-object s (string->symbol (cadr m)) s s)
	  s)))
  (define re:beginner: (regexp "^beginner:(.*)$"))
                                                                                                        ;                         ; 
                                       ;                     ;                                          ;         ;               ; 
 ; ;;  ;;;    ;;;   ;;;   ; ;;    ;;; ;;;; ; ;; ;   ;   ;;; ;;;;         ;;;   ;;;   ; ;;; ;;   ; ;;;   ;   ;;;  ;;;;  ;;;    ;;; ; 
 ;;   ;   ;  ;     ;   ;  ;;  ;  ;     ;   ;;   ;   ;  ;     ;          ;     ;   ;  ;;  ;;  ;  ;;   ;  ;  ;   ;  ;   ;   ;  ;   ;; 
 ;    ;   ;  ;     ;   ;  ;   ;  ;     ;   ;    ;   ;  ;     ;          ;     ;   ;  ;   ;   ;  ;    ;  ;  ;   ;  ;   ;   ;  ;    ; 
 ;    ;;;;;  ;     ;   ;  ;   ;   ;;   ;   ;    ;   ;  ;     ;   ;;;;;  ;     ;   ;  ;   ;   ;  ;    ;  ;  ;;;;;  ;   ;;;;;  ;    ; 
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;   ;  ;     ;          ;     ;   ;  ;   ;   ;  ;    ;  ;  ;      ;   ;      ;    ; 
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;  ;;  ;     ;          ;     ;   ;  ;   ;   ;  ;;   ;  ;  ;      ;   ;      ;   ;; 
 ;     ;;;;   ;;;   ;;;   ;   ;  ;;;    ;; ;     ;; ;   ;;;   ;;         ;;;   ;;;   ;   ;   ;  ; ;;;   ;   ;;;;   ;;  ;;;;   ;;; ; 
                                                                                                ;                                   
                                                                                                ;                                   
                                                                                                                                    

  ; reconstruct-completed : reconstructs a completed expression or definition.  
  
  (define (reconstruct-completed mark-list vals render-settings)
    (unless (and (pair? mark-list) (null? (cdr mark-list)) (eq? (mark-label (car mark-list)) 'top-level))
      (error `reconstruct-completed "expected mark-list of length one with mark having label 'top-level, got: ~a" mark-list))
    (let skipto-loop ([expr (mark-source (car mark-list))])
      (cond 
        [(syntax-property expr 'stepper-skipto) =>
         (lambda (skipto)
           (skipto-reconstruct skipto expr
                               skipto-loop))]
        [(syntax-property expr 'stepper-define-struct-hint)
         (error 'reconstruct-completed "define-structs should not be passed to reconstruct-completed")]
        [else
         (first-of-one (unwind-no-highlight
                        (kernel:kernel-syntax-case expr #f
                          [(define-values vars-stx body)
                           (let* ([vars (syntax->list #'vars-stx)]
                                  [recon-vals (map (lambda (val var) 
                                                     (recon-value val render-settings (or (syntax-property var 'stepper-lifted-name) var))) 
                                                   vals
                                                   vars)])
                             (if (= (length recon-vals) 1)
                                 (attach-info #`(define-values vars-stx #,(car recon-vals)) expr)
                                 (attach-info #'(define-values vars-stx (values #,@recon-vals)) expr)))]
                          [else
                           (let* ([recon-vals (map (lambda (val)
                                                     (recon-value val render-settings))
                                                   vals)])
                             (if (= (length recon-vals) 1)
                                 (attach-info (car recon-vals) expr)
                                 (attach-info #`(values #,@recon-vals) expr)))])))])))
  
  ; : (-> syntax? syntax? syntax?)
  (define (reconstruct-top-level source reconstructed)
    (cond 
      [(syntax-property source 'stepper-skipto) =>
       (lambda (skipto)
         (skipto-reconstruct skipto source
                             (lambda (expr)
                               (reconstruct-top-level expr reconstructed))))]
      [else
       (kernel:kernel-syntax-case source #f
          [(define-values vars-stx body)
           (attach-info #`(define-values vars-stx #,reconstructed)
                        source)]
          [else
           reconstructed])]))
  
                                                                                                                
                                                                                                                
                                                                                                                
                                       ;                     ;                                               ;  
 ; ;;  ;;;    ;;;   ;;;   ; ;;    ;;; ;;;; ; ;; ;   ;   ;;; ;;;;         ;;;  ;   ;  ; ;; ; ;;  ;;;   ; ;;  ;;;;
 ;;   ;   ;  ;     ;   ;  ;;  ;  ;     ;   ;;   ;   ;  ;     ;          ;     ;   ;  ;;   ;;   ;   ;  ;;  ;  ;  
 ;    ;   ;  ;     ;   ;  ;   ;  ;     ;   ;    ;   ;  ;     ;          ;     ;   ;  ;    ;    ;   ;  ;   ;  ;  
 ;    ;;;;;  ;     ;   ;  ;   ;   ;;   ;   ;    ;   ;  ;     ;   ;;;;;  ;     ;   ;  ;    ;    ;;;;;  ;   ;  ;  
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;   ;  ;     ;          ;     ;   ;  ;    ;    ;      ;   ;  ;  
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;  ;;  ;     ;          ;     ;  ;;  ;    ;    ;      ;   ;  ;  
 ;     ;;;;   ;;;   ;;;   ;   ;  ;;;    ;; ;     ;; ;   ;;;   ;;         ;;;   ;; ;  ;    ;     ;;;;  ;   ;   ;;
                                                                                                                
                                                                                                                
                                                                                                                
  
  ; reconstruct-current : takes a list of marks, the kind of break, and
  ; any values that may have been returned at the break point. It produces a list of sexps
  ; (the result of reconstruction) --- which may contain holes, indicated by the 
  ; highlight-placeholder --- and a list of sexps which go in the holes
  
  (define (reconstruct-current mark-list break-kind returned-value-list render-settings)
    (local
        (
         
         ; ;;  ;;;    ;;;   ;;;   ; ;;          ;  ; ;;   ; ;;    ;;;   ; ;;
         ;;   ;   ;  ;     ;   ;  ;;  ;         ;  ;;  ;  ;;  ;  ;   ;  ;;  
         ;    ;   ;  ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;   ;  ;   
         ;    ;;;;;  ;     ;   ;  ;   ;  ;;;;;  ;  ;   ;  ;   ;  ;;;;;  ;   
         ;    ;      ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;      ;   
         ;    ;      ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;      ;   
         ;     ;;;;   ;;;   ;;;   ;   ;         ;  ;   ;  ;   ;   ;;;;  ;   
         
         
         (define (recon-inner mark-list so-far)
           (let* ([recon-source-current-marks 
                   (lambda (expr)
                     (recon-source-expr expr mark-list null null render-settings))]
                  [top-mark (car mark-list)]
                  [expr (mark-source top-mark)]
                  
                  [recon-let
                   (lambda ()
                     (with-syntax ([(label ((vars rhs) ...) . bodies) expr])
                       (let*-2vals ([binding-sets (map syntax->list (syntax->list #'(vars ...)))]
                                    [binding-list (apply append binding-sets)]
                                    [glumps 
                                     (map (lambda (binding-set rhs)
                                            (make-let-glump
                                             (map (lambda (binding)
                                                    (syntax-property binding
                                                                     'stepper-lifted-name
                                                                     (binding-lifted-name mark-list binding)))
                                                 binding-set)
                                             rhs
                                             (map (lambda (arg-binding) 
                                                     (lookup-binding mark-list arg-binding))
                                                  binding-set)))
                                          binding-sets
                                          (syntax->list #`(rhs ...)))]
                                    [num-defns-done (lookup-binding mark-list let-counter)]
                                    [(done-glumps not-done-glumps)
                                     (n-split-list num-defns-done glumps)]
                                    [recon-lifted 
                                     (lambda (names expr)
                                       #`(#,names #,expr))]
                                    [before-bindings
                                     (map
                                      (lambda (glump)
                                        (let* ([name-set (let-glump-name-set glump)]
                                               [rhs-val-set (map (lambda (val)
                                                                   (if (> (length name-set) 0)
                                                                       (recon-value val render-settings (car name-set))
                                                                       (recon-value val render-settings))) 
                                                                 (let-glump-val-set glump))])
                                          (if (= (length rhs-val-set) 1)
                                              #`(#,name-set #,@rhs-val-set)
                                              #`(#,name-set (values #,rhs-val-set)))))
                                      done-glumps)]
                                    [reconstruct-remaining-def
                                     (lambda (glump)
                                       (let ([rhs-source (let-glump-exp glump)]
                                             [rhs-name-set (let-glump-name-set glump)])
                                         (recon-lifted rhs-name-set
                                                       (recon-source-current-marks rhs-source))))]
                                    [after-bindings
                                     (if (pair? not-done-glumps)
                                         (if (eq? so-far nothing-so-far)
                                             (map reconstruct-remaining-def not-done-glumps)
                                             (cons (recon-lifted (let-glump-name-set (car not-done-glumps)) so-far)
                                                   (map reconstruct-remaining-def (cdr not-done-glumps))))
                                         null)]
                                    [recon-bindings (append before-bindings after-bindings)]
                                    [rectified-bodies (map (lambda (body) (recon-source-expr body mark-list binding-list binding-list render-settings))
                                                           (syntax->list (syntax bodies)))])
                         (attach-info #`(label #,recon-bindings #,@rectified-bodies) expr))))])
             (kernel:kernel-syntax-case expr #f 
               ; variable references
               [id
                (identifier? (syntax id))
                (if (eq? so-far nothing-so-far)
                    (recon-source-current-marks expr)
                    (error 'recon-inner "variable reference given as context: ~a" expr))]
               
               [(#%top . id)
                (if (eq? so-far nothing-so-far)
                    (recon-source-current-marks expr)
                    (error 'recon-inner "variable reference given as context: ~a" expr))]
               
               ; applications
               [(#%app . terms)
                (attach-info
                 (let* ([sub-exprs (syntax->list (syntax terms))]
                        [arg-temps (build-list (length sub-exprs) get-arg-var)]
                        [arg-vals (map (lambda (arg-temp) 
                                         (lookup-binding mark-list arg-temp))
                                       arg-temps)])
                   (case (mark-label (car mark-list))
                     ((not-yet-called)
                      (let*-2vals ([(evaluated unevaluated) (split-list (lambda (x) (eq? (cadr x) *unevaluated*))
                                                                        (zip sub-exprs arg-vals))]
                                   [rectified-evaluated (map (lx (recon-value _ render-settings)) (map cadr evaluated))])
                        (if (null? unevaluated)
                            #`(#%app . #,rectified-evaluated)
                            #`(#%app 
                               #,@rectified-evaluated
                               #,so-far 
                               #,@(map recon-source-current-marks (cdr (map car unevaluated)))))))
                     ((called)
                      (if (eq? so-far nothing-so-far)
                          (datum->syntax-object #'here `(,#'#%app ...)) ; in unannotated code
                          (datum->syntax-object #'here `(,#'#%app ... ,so-far ...))))
                     (else
                      (error "bad label in application mark in expr: ~a" expr))))
                 expr)]
               
               ; define-struct 
               ;               
               ;               [(z:struct-form? expr)
               ;                 (if (comes-from-define-struct? expr)
               ;                     so-far
               ;                     (let ([super-expr (z:struct-form-super expr)]
               ;                           [raw-type (utils:read->raw (z:struct-form-type expr))]
               ;                           [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
               ;                       (if super-expr
               ;                           `(struct (,raw-type ,so-far)
               ;                                    ,raw-fields)
               ;                           `(struct ,raw-type ,raw-fields))))]
               
               ; if
               [(if test then else)
                (attach-info
                 (let ([test-exp (if (eq? so-far nothing-so-far)
                                     (recon-value (lookup-binding mark-list if-temp) render-settings)
                                     so-far)])
                   #`(if #,test-exp 
                         #,(recon-source-current-marks (syntax then))
                         #,(recon-source-current-marks (syntax else))))
                 expr)]
               
               ; one-armed if
               
               [(if test then)
                (attach-info
                 (let ([test-exp (if (eq? so-far nothing-so-far)
                                     (recon-value (lookup-binding mark-list if-temp) render-settings)
                                     so-far)])
                   #`(if #,test-exp 
                         #,(recon-source-current-marks (syntax then))))
                 expr)]
               
               ; quote : there is no break on a quote.
               
               ; begin : may not occur directly, but will occur in the expansion of cond, now that I'm no longer
               ; masking that out with stepper-skipto. Furthermore, exactly one expression can occur inside it.
               
               [(begin clause)
                (attach-info
                 (if (eq? so-far nothing-so-far)
                     #`(begin #,(recon-source-current-marks (syntax clause)))
                     (error 
                      'recon-inner
                      "stepper:reconstruct: one-clause begin appeared as context: ~a" (syntax-object->datum expr)))
                 expr)]
               
               ; begin0 : may not occur directly except in advanced
               
               ; let-values
               
               [(let-values . rest) (recon-let)]
               
               [(letrec-values . rest) (recon-let)]
               
               ; define-values : define's don't get marks, so they can't occur here
               
               ; lambda : there is no break on a lambda
               
               [else
                (error
                 'recon-inner
                 "stepper:reconstruct: unknown object to reconstruct: ~a" (syntax-object->datum expr))])))
         
         ; the main recursive reconstruction loop is in recon:
         ; recon : (syntax-object mark-list boolean -> syntax-object)
         
         (define (recon so-far mark-list first)
           (cond [(null? mark-list) ; now taken to indicate a callback:
                  so-far
                  ;(error `recon "expcted a top-level mark at the end of the mark list.")
                  ]
                 [else
                  (case (mark-label (car mark-list)) 
                    [(top-level)
                     (if (null? (cdr mark-list))
                         (reconstruct-top-level (mark-source (car mark-list)) so-far)
                         (error 'recon "top-level-define mark found at non-end of mark list"))]
                    [else
                     (let ([reconstructed (recon-inner mark-list so-far)])
                       (recon
                        (if first
                            (mark-as-highlight reconstructed)
                            reconstructed)
                        (cdr mark-list)
                        #f))])]))

         ; uncomment to see all breaks coming in:
         ; (define _ (printf "break-kind: ~a\ninnermost source: ~a\n" break-kind
         ;           (and (pair? mark-list)
         ;                (syntax-object->datum (mark-source (car mark-list))))))
         
         (define answer
           (case break-kind
             ((result-value-break result-exp-break)
              (let* ([innermost (if (null? returned-value-list) ; is it an expr -> expr reduction?
                                    (recon-source-expr (mark-source (car mark-list)) mark-list null null render-settings)
                                    (recon-value (car returned-value-list) render-settings))])
                (unwind (recon (mark-as-highlight innermost) (cdr mark-list) #f) #f)))
             ((normal-break)
              (unwind (recon nothing-so-far mark-list #t) #f))
             ((double-break)
              (let* ([source-expr (mark-source (car mark-list))]
                     [innermost-before (mark-as-highlight (recon-source-expr source-expr mark-list null null render-settings))]
                     [newly-lifted-bindings (syntax-case source-expr (letrec-values)
                                              [(letrec-values ([vars . rest] ...) . bodies)
                                               (apply append (map syntax->list (syntax->list #`(vars ...))))]
                                              [(let-values ([vars . rest] ...) . bodies)
                                               (apply append (map syntax->list (syntax->list #`(vars ...))))]
                                              [else (error 'reconstruct "expected a let-values as source for a double-break, got: ~e"
                                                           (syntax-object->datum source-expr))])]
                     [innermost-after (mark-as-highlight (recon-source-expr (mark-source (car mark-list)) mark-list null newly-lifted-bindings render-settings))])
                (list (unwind (recon innermost-before (cdr mark-list) #f) #f)
                      (unwind (recon innermost-after (cdr mark-list) #f) #t))))
             ((late-let-break)
              (let* ([one-level-recon (unwind-only-highlight (mark-as-highlight (recon-inner mark-list nothing-so-far)))])
                (sublist 0 (- (length one-level-recon) 1) one-level-recon)))
             (else
              (error 'reconstruct-current-def "unknown break kind: " break-kind)))))
      
      answer)))
