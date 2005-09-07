;step collector state machine (not yet implemented):
;     
; datatype held-type = NO-HELD-STEP | SKIPPED-STEP | HELD(args)
;
; states: global state of held
; global: held : held-type
; edge-names: first, skipped-first, second, skipped-second, double, late-let 
;
;transitions (& actions):
;
; held = NO-HELD-STEP : 
;  first(x) : held := HELD(x)
;  skipped-first : held := SKIPPED-STEP
;  second(x) : trigger(NO-HELD-STEP, x), held := NO-HELD-STEP ; this happens when evaluating unannotated code
;  skipped-second : held := NO-HELD-STEP ; I believe this can also arise in unannotated code
;  double(x) : double-trigger(x), held := NO-HELD-STEP
;  late-let(x) : late-let-trigger(x), held := NO-HELD-STEP
;
; held = SOME(SKIPPED-STEP) :
;  first(x) : ERROR
;  skipped-first : ERROR
;  second(x) : held := NO-HELD-STEP ; this happens e.g. for evaluation of top-level var bound to a procedure
;  skipped-second : held := NO-HELD-STEP
;  double(x) : ERROR
;  late-let(x) : ERROR
;
; held = SOME(HELD(args))
;  first(x) : ERROR
;  skipped-first : ERROR
;  second(x) : trigger(HELD(args),x), held = NO-HELD-STEP
;  skipped-second : held = NO-HELD-STEP
;  double(x) : ERROR
;  late-let(x) : ERROR


(module model mzscheme
  (require (lib "contract.ss")
           (lib "etc.ss")
           (lib "list.ss")
           "my-macros.ss"
           (prefix a: "annotate.ss")
           (prefix r: "reconstruct.ss")
           "shared.ss"
           "marks.ss"
           "testing-shared.ss"
           "model-settings.ss")
 

  (define program-expander-contract
    ((-> void?) ; init
     ((union eof-object? syntax? (cons/c string? any/c)) (-> void?) . -> . void?) ; iter
     . -> .
     void?))
    

  (provide/contract [go (program-expander-contract       ; program-expander
                         (step-result? . -> . void?)     ; receive-result
                         (union render-settings? false/c) ; render-settings
                         boolean?                        ; track-inferred-names?
                         . -> .
                         void?)])
  
  ; go starts a stepper instance
  ; see provide stmt for contract 
  (define (go program-expander receive-result render-settings track-inferred-names?)
    
    (local
        
        ((define finished-exprs null)
         
         (define held-expr-list no-sexp)
         (define held-step-was-app? #f)
         
         (define basic-eval (current-eval))
         
         ; redivide takes a list of sexps and divides them into the 'before', 'during', and 'after' lists,
         ; where the before and after sets are maximal-length lists where none of the s-expressions contain
         ; a highlight-placeholder
         ; (->* ((listof syntax)) (list/c syntax syntax syntax))
         (define (redivide exprs)
           (letrec ([contains-highlight
                     (lambda (expr)
                       (or (syntax-property expr 'stepper-highlight)
                           (syntax-case expr ()
                             [(a . rest) (or (contains-highlight #`a) (contains-highlight #`rest))]
                             [else #f])))])
             (let* ([list-length (length exprs)]
                    [split-point-a (- list-length (length (or (memf contains-highlight exprs) null)))]
                    [split-point-b (length (or (memf contains-highlight (reverse exprs)) null))])
               (if (<= split-point-b split-point-a)
                   (error 'redivide-exprs "s-expressions did not contain the highlight-placeholder: ~v" (map syntax-object->hilite-datum exprs))
                   (values (sublist 0 split-point-a exprs) ; before
                           (sublist split-point-a split-point-b exprs) ; during
                           (sublist split-point-b list-length exprs)))))) ; after

         
;         (redivide `(3 4 (+ (define ,highlight-placeholder) 13) 5 6))
;         (values `(3 4) `((+ (define ,highlight-placeholder) 13)) `(5 6))
;         
;         (redivide `(,highlight-placeholder 5 6))
;         (values `() `(,highlight-placeholder) `(5 6))
;         
;         (redivide `(4 5 ,highlight-placeholder ,highlight-placeholder))
;         (values `(4 5) `(,highlight-placeholder ,highlight-placeholder) `())
;         
;         (printf "will be errors:~n")
;         (equal? (redivide `(1 2 3 4))
;                 error-value)
;         
;         (redivide `(1 2 ,highlight-placeholder 3 ,highlight-placeholder 4 5))
;         (values `(1 2) `(,highlight-placeholder 3 ,highlight-placeholder) `(4 5))
         
         
         (define break 
           (opt-lambda (mark-set break-kind [returned-value-list null])
             
             (let* ([mark-list (and mark-set (extract-mark-list mark-set))])

               (define (double-redivide finished-exprs new-exprs-before new-exprs-after)
                 (let*-values ([(before current after) (redivide new-exprs-before)]
                               [(before-2 current-2 after-2) (redivide new-exprs-after)])
                   (unless (equal? (map syntax-object->hilite-datum before) 
                                   (map syntax-object->hilite-datum before-2))
                     (error 'double-redivide "reconstructed before defs are not equal."))
                   (unless (equal? (map syntax-object->hilite-datum after) 
                                   (map syntax-object->hilite-datum after-2))
                     (error 'double-redivide "reconstructed after defs are not equal."))
                   (values (append finished-exprs before) current current-2 after)))
               
               (define (reconstruct-helper)
                 (r:reconstruct-current mark-list break-kind returned-value-list render-settings))
               
               (if (r:skip-step? break-kind mark-list render-settings)
                   (when (eq? break-kind 'normal-break)
                     (set! held-expr-list skipped-step))
                   (case break-kind
                     [(normal-break)
                      (begin
                        (set! held-expr-list (reconstruct-helper))
                        (set! held-step-was-app? (r:step-was-app? mark-list)))]
                     
                     [(result-exp-break result-value-break)
                      (if (eq? held-expr-list skipped-step)
                          (set! held-expr-list no-sexp)
                          (let* ([reconstructed (reconstruct-helper)]
                                 [result
                                  (if (not (eq? held-expr-list no-sexp))
                                      (let*-values 
                                          ([(step-kind) (if (and held-step-was-app?
                                                                 (eq? break-kind 'result-exp-break))
                                                            'user-application
                                                            'normal)]
                                           [(new-finished current-pre current-post after) 
                                            (double-redivide finished-exprs held-expr-list reconstructed)])
                                        (make-before-after-result new-finished current-pre current-post after step-kind))
                                      
                                      (let*-values
                                          ([(before current after) (redivide reconstructed)])
                                        (make-before-after-result (append finished-exprs before) (list 
                                                                                                  (syntax-property #`(... ...)
                                                                                                                   'stepper-highlight
                                                                                                                   #t))
                                                                  current after 'normal)))])
                            (set! held-expr-list no-sexp)
                            (receive-result result)))]
                     [(double-break)
                      ; a double-break occurs at the beginning of a let's evaluation.
                      (let* ([reconstruct-quadruple (reconstruct-helper)])
                        (when (not (eq? held-expr-list no-sexp))
                          (error 'break-reconstruction
                                 "held-expr-list not empty when a double-break occurred"))
                        (let*-values 
                            ([(new-finished current-pre current-post after) 
                              (double-redivide finished-exprs 
                                               (list-ref reconstruct-quadruple 0) 
                                               (list-ref reconstruct-quadruple 1))])
                          (receive-result (make-before-after-result new-finished
                                                                    current-pre
                                                                    current-post
                                                                    after
                                                                    'normal))))]
                     [(late-let-break)
                      (let ([new-finished (r:reconstruct-current mark-list break-kind returned-value-list render-settings)])
                        (set! finished-exprs (append finished-exprs new-finished)))]
                     
                     [(expr-finished-break)
                      (let ([reconstructed (r:reconstruct-completed mark-list returned-value-list render-settings)])
                        (set! finished-exprs (append finished-exprs (list reconstructed))))]
                     
                     [(define-struct-break)
                      (set! finished-exprs (append finished-exprs
                                                   (list (car returned-value-list))))]
                     
                     [else (error 'break "unknown label on break")])))))
         
         (define (step-through-expression expanded expand-next-expression)
           (let* ([annotated (a:annotate expanded break track-inferred-names?)])
             (eval-syntax annotated)
             (expand-next-expression)))
         
         (define (err-display-handler message exn)
           (if (not (eq? held-expr-list no-sexp))
                 (let*-values ([(before current after) (redivide held-expr-list)])
                   (set! held-expr-list no-sexp)
                   (receive-result (make-before-error-result (append finished-exprs before) 
                                                             current message after)))
               (receive-result (make-error-result finished-exprs message)))))
      
      (program-expander
       (lambda () 
         ; swap these to allow errors to escape (e.g., when debugging)
         (error-display-handler err-display-handler)
         ;(void)
         )
       (lambda (expanded continue-thunk) ; iter
         (if (eof-object? expanded)
             (begin
               (receive-result (make-finished-result finished-exprs))
               (receive-result (make-finished-stepping)))
             (step-through-expression expanded continue-thunk)))))))

