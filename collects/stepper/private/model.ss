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
           (lib "match.ss")
           (prefix a: "annotate.ss")
           (prefix r: "reconstruct.ss")
           "shared.ss"
           "marks.ss"
           "model-settings.ss"
           "macro-unwind.ss")
  
  
  (define program-expander-contract
    ((-> void?) ; init
     ((or/c eof-object? syntax? (cons/c string? any/c)) (-> void?) . -> . void?) ; iter
     . -> .
     void?))
  
  
  (provide/contract [go (program-expander-contract       ; program-expander
                         (step-result? . -> . void?)     ; receive-result
                         (or/c render-settings? false/c) ; render-settings
                         boolean?                        ; track-inferred-names?
                         string?                         ; language-level-name
                         . -> .
                         void?)])
  
  ; go starts a stepper instance
  ; see provide stmt for contract 
  (define (go program-expander receive-result render-settings track-inferred-names? language-level-name)
    
    
    
    ;; finished-exps: (listof (list/c syntax-object? (or/c number? false?)( -> any)))
    ;;  because of mutation, these cannot be fixed renderings, but must be re-rendered at each step.
    (define finished-exps null)
    (define/contract add-to-finished
      ((-> syntax?) (or/c (listof natural-number/c) false/c) (-> any) . -> . void?)
      (lambda (exp-thunk lifting-indices getter)
        (set! finished-exps (append finished-exps (list (list exp-thunk lifting-indices getter))))))
    
    ;; the "held" variables are used to store the "before" step.  
    (define held-exp-list no-sexp)
    (define held-step-was-app? #f)
    (define held-finished-list null)
    
    (define basic-eval (current-eval))
    
    ;; highlight-mutated-expressions : 
    ;; ((listof (list/c syntax? syntax?)) (listof (list/c syntax? syntax?)) . -> . (list/c (listof syntax?) (listof syntax?)))
    ;; highlights changes occurring due to mutation.  This function accepts the left-hand-side
    ;; expressions and the right-hand-side expressions, and matches them against each other 
    ;; to see which ones have changed due to mutation, and highlights these.  
    ;; POSSIBLE RESEARCH POINT: if, say, (list 3 4) is mutated to (list 4 5), should the 4 & 5 be 
    ;;  highlighted individually or should the list as a whole be highlighted.  Is either one "wrong?"
    ;;  equivalences between reduction semantics?
    ;;
    ;; 2005-11-14: punting. just highlight the whole damn thing if there are any differences.
    ;;  in fact, just test for eq?-ness.
    
    #;(define (highlight-mutated-expressions lefts rights)
        (if (or (null? lefts) (null? rights))
            (list lefts rights)
            (let ([left-car (car lefts)]
                  [right-car (car rights)])
              (if (eq? (syntax-property left-car 'user-source)
                       (syntax-property right-car 'user-source))
                  (let ([highlights-added (highlight-mutated-expression left-car right-car)]
                        [rest (highlight-mutated-expressions (cdr lefts) (cdr rights))])
                    (cons (cons (car highlights-added) (car rest))
                          (cons (cadr highlights-added) (cadr rest))))))))
    
    ;; highlight-mutated-expression: syntax? syntax? -> syntax?
    ;; given two expressions, highlight 'em both if they differ at all. 
    
    ;; notes: wanted to use simple "eq?" test... but this will fail when a being-stepped definition (e.g.
    ;; in a let) turns into a permanent one.  We pay a terrible price for the lifting thing.  And, for the fact
    ;; that the highlighting follows from the reductions but can't obviously be deduced from them.
    
    #;(define (highlight-mutated-expression left right)
        (cond 
          ;; if either one is already highlighted, leave them alone.
          [(or (syntax-property left 'stepper-highlight)
               (syntax-property right 'stepper-highlight))
           (list left right)]
          
          ;; first pass: highlight if not eq?.  Should be broken for local-bound things
          ;; as they pass into permanence.
          [(eq? left right)
           (list left right)]
          
          [else (list (syntax-property left 'stepper-highlight)
                      (syntax-property right 'stepper-highlight))]))
    
    ;; REDIVIDE MAKES NO SENSE IN THE NEW INTERFACE.  THIS WILL BE DELETED AFTER BEING PARTED OUT.
    ; redivide takes a list of sexps and divides them into the 'before', 'during', and 'after' lists,
    ; where the before and after sets are maximal-length lists where none of the s-expressions contain
    ; a highlight-placeholder
    ; (->* ((listof syntax)) (list/c syntax syntax syntax))
    #;(define (redivide exprs)
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
    
    (define (>>> x)
      (fprintf (current-output-port) ">>> ~v\n" x)
      x)
    
    (define break 
      (opt-lambda (mark-set break-kind [returned-value-list #f])
        
        
        (let* ([mark-list (and mark-set (extract-mark-list mark-set))])
          
          (define (reconstruct-all-completed)
            (map (match-lambda 
                   [`(,source-thunk ,lifting-indices ,getter)
                     (match (r:reconstruct-completed (source-thunk) lifting-indices getter render-settings)
                       [#(exp #f) (first-of-one (unwind-no-highlight exp))]
                       [#(exp #t) exp])]) 
                 finished-exps))
          
          ;; TO BE SCRAPPED
          #;(define (double-redivide finished-exps new-exprs-before new-exprs-after)
              (let*-values ([(before current after) (redivide new-exprs-before)]
                            [(before-2 current-2 after-2) (redivide new-exprs-after)])
                (unless (equal? (map syntax-object->hilite-datum before) 
                                (map syntax-object->hilite-datum before-2))
                  (error 'double-redivide "reconstructed before defs are not equal."))
                (unless (equal? (map syntax-object->hilite-datum after) 
                                (map syntax-object->hilite-datum after-2))
                  (error 'double-redivide "reconstructed after defs are not equal."))
                (values (append finished-exps before) current current-2 after)))
          
          #;(printf "break called with break-kind: ~a ..." break-kind)
          (if (r:skip-step? break-kind mark-list render-settings)
              (begin
                #;(printf " but it was skipped!\n")
                  (when (or (eq? break-kind 'normal-break)
                            (eq? break-kind 'nomal-break/values)) ;; not sure about this...
                    (set! held-exp-list skipped-step)))
              
              (begin
                #;(printf "and it wasn't skipped.\n")
                  (case break-kind
                    [(normal-break normal-break/values)
                     (begin
                       (when (and (eq? break-kind 'normal-break) returned-value-list)
                         (error 'break "broken invariant: normal-break can't have returned values"))
                       (set! held-finished-list (reconstruct-all-completed))
                       (set! held-exp-list (unwind (r:reconstruct-left-side mark-list returned-value-list render-settings) #f))
                       (set! held-step-was-app? (r:step-was-app? mark-list)))]
                    
                    [(result-exp-break result-value-break)
                     (if (eq? held-exp-list skipped-step)
                         ; don't render if before step was a skipped-step
                         (set! held-exp-list no-sexp)
                         
                         (let* ([new-finished-list (reconstruct-all-completed)]
                                [reconstructed (unwind (r:reconstruct-right-side mark-list returned-value-list render-settings) #f)]
                                [result
                                 (if (eq? held-exp-list no-sexp)
                                     ;; in this case, there was no "before" step, due to 
                                     ;; unannotated code.  In this case, we make the 
                                     ;; optimistic guess that none of the finished expressions
                                     ;; were mutated.  It would be somewhat painful to do a better 
                                     ;; job, and the stepper makes no guarantees in this case.
                                     (make-before-after-result
                                      (list #`(... ...))
                                      (append new-finished-list reconstructed)
                                      'normal)
                                     
                                     (let*-values 
                                         ([(step-kind) (if (and held-step-was-app?
                                                                (eq? break-kind 'result-exp-break))
                                                           'user-application
                                                           'normal)]
                                          [(left-exps right-exps)
                                           ;; write this later:
                                           #;(identify-changed (append held-finished-list held-exps) (append new-finished-list reconstructed))
                                             (values (append held-finished-list held-exp-list) 
                                                     (append new-finished-list reconstructed))])
                                       
                                       (make-before-after-result left-exps right-exps step-kind)))])
                           (set! held-exp-list no-sexp)
                           (receive-result result)))]
                    
                    [(double-break)
                     ;; a double-break occurs at the beginning of a let's evaluation.
                     (when (not (eq? held-exp-list no-sexp))
                       (error 'break-reconstruction
                              "held-exp-list not empty when a double-break occurred"))
                     (let* ([new-finished-list (reconstruct-all-completed)]
                            [reconstruct-result (r:reconstruct-double-break mark-list render-settings)]
                            [left-side (unwind (car reconstruct-result) #f)]
                            [right-side (unwind (cadr reconstruct-result) #t)])
                       ;; add highlighting code as for other cases...
                       (receive-result (make-before-after-result (append new-finished-list left-side)
                                                                 (append new-finished-list right-side)
                                                                 'normal)))]
                    
                    
                    [(expr-finished-break)
                     (unless (not mark-list)
                       (error 'break "expected no mark-list with expr-finished-break"))
                     ;; in an expr-finished-break, the returned-vals hold (listof (list/c source lifting-index getter))
                     ;; this will now include define-struct breaks, for which the source is the source and the getter
                     ;; causes an error.
                     (for-each (lambda (source/index/getter)
                                 (apply add-to-finished source/index/getter))
                               returned-value-list)]
                    
                    [else (error 'break "unknown label on break")]))))))
    
    
    
    
    (define (step-through-expression expanded expand-next-expression)
      (let* ([annotated (a:annotate expanded break track-inferred-names? language-level-name)])
        (eval-syntax annotated)
        (expand-next-expression)))
    
    (define (err-display-handler message exn)
      (if (not (eq? held-exp-list no-sexp))
          (begin
            (receive-result (make-before-error-result (append held-finished-list held-exp-list)
                                                      message))
            (set! held-exp-list no-sexp))
          (receive-result (make-error-result message))))
    
    (program-expander
     (lambda () 
       ; swap these to allow errors to escape (e.g., when debugging)
       (error-display-handler err-display-handler)
       #;(void)
       )
     (lambda (expanded continue-thunk) ; iter
       (if (eof-object? expanded)
           (begin
             (receive-result (make-finished-stepping)))
           (step-through-expression expanded continue-thunk)))))
  
  
  (define (first-of-one x) 
    (unless (and (pair? x) (null? (cdr x)))
      (error 'first-of-one "expected a list of length one in: ~v" x))
    (car x)))

