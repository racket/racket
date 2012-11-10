#lang racket/base

(require (prefix-in kernel: syntax/kerncase)
         racket/match
         racket/contract
         "marks.rkt"
         "shared.rkt"
         "syntax-property.rkt"
         "my-macros.rkt"
         #;"xml-box.rkt"
         (prefix-in beginner-defined: "beginner-defined.rkt")
         (for-syntax racket/base))

(define-syntax (where stx)
  (syntax-case stx ()
    [(_ body bindings)
     (syntax/loc stx (letrec bindings body))]))

; CONTRACTS


; PROVIDE
(provide/contract
 [annotate
  (syntax?                         ; syntax to annotate
   ((or/c continuation-mark-set? false/c) 
    break-kind?
    (or/c list? false/c)
    . -> .
    any/c)                       ; procedure for runtime break
   boolean?   ; show-lambdas-as-lambdas?
   . -> .
   syntax?)]                       ; result
 
 #;[top-level-rewrite (-> syntax? syntax?)])


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
;  .........you-know,-this-flag-doesn't-feel-quite-as...............................................
;  .                                                                                               .
;  ..........lighthearted-as-it-did-when-I-created-it-in-1998.......................................
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


;; given an expression to annotate, and a 'break' expression to call 
;; when a breakpoint occurs, and a boolean indicating whether
;; lambdas are to be displayed as lambdas, return an annotated expression.
(define (annotate main-exp break show-lambdas-as-lambdas?)
  
  #;(define _ (>>> main-exp #;(syntax->datum main-exp)))
  
  (define binding-indexer
    (let ([binding-index 0])
      (lambda ()
        (let ([temp binding-index])
          (set! binding-index (+ binding-index 1))
          temp))))
  
  (define (normal-break)
    (break (current-continuation-marks) 'normal-break #f))
  
  (define (result-exp-break)
    (break (current-continuation-marks) 'result-exp-break #f))
  
  (define (result-value-break vals-list)
    (break (current-continuation-marks) 'result-value-break vals-list))
  
  (define (normal-break/values vals-list)
    (break (current-continuation-marks) 'normal-break/values vals-list))
  
  (define (exp-finished-break info-list)
    (break #f 'expr-finished-break info-list))
  
  (define (double-break)
    (break (current-continuation-marks) 'double-break #f))
  
  (define ((make-opaque-exp-break exp))
    (exp-finished-break 
     (list (list (lambda () exp) 
                 #f 
                 (lambda () (error 'make-define-struct-break
                                   "no getter for a define-struct"))))))
  
  ; wcm-pre-break-wrap : call wcm-wrap with a pre-break on the expr
  (define (wcm-pre-break-wrap debug-info exp)
    (wcm-wrap debug-info (pre-break-wrap exp)))
  
  ;; wrap a pre-break around stx
  (define (pre-break-wrap stx)
    #`(begin (#%plain-app #,result-exp-break) #,stx))
  
  ;; wrap a normal break around stx
  (define (break-wrap exp)
    #`(begin (#%plain-app #,normal-break) #,exp));
  
  ;; wrap a double-break around exp
  (define (double-break-wrap exp)
    #`(begin (#%plain-app #,double-break) #,exp))
  
  ;; abstraction used in the next two defs
  (define (return-value-wrap-maker break-proc) 
    (lambda (exp)
      #`(#%plain-app
         call-with-values
         (#%plain-lambda () #,exp)
         (#%plain-lambda args
           (#%plain-app #,break-proc args)
           (#%plain-app #,apply values args)))))
  
  ;; wrap a return-value-break around exp
  (define return-value-wrap
    (return-value-wrap-maker result-value-break))
  
  ;; wrap a normal-break/values around exp
  (define normal-break/values-wrap
    (return-value-wrap-maker normal-break/values))
  
  (define (top-level-annotate/inner exp source-exp defined-name)
    (match-let* 
        ([(vector annotated dont-care)
          (annotate/inner exp 'all #f defined-name)])
      #`(with-continuation-mark #,debug-key 
          #,(make-top-level-mark source-exp)
          ;; inserting eta-expansion to prevent destruction of top-level mark
          (#%plain-app
           call-with-values (#%plain-lambda () #,annotated)
           (#%plain-lambda args (#%plain-app #,apply values args))))))
  
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
  ;(syntax-object BINDING-SET bool bool (or/c #f symbol (list binding symbol)) -> 
  ;          sexp (list-of z:varref))
  
  
  
  
  
  ;                                                       ;  ;                           
  ;                              ;          ;             ;                              
  ;   ;;;   ; ;;   ; ;;    ;;;  ;;;;  ;;;  ;;;;  ;;;     ;   ;  ; ;;   ; ;;    ;;;   ; ;;
  ;  ;   ;  ;;  ;  ;;  ;  ;   ;  ;   ;   ;  ;   ;   ;    ;   ;  ;;  ;  ;;  ;  ;   ;  ;;  
  ;      ;  ;   ;  ;   ;  ;   ;  ;       ;  ;   ;   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   
  ;   ;;;;  ;   ;  ;   ;  ;   ;  ;    ;;;;  ;   ;;;;;   ;    ;  ;   ;  ;   ;  ;;;;;  ;   
  ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;   ;  ;   ;       ;    ;  ;   ;  ;   ;  ;      ;   
  ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;   ;  ;   ;       ;    ;  ;   ;  ;   ;  ;      ;   
  ;   ;;;;; ;   ;  ;   ;   ;;;    ;;  ;;;;;  ;;  ;;;;   ;    ;  ;   ;  ;   ;   ;;;;  ;   
  ;  ;                                 
  ;                                 
  
  (define annotate/inner
    #;(syntax? binding-set? boolean? (or/c false/c syntax? (list/p syntax? syntax?)) (or/c false/c integer?)
               . -> . (vector/p syntax? binding-set?))
    (lambda (exp tail-bound pre-break? procedure-name-info)
      
      ;; annotate an exp with a stepper/skipto or stepper-skipto/discard
      ;; label
      (define (dont-annotate traversal)
        ;; mutable, to catch free vars. Mutated several times, we
        ;; only care about the last. A bit yecchy.
        (define free-vars-captured #f)
        
        (define (subterm-recur subterm)
          (match-let* 
              ([(vector stx free-vars)
                (annotate/inner subterm tail-bound pre-break?
                                procedure-name-info)])
            (set! free-vars-captured free-vars)
            stx))
        
        (define annotated (skipto/auto exp traversal subterm-recur))
        
        (vector (wcm-wrap skipto-mark annotated) free-vars-captured))

      ;; recurrence procedures, used to recur on sub-expressions:

      (define (tail-recur exp) (annotate/inner exp tail-bound
                                               #t procedure-name-info))
      (define (non-tail-recur exp) (annotate/inner exp null #f #f))
      (define (result-recur exp) (annotate/inner exp null
                                                 #f procedure-name-info))
      (define (set!-rhs-recur exp name) (annotate/inner exp null #f name))
      (define (let-rhs-recur exp binding-names dyn-index-syms)
        (define proc-name-info 
          (if (not (null? binding-names))
              (list (car binding-names) (car dyn-index-syms))
              #f))
        (annotate/inner exp null #f proc-name-info))
      (define (lambda-body-recur exp) (annotate/inner exp 'all #t #f))
      
      
      ; let bodies have a startling number of recurrence patterns. ouch!  
      ;; ... looks like these can maybe be collapsed with a simpler desired reduction sequence
      ;; (a.k.a. not safe-for-space).
                  
      ;; no pre-break, tail w.r.t. new bindings:
      (define (let-body-recur/single exp bindings)
        (annotate/inner exp (binding-set-union (list tail-bound bindings))
                        #f procedure-name-info))
                  
      ;; different flavors of make-debug-info allow users to provide only the needed fields:
                  
      (define (make-debug-info-normal free-bindings)
        (make-debug-info exp tail-bound free-bindings 'none #t))
      
      (define (make-debug-info-app tail-bound free-bindings label)
        (make-debug-info exp tail-bound free-bindings label #t))
      
      (define (make-debug-info-let free-bindings binding-list let-counter)
        (make-debug-info 
         exp 
         (binding-set-union (list tail-bound 
                                  binding-list
                                  (list let-counter)))
         (varref-set-union (list free-bindings 
                                 binding-list
                                 (list let-counter))) ; NB using bindings as varrefs
         'let-body
         #t))
      (define (make-debug-info-fake-exp exp free-bindings)
        (make-debug-info (stepper-syntax-property exp 'stepper-fake-exp #t) 
                         tail-bound free-bindings 'none #t))
      
      (define (make-debug-info-fake-exp/tail-bound exp tail-bound free-bindings)
        (make-debug-info (stepper-syntax-property exp 'stepper-fake-exp #t) 
                         tail-bound free-bindings 'none #t))
                  
      (define outer-wcm-wrap (if pre-break?
                                 wcm-pre-break-wrap
                                 wcm-wrap))
      (define (wcm-break-wrap debug-info exp)
        (outer-wcm-wrap debug-info (break-wrap exp)))
                  
      ;; used for things that are values:
      (define (normal-bundle free-vars annotated)
        (vector (outer-wcm-wrap (make-debug-info-normal free-vars)
                                annotated)
                free-vars))
      
      
      ;    @@                 @@         @@        
      ;     @                  @          @        
      ;     @     $@$: @@+-$:  @-@$    $@:@   $@$: 
      ;     @       -@  @+@$@  @+ *$  $* *@     -@ 
      ;     @    -$@$@  @ @ @  @   @  @   @  -$@$@ 
      ;     @    $*  @  @ @ @  @   @  @   @  $*  @ 
      ;     @    @- *@  @ @ @  @  +$  $* *@  @- *@ 
      ;   @@@@@  -$$-@@@@@@@@@@@+@$    $@:@@ -$$-@@
      ;                                                        
      
      (define (lambda-clause-abstraction clause)
        (with-syntax ([(args-stx . bodies) clause])
          (match-let* 
              ([(vector annotated-body free-varrefs)
                ; wrap bodies in explicit begin if more than 1 
                ; user-introduced (non-skipped) bodies
                ; NB: CAN'T HAPPEN in beginner up through int/lambda
                (let ([non-skipped-bodies
                       (filter 
                        (lambda (clause)
                          (not (to-be-skipped? clause)))
                        (syntax->list (syntax bodies)))])
                  (if (> (length non-skipped-bodies) 1)
                      (lambda-body-recur (syntax (begin . bodies)))
                      (match-let* 
                          ([(vector annotated-bodies free-var-sets)
                            (2vals-map lambda-body-recur
                                       (syntax->list #`bodies))])
                        (vector #`(begin . #,annotated-bodies)
                                (varref-set-union free-var-sets)))))]
               [new-free-varrefs 
                (varref-set-remove-bindings
                 free-varrefs
                 (arglist-flatten #'args-stx))])
            (vector (datum->syntax
                     #'here
                     `(,#'args-stx ,annotated-body) #'clause)
                    new-free-varrefs))))
      
      
      (define (outer-lambda-abstraction annotated-lambda free-varrefs)
        (let*
            ([closure-info (make-debug-info-app 'all free-varrefs 'none)]
             ;; if we manually disable the storage of names, 
             ;; lambdas get rendered as lambdas.
             ;; Yikes, this seems like a pretty gross hack... JBC 2010-12
             [closure-name 
              (if show-lambdas-as-lambdas?
                  #f
                  (cond [(syntax? procedure-name-info) procedure-name-info]
                        [(pair? procedure-name-info) (car procedure-name-info)]
                        [else #f]))]
             
             [closure-storing-proc
              (lambda (clo debug-info maybe-index)
                (annotated-proc
                 clo
                 (make-closure-record 
                  closure-name
                  debug-info
                  #f
                  maybe-index)))]
             
             [captured
              (cond [(pair? procedure-name-info)
                     #`(#%plain-app 
                        #,closure-storing-proc
                        #,annotated-lambda
                        #,closure-info 
                        #,(cadr procedure-name-info))]
                    [else
                     #`(#%plain-app
                        #,closure-storing-proc
                        #,annotated-lambda
                        #,closure-info
                        #f)])]
             
             ;; gnarr! I can't find a test case
             ;; that depends on the attachment of the inferred name...
             [inferred-name-struct
              (if closure-name
                  (syntax-property
                   captured
                   'inferred-name
                   (syntax-e closure-name))
                  captured)])
         
         (normal-bundle free-varrefs inferred-name-struct)))
      
      
                  
      
      
      ;    @@                 
      ;     @            @    
      ;     @    -@@$   @@@@@ 
      ;     @    $  -$   @    
      ;     @    @@@@@   @    
      ;     @    $       @    
      ;     @    +:      @: :$
      ;   @@@@@   $@@+   :@@$-
      
      
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
      ; okay, because expand guarantees that reordering them will not cause capture.
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
      
      ; 2005-08: note that the set!-based approach on the let-counter is broken in the presence of 
      ;  continuations; backing up a computation using a set! will not revert the
      ;  counter, and the stepper may think that the computation is in a different 
      ;  place.  To fix this, we must go to a pure let* with nested marks at each right-hand-side.
      
      ; 2006-01: oh dear heaven.  Begin expands into a let-values.  This means that the 
      ;  let-values really has most of the complexity of the whole stepper, all in one 
      ;  place.  Re-formulating the bodies as a begin and re-calling annotate/inner broke
      ;  implied invariants (in particular, that annotate/inner was only called on subexprs)
      ;  and confused the heck out of me for some time today.  Bleah.  I'm just going to 
      ;  do the whole expansion here. Also, I'm going to make this expansion call/cc-clean, 
      ;  because I think it'll actually be easier to state & read this way.
      
      ; 2006-11: appears to work now.  I'm about to try to transfer this new idiom to begin0;
      ;  wish me luck.
      
      
      (define (let-abstraction stx output-identifier make-init-list)
        (with-syntax ([(_ ([(var ...) val] ...) . bodies) stx])
          (match-let*
              ([binding-sets (map syntax->list (syntax->list #'((var ...) ...)))]
               [binding-list (apply append binding-sets)]
               [vals (syntax->list #'(val ...))]
               [lifted-var-sets (map (lx (map get-lifted-var _)) binding-sets)]
               [lifted-vars (apply append lifted-var-sets)]
               [(vector annotated-vals free-varref-sets-vals)
                (2vals-map let-rhs-recur vals binding-sets lifted-var-sets)]
               [bodies-list (syntax->list #'bodies)]
               [(vector annotated-body free-varrefs-body)
                (if (= (length bodies-list) 1)
                    (let-body-recur/single (car bodies-list) binding-list)
                    ;; oh dear lord, we have to unfold these like an application:
                    (let unroll-loop ([bodies-list bodies-list] [outermost? #t])
                      (cond [(null? bodies-list)
                             (error 'annotate "no bodies in let")]
                            [(null? (cdr bodies-list))
                             (tail-recur (car bodies-list))]
                            [else
                             (match-let*
                              ([(vector rest free-vars-rest) 
                                (unroll-loop (cdr bodies-list) #f)]
                               [(vector this-one free-vars-this)
                                (non-tail-recur (car bodies-list))]
                               [free-vars-all
                                (varref-set-union (list free-vars-rest
                                                        free-vars-this))]
                               [debug-info (make-debug-info-fake-exp 
                                            #`(begin #,@bodies-list)
                                            free-vars-all)]
                               [begin-form 
                                 #`(begin 
                                     #,(normal-break/values-wrap this-one) 
                                     #,rest)])
                              (vector (if outermost?
                                          (wcm-wrap debug-info begin-form)
                                          (wcm-pre-break-wrap debug-info
                                                              begin-form))
                                      free-vars-all))])))])
           
           ((vector (quasisyntax/loc 
                        exp 
                      (let ([#,counter-id (#,binding-indexer)])
                        (#,output-identifier #,outer-initialization #,wrapped-begin))) 
                    free-varrefs)
            
            . where .
            
            ([free-varrefs (varref-set-remove-bindings 
                            (varref-set-union (cons free-varrefs-body
                                                    free-varref-sets-vals)) 
                            binding-list)]
             [counter-id #`lifting-counter]
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
             [exp-finished-clauses
              
              (with-syntax  ([(_ let-clauses . dc) stx]
                             [((lifted-var ...) ...) lifted-var-sets])
                (with-syntax ([(exp-thunk ...) (map (lx (lambda () _))
                                                    (syntax->list #`let-clauses))])
                  #`(#%plain-app 
                     list 
                     (#%plain-app
                      list exp-thunk
                      (#%plain-app
                       list lifted-var ...)
                      (#%plain-lambda () (#%plain-app list var ...))) ...)))]
             ; time to work from the inside out again
             ; without renaming, this would all be much much simpler.
             [wrapped-begin (outer-wcm-wrap (make-debug-info-let free-varrefs
                                                                 binding-list
                                                                 let-counter) 
                                            (double-break-wrap
                                             #`(begin #,@(apply append (zip set!-clauses counter-clauses))
                                                      (#%plain-app #,exp-finished-break #,exp-finished-clauses)
                                                      #,annotated-body)))])))))
      
      
                  
                  
                  
                  
                  
      
      
      ;     @     :@@$ 
      ;           @:   
      ;   -@@    @@@@@ 
      ;     @     @    
      ;     @     @    
      ;     @     @    
      ;     @     @    
      ;   @@@@@  @@@@@ 
      
      ; if-abstraction: (-> syntax? syntax? (or/c false/c syntax?) (values syntax? varref-set?))
      (define (if-abstraction test then else) 
        (match-let*
            ([(vector annotated-test free-varrefs-test) 
              (non-tail-recur test)]
             [test-with-break
              (normal-break/values-wrap annotated-test)]
             [(vector annotated-then free-varrefs-then) 
              (tail-recur then)]
             [(vector annotated-else free-varrefs-else)
              (if else
                  (tail-recur else)
                  (vector #f null))]
             [free-varrefs (varref-set-union (list free-varrefs-test 
                                                   free-varrefs-then 
                                                   free-varrefs-else))]
             [annotated-if 
              (if else
                  (quasisyntax/loc exp 
                    (if #,test-with-break #,annotated-then #,annotated-else))
                  (quasisyntax/loc exp 
                    (if #,test-with-break #,annotated-then)))])
          (vector
           (outer-wcm-wrap (make-debug-info-normal free-varrefs) annotated-if)
           free-varrefs)))
      
      
                  
      
      
      ;                                            
      ;                                            
      ;                                        ;;; 
      ;                                       ;    
      ;                                       ;    
      ;   ;   ;   ;;;;  ; ;;;  ; ;;;   ;;;  ;;;;;; 
      ;   ;   ;  ;   ;  ;;  ;  ;;  ;  ;   ;   ;    
      ;    ; ;   ;   ;  ;      ;      ;;;;;   ;    
      ;    ; ;   ;   ;  ;      ;      ;       ;    
      ;    ; ;   ;  ;;  ;      ;      ;       ;    
      ;     ;     ;; ;  ;      ;       ;;;;   ;    
      ;                                            
      ;                                            
      ;                                            
      
      
      (define (varref-abstraction var)
        (match-let* 
            ([free-varrefs (list var)]
             [varref-break-wrap
              (lambda ()
                (wcm-break-wrap (make-debug-info-normal free-varrefs)
                                (return-value-wrap var)))]
             [varref-no-break-wrap
              (lambda ()
                (outer-wcm-wrap (make-debug-info-normal free-varrefs) var))]
             ;; JBC: shouldn't this be the namespace of the user's code... ?
             [base-namespace-symbols (namespace-mapped-symbols (make-base-namespace))]
             [module-bound-varref-break-wrap
              (lambda ()
                (varref-break-wrap)
                #;(if (or (memq (syntax-e var) beginner-defined:must-reduce)
                        (and (stepper-syntax-property var 'lazy-op)
                             (not (memq (syntax->datum var) base-namespace-symbols))))
                    (varref-break-wrap)
                    (varref-no-break-wrap)))])
          (vector 
           (match (stepper-syntax-property var 'stepper-binding-type)
             [(or 'lambda-bound 'macro-bound)   (varref-no-break-wrap)]
             ['let-bound                        (varref-break-wrap)]
             ['non-lexical ;; is it from this module or not?
              (match (identifier-binding var)
                ;; this can only come up when stepping through non-module code...
                ;; perhaps we should just signal an error here.
                (#f (varref-break-wrap))
                ['lexical  
                 ;; my reading of the docs suggest that this should not occur in v4...
                 (error 'varref-abstraction 
                        "identifier-binding should not be 'lexical")]
                [(list-rest (? module-path-index? path-index) dontcare)
                 (let-values ([(module-path dc5)
                               (module-path-index-split path-index)])
                   (if module-path
                       ;; not a module-local variable:
                       (module-bound-varref-break-wrap)
                       ;; a module-local-variable:
                       (varref-break-wrap)))]
                [other (error 
                        'annotate
                        "unexpected value for identifier-binding: ~v" other)])]
             [other
              (error 'annotate 
                     "unexpected value for stepper-binding-type on variable ~e: ~e"
                     (syntax->datum var)
                     other)])
           free-varrefs)))
                  
      (define (recertifier vals)
        (match-let* ([(vector new-exp bindings) vals])
          (vector new-exp
                  (map (lambda (b) b)
                       bindings))))

      ;; this is a terrible hack... until some other language form needs it. 
      ;; It wraps the given annotated expression with a break that adds the 
      ;; result to the list of completed expressions
      (define maybe-final-val-wrap
        (match-lambda 
          [(vector annotated free-vars) 
           (vector (if (stepper-syntax-property exp 'stepper-use-val-as-final)
                       #`(#%plain-app
                          call-with-values 
                          (#%plain-lambda () #,annotated)
                          (#%plain-lambda 
                           results
                           (#,exp-finished-break 
                            (#%plain-app list
                                         (#%plain-app 
                                          list
                                          #,(lambda () exp)
                                          #f
                                          (#%plain-lambda () results))))
                                          (#%plain-app values results)))
                       annotated)
                   free-vars)]
          [error 'maybe-final-val-wrap "stepper internal error 20080527"]))
      
      (cond [(stepper-syntax-property exp 'stepper-skipto) 
             (dont-annotate 'rebuild)]
            [(stepper-syntax-property exp 'stepper-skipto/discard)
             (dont-annotate 'discard)]
            [(to-be-skipped? exp)
             (vector (wcm-wrap 13 exp) null)]
            
            [else
             (let ([exp (syntax-disarm exp saved-code-inspector)])
              (recertifier
              (maybe-final-val-wrap
               (kernel:kernel-syntax-case 
                exp #f
                
                [(#%plain-lambda . clause)
                 (match-let* 
                     ([(vector annotated-clause free-varrefs)
                       (lambda-clause-abstraction (syntax clause))]
                      [annotated-lambda
                       (with-syntax ([annotated-clause annotated-clause])
                         (syntax/loc exp (#%plain-lambda . annotated-clause)))])
                   (outer-lambda-abstraction annotated-lambda free-varrefs))]
                
                [(case-lambda . clauses)
                 (match-let* 
                     ([(vector annotated-cases free-varrefs-cases)
                       (2vals-map lambda-clause-abstraction (syntax->list (syntax clauses)))]
                      [annotated-case-lambda (with-syntax ([annotated-cases annotated-cases])
                                               (syntax/loc exp (case-lambda . annotated-cases)))]
                      [free-varrefs (varref-set-union free-varrefs-cases)])
                   (outer-lambda-abstraction annotated-case-lambda free-varrefs))]
                
                
                
                [(if test then else) (if-abstraction (syntax test) (syntax then) (syntax else))]
                
                
                ;                                     
                ;                                     
                ;   ;                     ;           
                ;   ;                                 
                ;   ;                                 
                ;   ; ;;    ;;;    ;;;; ;;;     ; ;;  
                ;   ;;  ;  ;   ;  ;   ;   ;     ;;  ; 
                ;   ;   ;  ;;;;;  ;   ;   ;     ;   ; 
                ;   ;   ;  ;      ;   ;   ;     ;   ; 
                ;   ;   ;  ;      ;  ;;   ;     ;   ; 
                ;   ;;;;    ;;;;   ;; ;   ;;;   ;   ; 
                ;                     ;               
                ;                 ;;;;                
                ;                                     
                
                
                [(begin . bodies-stx)
                 (begin
                   (error 'annotate-inner "nothing expands into begin! : ~v" (syntax->datum exp))
                   #;(begin-abstraction (syntax->list #`bodies-stx)))]
                
                
                ;                                            
                ;                                            
                ;   ;                     ;             ;;   
                ;   ;                                  ;  ;  
                ;   ;                                 ;   ;; 
                ;   ; ;;    ;;;    ;;;; ;;;     ; ;;  ;  ; ; 
                ;   ;;  ;  ;   ;  ;   ;   ;     ;;  ; ;  ; ; 
                ;   ;   ;  ;;;;;  ;   ;   ;     ;   ; ; ;  ; 
                ;   ;   ;  ;      ;   ;   ;     ;   ; ;;   ; 
                ;   ;   ;  ;      ;  ;;   ;     ;   ; ;;  ;  
                ;   ;;;;    ;;;;   ;; ;   ;;;   ;   ;   ;;   
                ;                     ;                      
                ;                 ;;;;                       
                ;                                            
                
                ;; one-element begin0 is a special case, because in this case only
                ;; the body of the begin0 is in tail position.
                
                [(begin0 body)
                 (match-let* ([(vector annotated-body free-vars-body) 
                               (tail-recur #'body)])
                   (vector (wcm-break-wrap (make-debug-info-normal free-vars-body)
                                           (quasisyntax/loc exp (begin0 #,annotated-body)))
                           free-vars-body))]
                
                
                [(begin0 first-body . bodies-stx)
                 (match-let*
                     ([(vector annotated-first free-vars-first) (result-recur #'first-body)]
                      [(vector annotated-rest free-vars-rest) (2vals-map non-tail-recur (syntax->list #`bodies-stx))]
                      [wrapped-rest (map normal-break/values-wrap annotated-rest)]
                      [all-free-vars (varref-set-union (cons free-vars-first free-vars-rest))]
                      [early-debug-info (make-debug-info-normal all-free-vars)]
                      [tagged-temp (stepper-syntax-property begin0-temp 'stepper-binding-type 'stepper-temp)]
                      [debug-info-maker
                       (lambda (rest-exps)
                         (make-debug-info-fake-exp/tail-bound                                               
                          #`(begin0 #,@rest-exps)
                          (binding-set-union (list (list tagged-temp) tail-bound))
                          (varref-set-union (list (list tagged-temp) all-free-vars))))]
                      [rolled-into-fakes 
                       (let loop ([remaining-wrapped wrapped-rest] 
                                  [remaining-src (syntax->list #`bodies-stx)]
                                  [first-time? #t])
                         ((if first-time? wcm-wrap wcm-pre-break-wrap)
                          (debug-info-maker remaining-src)   
                          (cond [(null? remaining-src) begin0-temp]
                                [else #`(begin #,(car remaining-wrapped) #,(loop (cdr remaining-wrapped)
                                                                                 (cdr remaining-src)
                                                                                 #f))])))])
                   (vector (wcm-wrap early-debug-info
                                     #`(let ([#,begin0-temp #,annotated-first])
                                         #,rolled-into-fakes))
                           all-free-vars))]
                                            
                [(let-values . _)
                 (let-abstraction exp 
                                  #`let-values
                                  (lambda (bindings)
                                    (map (lambda (_) *unevaluated*) bindings)))]
                
                [(letrec-values . _)
                 (let-abstraction exp 
                                  #`letrec-values
                                  (lambda (bindings) (map (lambda (b) #`#,b) bindings)))]
                
                
                ;                          $   
                ;                  @       @   
                ;   :@@+@  -@@$   @@@@@    @   
                ;   @$ -@  $  -$   @       @   
                ;   :@@$-  @@@@@   @       @   
                ;      *@  $       @           
                ;   @  :@  +:      @: :$       
                ;   $+@@:   $@@+   :@@$-   $   
                
                
                [(set! var val)
                 (match-let*
                     ([(vector annotated-val val-free-varrefs)
                       (set!-rhs-recur (syntax val) 
                                       (syntax-case (syntax var) (#%top)
                                         [(#%top . real-var) (syntax-e (syntax real-var))]
                                         [else (syntax var)]))]
                      [free-varrefs (varref-set-union (list val-free-varrefs (list #`var)))]
                      [annotated-set!
                       (return-value-wrap
                        (quasisyntax/loc exp (set! var #,(normal-break/values-wrap annotated-val))))])
                   (vector
                    (outer-wcm-wrap (make-debug-info-normal free-varrefs) annotated-set!)
                    free-varrefs))]
                
                
                ;                         @           
                ;    $@-@@@@  @@   $@$   @@@@@  -@@$  
                ;   $- :@  @   @  $- -$   @     $  -$ 
                ;   @   @  @   @  @   @   @     @@@@@ 
                ;   @   @  @   @  @   @   @     $     
                ;   $- :@  @: +@  $- -$   @: :$ +:    
                ;    $@-@  :@$-@@  $@$    :@@$-  $@@+ 
                ;       @                             
                ;      @@@                            
                
                [(quote _)
                 (normal-bundle null exp)]
                
                [(quote-syntax _)
                 (normal-bundle null exp)]
                
                
                ;  @@@ @@@         $@+@        @@+-$: 
                ;   @   @         $+ -@         @+@$@ 
                ;   $-@ @  @@@@@  @      @@@@@  @ @ @ 
                ;   ++@+$         @             @ @ @ 
                ;   :@@$+         $* -$         @ @ @ 
                ;   -@$@*          $@$-        @@@@@@@
                
                
                [(with-continuation-mark key mark body)
                 ;(match-let* ([(annotated-key free-varrefs-key)
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
                
                
                ;                         @@      @                           @                 
                ;                          @                          @                         
                ;    $@$: @@:@$- @@:@$-    @    -@@     $@+@   $@$:  @@@@@  -@@     $@$  @@:@@: 
                ;      -@  @: -$  @: -$    @      @    $+ -@     -@   @       @    $- -$  @+ :@ 
                ;   -$@$@  @   @  @   @    @      @    @      -$@$@   @       @    @   @  @   @ 
                ;   $*  @  @   @  @   @    @      @    @      $*  @   @       @    @   @  @   @ 
                ;   @- *@  @: -$  @: -$    @      @    $* -$  @- *@   @: :$   @    $- -$  @   @ 
                ;   -$$-@@ @-@$   @-@$   @@@@@  @@@@@   $@$-  -$$-@@  :@@$- @@@@@   $@$  @@@ @@@
                ;          @      @                                                             
                ;         @@@    @@@                                                            
                
                
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
                
                ;; the call/cc-safe version of this appears to work, and it lives in the definition of let.  I should 
                ;; transfer that knowledge to here.  -- JBC, 2006-10-11
                
                [(#%plain-app . terms)
                 (match-let*
                     ([(vector annotated-terms free-varrefs-terms)
                       (2vals-map non-tail-recur (syntax->list (syntax terms)))]
                      [free-varrefs (varref-set-union free-varrefs-terms)])
                   (vector
                    (let* ([arg-temps (build-list (length annotated-terms) get-arg-var)]
                           [tagged-arg-temps (map (lambda (var) (stepper-syntax-property var 'stepper-binding-type 'stepper-temp))
                                                  arg-temps)]
                           [let-clauses #`((#,tagged-arg-temps 
                                            (#%plain-app values #,@(map (lambda (_) *unevaluated*) tagged-arg-temps))))]
                           [set!-list (map (lambda (arg-symbol annotated-sub-exp)
                                             #`(set! #,arg-symbol #,annotated-sub-exp))
                                           tagged-arg-temps annotated-terms)]
                           [new-tail-bound (binding-set-union (list tail-bound tagged-arg-temps))]
                           [app-debug-info (make-debug-info-app new-tail-bound tagged-arg-temps 'called)]
                           [app-term (quasisyntax/loc exp (#%plain-app #,@tagged-arg-temps))]
                           [debug-info (make-debug-info-app new-tail-bound
                                                            (varref-set-union (list free-varrefs tagged-arg-temps)) ; NB using bindings as vars
                                                            'not-yet-called)]
                           [let-body (outer-wcm-wrap debug-info #`(begin #,@set!-list
                                                                         #,(break-wrap
                                                                            (wcm-wrap
                                                                             app-debug-info
                                                                             #`(if (#%plain-app #,annotated-proc? #,(car tagged-arg-temps))
                                                                                   #,app-term
                                                                                   #,(return-value-wrap app-term))))))])
                      #`(let-values #,let-clauses #,let-body))
                    ;)
                    free-varrefs))]
                
                
                ;      @@                             
                ;       @          @                  
                ;    $@:@   $@$:  @@@@@ @@  @@ @@+-$: 
                ;   $* *@     -@   @     @   @  @+@$@ 
                ;   @   @  -$@$@   @     @   @  @ @ @ 
                ;   @   @  $*  @   @     @   @  @ @ @ 
                ;   $* *@  @- *@   @: :$ @: +@  @ @ @ 
                ;    $@:@@ -$$-@@  :@@$- :@$-@@@@@@@@@
                
                
                [(#%top . var-stx) (varref-abstraction #`var-stx)]
                
                [var-stx
                 (identifier? #`var-stx)
                 (varref-abstraction #`var-stx)]
                
                [else 
                 (error 'annotate "unexpected syntax for expression: ~v" (syntax->datum exp))]))))])))
  
  ;; annotate/top-level : syntax-> syntax
  ;; expansion of teaching level language programs produces two kinds of 
  ;; expressions: modules containing all of the code in the def'ns window, and
  ;; require statements that invoke those modules.  In the first case, we must annotate
  ;; the expressions inside the top-level module, and in the second, we should just
  ;; leave it alone.
  
  (define/contract annotate/top-level
    (syntax? . -> . syntax?)
    (lambda (exp)
      (syntax-case exp (module #%plain-module-begin let-values dynamic-wind #%plain-lambda #%plain-app define-values)
        [(module name lang
           (#%plain-module-begin . bodies))
         #`(module name lang (#%plain-module-begin #,@(map annotate/module-top-level (syntax->list #`bodies))))]
        ; the 'require' form is used for the test harness
        [(require module-name) exp]
        ; the 'dynamic-require' form is used by the actual expander 
        [(let-values ([(done-already?) . rest1])
           (#%plain-app dynamic-wind
                        void
                        (#%plain-lambda () . rest2)
                        (#%plain-lambda () . rest3)))
         exp]
        ; STC: for lazy, handle defines
        [(define-values (ids ...) bodies) (annotate/module-top-level exp)]
        [else (annotate/module-top-level exp)]
        #;[else
         (error `annotate/top-level "unexpected top-level expression: ~a\n"
                (syntax->datum exp))
         #;(annotate/module-top-level exp)])))
  
  #;(define/contract annotate/top-level/acl2
    (syntax? . -> . syntax?)
    (lambda (exp)
      (syntax-case exp (begin define-values #%plain-app)
        [(begin contract-thingy 
                (begin body (begin)))
         #`(begin contract-thingy (begin #,(annotate/module-top-level #`body) (begin)))]
        
        #;(define-values
            (lifted)
            (begin
              (#%app
               contract/proc
               provide/contract-contract-id-zp
               zp
               provide/contract-pos-module-source-zp
               (#%app module-source-as-symbol (quote-syntax here))
               (quote-syntax zp))))
        #;(if (#%app null? (#%app lifted (#%datum . 3))) 'y 'x)
        
        
        [else (annotate/module-top-level exp)]
        
        #;[else (begin
                  (eprintf "~v\n" (syntax->datum exp))
                  (error `annotate/top-level "unexpected top-level expression: ~a\n" (syntax->datum exp)))])))
  
  
  
  ;; annotate expressions at the top level within a module.
  (define (annotate/module-top-level exp)
    (cond [(stepper-syntax-property exp 'stepper-replace)]
          [(to-be-skipped? exp) exp]
          ;; for kathy's test engine:
          [(syntax-property exp 'test-call) exp]
          [(stepper-syntax-property exp 'stepper-black-box-expr)
           #`(begin #,exp
                    (#%plain-app #,(make-opaque-exp-break exp)))]
          [(stepper-syntax-property exp 'stepper-skipto)
           (skipto/auto exp 'rebuild annotate/module-top-level)] 
          [else 
           (syntax-case exp (#%app #%plain-app call-with-values define-values define-syntaxes 
                                   #%require #%provide begin #%plain-lambda lambda)
             [(define-values (new-var ...) e)
              (let* ([name-list (syntax->list #`(new-var ...))]
                     [defined-name (if (and (pair? name-list) (null? (cdr name-list)))
                                       (car name-list)
                                       #f)])
                (stepper-recertify
                 #`(begin
                     (define-values (new-var ...)
                       #,(top-level-annotate/inner (top-level-rewrite #`e) exp defined-name))
                     ;; this next expression should deliver the newly computed values to an
                     ;; exp-finished-break
                     (#%plain-app #,exp-finished-break
                                  (#%plain-app list 
                                               (#%plain-app list
                                                            #,(lambda () exp)
                                                            #f
                                                            (#%plain-lambda ()
                                                                            (#%plain-app
                                                                             list
                                                                             new-var ...))))))
                 #'e))]
             [(define-syntaxes (new-vars ...) e)
              exp]
             [(#%require specs ...)
              ;; this should only include requires inserted automatically, as others should 
              ;; get caught above in the "stepper-black-box-expr" check:
              exp]
             [(#%provide specs ...)
              exp]
             [(begin .  bodies)
              #`(begin #,@(map annotate/module-top-level (syntax->list #`bodies)))]
             ; STC: for lazy racket, need this case to catch and hide toplevel-forcer
             ; stepper tests will expand to this case, with call-with-values
             [(#%plain-app 
               call-with-values 
               (#%plain-lambda 
                ()
                (#%plain-app (#%plain-app toplevel-forcer) operand)) 
               print-values)
              (stepper-recertify
               #`(#%plain-app
                  call-with-values
                  (#%plain-lambda 
                   () 
                   (#%plain-app 
                    (#%plain-app toplevel-forcer)
                    #,(top-level-annotate/inner (top-level-rewrite #'operand) exp #f)))
                  (#%plain-lambda 
                   vals
                   (begin
                     (#,exp-finished-break
                      (#%plain-app 
                       list 
                       (#%plain-app 
                        list 
                        #,(lambda () exp) #f (#%plain-lambda () vals))))
                     (#%plain-app 
                      call-with-values 
                      (#%plain-lambda () vals) values))))
               exp)]
             [(#%plain-app call-with-values (#%plain-lambda () body) print-values)
              ;; re-extract the plain-lambda term, to use in recertification:
              (let ([lam-for-cert (syntax-case exp (#%plain-app call-with-values)
                                    [(#%plain-app call-with-values lam print-values) #'lam]
                                    [other (error 'annotate/module-top-level "unreachable 2010-01-23 22:14")])])
                ;; this recertify looks to be superfluous now that it has the "transparent" certificate-mode tag,
                ;; but it can't hurt, and I'd rather just leave it in here.
                (stepper-recertify 
                 #`(#%plain-app
                    call-with-values 
                    #,(stepper-recertify
                       #`(#%plain-lambda () #,(top-level-annotate/inner (top-level-rewrite #`body) exp #f))
                       lam-for-cert)
                    (#%plain-lambda vals
                                    (begin
                                      (#,exp-finished-break (#%plain-app list (#%plain-app list #,(lambda () exp) #f (#%plain-lambda () vals))))
                                      (#%plain-app
                                       call-with-values (#%plain-lambda () vals)
                                       print-values))))
                 exp))]
             ; STC: for lazy racket, need this case to catch and hide toplevel-forcer
             ; This is similar to app case above, but with toplevel-forcer
             ; normal lazy stepper operation expands to this case
             [(#%plain-app (#%plain-app toplevel-forcer) operand)
              (stepper-recertify
               #`(#%plain-app
                  call-with-values
                  (#%plain-lambda 
                   () 
                   (#%plain-app 
                    (#%plain-app toplevel-forcer)
                    #,(top-level-annotate/inner (top-level-rewrite #'operand) exp #f)))
                  (#%plain-lambda 
                   vals
                   (begin
                     (#,exp-finished-break
                      (#%plain-app 
                       list 
                       (#%plain-app 
                        list 
                        #,(lambda () exp) #f (#%plain-lambda () vals))))
                     (#%plain-app 
                      call-with-values 
                      (#%plain-lambda () vals) values))))
               exp)]
             [any
              (stepper-syntax-property exp 'stepper-test-suite-hint)
              (top-level-annotate/inner (top-level-rewrite exp) exp #f)]
             [else
              (top-level-annotate/inner (top-level-rewrite exp) exp #f)
              ;; the following check can't be permitted in the presence of things like test-suite cases
              ;; which produce arbitrary expressions at the top level.
              #;(error `annotate/module-top-level "unexpected module-top-level expression to annotate: ~a\n" (syntax->datum exp))])]))
  
  
  ; body of local
  (annotate/top-level main-exp))

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

; top-level-rewrite performs several tasks; it labels variables with their types 
; (let-bound, lambda-bound, or non-lexical), it flags if's which could come from 
; cond's, it labels the begins in conds with 'stepper-skip annotations

; label-var-types returns a syntax object which is identical to the 
; original except that the variable references are labeled with the 
; stepper-syntax-property 'stepper-binding-type, which is set to either 
; let-bound, lambda-bound, or non-lexical. (It can also be 'macro-bound, set
; earlier during macro expansion.)

(define (top-level-rewrite stx)
  (let loop ([stx stx]
             [let-bound-bindings null]
             [cond-test (lx #f)])
    (define (recur-regular stx)
      (loop stx let-bound-bindings (lx #f)))
    
    (define (recur-with-bindings exp vars)
      (loop exp (append vars let-bound-bindings) (lx #f)))
    
    (define (recur-in-cond stx new-cond-test)
      (loop stx let-bound-bindings new-cond-test))
    
    (define (do-let/rec stx rec?)
      (with-syntax ([(label ((vars rhs) ...) . bodies) stx])
        (let* ([vars-list 
                (apply append
                       (map syntax->list
                            (syntax->list (syntax (vars ...)))))]
               [labelled-vars-list 
                (map (lambda (var-list)
                       (map (lambda (exp)
                              (recur-with-bindings exp vars-list))
                            (syntax->list var-list)))
                     (syntax->list (syntax (vars ...))))]
               [rhs-list 
                (if rec?
                    (map (lambda (exp)
                           (recur-with-bindings exp vars-list))
                         (syntax->list #'(rhs ...)))
                    (map recur-regular (syntax->list #'(rhs ...))))]
               [new-bodies 
                (map (lambda (exp)
                       (recur-with-bindings exp vars-list))
                     (syntax->list #'bodies))]
               [new-bindings (map list labelled-vars-list rhs-list)])
          (datum->syntax
           stx
           `(,#'label ,new-bindings ,@new-bodies) stx stx))))
    
    
    ; evaluated at runtime, using 3D code:
    (define (put-into-xml-table val)
      (hash-set! finished-xml-box-table val #t)
      val)
    
    (cond 
      [(or (to-be-skipped? stx)
           (stepper-syntax-property stx 'stepper-black-box-expr))
       stx]
      [else
       (define rewritten
         (let ([stx (syntax-disarm stx saved-code-inspector)])
           (kernel:kernel-syntax-case 
          stx
          #f
          ; cond :
          [(if test (let-values () then) else-stx)
           (let ([origin (syntax-property stx 'origin)]
                 [rebuild-if
                  (lambda (new-cond-test)
                    (let* ([new-then (recur-regular (syntax then))]
                           [rebuilt 
                            (stepper-syntax-property
                             (rebuild-stx 
                              `(if ,(recur-regular (syntax test))
                                   ,new-then
                                   ,(recur-in-cond (syntax else-stx)
                                                   new-cond-test))
                                          stx)
                             'stepper-hint
                             'comes-from-cond)])
                      ; move the stepper-else mark to the if, if it's present:
                      (if (stepper-syntax-property (syntax test) 'stepper-else)
                          (stepper-syntax-property rebuilt 'stepper-else #t)
                          rebuilt)))])
             (cond [(cond-test stx) ; continuing an existing 'cond'
                    (rebuild-if cond-test)]
                   [(and origin (pair? origin) 
                         (eq? (syntax-e (car origin)) 'cond)) ; starting a new 'cond'
                    (rebuild-if (lambda (test-stx) 
                                  (and (eq? (syntax-source stx)
                                            (syntax-source test-stx))
                                       (eq? (syntax-position stx)
                                            (syntax-position test-stx)))))]
                   [else ; not from a 'cond' at all.
                    (rebuild-stx `(if ,@(map recur-regular (list (syntax test) (syntax (begin then)) (syntax else-stx)))) stx)]))]
          [(begin body) ; else clauses of conds; ALWAYS AN ERROR CALL
           (cond-test stx)
           (stepper-syntax-property stx 'stepper-skip-completely #t)]
          
          ; wrapper on a local.  This is necessary because 
          ; teach.rkt expands local into a trivial let wrapping a bunch of
          ; internal defines, and therefore the letrec-values on 
          ; which I want to hang the 'stepper-hint doesn't yet
          ; exist.  So we patch it up after expansion.  And we 
          ; discard the outer 'let' at the same time.
          [(let-values () expansion-of-local)
           (eq? (stepper-syntax-property stx 'stepper-hint) 'comes-from-local)
           (syntax-case #`expansion-of-local (letrec-values)
             [(letrec-values (bogus-clause clause ...) . bodies)
              (recur-regular
               (stepper-syntax-property #`(letrec-values (clause ...) . bodies) 'stepper-hint 'comes-from-local))]
             [else (error 'top-level-rewrite "expected a letrec-values inside a local, given: ~e" 
                          (syntax->datum #`expansion-of-local))])]
          
          ; let/letrec :
          [(let-values x ...) (do-let/rec stx #f)]
          [(letrec-values x ...) (do-let/rec stx #t)]
          
          ; varref :
          [var
           (identifier? (syntax var))
           (stepper-syntax-property 
            (syntax var) 
            'stepper-binding-type
            (if (eq? (identifier-binding (syntax var)) 'lexical)
                (cond [(ormap (lx (bound-identifier=? _ (syntax var)))
                              let-bound-bindings)
                       'let-bound]
                      [else
                       'lambda-bound])
                'non-lexical))]
          
          [else
           (let ([content (syntax-e stx)])
             (if (pair? content)
                 (rebuild-stx (syntax-pair-map content recur-regular) stx)
                 stx))])))
       
       (if (eq? (stepper-syntax-property stx 'stepper-xml-hint) 'from-xml-box)
           (stepper-syntax-property #`(#%plain-app 
                                       #,put-into-xml-table
                                       #,rewritten) 
                                    'stepper-skipto
                                    (list syntax-e cdr car))
           (stepper-recertify rewritten stx))])))



;; recertify the output of the stepper, to allow it to run:
(define (stepper-recertify new-stx old-stx)
  (syntax-rearm new-stx old-stx #t))

;; does this stx have the 'stepper-skip-completely property?
(define (to-be-skipped? stx)
  (stepper-syntax-property stx 'stepper-skip-completely))
