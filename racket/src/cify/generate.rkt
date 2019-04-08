#lang racket/base
(require racket/list
         racket/unsafe/undefined
         "out.rkt"
         "match.rkt"
         "sort.rkt"
         "id.rkt"
         "vehicle.rkt"
         "primitive.rkt"
         "free-var.rkt"
         "function.rkt"
         "struct.rkt"
         "arg.rkt"
         "union-find.rkt"
         "state.rkt"
         "simple.rkt"
         "inline.rkt"
         "return.rkt"
         "ref.rkt"
         "runstack.rkt")

(provide generate-header
         generate-footer
         generate-struct
         generate-prototypes
         generate-vehicles
         generate-tops)

(define (generate-header)
  (out "#ifdef MZ_PRECISE_GC")
  (out "START_XFORM_SKIP;")
  (out "#endif"))

(define (generate-footer)
  (out-next)
  (out "#ifdef MZ_PRECISE_GC")
  (out "END_XFORM_SKIP;")
  (out "#endif"))

(define (generate-struct name names)
  (out-next)
  (out-open "struct ~a_t {" name)
  (for ([name (in-sorted-hash-keys names symbol<?)])
    (out "Scheme_Object *~a;" (cify name)))
  (out-close "};"))

(define (generate-prototypes vehicles)
  (out-next)
  (for ([vehicle (in-list vehicles)])
    (generate-prototype vehicle #:open? #f)))

(define (generate-prototype vehicle #:open? open?)
  ((if open? out-open out)
   "static Scheme_Object *~a(int c_argc, Scheme_Object **c_argv~a)~a"
   (cify (vehicle-id vehicle))
   (if (vehicle-closure? vehicle) ", Scheme_Object *c_self" "")
   (if open? " {" ";")))

(define (generate-vehicles vehicles lambdas knowns top-names state prim-names prim-knowns)
  (define (generate-vehicle vehicle)
    (out-next)
    (define lams (vehicle-lams vehicle))
    (define multi? (pair? (cdr lams)))
    (define leaf? (and (not multi?)
                       (lam-can-leaf? (car lams))
                       (zero? (hash-count (lam-loop-targets (car lams))))
                       (lambda-no-rest-args? (lam-e (car lams)))))
    (when leaf? (out "/* leaf */"))
    (generate-prototype vehicle #:open? #t)
    (when (or (not leaf?) (vehicle-uses-top? vehicle))
      (out "c_LINK_THREAD_LOCAL"))
    (define overflow-check?
      (and (vehicle-called-direct? vehicle) (vehicle-calls-non-immediate? vehicle)))
    (unless leaf?
      (out "Scheme_Object **c_runbase, **c_orig_runstack = c_current_runstack;")
      (out-open "if (c_check_~arunstack_space(~a, c_orig_runstack, c_current_runstack_start))"
                (if overflow-check? "overflow_or_" "")
                (vehicle-max-runstack-depth vehicle))
      (out "return c_handle_overflow_or_space(~a, c_argc, c_argv, ~a);"
           (if (vehicle-closure? vehicle) "c_self" (format "c_top->~a" (cify (vehicle-id vehicle))))
           (vehicle-max-runstack-depth vehicle))
      (out-close!)
      (out-open "if (c_argv == c_orig_runstack)")
      (out "c_runbase = c_argv + c_argc;")
      (out-close+open "else")
      (out "c_runbase = c_orig_runstack;")
      (out-close!))
    (cond
      [multi?
       (out-open "switch(SCHEME_INT_VAL(SCHEME_PRIM_CLOSURE_ELS(c_self)[0])) {")
       (out "default:")
       (for ([lam (in-list lams)]
             [i (in-naturals)])
         (out-open "case ~a:" i)
         (when (lam-no-rest-args? lam)
           (ensure-lambda-args-in-place leaf? lam))
         (out "goto c_entry_~a;" (cify (lam-id lam)))
         (out-close!))
       (out-close "}")]
      [else
       (when (lam-no-rest-args? (car lams))
         (ensure-lambda-args-in-place leaf? (car lams)))])
    (for ([lam (in-list lams)])
      (when (or multi? (lam-need-entry? lam))
        (out-margin "c_entry_~a:" (cify (lam-id lam))))
      (generate-lambda lam multi? leaf? (or multi? overflow-check?)))
    (out-close "}"))

  (define (generate-lambda lam multi? leaf? bracket?)
    (define e (lam-e lam))
    (define id (lam-id lam))
    (define free-var-refs (lam-free-var-refs lam))
    (define closure-offset (if multi? 1 0))
    (when bracket? (out-open "{"))
    (match e
      [`(lambda . ,_) (generate-lambda-case lam leaf? e free-var-refs closure-offset)]
      [`(case-lambda [,idss . ,bodys] ...)
       (for ([ids (in-list idss)]
             [body (in-list bodys)]
             [i (in-naturals)])
         (out-open "~aif (c_argc ~a ~a) {" (if (zero? i) "" "else ") (if (list? ids) "==" ">=") (args-length ids))
         (generate-lambda-case lam leaf? `(lambda ,ids . ,body) free-var-refs closure-offset)
         (out-close "}"))
       (out "~areturn c_wrong_arity(~s, c_argc, c_argv);"
            (if (null? idss) "" "else ")
            (format "~a" id))])
    (when bracket? (out-close "}")))

  ;; Returns a boolean indicating whether the functon can be a leaf
  (define (generate-lambda-case lam leaf? e free-var-refs closure-offset)
    (define name (lam-id lam))
    (match e
      [`(lambda ,ids . ,body)
       (define n (args-length ids))
       (when (not (lam-no-rest-args? lam))
         (define rest-arg? (not (list? ids)))
         (ensure-args-in-place leaf? n free-var-refs
                               #:rest-arg? rest-arg?
                               #:rest-arg-used? (and rest-arg? (referenced? (hash-ref state (extract-rest-arg ids) #f)))))
       (unless (null? ids) (out-open "{"))
       (when (and leaf? (for/or ([id (in-list ids)])
                          (referenced? (hash-ref state id #f))))
         (out "Scheme_Object **c_runbase = c_argv + ~a;" n))
       ;; At this point, for a non-leaf, runstack == runbase - argument-count (including rest)
       (define runstack (make-runstack state))
       (define pushed-arg-count
         (let loop ([ids ids])
           (cond
             [(null? ids) 0]
             [(symbol? ids) (loop (list ids))]
             [else
              ;; Push last first:
              (define count (add1 (loop (cdr ids))))
              (runstack-push! runstack (car ids) #:referenced? (referenced? (hash-ref state (car ids) #f)))
              count])))
       (runstack-synced! runstack) ; since runstack = start of arguments
       ;; Unpack closure
       (for ([ref (in-list free-var-refs)])
         (runstack-push! runstack (ref-id ref) #:local? leaf?))
       (for ([ref (in-list free-var-refs)]
             [i (in-naturals)])
         (define id (ref-id ref))
         (out "~a = SCHEME_PRIM_CLOSURE_ELS(c_self)[~a];" (runstack-assign runstack id) (+ closure-offset i)))
       (when (hash-ref (lam-loop-targets lam) n #f)
         (out-margin "c_recur_~a_~a:" (cify name) n))
       (clear-unused-ids ids runstack state)
       (box-mutable-ids ids runstack lam state top-names)
       (generate (tail-return name lam ids leaf?) `(begin . ,body) lam (add-args (lam-env lam) ids) runstack
                 knowns top-names state lambdas prim-names prim-knowns)
       (runstack-pop! runstack pushed-arg-count)
       (unless (null? ids) (out-close "}"))
       (let ([vehicle (lam-vehicle lam)])
         (set-vehicle-max-runstack-depth! vehicle (max (runstack-max-depth runstack)
                                                       (vehicle-max-runstack-depth vehicle))))
       (set-lam-can-leaf?! lam (not (runstack-ever-synced? runstack)))]))

  (for ([vehicle (in-list vehicles)])
    (generate-vehicle vehicle)))

(define (lam-constant-args-count? lam)
  (match (lam-e lam)
    [`(lambda (,_ ...) . ,_) #t]
    [`,_ #f]))

(define (lam-no-rest-args? lam)
  (match (lam-e lam)
    [`(lambda (,_ ...) . ,_) #t]
    [`(case-lambda [(,_ ...) . ,_] ...) #t]
    [`,_ #f]))

;; Only used when no rest args:
(define (ensure-lambda-args-in-place leaf? lam)
  (match (lam-e lam)
    [`(lambda ,ids . ,_)
     (ensure-args-in-place leaf? (length ids) #:rest-arg? #f (lam-free-var-refs lam))]
    [`,_
     (ensure-args-in-place leaf? "c_argc" #:rest-arg? #f (lam-free-var-refs lam))]))

(define (ensure-args-in-place leaf? expected-n free-var-refs
                              #:rest-arg? rest-arg?
                              #:rest-arg-used? [rest-arg-used? #t])
  ;; Generate code to make sure that `c_runbase` minus the number of
  ;; argument variables (including a "rest" args) holds arguments,
  ;; converting "rest" args to a list as needed. We don't need to
  ;; perform this check (or set `c_argv` and `c_argc`) for a call
  ;; within a vehicle for a non-`case-lambda`, because it will
  ;; definitely hold then.
  (unless leaf?
    (cond
      [rest-arg?
       (out "~ac_ensure_args_in_place_rest(c_argc, c_argv, c_runbase, ~a, 1, ~a, ~a);"
            (if (null? free-var-refs) "(void)" "c_self = ")
            expected-n
            (if rest-arg-used? "c_rest_arg_used" "c_rest_arg_unused")
            (if (null? free-var-refs) "NULL" "c_self"))]
      [(eqv? 0 expected-n)
       ;; No args; we can always assume that 0 arguments are at `_runbase`
       (void)]
      [else
       ;; No rest arg
       (out "c_ensure_args_in_place(~a, c_argv, c_runbase);" expected-n)])))

(define (box-mutable-ids ids runstack in-lam state top-names)
  (let loop ([ids ids])
    (unless (null? ids)
      (cond
        [(symbol? ids) (loop (list ids))]
        [else
         (when (and (mutated? (hash-ref state (car ids) #f))
                    (not (hash-ref top-names (car ids) #f)))
           (define s (let ([id (car ids)])
                       (if (hash-ref top-names id #f)
                           (format "~a =" (top-ref in-lam id))
                           (runstack-assign runstack id))))
           (runstack-sync! runstack)
           (out "~a = scheme_box_variable(~a);" s s))
         (loop (cdr ids))]))))

(define (clear-unused-ids ids runstack state)
  (let loop ([ids ids])
    (unless (null? ids)
      (cond
        [(symbol? ids) (loop (list ids))]
        [else
         (define id (car ids))
         (when (and (not (referenced? (hash-ref state id #f)))
                    (not (state-implicitly-referenced? state id)))
           (runstack-stage-clear! runstack id state))
         (loop (cdr ids))]))))

;; ----------------------------------------

(define (generate ret e in-lam env runstack knowns top-names state lambdas prim-names prim-knowns)
  (define (generate ret e env)
    (match e
      [`(quote ,v)
       (generate-quote ret v)]
      [`(lambda . ,_)
       (generate-closure ret e env)]
      [`(case-lambda . ,_)
       (generate-closure ret e env)]
      [`(begin ,e)
       (generate ret e env)]
      [`(begin ,e . ,r)
       (generate (multiple-return "") e env)
       (generate ret `(begin . ,r) env)]
      [`(begin0 ,e . ,r)
       (define vals-id (genid 'c_vals))
       (out-open "{")
       (runstack-push! runstack vals-id)
       (out "int ~a_count;" vals-id)
       (generate (multiple-return (lambda (s)
                                    (out-open "{")
                                    (out "~a = ~a;" (runstack-assign runstack vals-id) s)
                                    (out-open "if (~a == SCHEME_MULTIPLE_VALUES) {" (runstack-ref runstack vals-id #:values-ok? #t))
                                    (out "Scheme_Object **~a_vals;" vals-id)
                                    (out "~a_vals = c_current_thread->ku.multiple.array;" vals-id)
                                    (out "~a_count = c_current_thread->ku.multiple.count;" vals-id)
                                    (out "if (SAME_OBJ(~a_vals, c_current_thread->values_buffer))" vals-id)
                                    (out "  c_current_thread->values_buffer = NULL;")
                                    (out "~a = (Scheme_Object *)~a_vals;" (runstack-assign runstack vals-id) vals-id)
                                    (out-close+open "} else")
                                    (out "~a_count = 1;" vals-id)
                                    (out-close!)
                                    (out-close "}")))
                 e env)
       (generate (multiple-return "") `(begin . ,r) env)
       (runstack-sync! runstack)
       (out-open "if (~a_count != 1)" vals-id)
       (return ret runstack #:can-omit? #t
               (format "scheme_values(~a_count, (Scheme_Object **)~a)" vals-id (runstack-ref runstack vals-id #:values-ok? #t)))
       (cond
         [(return-can-omit-single? ret)
          (out-close!)]
         [else
          (out-close+open "else")
          (return ret runstack #:can-omit? #t #:can-pre-pop? #t
                  (runstack-ref runstack vals-id))
          (out-close!)])
       (runstack-pop! runstack)
       (out-close "}")]
      [`(if ,orig-tst ,thn ,els)
       (define-values (tsts sync-for-gc? wrapper) (extract-inline-predicate orig-tst in-lam knowns #:compose? #t))
       (define tst-ids (for/list ([tst (in-list tsts)])
                         (if (simple? tst in-lam state knowns)
                             #f
                             (genid 'c_if))))
       (define all-simple? (for/and ([tst-id (in-list tst-ids)])
                             (not tst-id)))
       ;; The last `tst-id` doesn't need to be on the runstack
       (define immediate-tst-id (for/fold ([id #f]) ([tst-id (in-list tst-ids)])
                                  (or tst-id id)))
       (unless all-simple? (out-open "{"))
       (define tst-id-count
         (for/sum ([tst-id (in-list tst-ids)]
                   #:when (and tst-id (not (eq? tst-id immediate-tst-id))))
           (runstack-push! runstack tst-id)
           1))
       (when immediate-tst-id
         (out "Scheme_Object *~a;" (cify immediate-tst-id)))
       (for ([tst-id (in-list tst-ids)]
             [tst (in-list tsts)]
             #:when tst-id)
         (generate (if (eq? tst-id immediate-tst-id)
                       (format "~a =" (cify tst-id))
                       (make-runstack-assign runstack tst-id))
                   tst env))
       (call-with-simple-shared
        (cons 'begin (for/list ([tst-id (in-list tst-ids)]
                                [tst (in-list tsts)]
                                #:when (not tst-id))
                       tst))
        runstack state
        #:about-to-sync? sync-for-gc?
        (lambda (shared)
          (when sync-for-gc?
            (runstack-sync! runstack))
          (out-open "if (~a) {"
                    (wrapper (apply string-append
                                    (add-between
                                     (for/list ([tst-id (in-list tst-ids)]
                                                [tst (in-list tsts)])
                                       (format "~a"
                                               (cond
                                                 [(not tst-id) (generate-simple tst shared env runstack in-lam state top-names knowns prim-names)]
                                                 [(eq? tst-id immediate-tst-id) (cify tst-id)]
                                                 [tst-id (runstack-ref runstack tst-id)])))
                                     ", "))))
          (define-values (thn-refs els-refs) (let ([p (hash-ref state e '(#hasheq() . #hasheq()))])
                                               (values (car p) (cdr p))))
          (define pre-branch (runstack-branch-before! runstack))
          (define pre-ref-use (ref-use-branch-before! state))
          (runstack-stage-clear-unused! runstack thn-refs els-refs state)
          (generate ret thn env)
          (out-close+open "} else {")
          (runstack-stage-clear-unused! runstack els-refs thn-refs state)
          (define post-branch (runstack-branch-other! runstack pre-branch))
          (define post-ref-use (ref-use-branch-other! state pre-ref-use))
          (generate ret els env)
          (when (state-first-pass? state)
            (define-values (thn-refs els-refs) (runstack-branch-refs runstack pre-branch post-branch))
            (hash-set! state e (cons thn-refs els-refs)))
          (runstack-branch-merge! runstack pre-branch post-branch)
          (ref-use-branch-merge! state pre-ref-use post-ref-use)
          (out-close "}")
          (runstack-pop! runstack tst-id-count)))
       (unless all-simple? (out-close "}"))]
      [`(with-continuation-mark ,key ,val ,body)
       (define wcm-id (genid 'c_wcm))
       (define wcm-key-id (genid 'c_wcm_key))
       (define wcm-val-id (genid 'c_wcm_val))
       (out-open "{")
       (define simple-key? (simple? key in-lam state knowns))
       (define simple-val? (simple? val in-lam state knowns))
       (define simple-either? (or simple-key? simple-val?))
       (runstack-push! runstack wcm-key-id #:local? simple-either?)
       (runstack-push! runstack wcm-val-id #:local? simple-either?)
       (unless (tail-return? ret)
         (out "c_saved_mark_stack_t ~a_frame = c_push_mark_stack();" wcm-id))
       (cond
         [(and simple-key? (not simple-val?))
          ;; Value first
          (generate (make-runstack-assign runstack wcm-val-id) val env)
          (generate (make-runstack-assign runstack wcm-key-id) key env)]
         [else
          ;; Key first
          (generate (make-runstack-assign runstack wcm-key-id) key env)
          (generate (make-runstack-assign runstack wcm-val-id) val env)])
       (define set-cont-mark (format "scheme_set_cont_mark(~a, ~a);"
                                     (runstack-ref runstack wcm-key-id)
                                     (runstack-ref runstack wcm-val-id)))
       (runstack-pop! runstack 2)
       (runstack-sync! runstack)
       (out set-cont-mark)
       (generate ret body env)
       (unless (tail-return? ret)
         (out "c_pop_mark_stack(~a_frame);" wcm-id))
       (out-close "}")]
      [`(let () . ,body) (generate ret `(begin . ,body) env)]
      [`(let (,_) . ,_) (generate-let ret e env)]
      [`(let ([,id ,rhs] . ,rest) . ,body)
       ;; One at a time, so runstack slot can be allocated after RHS:
       (generate ret `(let ([,id ,rhs]) (let ,rest . ,body)) env)]
      [`(letrec . ,_) (generate-let ret e env)]
      [`(letrec* . ,_) (generate-let ret e env)]
      [`(call-with-values (lambda () . ,body1) (lambda (,ids ...) . ,body2))
       (define values-ret (if (for/or ([id (in-list ids)])
                                (or (referenced? (hash-ref state id #f))
                                    (hash-ref top-names id #f)))
                              "/*needed*/"
                              ""))
       (generate (only-multiple-return values-ret) `(begin . ,body1) env)
       (out-open "{")
       (define bind-count
         (for/sum ([id (in-list ids)]
                   #:unless (or (hash-ref top-names id #f)
                                (not (referenced? (hash-ref state id #f)))))
           (runstack-push! runstack id)
           1))
       (generate-multiple-value-binds ids runstack in-lam state top-names)
       (box-mutable-ids ids runstack in-lam state top-names)
       (generate ret `(begin . ,body2) (add-args env ids))
       (runstack-pop! runstack bind-count)
       (out-close "}")]
      [`(set! ,ref ,rhs)
       (define top? (hash-ref top-names ref #f))
       (define target
         (cond
           [top? (top-ref in-lam ref)]
           [else (genid 'c_set)]))
       (unless top?
         (out-open "{")
         (out "Scheme_Object *~a;" target))
       (generate (format "~a =" target) rhs env)
       (unless top?
         (when (ref? ref) (ref-use! ref state))
         (out "SCHEME_UNBOX_VARIABLE_LHS(~a) = ~a;"
              (runstack-ref runstack (unref ref) #:ref (and (ref? ref) ref))
              target)
         (out-close "}"))
       (generate ret '(void) env)]
      [`(void) (return ret runstack #:can-omit? #t #:can-pre-pop? #t "scheme_void")]
      [`(void . ,r)
       (generate ret `(begin ,@r (void)) env)]
      [`(values ,r)
       (generate ret r env)]
      [`null (return ret runstack #:can-omit? #t #:can-pre-pop? #t "scheme_null")]
      [`eof-object (return ret runstack #:can-omit? #t #:can-pre-pop? #t "scheme_eof")]
      [`unsafe-undefined (return ret runstack #:can-omit? #t #:can-pre-pop? #t "scheme_undefined")]
      [`(,rator ,rands ...)
       (generate-app ret rator rands env)]
      [`,_
       (cond
         [(symbol-ref? e)
          (when (ref? e) (ref-use! e state))
          (define can-omit? (not (and (ref? e) (ref-last-use? e))))
          (define id (unref e))
          (return ret runstack #:can-omit? can-omit? #:can-pre-pop? #t
                  (cond
                    [(hash-ref env id #f)
                     (cond
                       [(mutated? (hash-ref state id #f))
                        (format "SCHEME_UNBOX_VARIABLE(~a)"
                                (runstack-ref runstack id #:ref e))]
                       [else
                        (when (and (return-can-omit? ret)
                                   can-omit?
                                   (state-first-pass? state))
                          (adjust-state! state id -1))
                        (runstack-ref runstack id #:ref e)])]
                    [(hash-ref top-names id #f) (top-ref in-lam id)]
                    [else (format "c_prims.~a" (cify id))]))]
         [else (generate-quote ret e)])]))
  
  (define (generate-let ret e env)
    (match e
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (define body-env (for/fold ([env env]) ([id (in-list ids)]
                                               ;; Leave out of the environment if flattened
                                               ;; into the top sequence:
                                               #:unless (hash-ref top-names id #f))
                          (when (eq? let-id 'letrec)
                            (unless (function? (hash-ref knowns id #f))
                              (log-error "`letrec` binding should have been treated as closed: ~e" id)))
                          (hash-set env id #t)))
       (define rhs-env (if (eq? let-id 'let) env body-env))
       (out-open "{")
       (define (push-binds)
         (for/sum ([id (in-list ids)]
                   #:unless (or (not (referenced? (hash-ref state id #f)))
                                ;; flattened into top?
                                (hash-ref top-names id #f)))
           (runstack-push! runstack id #:track-local? (eq? let-id 'let))
           1))
       (define let-one? (and (eq? let-id 'let)
                             (= 1 (length ids))
                             (referenced? (hash-ref state (car ids) #f))
                             (not (hash-ref top-names (car ids) #f))))
       (define pre-bind-count (if let-one? 0 (push-binds)))
       (define let-one-id (and let-one? (genid 'c_let)))
       (when let-one?
         (out "Scheme_Object *~a;" (cify let-one-id)))
       (when (eq? let-id 'letrec*)
         (box-mutable-ids ids runstack in-lam state top-names))
       (for ([id (in-list ids)]
             [rhs (in-list rhss)])
         (cond
           [(eq? let-id 'letrec*)
            (generate "" `(set! ,id ,rhs) rhs-env)]
           [(not (referenced? (hash-ref state id #f)))
            (generate "" rhs rhs-env)]
           [else
            (define ret (cond
                          [let-one? (format "~a =" (cify let-one-id))]
                          [(hash-ref top-names id #f) (format "~a =" (top-ref in-lam id))]
                          [else
                           (make-runstack-assign runstack id)]))
            (generate ret rhs rhs-env)]))
       (when let-one?
         (runstack-push! runstack (car ids) #:track-local? #t)
         (out "~a = ~a;" (runstack-assign runstack (car ids)) (cify let-one-id)))
       (when (eq? let-id 'let)
         (box-mutable-ids ids runstack in-lam state top-names))
       (generate ret `(begin . ,body) body-env)
       (runstack-pop! runstack (if let-one? 1 pre-bind-count)
                      #:track-local? (eq? let-id 'let))
       (out-close "}")
       (when (state-first-pass? state)
         ;; For any variable that has become unused, mark a
         ;; right-hand side function as unused
         (for ([id (in-list ids)]
               [rhs (in-list rhss)])
           (unless (referenced? (hash-ref state id #f))
             (define lam-e (match rhs
                             [`(lambda . ,_) rhs]
                             [`(case-lambda . ,_) rhs]
                             [`,_ #f]))
             (when lam-e
               (define lam (hash-ref lambdas lam-e #f))
               (set-lam-unused?! lam #t)))))]))

  (define (generate-app ret rator rands env)
    (define n (length rands))
    (cond
      [(and (symbol? rator)
            (inline-function rator n rands in-lam knowns))
       (generate-inline-app ret rator rands n env)]
      [(and (symbol? rator)
            (let ([k (hash-ref knowns rator #f)])
              (and (struct-constructor? k)
                   (struct-info-pure-constructor? (struct-constructor-si k))
                   k)))
       => (lambda (k)
            (generate-inline-construct ret k rands n env))]
      [else
       (generate-general-app ret rator rands n env)]))

  (define (generate-inline-app ret rator rands n env)
    (define need-sync? (not (inline-function rator n rands in-lam knowns #:can-gc? #f)))
    (define tmp-ids (for/list ([rand (in-list rands)]
                               [i (in-naturals)])
                      (and (not (simple? rand in-lam state knowns))
                           (genid (format "c_arg_~a_" i)))))
    (define all-simple? (for/and ([tmp-id (in-list tmp-ids)])
                          (not tmp-id)))
    (unless all-simple? (out-open "{"))
    (define tmp-count
      (for/sum ([tmp-id (in-list tmp-ids)]
                #:when tmp-id)
        (runstack-push! runstack tmp-id)
        1))
    (for ([tmp-id (in-list tmp-ids)]
          [rand (in-list rands)])
      (when tmp-id
        (generate (make-runstack-assign runstack tmp-id)
                  rand env)))
    (define inline-app (cons rator (for/list ([tmp-id (in-list tmp-ids)]
                                              [rand (in-list rands)])
                                     (or tmp-id rand))))
    (call-with-simple-shared
     inline-app
     runstack state
     #:about-to-sync? need-sync?
     (lambda (shared)
       (when need-sync?
         (runstack-sync! runstack))
       (define s (generate-simple inline-app shared env runstack in-lam state top-names knowns prim-names))
       (return ret runstack #:can-pre-pop? #t s)
       (runstack-pop! runstack tmp-count)))
    (unless all-simple? (out-close "}")))

  (define (generate-inline-construct ret k rands n env)
    (define si (struct-constructor-si k))
    (out-open "{")
    (define struct-tmp-id (genid 'c_structtmp))
    (out "Scheme_Object *~a;" (cify struct-tmp-id))
    (runstack-sync! runstack)
    (out "~a = c_malloc_struct(~a);" (cify struct-tmp-id) (struct-info-field-count si))
    (out "c_struct_set_type(~a, ~a);" (cify struct-tmp-id) (top-ref in-lam (struct-info-struct-id si)))
    (define all-simple? (for/and ([rand (in-list rands)])
                          (simple? rand in-lam state knowns)))
    (define struct-id (and (not all-simple?) (genid 'c_struct)))
    (unless all-simple?
      (out-open "{")
      (runstack-push! runstack struct-id #:track-local? #t)
      (out "~a = ~a;" (runstack-assign runstack struct-id) (cify struct-tmp-id)))
    (for ([rand (in-list rands)]
          [i (in-naturals)])
      (define to-struct-s (format "c_STRUCT_ELS(~a)[~a] ="
                                  (if all-simple?
                                      (cify struct-tmp-id)
                                      (runstack-ref runstack struct-id))
                                  i))
      (generate (if all-simple?
                    to-struct-s
                    (format "~a =" (cify struct-tmp-id)))
                rand env)
      (unless all-simple?
        (out "~a ~a;" to-struct-s (cify struct-tmp-id))))
    (return ret runstack (if all-simple?
                             (cify struct-tmp-id)
                             (runstack-ref runstack struct-id)))
    (unless all-simple?
      (runstack-pop! runstack 1 #:track-local? #t)
      (out-close "}"))
    (out-close "}"))

  (define (generate-general-app ret rator rands n env)
    (define known-target-lam (let ([f (hash-ref knowns rator #f)])
                               (and (function? f) (hash-ref lambdas (function-e f)))))
    ;; If the target is known for a tail call, put this lambda and
    ;; that one in the same vehicle, so the tail call can be a jump:
    (when (and (tail-return? ret)
               known-target-lam
               (state-first-pass? state))
      (union! state (tail-return-lam ret) known-target-lam))
    ;; If it's known, we can jump as long as the target is in the same
    ;; vehicle (which we just ensured, at least for a second pass)
    (define direct? (and known-target-lam
                         (or (not (tail-return? ret))
                             (eq? (find! state known-target-lam)
                                  (find! state (tail-return-lam ret))))
                         (compatible-args? n (lam-e known-target-lam))))
    (define direct-tail? (and direct? (tail-return? ret)))
    ;; Do we need to evaluate the rator expression? If so, `rator-id`
    ;; will be non-#f:
    (define rator-id (cond
                       [direct? #f]
                       [(simple? rator in-lam state knowns) #f]
                       [else (genid 'c_rator)]))
    ;; For a non-tail call, make a runstack id for every argument;
    ;; that part of the runstack will be argv.
    ;; For a tail call, we only need an arg-id for a non-simple
    ;; expression, and we don't need one for the last non-simple.
    (define arg-ids (for/list ([rand (in-list rands)]
                               [i (in-naturals)])
                      (if (and direct-tail?
                               (simple? rand in-lam state knowns))
                          #f
                          (genid (format "c_arg_~a_" i)))))
    (define last-non-simple-arg-id
      (and direct-tail? (for/last ([arg-id (in-list arg-ids)])
                          arg-id)))
    (define open? (not (and (zero? n) (not rator-id))))
    (when open? (out-open "{"))
    ;; We can perform less runstack work by evaluating the first
    ;; argument before making room on the runstack:
    (define-values (first-non-simple-id first-non-simple-e)
      (if rator-id
          (values rator-id rator)
          (let loop ([arg-ids arg-ids] [rands rands])
            (cond
              [(null? arg-ids) (values #f #f)]
              [(simple? (car rands) in-lam state knowns) (loop (cdr arg-ids) (cdr rands))]
              [else (values (car arg-ids) (car rands))]))))
    (define first-tmp-id (and first-non-simple-id
                              (genid 'c_argtmp)))
    (when first-non-simple-id
      (out "Scheme_Object *~a;" (cify first-tmp-id))
      (generate (format "~a =" (cify first-tmp-id))
                first-non-simple-e env))
    (when last-non-simple-arg-id ; could be the same as `first-non-simple-id`
      (out "Scheme_Object *~a;" (cify last-non-simple-arg-id)))
    (define declared-tmp? (or first-non-simple-id last-non-simple-arg-id))
    (when declared-tmp? (out-open "{"))
    ;; Allocate the runstack room; put space for the rator,
    ;; if needed, at the end, so it's after the runstack as argv
    (when rator-id
      (runstack-push! runstack rator-id))
    (define arg-push-count
      (for/sum ([arg-id (reverse arg-ids)]
                #:when (and arg-id
                            (not (eq? arg-id last-non-simple-arg-id))))
        (runstack-push! runstack arg-id)
        1))
    (define (generate-assign id e)
      ;; Generate or use an already-generated non-simple in tmp
      (cond
        [(eq? id first-non-simple-id)
         (out "~a = ~a;"
              (if (eq? id last-non-simple-arg-id)
                  (cify id)
                  (runstack-assign runstack id))
              (cify first-tmp-id))]
        [(eq? id last-non-simple-arg-id)
         (generate (format "~a =" (cify id))
                   e env)]
        [else
         (generate (make-runstack-assign runstack id)
                   e env)]))
    (define (generate-args #:simple? gen-simple?)
      (for ([arg-id (in-list arg-ids)]
            [rand (in-list rands)]
            #:when (and arg-id
                        (eq? (and gen-simple? #t)
                             (simple? rand in-lam state knowns))))
        (generate-assign arg-id rand)))
    ;; For a non-tail call, generate simple arguments first, so
    ;; that the allocated runstacks are filled:
    (unless direct-tail?
      (generate-args #:simple? #t))
    (when rator-id
      (generate-assign rator-id rator))
    (generate-args #:simple? #f)
    ;; Now that the arguments are ready (except simple arguments
    ;; for a direct tail call), we finish in various ways:
    (cond
      ;; Special case for `values`:
      [(eq? rator 'values)
       (runstack-sync! runstack) ; now argv == runstack
       (return ret runstack #:can-omit? #t
               (if (zero? n)
                   "c_zero_values()"
                   (format "scheme_values(~a, c_current_runstack)" n)))]
      ;; Call to a non-inlined primitive or to an unknown target
      [(not direct?)
       (call-with-simple-shared
        (if rator-id #f rator)
        runstack state
        (lambda (shared)
          (define rator-s (if rator-id
                              (runstack-ref runstack rator-id)
                              (generate-simple rator shared env runstack in-lam state top-names knowns prim-names)))
          (define direct-prim? (and (symbol? rator)
                                    (direct-call-primitive? rator prim-knowns)))
          (define use-tail-apply? (and (tail-return? ret)
                                       (or (not (symbol? rator))
                                           (hash-ref env rator #f)
                                           (hash-ref top-names rator #f)
                                           (not direct-prim?))))
          (define template (cond
                             [use-tail-apply? "_scheme_tail_apply(~a, ~a, ~a)"]
                             [direct-prim? "c_extract_prim(~a)(~a, ~a)"]
                             [(or (multiple-return? ret) (tail-return? ret)) "_scheme_apply_multi(~a, ~a, ~a)"]
                             [else "_scheme_apply(~a, ~a, ~a)"]))
          (unless (or use-tail-apply?
                      (and direct-prim? (immediate-primitive? rator prim-knowns)))
            (lam-calls-non-immediate! in-lam))
          (when use-tail-apply?
            (set-lam-can-tail-apply?! in-lam #t))
          (runstack-sync! runstack) ; now argv == runstack
          (return ret runstack (format template rator-s n (if (zero? n) "NULL" (runstack-stack-ref runstack))))))]
      ;; Tail call to a known target:
      [(tail-return? ret)
       ;; Put simple arguments in temporaries:
       (define any-simple? (for/or ([arg-id (in-list arg-ids)]) (not arg-id)))
       (define arg-tmp-ids (for/list ([arg-id (in-list arg-ids)]
                                      [rand (in-list rands)]
                                      [i (in-naturals)])
                             (cond
                               [arg-id #f]
                               [(and (symbol-ref? rand)
                                     (eqv? (runstack-ref-pos runstack (unref rand)) (- n i))
                                     ((- n i) . <= . (args-length (tail-return-self-args ret)))
                                     (not (mutated? (hash-ref state (unref rand) #f))))
                                ;; No need to copy an argument to itself, which is
                                ;; common for lifted loops:
                                (when (state-first-pass? state)
                                  (adjust-state! state (unref rand) -1))
                                #f]
                               [else
                                (genid 'c_argtmp)])))
       (when any-simple?
         (out-open "{")
         (for ([arg-tmp-id (in-list arg-tmp-ids)]
               #:when arg-tmp-id)
           (out "Scheme_Object *~a;" (cify arg-tmp-id)))
         (for ([arg-tmp-id (in-list arg-tmp-ids)]
               [rand (in-list rands)]
               #:when arg-tmp-id)
           (generate (format "~a =" (cify arg-tmp-id))
                     rand env)))
       ;; Non-simple args are on the runstack. We need to move from
       ;; last to first, since the runstack staging area and the
       ;; and target argument area may overlap.
       (for ([i (in-range n 0 -1)]
             [arg-id (in-list (reverse arg-ids))])
         (when arg-id
           (out "c_runbase[~a] = ~a;"
                (- i (add1 n))
                (if (eq? arg-id last-non-simple-arg-id)
                    (cify arg-id)
                    (runstack-ref runstack arg-id)))))
       ;; Move the simple arguments into place:
       (for ([i (in-range n)]
             [arg-tmp-id (in-list arg-tmp-ids)])
         (when arg-tmp-id
           (out "c_runbase[~a] = ~a;" (- i n) (cify arg-tmp-id))))
       ;; For any argument that was skipped because it's already in
       ;; place, record that we need it live to here:
       (for ([arg-id (in-list arg-ids)]
             [arg-tmp-id (in-list arg-tmp-ids)]
             [rand (in-list rands)])
         (unless (or arg-id arg-tmp-id)
           (out "/* in place: ~a */" (cify (ref-id rand)))
           (runstack-ref-use! runstack rand)
           (ref-use! rand state)
           (state-implicit-reference! state (ref-id rand))))
       (when any-simple?
         (out-close "}"))
       ;; Set the runstack pointer to the argument start, then jump
       (out "c_current_runstack = c_runbase - ~a;" n)
       (when (if (eq? in-lam known-target-lam)
                 (n . <= . (args-length (tail-return-self-args ret)))
                 (symbol<? (lam-id known-target-lam) (lam-id in-lam))) ; direction is arbitrary
         ;; ... after checking for a break or thread swap
         (out "c_use_fuel();"))
       (cond
         [(and (eq? known-target-lam (tail-return-lam ret))
               (= n (args-length (tail-return-self-args ret))))
          (hash-set! (lam-loop-targets (tail-return-lam ret)) n #t)
          (for ([free-var-ref (in-list (lam-free-var-refs known-target-lam))])
            (ref-implicit-use! (ref-id free-var-ref) state))
          (out "goto c_recur_~a_~a;" (cify (lam-id known-target-lam)) n)]
         [else
          (set-lam-need-entry?! known-target-lam #t)
          (set-lam-max-jump-argc! known-target-lam (max n (lam-max-jump-argc known-target-lam)))
          (unless (lam-constant-args-count? known-target-lam)
            (out "c_argc = ~a;" n)
            (unless (lam-no-rest-args? known-target-lam)
              (out "c_argv = c_runbase - ~a;" n)))
          (unless (null? (lam-free-var-refs known-target-lam))
            (out "c_self = ~a;" (top-ref in-lam (lam-id known-target-lam))))
          (out "goto c_entry_~a;" (cify (lam-id known-target-lam)))])
       (lam-add-transitive-tail-apply! in-lam known-target-lam)]
      ;; Non-tail call to a known-target:
      [else
       (lam-called-direct! known-target-lam)
       (lam-calls-non-immediate! in-lam)
       (runstack-sync! runstack) ; now argv == runstack
       (return ret runstack
               (let ([s (format "~a(~a, ~a~a)"
                                (cify (vehicle-id (lam-vehicle known-target-lam))) n
                                (runstack-stack-ref runstack)
                                (if (vehicle-closure? (lam-vehicle known-target-lam))
                                    (format ", ~a" (top-ref in-lam rator))
                                    ""))])
                 (if (lam-can-tail-apply? known-target-lam)
                     (format "scheme_force_~avalue(~a)" (if (multiple-return? ret) "" "one_") s)
                     s)))])
    (when declared-tmp? (out-close "}"))
    (runstack-pop! runstack (+ arg-push-count (if rator-id 1 0)))
    (when open? (out-close "}")))

  (define (generate-closure ret e env)
    (define lam (hash-ref lambdas e))
    (cond
      [(and (lam-moved-to-top? lam)
            (not (state-tops-pass? state)))
       ;; Lifted out after discovering that it has no free variables
       (return ret runstack #:can-pre-pop? #t (top-ref in-lam (lam-id lam)))]
      [else
       (when in-lam (set-lam-under-lambda?! lam #t))
       (define name (format "~a" (or (extract-lambda-name (lam-e lam))
                                     (lam-id lam))))
       (define free-var-refs (get-free-vars e env lambdas knowns top-names state))
       (define index-in-closure? (pair? (cdr (vehicle-lams (lam-vehicle lam)))))
       (define-values (min-a max-a) (lambda-arity e #:precise-cases? #t))
       (cond
         [(and (null? free-var-refs)
               (not index-in-closure?))
          (runstack-sync! runstack)
          (return ret runstack #:can-omit? #t #:can-pre-pop? #t
                  (format "scheme_make_prim_w_~aarity(~a, ~s, ~a, ~a)"
                          (if (string? max-a) "case_" "")
                          (cify (lam-id lam)) name min-a max-a))]
         [else
          (define len (+ (length free-var-refs) (if index-in-closure? 1 0)))
          (out-open "{")
          (define clo-ids (for/list ([i (in-range len)])
                            (genid 'c_clo)))
          (for ([id (in-list (reverse clo-ids))])
            (runstack-push! runstack id))
          (when index-in-closure?
            (out "~a = scheme_make_integer(~a);" (runstack-assign runstack (car clo-ids)) (lam-index lam)))
          (for ([free-var-ref (in-list free-var-refs)]
                [clo-id (in-list (if index-in-closure? (cdr clo-ids) clo-ids))])
            (ref-use! free-var-ref state)
            (out "~a = ~a;"
                 (runstack-assign runstack clo-id)
                 (runstack-ref runstack (ref-id free-var-ref) #:ref free-var-ref)))
          (runstack-sync! runstack)
          (return ret runstack #:can-omit? #t
                  (format "scheme_make_prim_closure_w_~aarity(~a, ~a, ~a, ~s, ~a, ~a)"
                          (if (string? max-a) "case_" "")
                          (cify (vehicle-id (lam-vehicle lam)))
                          len (runstack-stack-ref runstack) name min-a max-a))
          (runstack-pop! runstack (length clo-ids))
          (out-close "}")])]))

  (define (generate-quote ret e)
    (cond
      [(return-can-omit? ret) (void)]
      [(pair? e)
       (define simple-car? (simple-quote? (car e)))
       (cond
         [(or simple-car?
              (simple-quote? (cdr e)))
          (out-open "{")
          (define pair-id (genid 'c_pair))
          (out "Scheme_Object *~a;" (cify pair-id))
          (generate-quote (format "~a =" (cify pair-id)) (if simple-car?
                                                             (cdr e)
                                                             (car e)))
          (return ret runstack #:can-pre-pop? #t
                  (if simple-car?
                      (format "scheme_make_pair(~a, ~a)" (generate-single-quote (car e)) (cify pair-id))
                      (format "scheme_make_pair(~a, ~a)" (cify pair-id) (generate-single-quote (cdr e)))))
          (out-close "}")]
         [else
          (define car-id (genid 'c_car))
          (define cdr-id (genid 'c_cdr))
          (out-open "{")
          (runstack-push! runstack car-id)
          (runstack-push! runstack cdr-id)
          (generate-quote (make-runstack-assign runstack car-id) (car e))
          (generate-quote (make-runstack-assign runstack cdr-id) (cdr e))
          (runstack-pop! runstack 2) ; must call `scheme_make_pair` immediately after sync!
          (runstack-sync! runstack)
          (return ret runstack #:can-pre-pop? #t
                  (format "scheme_make_pair(~a, ~a)"
                          (runstack-ref runstack car-id)
                          (runstack-ref runstack cdr-id)))
          (out-close "}")])]
      [(vector? e)
       (define vec-id (genid 'c_vec))
       (out-open "{")
       (runstack-push! runstack vec-id)
       (unless (zero? (vector-length e))
         (out "Scheme_Object *~a_elem;" vec-id))
       (runstack-sync! runstack)
       (out "~a = scheme_make_vector(~a, NULL);" (runstack-assign runstack vec-id) (vector-length e))
       (for ([e (in-vector e)]
             [i (in-naturals)])
         (generate-quote (format "~a_elem =" vec-id) e)
         (out "SCHEME_VEC_ELS(~a)[~a] = ~a_elem;" (runstack-ref runstack vec-id) i vec-id))
       (return ret runstack #:can-pre-pop? #t (runstack-ref runstack vec-id))
       (runstack-pop! runstack)
       (out-close "}")]
      [else
       (return ret runstack #:can-pre-pop? #t
               (generate-single-quote e))]))

  ;; Covers all simple quotes, but also more:
  (define (generate-single-quote e)
    (cond
      [(symbol? e)
       (format "scheme_intern_symbol(~s)" (symbol->string e))]
      [(string? e)
       (define s (string->bytes/utf-8 e))
       (format "scheme_make_sized_utf8_string(~a, ~a)"
               (substring (format "~s" s) 1)
               (bytes-length s))]
      [(bytes? e)
       (format "scheme_make_sized_byte_string(~a, ~a, 0)"
               (substring (format "~s" e) 1)
               (bytes-length e))]
      [(number? e)
       (cond
         [(always-fixnum? e)
          (format "scheme_make_integer(~a)" e)]
         [(exact-integer? e)
          (cond
            [(and (< e (expt 2 63))
                  (>= e (- (expt 2 63))))
             (format "scheme_make_integer_value_from_long_halves(~aL, ~aL)"
                     (bitwise-and e (sub1 (expt 2 32)))
                     (arithmetic-shift e -32))]
            [else
             (error 'generate-quite "number is too large: ~e" e)])]
         [(eqv? e +inf.0) "scheme_inf_object"]
         [(eqv? e -inf.0) "scheme_minus_inf_object"]
         [(eqv? e +nan.0) "scheme_nan_object"]
         [(eqv? e +inf.f) "scheme_single_inf_object"]
         [(eqv? e -inf.f) "scheme_single_minus_inf_object"]
         [(eqv? e +nan.f) "scheme_single_nan_object"]
         [else
          (format "scheme_make_double(~a)" e)])]
      [(boolean? e) (if e "scheme_true" "scheme_false")]
      [(null? e) "scheme_null"]
      [(void? e) "scheme_void"]
      [(eq? unsafe-undefined e) "scheme_undefined"]
      [(char? e) (format "scheme_make_character(~a)" (char->integer e))]
      [else
       (error 'generate-quote "not handled: ~e" e)]))

  (generate ret e env))

;; ----------------------------------------

(define (generate-tops e max-runstack-depth exports knowns top-names state lambdas prim-names prim-knowns)
  (define runstack (make-runstack state))

  (define (generate-tops e)
    (generate-init-prims)
    (out-next)
    (out-open "void scheme_init_startup_instance(Scheme_Instance *c_instance) {")
    (out "c_LINK_THREAD_LOCAL")
    (out "Scheme_Object **c_runbase = c_current_runstack;")
    (out "MZ_GC_DECL_REG(1);")
    (out "MZ_GC_VAR_IN_REG(0, c_instance);")
    (out "MZ_GC_REG();")

    (out "REGISTER_SO(c_top);")
    (out "c_top = scheme_malloc(sizeof(struct startup_instance_top_t));")

    (out "c_check_top_runstack_depth(~a);" max-runstack-depth)
    (generate-moved-to-top lambdas)
    (generate-top e)
    ;; Expects `([<int-id> <ext-id>] ...)` for `exports`
    (for ([ex (in-list exports)])
      (out "scheme_instance_add(c_instance, ~s, ~a);"
           (format "~a" (cadr ex))
           (top-ref #f (no-c-prefix (car ex)))))

    (out "MZ_GC_UNREG();")
    (out-close "}")
    (runstack-max-depth runstack))

  (define (generate-moved-to-top lambdas)
    (for ([lam (in-sorted-hash-values lambdas (compare symbol<? lam-id))])
      (when (lam-moved-to-top? lam)
        (generate-top `(define ,(lam-id lam) ,(lam-e lam))))))

  (define (generate-top e)
    (match e
      [`(begin . ,es)
       (for ([e (in-list es)])
         (generate-top e))]
      [`(define ,id (let . ,_))
       (generate-top-let e)]
      [`(define ,id (letrec . ,_))
       (generate-top-let e)]
      [`(define ,id (letrec* . ,_))
       (generate-top-let e)]
      [`(define ,id ,rhs)
       (generate (format "~a =" (top-ref #f id)) rhs #f #hasheq()
                 runstack knowns top-names state lambdas prim-names prim-knowns)]
      [`(define-values (,ids ...) ,rhs)
       (generate (only-multiple-return "/*needed*/") rhs #f #hasheq()
                 runstack knowns top-names state lambdas prim-names prim-knowns)
       (generate-multiple-value-binds ids runstack #f state top-names)]
      [`,_
       (generate "" e #f #hasheq()
                 runstack knowns top-names state lambdas prim-names prim-knowns)]))

  (define (generate-top-let e)
    (match e
      [`(define ,id (,let-id ([,ids ,rhss] ...) ,rhs))
       (cond
         [(hash-ref knowns id #f)
          (define new-e `(begin
                           ,@(for/list ([id (in-list ids)]
                                        [rhs (in-list rhss)])
                               `(define ,id ,rhs))
                           (define ,id ,rhs)))
          (generate-top new-e)]
         [else
          (match e
            [`(define ,id ,rhs)
             ;; Hide immediate `let` with a `begin`:
             (define new-e `(define ,id (begin ,rhs)))
             (generate-top new-e)])])]))

  (define (generate-init-prims)
    (out-next)
    (out-open "void scheme_init_startup() {")
    (out "REGISTER_SO(c_prims);")
    (for ([id (in-sorted-hash-keys prim-names symbol<?)])
      (out "c_prims.~a = scheme_builtin_value(~s);" (cify id) (format "~a" id)))
    (out-close "}"))

  (generate-tops e))

;; ----------------------------------------

(define (generate-multiple-value-binds ids runstack in-lam state top-names)
  (for ([id (in-list ids)]
        [i (in-naturals)]
        #:when (or (hash-ref top-names id #f)
                   (referenced? (hash-ref state id #f))))
    (define s (if (hash-ref top-names id #f)
                  (top-ref in-lam id)
                  (runstack-assign runstack id)))
    (out "~a = c_current_thread->ku.multiple.array[~a];" s i)))

;; ----------------------------------------

;; Recognize the patterns that the linklet flattener uses to record a
;; function's name within an S-expression, taking into account that lifting
;; may have pushed the pattern under a `let`:
(define (extract-lambda-name e)
  (define (extract body)
    (match body
      [`(,e ,e2 . ,_)
       (extract-one e)]
      [`((begin . ,body))
       (extract body)]
      [`((let ,binds . ,body)) (extract body)]
      [`((letrec ,binds . ,body)) (extract body)]
      [`((letrec* ,binds . ,body)) (extract body)]
      [`,_ #f]))
  (define (extract-one e)
    (match e
      [`(quote ,id) (and (symbol? id) id)]
      [`(begin ,e . ,_) (extract-one e)]
      [`,_ #f]))
  (match e
    [`(lambda ,_ . ,body)
     (extract body)]
    [`(case-lambda [,_ . ,body] . ,_)
     (extract body)]
    [`,_ #f]))

