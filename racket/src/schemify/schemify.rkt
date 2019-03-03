#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "export.rkt"
         "struct-type-info.rkt"
         "simple.rkt"
         "source-sym.rkt"
         "find-definition.rkt"
         "mutated.rkt"
         "mutated-state.rkt"
         "left-to-right.rkt"
         "serialize.rkt"
         "let.rkt"
         "equal.rkt"
         "optimize.rkt"
         "find-known.rkt"
         "infer-known.rkt"
         "inline.rkt"
         "letrec.rkt"
         "infer-name.rkt")

(provide schemify-linklet
         schemify-body)

;; Convert a linklet to a Scheme `lambda`, dealing with several
;; issues:
;;
;;   - imports and exports are represented by `variable` objects that
;;     are passed to the function; to avoid obscuring the program to
;;     the optimizer, though, refer to the definitions of exported
;;     variables instead of going through the `variable`, whenever
;;     possible, and accept values instead of `variable`s for constant
;;     imports;
;;
;;   - wrap expressions in a sequence of definitions plus expressions
;;     so that the result body is a sequence of definitions followed
;;     by a single expression;
;;
;;   - convert function calls and `let` forms to enforce left-to-right
;;     evaluation;
;;
;;   - convert function calls to support applicable structs, using
;;     `#%app` whenever a call might go through something other than a
;;     plain function;
;;
;;   - convert all `letrec` patterns that might involve `call/cc` to
;;     ensure that locations are allocated at the right time;
;;
;;   - explicily handle all potential too-early variable uses, so that
;;     the right name and enclosing module are reported;
;;
;;   - convert `make-struct-type` bindings to a pattern that Chez can
;;     recognize;
;;
;;   - optimize away `variable-reference-constant?` uses, which is
;;     important to make keyword-argument function calls work directly
;;     without keywords;
;;
;;  - similarly optimize away `variable-reference-from-unsafe?`;
;;
;;   - simplify `define-values` and `let-values` to `define` and
;;     `let`, when possible, and generally avoid `let-values`.

;; The given linklet can have parts wrapped as annotations. When
;; called from the Racket expander, those annotation will be
;; "correlated" objects that just support source locations.

;; Returns (values schemified-linklet import-abi export-info).
;; An import ABI is a list of list of booleans, parallel to the
;; linklet imports, where #t to means that a value is expected, and #f
;; means that a variable (which boxes a value) is expected.
(define (schemify-linklet lk serializable? datum-intern? for-jitify? allow-set!-undefined?
                          unsafe-mode? no-prompt?
                          prim-knowns get-import-knowns import-keys)
  (define (im-int-id id) (unwrap (if (pair? id) (cadr id) id)))
  (define (im-ext-id id) (unwrap (if (pair? id) (car id) id)))
  (define (ex-int-id id) (unwrap (if (pair? id) (car id) id)))
  (define (ex-ext-id id) (unwrap (if (pair? id) (cadr id) id)))
  ;; Assume no wraps unless the level of an id or expression
  (match lk
    [`(linklet ,im-idss ,ex-ids . ,bodys)
     ;; For imports, map symbols to gensymed `variable` argument names,
     ;; keeping `import` records in groups:
     (define grps
       (for/list ([im-ids (in-list im-idss)]
                  [index (in-naturals)])
         ;; An import key from `import-keys` lets us get cross-module
         ;; information on demand
         (import-group index (and import-keys (vector-ref import-keys index))
                       get-import-knowns #f #f
                       '())))
     ;; Record import information in both the `imports` table and within
     ;; the import-group record
     (define imports
       (let ([imports (make-hasheq)])
         (for ([im-ids (in-list im-idss)]
               [grp (in-list grps)])
           (set-import-group-imports!
            grp
            (for/list ([im-id (in-list im-ids)])
              (define id (im-int-id im-id))
              (define ext-id (im-ext-id im-id))
              (define int-id (gensym (symbol->string id)))
              (define im (import grp int-id id ext-id))
              (hash-set! imports id im)
              (hash-set! imports int-id im) ; useful for optimizer to look up known info late
              im)))
         imports))
     ;; Inlining can add new import groups or add imports to an existing group
     (define new-grps '())
     (define add-import!
       (make-add-import! imports
                         grps
                         get-import-knowns
                         (lambda (new-grp) (set! new-grps (cons new-grp new-grps)))))
     ;; For exports, too, map symbols to gensymed `variable` argument names
     (define exports
       (for/fold ([exports (hasheq)]) ([ex-id (in-list ex-ids)])
         (define id (ex-int-id ex-id))
         (hash-set exports id (export (gensym (symbol->string id)) (ex-ext-id ex-id)))))
     ;; Lift any quoted constants that can't be serialized
     (define-values (bodys/constants-lifted lifted-constants)
       (if serializable?
           (convert-for-serialize bodys #f datum-intern?)
           (values bodys null)))
     ;; Collect source names for define identifiers, to the degree that the source
     ;; name differs from the
     (define src-syms (get-definition-source-syms bodys))
     ;; Schemify the body, collecting information about defined names:
     (define-values (new-body defn-info mutated)
       (schemify-body* bodys/constants-lifted prim-knowns imports exports
                       for-jitify? allow-set!-undefined? add-import! #f unsafe-mode? no-prompt?))
     (define all-grps (append grps (reverse new-grps)))
     (values
      ;; Build `lambda` with schemified body:
      (make-let*
       lifted-constants
       `(lambda (instance-variable-reference
                 ,@(for*/list ([grp (in-list all-grps)]
                               [im (in-list (import-group-imports grp))])
                     (import-id im))
                 ,@(for/list ([ex-id (in-list ex-ids)])
                     (export-id (hash-ref exports (ex-int-id ex-id)))))
          ,@new-body))
      ;; Imports (external names), possibly extended via inlining:
      (for/list ([grp (in-list all-grps)])
        (for/list ([im (in-list (import-group-imports grp))])
          (import-ext-id im)))
      ;; Exports (external names, but paired with source name if it's different):
      (for/list ([ex-id (in-list ex-ids)])
        (define sym (ex-ext-id ex-id))
        (define int-sym (ex-int-id ex-id))
        (define src-sym (hash-ref src-syms int-sym sym)) ; external name unless 'source-name
        (if (eq? sym src-sym) sym (cons sym src-sym)))
      ;; Import keys --- revised if we added any import groups
      (if (null? new-grps)
          import-keys
          (for/vector #:length (length all-grps) ([grp (in-list all-grps)])
            (import-group-key grp)))
      ;; Import ABI: request values for constants, `variable`s otherwise
      (for/list ([grp (in-list all-grps)])
        (define im-ready? (import-group-lookup-ready? grp))
        (for/list ([im (in-list (import-group-imports grp))])
          (and im-ready?
               (known-constant? (import-group-lookup grp (import-ext-id im))))))
      ;; Convert internal to external identifiers for known-value info
      (for/fold ([knowns (hasheq)]) ([ex-id (in-list ex-ids)])
        (define id (ex-int-id ex-id))
        (define v (known-inline->export-known (hash-ref defn-info id #f)
                                              prim-knowns imports exports
                                              serializable?))
        (cond
         [(not (set!ed-mutated-state? (hash-ref mutated id #f)))
          (define ext-id (ex-ext-id ex-id))
          (hash-set knowns ext-id (or v a-known-constant))]
         [else knowns])))]))

;; ----------------------------------------

(define (schemify-body l prim-knowns imports exports for-cify? unsafe-mode? no-prompt?)
  (define-values (new-body defn-info mutated)
    (schemify-body* l prim-knowns imports exports
                    #f #f (lambda (im ext-id index) #f)
                    for-cify? unsafe-mode? no-prompt?))
  new-body)

(define (schemify-body* l prim-knowns imports exports
                        for-jitify? allow-set!-undefined? add-import!
                        for-cify? unsafe-mode? no-prompt?)
  ;; Various conversion steps need information about mutated variables,
  ;; where "mutated" here includes visible implicit mutation, such as
  ;; a variable that might be used before it is defined:
  (define mutated (mutated-in-body l exports prim-knowns (hasheq) imports unsafe-mode?))
  ;; Make another pass to gather known-binding information:
  (define knowns
    (for/fold ([knowns (hasheq)]) ([form (in-list l)])
      (define-values (new-knowns info)
        (find-definitions form prim-knowns knowns imports mutated unsafe-mode?
                          #:optimize? #t))
      new-knowns))
  ;; For non-exported definitions, we may need to create some variables
  ;; to guard against multiple returns
  (define extra-variables (make-hasheq))
  (define (add-extra-variables l)
    (append (for/list ([(int-id ex) (in-hash extra-variables)])
              `(define ,(export-id ex) (make-internal-variable 'int-id)))
            l))
  ;; While schemifying, add calls to install exported values in to the
  ;; corresponding exported `variable` records, but delay those
  ;; installs to the end, if possible
  (define schemified
    (let loop ([l l] [in-mut-l l] [accum-exprs null] [accum-ids null])
      (define mut-l (update-mutated-state! l in-mut-l mutated))
      (define (make-set-variables)
        (for/list ([id (in-wrap-list accum-ids)]
                   #:when (hash-ref exports (unwrap id) #f))
          (make-set-variable id exports knowns mutated)))
      (define (make-expr-defns es)
        (if (or for-jitify? for-cify?)
            (reverse es)
            (for/list ([e (in-list (reverse es))])
              (make-expr-defn e))))
      (cond
        [(null? l)
         ;; Finish by making sure that all pending variables in `accum-ids` are
         ;; moved into their `variable` records:
        (define set-vars (make-set-variables))
        (cond
         [(null? set-vars)
          (cond
           [(null? accum-exprs) '((void))]
           [else (reverse accum-exprs)])]
         [else (append (reverse accum-exprs)
                       set-vars)])]
       [else
        (define form (car l))
        (define schemified (schemify form
                                     prim-knowns knowns mutated imports exports
                                     allow-set!-undefined?
                                     add-import!
                                     for-cify? for-jitify?
                                     unsafe-mode? no-prompt?))
        ;; For the case that the right-hand side won't capture a
        ;; continuation or return multiple times, we can generate a
        ;; simple definition:
        (define (finish-definition ids [accum-exprs accum-exprs] [accum-ids accum-ids])
          (append
           (make-expr-defns accum-exprs)
           (cons
            schemified
            (let id-loop ([ids ids] [accum-exprs null] [accum-ids accum-ids])
              (cond
                [(wrap-null? ids) (loop (wrap-cdr l) mut-l accum-exprs accum-ids)]
                [(or (or for-jitify? for-cify?)
                     (via-variable-mutated-state? (hash-ref mutated (unwrap (wrap-car ids)) #f)))
                 (define id (unwrap (wrap-car ids)))
                 (cond
                   [(hash-ref exports id #f)
                    (id-loop (wrap-cdr ids)
                             (cons (make-set-variable id exports knowns mutated)
                                   accum-exprs)
                             accum-ids)]
                   [else
                    (id-loop (wrap-cdr ids) accum-exprs accum-ids)])]
                [else
                 (id-loop (wrap-cdr ids) accum-exprs (cons (wrap-car ids) accum-ids))])))))
        ;; For the case when the right-hand side might capture a
        ;; continuation or return multiple times, so we need a prompt.
        ;; The `variable` records are set within the prompt, while
        ;; definitions appear outside the prompt to just transfer the
        ;; value into a `variable` record (if it's not one that is
        ;; mutable, and therefore always access via the `variable`
        ;; record):
        (define (finish-wrapped-definition ids rhs)
          (append
           (make-expr-defns accum-exprs)
           (make-expr-defns (make-set-variables))
           (cond
             [no-prompt?
              (cons
               schemified
               (loop (wrap-cdr l) mut-l null ids))]
             [else
              (define expr
                `(call-with-module-prompt
                  (lambda () ,rhs)
                  ',ids
                  ',(for/list ([id (in-list ids)])
                      (variable-constance (unwrap id) knowns mutated))
                  ,@(for/list ([id (in-list ids)])
                      (id-to-variable (unwrap id) exports knowns mutated extra-variables))))
              (define defns
                (for/list ([id (in-list ids)])
                  (make-define-variable id exports knowns mutated extra-variables)))
              (cons
               (if for-jitify?
                   expr
                   (make-expr-defn expr))
               (append defns
                       (loop (wrap-cdr l) mut-l null null)))])))
        ;; Dispatch on the schemified form, distinguishing definitions
        ;; from expressions:
        (match schemified
          [`(define ,id ,rhs)
           (cond
             [(simple? #:pure? #f rhs prim-knowns knowns imports mutated)
              (finish-definition (list id))]
             [else
              (finish-wrapped-definition (list id) rhs)])]
          [`(define-values ,ids ,rhs)
           (cond
             [(simple? #:pure? #f rhs prim-knowns knowns imports mutated)
              (finish-definition ids)]
             [else
              (finish-wrapped-definition ids rhs)])]
          [`(splice . ,ls)
           (loop (append ls (wrap-cdr l)) in-mut-l accum-exprs accum-ids)]
          [`,_
           (match form
             [`(define-values ,ids ,_)
              ;; This is a rearranged `struct` form where any necessary
              ;; prompt is in place already. There may be arbitrary expressions
              ;; for properties, though, so sync exported variables
              (define set-vars (make-set-variables))
              (finish-definition ids (append set-vars accum-exprs) null)]
             [`,_
              (cond
                [(simple? #:pure? #f schemified prim-knowns knowns imports mutated)
                 (loop (wrap-cdr l) mut-l (cons schemified accum-exprs) accum-ids)]
                [else
                 ;; In case `schemified` triggers an error, sync exported variables
                 (define set-vars (make-set-variables))
                 (define expr (if no-prompt?
                                  schemified
                                  `(call-with-module-prompt (lambda () ,schemified))))
                 (loop (wrap-cdr l) mut-l (cons expr (append set-vars accum-exprs)) null)])])])])))
  ;; Return both schemified and known-binding information, where
  ;; the later is used for cross-linklet optimization
  (values (add-extra-variables schemified) knowns mutated))

(define (make-set-variable id exports knowns mutated [extra-variables #f])
  (define int-id (unwrap id))
  (define ex-id (id-to-variable int-id exports knowns mutated extra-variables))
  `(variable-set! ,ex-id ,id ',(variable-constance int-id knowns mutated)))

(define (id-to-variable int-id exports knowns mutated extra-variables)
  (export-id
   (or (hash-ref exports int-id #f)
       (and extra-variables
            (or (hash-ref extra-variables int-id #f)
                (let ([ex (export (gensym int-id) int-id)])
                  (hash-set! extra-variables int-id ex)
                  ex))))))

(define (make-define-variable id exports knowns mutated extra-variables)
  (define int-id (unwrap id))
  (define ex (or (hash-ref exports int-id #f)
                 (hash-ref extra-variables int-id)))
  `(define ,id (variable-ref/no-check ,(export-id ex))))

(define (make-expr-defn expr)
  `(define ,(gensym) (begin ,expr (void))))

(define (variable-constance id knowns mutated)
  (cond
    [(set!ed-mutated-state? (hash-ref mutated id #f))
     #f]
    [(known-consistent? (hash-ref knowns id #f))
     'consistent]
    [else
     'constant]))

;; ----------------------------------------

;; Schemify `let-values` to `let`, etc., and
;; reorganize struct bindings.
(define (schemify v prim-knowns knowns mutated imports exports allow-set!-undefined? add-import!
                  for-cify? for-jitify? unsafe-mode? no-prompt?)
  (let schemify/knowns ([knowns knowns] [inline-fuel init-inline-fuel] [v v])
    (define (schemify v)
      (define s-v
        (reannotate
         v 
         (match v
           [`(lambda ,formals ,body ...)
            (infer-procedure-name
             v
             `(lambda ,formals ,@(schemify-body body)))]
           [`(case-lambda [,formalss ,bodys ...] ...)
            (infer-procedure-name
             v
             `(case-lambda ,@(for/list ([formals (in-list formalss)]
                                        [body (in-list bodys)])
                               `[,formals ,@(schemify-body body)])))]
           [`(define-values (,struct:s ,make-s ,s? ,acc/muts ...)
               (let-values (((,struct: ,make ,?1 ,-ref ,-set!) ,mk))
                 (values ,struct:2
                         ,make2
                         ,?2
                         ,make-acc/muts ...)))
            #:guard (not (or for-jitify? for-cify?))
            ;; Convert a `make-struct-type` binding into a 
            ;; set of bindings that Chez's cp0 recognizes,
            ;; and push the struct-specific extra work into
            ;; `struct-type-install-properties!`
            (define sti (and (wrap-eq? struct: struct:2)
                             (wrap-eq? make make2)
                             (wrap-eq? ?1 ?2)
                             (make-struct-type-info mk prim-knowns knowns imports mutated)))
            (cond
              [(and sti
                    ;; make sure `struct:` isn't used too early, since we're
                    ;; reordering it's definition with respect to some arguments
                    ;; of `make-struct-type`:
                    (simple-mutated-state? (hash-ref mutated (unwrap struct:) #f))
                    ;; If any properties, need the first LHS to be non-set!ed, because that will
                    ;; let us reject multi-return from continuation capture in property expressions
                    (or no-prompt?
                        (null? (struct-type-info-rest sti))
                        (not (set!ed-mutated-state? (hash-ref mutated (unwrap struct:s) #f)))))
               (define can-impersonate? (not (struct-type-info-authentic? sti)))
               (define raw-s? (if can-impersonate? (gensym (unwrap s?)) s?))
               `(begin
                  (define ,struct:s (make-record-type-descriptor ',(struct-type-info-name sti)
                                                                 ,(schemify (struct-type-info-parent sti))
                                                                 ,(if (not (struct-type-info-prefab-immutables sti))
                                                                      #f
                                                                      `(structure-type-lookup-prefab-uid
                                                                        ',(struct-type-info-name sti)
                                                                        ,(schemify (struct-type-info-parent sti))
                                                                        ,(struct-type-info-immediate-field-count sti)
                                                                        0 #f
                                                                        ',(struct-type-info-prefab-immutables sti)))
                                                                 #f
                                                                 #f
                                                                 ',(for/vector ([i (in-range (struct-type-info-immediate-field-count sti))])
                                                                     `(mutable ,(string->symbol (format "f~a" i))))))
                  ,@(if (null? (struct-type-info-rest sti))
                        null
                        `((define ,(gensym)
                            (struct-type-install-properties! ,struct:s
                                                             ',(struct-type-info-name sti)
                                                             ,(struct-type-info-immediate-field-count sti)
                                                             0
                                                             ,(schemify (struct-type-info-parent sti))
                                                             ,@(schemify-body (struct-type-info-rest sti))))))
                  (define ,make-s ,(let ([ctr `(record-constructor
                                                (make-record-constructor-descriptor ,struct:s #f #f))])
                                     (if (struct-type-info-pure-constructor? sti)
                                         ctr
                                         `(struct-type-constructor-add-guards ,ctr ,struct:s ',(struct-type-info-name sti)))))
                  (define ,raw-s? (record-predicate ,struct:s))
                  ,@(if can-impersonate?
                        `((define ,s? (lambda (v) (if (,raw-s? v) #t ($value (if (impersonator? v) (,raw-s? (impersonator-val v)) #f))))))
                        null)
                  ,@(for/list ([acc/mut (in-list acc/muts)]
                               [make-acc/mut (in-list make-acc/muts)])
                      (define raw-acc/mut (if can-impersonate? (gensym (unwrap acc/mut)) acc/mut))
                      (match make-acc/mut
                        [`(make-struct-field-accessor ,(? (lambda (v) (wrap-eq? v -ref))) ,pos ,_)
                         (define raw-def `(define ,raw-acc/mut (record-accessor ,struct:s ,pos)))
                         (if can-impersonate?
                             `(begin
                                ,raw-def
                                (define ,acc/mut
                                  (lambda (s) (if (,raw-s? s)
                                                  (,raw-acc/mut s)
                                                  ($value (impersonate-ref ,raw-acc/mut ,struct:s ,pos s))))))
                             raw-def)]
                        [`(make-struct-field-mutator ,(? (lambda (v) (wrap-eq? v -set!))) ,pos ,_)
                         (define raw-def `(define ,raw-acc/mut (record-mutator ,struct:s ,pos)))
                         (define abs-pos (+ pos (- (struct-type-info-field-count sti)
                                                   (struct-type-info-immediate-field-count sti))))
                         (if can-impersonate?
                             `(begin
                                ,raw-def
                                (define ,acc/mut
                                  (lambda (s v) (if (,raw-s? s)
                                                    (,raw-acc/mut s v)
                                                    ($value (impersonate-set! ,raw-acc/mut ,struct:s ,pos ,abs-pos s v))))))
                             raw-def)]
                        [`,_ (error "oops")]))
                  (define ,(gensym)
                    (begin
                      (register-struct-constructor! ,make-s)
                      (register-struct-predicate! ,s?)
                      ,@(for/list ([acc/mut (in-list acc/muts)]
                                   [make-acc/mut (in-list make-acc/muts)])
                          (match make-acc/mut
                            [`(make-struct-field-accessor ,_ ,pos ,_)
                             `(register-struct-field-accessor! ,acc/mut ,struct:s ,pos)]
                            [`(make-struct-field-mutator ,_ ,pos ,_)
                             `(register-struct-field-mutator! ,acc/mut ,struct:s ,pos)]
                            [`,_ (error "oops")]))
                      (void))))]
              [else
               (match v
                 [`(,_ ,ids ,rhs)
                  `(define-values ,ids ,(schemify rhs))])])]
           [`(define-values (,id) ,rhs)
            `(define ,id ,(schemify rhs))]
           [`(define-values ,ids ,rhs)
            (let loop ([rhs rhs])
              (match rhs
                [`(values ,rhss ...)
                 (cond
                   [(= (length rhss) (length ids))
                    `(splice ; <- result goes back to schemify, so don't schemify rhss
                      ,@(for/list ([id (in-list ids)]
                                   [rhs (in-list rhss)])
                          `(define-values (,id) ,rhs)))]
                   [else
                    `(define-values ,ids ,(schemify rhs))])]
                [`(let-values () ,rhs)
                 (loop rhs)]
                [`,_
                 `(define-values ,ids ,(schemify rhs))]))]
           [`(quote ,_) v]
           [`(let-values () ,body)
            (schemify body)]
           [`(let-values () ,bodys ...)
            (schemify `(begin . ,bodys))]
           [`(let-values ([(,ids) ,rhss] ...) ,bodys ...)
            (define new-knowns
              (for/fold ([knowns knowns]) ([id (in-list ids)]
                                           [rhs (in-list rhss)])
                (define k (infer-known rhs #f #f id knowns prim-knowns imports mutated unsafe-mode?))
                (if k
                    (hash-set knowns (unwrap id) k)
                    knowns)))
            (define (merely-a-copy? id)
              (define u-id (unwrap id))
              (and (known-copy? (hash-ref new-knowns u-id #f))
                   (simple-mutated-state? (hash-ref mutated u-id #f))))
            (left-to-right/let (for/list ([id (in-list ids)]
                                          #:unless (merely-a-copy? id))
                                 id)
                               (for/list ([id (in-list ids)]
                                          [rhs (in-list rhss)]
                                          #:unless (merely-a-copy? id))
                                 (schemify rhs))
                               (for/list ([body (in-list bodys)])
                                 (schemify/knowns new-knowns inline-fuel body))
                               prim-knowns knowns imports mutated)]
           [`(let-values ([() (begin ,rhss ... (values))]) ,bodys ...)
            `(begin ,@(schemify-body rhss) ,@(schemify-body bodys))]
           [`(let-values ([,idss ,rhss] ...) ,bodys ...)
            (left-to-right/let-values idss
                                      (for/list ([rhs (in-list rhss)])
                                        (schemify rhs))
                                      (schemify-body bodys)
                                      mutated
                                      for-cify?)]
           [`(letrec-values () ,bodys ...)
            (schemify `(begin . ,bodys))]
           [`(letrec-values ([() (values)]) ,bodys ...)
            (schemify `(begin . ,bodys))]
           [`(letrec-values ([(,id) (values ,rhs)]) ,bodys ...)
            ;; special case of splitable values:
            (schemify `(letrec-values ([(,id) ,rhs]) . ,bodys))]
           [`(letrec-values ([(,ids) ,rhss] ...) ,bodys ...)
            (define new-knowns
              (for/fold ([knowns knowns]) ([id (in-list ids)]
                                           [rhs (in-list rhss)])
                (define k (infer-known rhs #f #t id knowns prim-knowns imports mutated unsafe-mode?))
                (if k
                    (hash-set knowns (unwrap id) k)
                    knowns)))
            `(letrec* ,(for/list ([id (in-list ids)]
                                  [rhs (in-list rhss)])
                         `[,id ,(schemify/knowns new-knowns inline-fuel rhs)])
                      ,@(for/list ([body (in-list bodys)])
                          (schemify/knowns new-knowns inline-fuel body)))]
           [`(letrec-values ([,idss ,rhss] ...) ,bodys ...)
            (cond
              [(letrec-splitable-values-binding? idss rhss)
               (schemify
                (letrec-split-values-binding idss rhss bodys))]
              [else
               ;; Convert
               ;;  (letrec*-values ([(id ...) rhs] ...) ....)
               ;; to
               ;;  (letrec* ([vec (call-with-values rhs vector)]
               ;;            [id (vector-ref vec 0)]
               ;;            ... ...)
               ;;    ....)
               `(letrec* ,(apply
                           append
                           (for/list ([ids (in-wrap-list idss)]
                                      [rhs (in-list rhss)])
                             (let ([rhs (schemify rhs)])
                               (cond
                                 [(null? ids)
                                  `([,(gensym "lr")
                                     ,(make-let-values null rhs '(void) for-cify?)])]
                                 [(and (pair? ids) (null? (cdr ids)))
                                  `([,(car ids) ,rhs])]
                                 [else
                                  (define lr (gensym "lr"))
                                  `([,lr ,(make-let-values ids rhs `(vector . ,ids) for-cify?)]
                                    ,@(for/list ([id (in-list ids)]
                                                 [pos (in-naturals)])
                                        `[,id (unsafe-vector*-ref ,lr ,pos)]))]))))
                         ,@(schemify-body bodys))])]
           [`(if ,tst ,thn ,els)
            `(if ,(schemify tst) ,(schemify thn) ,(schemify els))]
           [`(with-continuation-mark ,key ,val ,body)
            `(with-continuation-mark ,(schemify key) ,(schemify val) ,(schemify body))]
           [`(begin ,exp)
            (schemify exp)]
           [`(begin ,exps ...)
            `(begin . ,(schemify-body exps))]
           [`(begin0 ,exps ...)
            `(begin0 . ,(schemify-body exps))]
           [`(set! ,id ,rhs)
            (define int-id (unwrap id))
            (define ex (hash-ref exports int-id #f))
            (if ex
                `(,(if allow-set!-undefined? 'variable-set! 'variable-set!/check-undefined) ,(export-id ex) ,(schemify rhs) '#f)
                `(set! ,id ,(schemify rhs)))]
           [`(variable-reference-constant? (#%variable-reference ,id))
            (define u-id (unwrap id))
            (cond
              [(hash-ref mutated u-id #f) #f]
              [else
               (define im (hash-ref imports u-id #f))
               (cond
                 [(not im)
                  ;; Not imported and not mutable => a constant or local defined
                  ;; in this linklet or a direct primitive reference
                  #t]
                 [(known-constant? (import-lookup im)) #t]
                 [else
                  ;; Not statically known
                  `(variable-reference-constant? ,(schemify `(#%variable-reference ,id)))])])]
           [`(variable-reference-from-unsafe? (#%variable-reference))
            unsafe-mode?]
           [`(#%variable-reference)
            'instance-variable-reference]
           [`(#%variable-reference ,id)
            (define u (unwrap id))
            (define v (or (let ([ex (hash-ref exports u #f)])
                            (and ex (export-id ex)))
                          (let ([im (hash-ref imports u #f)])
                            (and im (import-id im)))))
            (if v
                `(make-instance-variable-reference 
                  instance-variable-reference
                  ,v)
                `(make-instance-variable-reference 
                  instance-variable-reference
                  ',(cond
                      [(hash-ref mutated u #f) 'mutable]
                      [(hash-ref prim-knowns u #f) 'primitive]
                      [else 'constant])))]
           [`(equal? ,exp1 ,exp2)
            (let ([exp1 (schemify exp1)]
                  [exp2 (schemify exp2)])
              (cond
                [(or (equal-implies-eq? exp1) (equal-implies-eq? exp2))
                 `(eq? ,exp1 ,exp2)]
                [(or (equal-implies-eqv? exp1) (equal-implies-eqv? exp2))
                 `(eqv? ,exp1 ,exp2)]
                [else
                 (left-to-right/app 'equal?
                                    (list exp1 exp2)
                                    #t for-cify?
                                    prim-knowns knowns imports mutated)]))]
           [`(call-with-values ,generator ,receiver)
            (cond
              [(and (lambda? generator)
                    (lambda? receiver))
               `(call-with-values ,(schemify generator) ,(schemify receiver))]
              [else
               (left-to-right/app (if for-cify? 'call-with-values '#%call-with-values)
                                  (list (schemify generator) (schemify receiver))
                                  #t for-cify?
                                  prim-knowns knowns imports mutated)])]
           [`((letrec-values ,binds ,rator) ,rands ...)
            (schemify `(letrec-values ,binds (,rator . ,rands)))]
           [`(,rator ,exps ...)
            (define (left-left-lambda-convert rator inline-fuel)
              (match rator
                [`(lambda ,formal-args ,bodys ...)
                 ;; Try to line up `formal-args` with `exps`
                 (let loop ([formal-args formal-args] [args exps] [binds '()])
                   (cond
                     [(wrap-null? formal-args)
                      (and (wrap-null? args)
                           (schemify/knowns knowns
                                            inline-fuel
                                            `(let-values ,(reverse binds) . ,bodys)))]
                     [(null? args) #f]
                     [(not (wrap-pair? formal-args))
                      (loop '() '() (cons (list (list formal-args)
                                                (if (wrap-null? args)
                                                    ''()
                                                    (cons 'list args)))
                                          binds))]
                     [else
                      (loop (wrap-cdr formal-args)
                            (wrap-cdr args)
                            (cons (list (list (wrap-car formal-args))
                                        (wrap-car args))
                                  binds))]))]
                [`(case-lambda [,formal-args ,bodys ...] . ,rest)
                 (or (left-left-lambda-convert `(lambda ,formal-args . ,bodys) inline-fuel)
                     (left-left-lambda-convert `(case-lambda . ,rest) inline-fuel))]
                [`,_ #f]))
            (define (inline-rator)
              (define u-rator (unwrap rator))
              (and (symbol? u-rator)
                   (let-values ([(k im) (find-known+import u-rator prim-knowns knowns imports mutated)])
                     (and (known-procedure/can-inline? k)
                          (left-left-lambda-convert
                           (inline-clone k im add-import! mutated imports)
                           (sub1 inline-fuel))))))
            (define (maybe-tmp e name)
              ;; use `e` directly if it's ok to duplicate
              (if (simple/can-copy? e prim-knowns knowns imports mutated)
                  e
                  (gensym name)))
            (define (wrap-tmp tmp e body)
              (if (eq? tmp e)
                  body
                  `(let ([,tmp ,e])
                     ,body)))
            (define (inline-field-access k s-rator im args)
              ;; For imported accessors or for JIT mode, inline the
              ;; selector with an `unsafe-struct?` test plus `unsafe-struct*-ref`.
              (define type-id (and (or im for-jitify?)
                                   (pair? args)
                                   (null? (cdr args))
                                   (inline-type-id k im add-import! mutated imports)))
              (cond
                [type-id
                 (define tmp (maybe-tmp (car args) 'v))
                 (define sel `(if (unsafe-struct? ,tmp ,(schemify type-id))
                                  (unsafe-struct*-ref ,tmp ,(known-field-accessor-pos k))
                                  (,s-rator ,tmp)))
                 (wrap-tmp tmp (car args)
                           sel)]
                [else #f]))
            (define (inline-field-mutate k s-rator im args)
              (define type-id (and (or im for-jitify?)
                                   (pair? args)
                                   (pair? (cdr args))
                                   (null? (cddr args))
                                   (inline-type-id k im add-import! mutated imports)))
              (cond
                [type-id
                 (define tmp (maybe-tmp (car args) 'v))
                 (define tmp-rhs (maybe-tmp (cadr args) 'rhs))
                 (define mut `(if (unsafe-struct? ,tmp ,(schemify type-id))
                                  (unsafe-struct*-set! ,tmp ,(known-field-mutator-pos k) ,tmp-rhs)
                                  (,s-rator ,tmp ,tmp-rhs)))
                 (wrap-tmp tmp (car args)
                           (wrap-tmp tmp-rhs (cadr args)
                                     mut))]
                [else #f]))
            (or (left-left-lambda-convert rator inline-fuel)
                (and (positive? inline-fuel)
                     (inline-rator))
                (let ([s-rator (schemify rator)]
                      [args (schemify-body exps)]
                      [u-rator (unwrap rator)])
                  (define-values (k im) (find-known+import u-rator prim-knowns knowns imports mutated))
                  (cond
                    [(and (not for-cify?)
                          (known-field-accessor? k)
                          (inline-field-access k s-rator im args))
                     => (lambda (e) e)]
                    [(and (not for-cify?)
                          (known-field-mutator? k)
                          (inline-field-mutate k s-rator im args))
                     => (lambda (e) e)]
                    [(and unsafe-mode?
                          (known-procedure/has-unsafe? k))
                     (left-to-right/app (known-procedure/has-unsafe-alternate k)
                                        args
                                        #t for-cify?
                                        prim-knowns knowns imports mutated)]
                    [else
                     (define plain-app? (or (known-procedure? k)
                                            (lambda? rator)))
                     (left-to-right/app s-rator
                                        args
                                        plain-app? for-cify?
                                        prim-knowns knowns imports mutated)])))]
           [`,_
            (let ([u-v (unwrap v)])
              (cond
                [(not (symbol? u-v))
                 v]
                [(eq? u-v 'call-with-values)
                 '#%call-with-values]
                [(and (via-variable-mutated-state? (hash-ref mutated u-v #f))
                      (hash-ref exports u-v #f))
                 => (lambda (ex) `(variable-ref ,(export-id ex)))]
                [(hash-ref imports u-v #f)
                 => (lambda (im)
                      (define k (import-lookup im))
                      (if (known-constant? k)
                          ;; Not boxed:
                          (cond
                            [(known-literal? k)
                             ;; We'd normally leave this to `optimize`, but
                             ;; need to handle it here before generating a
                             ;; reference to the renamed identifier
                             (known-literal-expr k)]
                            [else
                             (import-id im)])
                          ;; Will be boxed, but won't be undefined (because the
                          ;; module system won't link to an instance whose
                          ;; definitions didn't complete):
                          `(variable-ref/no-check ,(import-id im))))]
                [(hash-ref knowns u-v #f)
                 => (lambda (k)
                      (cond
                        [(and (known-copy? k)
                              (simple-mutated-state? (hash-ref mutated u-v #f)))
                         (schemify (known-copy-id k))]
                        [else v]))]
                [else v]))])))
      (optimize s-v prim-knowns knowns imports mutated))

    (define (schemify-body l)
      (for/list ([e (in-list l)])
        (schemify e)))

    (schemify v)))
