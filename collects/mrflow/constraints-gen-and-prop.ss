; The first label is the origin label (the origin of the arrow), the second label in the one
; flowing along the arrow. The destination of the arrow is embedded in the edge. The third
; label is used for tunneling (see create-simple-edge below).
; (define-type edge (label label label -> boolean))

(module constraints-gen-and-prop (lib "mrflow.ss" "mrflow")
  (require (prefix kern: (lib "kerncase.ss" "syntax"))
           (prefix list: (lib "list.ss"))
           (prefix etc: (lib "etc.ss"))
           
           (lib "match.ss")
           
           "labels.ss"
           "types.ss"
           "set-hash.ss"
           "assoc-set-hash.ss"
           (prefix util: "util.ss")
           (prefix hc: "hashcons.ss")
           (prefix cst: "constants.ss")
           ;(prefix types: "types.ss")
           (prefix err: "sba-errors.ss")
           )
  (provide
   make-sba-state
   initialize-primitive-type-schemes
   create-label-from-term
   check-primitive-types
   get-type-from-label
   pp-type
   
   get-mzscheme-position-from-label
   is-label-atom?
   get-span-from-label
   get-errors-from-label
   get-source-from-label
   get-parents-from-label
   get-children-from-label
   get-arrows-from-labels
   )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISC
  
  (define-struct arrows (in out tunnel) (make-inspector))
  
  ; type-scheme label
  (define-struct prim-data (type-scheme label))
  
  (define-struct sba-state (; label -> void
                            register-label-with-gui
                            ; error-table
                            errors
                            ; (hash-tableof symbol label)
                            top-level-name->label
                            ; (hash-tableof label (cons type (hash-table-of type-flow-var (cons label type))))
                            label->types
                            ; non-negative-exact-integer
                            type-var-counter
                            ; (hash-tableof (cons symbol type-scheme))
                            primitive-types-table
                            ; hashcons-table
                            hashcons-tbl
                            ))
  
  ; label -> boolean
  ; is the term associated with a label registerable (i.e. does it have
  ; an actual term associated with it in the user's code)?
  (define (gui-registerable? label)
    (let ([term (label-term label)])
      (or (syntax-original? term)
          (syntax-property term 'origin))))
  
  (set! make-sba-state
        (let ([real-make-sba-state make-sba-state])
          (lambda (register-label-with-gui)
            (real-make-sba-state (lambda (label)
                                   (when (gui-registerable? label)
                                     (register-label-with-gui label)))
                                 (err:error-table-make)
                                 (make-hash-table)
                                 (make-hash-table)
                                 0
                                 (make-hash-table)
                                 (hc:make-hashcons-table)))))
  
  
  ; length of list composed of label-cons
  (define (label-list-length start-label)
    (letrec ([count-length
              (lambda (label count)
                (if (label-cons? label)
                    (count-length (label-cons-cdr label) (add1 count))
                    (if (and (label-cst? label)
                             (null? (label-cst-value label)))
                        count
                        ;(error 'label-list-length
                        ;       "not a label list: ~a ~a ~a"
                        ;       (syntax-object->datum
                        ;        (label-term start-label))
                        ;       (pp-type sba-state (get-type-from-label sba-state start-label) 'label-list-length)
                        ;       label))))])
                        ; the assumption is that we'll never call this function
                        ; for something not a list. So if what we have doesn't
                        ; look like a list, then it's an infinite list.
                        +inf.0)))])
      (count-length start-label 0)))
  
  ; transform a label-based list into a cons-based list
  ; sba-state (label-listof top)  -> (listof top)
  (define (label-list->list sba-state start-label)
    (letrec ([ll->l
              (lambda (label)
                (cond
                  [(label-cons? label) (cons (label-cons-car label) (ll->l (label-cons-cdr label)))]
                  [(and (label-cst? label) (null? (label-cst-value label))) '()]
                  [else (error 'label-list->list
                               "not a label list: ~a"
                               (pp-type sba-state (get-type-from-label sba-state start-label) 'label-list->list))]))])
      (ll->l start-label)))
  
  ; like ormap, except that it continues processing the list even after the first non-#f
  ; is encountered
  (define ormap-strict
    (letrec ([ormap-strict-1-acc
              (lambda (f l acc)
                (if (null? l)
                    acc
                    (if (f (car l))
                        (ormap-strict-1-acc f (cdr l) #t)
                        (ormap-strict-1-acc f (cdr l) acc))))])
      (lambda (f l)
        (if (null? l)
            #t
            (ormap-strict-1-acc f (cdr l) (f (car l)))))))
  
  (define ormap2-strict
    (letrec ([ormap-strict-1-acc
              (lambda (f l1 l2 acc)
                (if (null? l1)
                    acc
                    (if (f (car l1) (car l2))
                        (ormap-strict-1-acc f (cdr l1) (cdr l2) #t)
                        (ormap-strict-1-acc f (cdr l1) (cdr l2) acc))))])
      (lambda (f l1 l2)
        (if (null? l1)
            #t
            (ormap-strict-1-acc f (cdr l1) (cdr l2) (f (car l1) (car l2)))))))

  ; like ormap, except that it continues processing the list even after the first non-#f
  ; is encountered
  ; l1 is a label-cons based list, l1 and l2 have the same length
  (define label-ormap-strict
    (letrec ([ormap-strict-2-acc
              (lambda (f l1 l2 acc)
                (if (null? l2)
                    acc
                    (if (f (label-cons-car l1) (car l2))
                        (ormap-strict-2-acc f (label-cons-cdr l1) (cdr l2) #t)
                        (ormap-strict-2-acc f (label-cons-cdr l1) (cdr l2) acc))))])
      (lambda (f l1 l2)
        (if (null? l2)
            #t
            (ormap-strict-2-acc f (label-cons-cdr l1) (cdr l2) (f (label-cons-car l1) (car l2)))))))
  
  ; (listof top) (listof top) -> (listof top)
  ; This is O(n^2) but we expect the lists to be small, otherwise use a hash table... It's only
  ; used in the GUI part anyway.
  ; Note that neither l1 nor l2 contains duplicates, because of the test in create-simple-edge
  (define (merge-lists l1 l2)
    (cond
      [(null? l1) l2]
      [else (let ([elt-l1 (car l1)])
              (if (memq elt-l1 l2)
                  (merge-lists (cdr l1) l2)
                  (cons elt-l1 (merge-lists (cdr l1) l2))))]))
  
  ; pretty-print code (represented as sexp)
  (define (unexpand t)
    (if (pair? t)
        (let ([kw (car t)])
          (if (list? t)
              (cond
                [(eq? kw '#%app) (map unexpand (cdr t))]
                [else (map unexpand t)])
              (cond
                [(eq? kw '#%datum) (cdr t)]
                [(eq? kw '#%top) (cdr t)]
                [else t])))
        t))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOCAL ENVIRONMENT
  
  ; (listof (cons symbol label)) (listof syntax-objects) (listof label)
  ; -> (listof (cons symbol label))
  ; the syntax objects in args are all atomic syntax objects for argument names
  ; the labels in args-labels are all simple labels (not pseudo-labels)
  (define (extend-env env args args-labels)
    ; doesn't matter whether we foldl or foldr
    (list:foldl
     (lambda (arg arg-label env)
       (cons (cons (syntax-e arg) arg-label)
             env))
     env args args-labels))
  
  ; syntax-object (listof (cons symbol label)) -> (or/c label #f)
  (define (lookup-env var env)
    (let ([name-label-pair (assq (syntax-e var) env)])
      (if name-label-pair
          (cdr name-label-pair)
          #f)))
  
  ; (listof (cons symbol label)) symbol label -> boolean
  (define (search-and-replace env arg label)
    (if (null? env)
        #f
        (if (eq? arg (caar env))
            (begin
              (set-cdr! (car env) label)
              #t)
            (search-and-replace (cdr env) arg label))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TOP LEVEL ENVIRONMENT
  
  ; sba-state syntax-object label -> void
  (define (add-top-level-name sba-state term label)
    (hash-table-put! (sba-state-top-level-name->label sba-state) (syntax-object->datum term) label))
  
  ; sba-state symbol -> (or/c label #f)
  ; finds the label for a top level var.
  (define (lookup-top-level-name sba-state name)
    (hash-table-get (sba-state-top-level-name->label sba-state) name cst:thunk-false))
  
  ; sba-state (listof label) term -> boolean
  ; Note that we make sure that all free variables are bound before
  ; creating the edges, and we check all free variables even if we
  ; already know some of them are unbound.
  ; Note also that a free variable can not be captured by a lexical
  ; binding, it has to be a top level binding.
  (define (lookup-and-bind-top-level-vars sba-state free-vars-labels-in term)
    (for-each
     (lambda (free-var-label-in)
       ; we do the top level lookup first, so we allow primitives to be redefined
       (let* ([free-var-name-in (syntax-e (label-term free-var-label-in))]
              [free-var-edge (extend-edge-for-values sba-state (create-simple-edge free-var-label-in))]
              [binding-label-in
               (let ([top-label (lookup-top-level-name sba-state free-var-name-in)])
                 (if top-label
                     top-label
                     (let ([primitive-data (lookup-primitive-data sba-state free-var-name-in)])
                       (if primitive-data
                           ; no polyvariance for primitives here...
                           ; but we need to make sure set! works for primitives by having
                           ; a flow from a label simulating the primitive's definition
                           (let* ([result-label (reconstruct-graph-from-type-scheme
                                                 sba-state
                                                 (prim-data-type-scheme primitive-data) (make-hash-table)
                                                 free-var-label-in)]
                                  [prim-def-label (prim-data-label primitive-data)]
                                  [result-edge (create-simple-edge result-label)])
                             (add-edge-and-propagate-set-through-edge prim-def-label result-edge)
                             result-label)
                           (cond
                             [(eq? free-var-name-in 'make-struct-type)
                              (create-make-struct-type-label sba-state term)]
                             ; we will process these two after the one above, for a given struct
                             ; definition, because, after program expansion,
                             ; make-struct-field-accessor/mutator appear in the body of a letrec-values
                             ; with make-struct-type being used in one of the letrec-values clauses.
                             [(eq? free-var-name-in 'make-struct-field-accessor)
                              (create-make-struct-field-accessor-label sba-state term)]
                             [(eq? free-var-name-in 'make-struct-field-mutator)
                              (create-make-struct-field-mutator-label sba-state term)]
                             [(eq? free-var-name-in 'set-car!)
                              (create-2args-mutator sba-state
                                                    label-cons?
                                                    cst:test-true
                                                    label-cons-car
                                                    cst:id
                                                    "pair"
                                                    "internal error 1: all types must be a subtype of top"
                                                    term)]
                             [(eq? free-var-name-in 'set-cdr!)
                              (create-2args-mutator sba-state
                                                    label-cons?
                                                    cst:test-true
                                                    label-cons-cdr
                                                    cst:id
                                                    "pair"
                                                    "internal error 2: all types must be a subtype of top"
                                                    term)]
                             ; we just inject the string type into the first arg
                             [(eq? free-var-name-in 'string-set!)
                              (create-3args-mutator sba-state
                                                    (lambda (inflowing-label)
                                                      (subtype-type sba-state
                                                                    (get-type-from-label sba-state inflowing-label)
                                                                    (make-type-cst 'string)
                                                                    'lookup-and-bind-top-level-vars1
                                                                    #f #f))
                                                    (lambda (inflowing-label)
                                                      (subtype-type sba-state
                                                                    (get-type-from-label sba-state inflowing-label)
                                                                    (make-type-cst 'exact-integer)
                                                                    'lookup-and-bind-top-level-vars2
                                                                    #f #f))
                                                    (lambda (inflowing-label)
                                                      (subtype-type sba-state
                                                                    (get-type-from-label sba-state inflowing-label)
                                                                    (make-type-cst 'char)
                                                                    'lookup-and-bind-top-level-vars3
                                                                    #f #f))
                                                    cst:id
                                                    (lambda (inflowing-label)
                                                      (let ([label (make-label-cst
                                                                    #f #f #f #f #f
                                                                    (label-term inflowing-label)
                                                                    (make-hash-table)
                                                                    (make-hash-table)
                                                                    'string)])
                                                        (initialize-label-set-for-value-source label)
                                                        label))
                                                    "string"
                                                    "exact-integer"
                                                    "char"
                                                    term)]
                             [(eq? free-var-name-in 'string-fill!)
                              (create-2args-mutator sba-state
                                                    (lambda (inflowing-label)
                                                      (subtype-type sba-state
                                                                    (get-type-from-label sba-state inflowing-label)
                                                                    (make-type-cst 'string)
                                                                    'lookup-and-bind-top-level-vars4
                                                                    #f #f))
                                                    (lambda (inflowing-label)
                                                      (subtype-type sba-state
                                                                    (get-type-from-label sba-state inflowing-label)
                                                                    (make-type-cst 'char)
                                                                    'lookup-and-bind-top-level-vars5
                                                                    #f #f))
                                                    cst:id
                                                    (lambda (inflowing-label)
                                                      (let ([label (make-label-cst
                                                                    #f #f #f #f #f
                                                                    (label-term inflowing-label)
                                                                    (make-hash-table)
                                                                    (make-hash-table)
                                                                    'string)])
                                                        (initialize-label-set-for-value-source label)
                                                        label))
                                                    "string"
                                                    "char"
                                                    term)]
                             ; inject third arg into first
                             [(eq? free-var-name-in 'vector-set!)
                              (create-3args-mutator sba-state
                                                    (lambda (inflowing-label)
                                                      (subtype-type sba-state
                                                                    (get-type-from-label sba-state inflowing-label)
                                                                    (make-type-vector (make-type-cst 'top))
                                                                    'lookup-and-bind-top-level-vars6
                                                                    #f #f))
                                                    (lambda (inflowing-label)
                                                      (subtype-type sba-state
                                                                    (get-type-from-label sba-state inflowing-label)
                                                                    (make-type-cst 'exact-integer)
                                                                    'lookup-and-bind-top-level-vars7
                                                                    #f #f))
                                                    cst:test-true
                                                    label-vector-element
                                                    cst:id
                                                    "vector"
                                                    "exact-integer"
                                                    "internal error 3: all types must be a subtype of top"
                                                    term)]
                             [(eq? free-var-name-in 'vector-fill!)
                              (create-2args-mutator sba-state
                                                    (lambda (inflowing-label)
                                                      (subtype-type sba-state
                                                                    (get-type-from-label sba-state inflowing-label)
                                                                    (make-type-vector (make-type-cst 'top))
                                                                    'lookup-and-bind-top-level-vars8
                                                                    #f #f))
                                                    cst:test-true
                                                    label-vector-element
                                                    cst:id
                                                    "vector"
                                                    "internal error 4: all types must be a subtype of top"
                                                    term)]
                             [else
                              (begin
                                (set-error-for-label sba-state
                                                     free-var-label-in
                                                     'red
                                                     ;(format "reference to undefined identifier: ~a in function ~a"
                                                     ;        free-var-name-in
                                                     ;        (unexpand (syntax-object->datum term))))
                                                     (format "reference to undefined identifier: ~a"
                                                             (syntax-object->datum (label-term free-var-label-in))))
                                #f)])))))])
         (when binding-label-in
           (add-edge-and-propagate-set-through-edge
            binding-label-in
            (extend-edge-for-values sba-state (create-simple-edge free-var-label-in))))))
     free-vars-labels-in)
    ; we act as if all the lookups always work, so we propagate as much as possible
    ; and find as many errors as possible.
    #t)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROPAGATION
  
  ; label boolean -> edge
  ; creates simple edge function that just propagates labels into in-label's set
  ; and propagates down the flow, taking cycles into account.
  ; note that the out-label is not a parameter of create-simple-edge, but a parameter of
  ; the resulting edge. This is both because it makes for nicer edges and edge creation code
  ; (since the edges are origin independant), and for historical reasons (because we used to
  ; create fake top level variables for lambdas and the edges that were added to these fake
  ; labels were moved to the label for the actual lambda when the enclosing lambda was applied
  ; - having to move edges meant they had to be origin independant). This also means that we
  ; can nicely re-use the same edge over and over when dealing with multiple values (see
  ; extend-edge-for-values below).
  (define (create-simple-edge in-label)
    (let ([in-set (label-set in-label)])
      (cons
       (if (label-prim? in-label)
           (lambda (out-label inflowing-label tunnel-label)
             ; entering tunnel => initialize tunnel entrance
             (unless tunnel-label
               ;(when (or (label-cons? inflowing-label)
               ;          (and (label-cst? inflowing-label)
               ;               (number? (label-cst-value inflowing-label))
               ;               (or (= 1 (label-cst-value inflowing-label))
               ;                   (= 2 (label-cst-value inflowing-label)))))
               ;  (printf "starting tunnel for ~a: ~a~n" inflowing-label out-label);)
               (set! tunnel-label out-label))
             ; Note: we assume that primitives don't have internal cycles, so we
             ; don't have to keep track of in/out edges. We still have to put the
             ; inflowing-label in the set, because otherwise nothing is going to be
             ; propagated when we add a new edge to the in-label.
             (let ([arrows (hash-table-get in-set inflowing-label cst:thunk-false)])
               (if arrows
                   (if (memq tunnel-label (arrows-tunnel arrows))
                       ; we have seen this inflowing-label before, and we already know about
                       ; this tunnel entrance => do nothing.
                       #t
                       ; we have seen this inflowing label before, but not from the same tunnel
                       ; entrance, so add the new entrance and propagate further down, so other
                       ; labels down the flow will know about the new tunnel entrance too...
                       (begin
                         (set-arrows-tunnel! arrows (cons tunnel-label (arrows-tunnel arrows)))
                         (ormap-strict (lambda (edge)
                                         (edge in-label inflowing-label tunnel-label))
                                       (hash-table-map (label-edges in-label)
                                                       cst:select-right))))
                   ; first time we see this inflowing-label
                   (begin
                     (hash-table-put! in-set inflowing-label (make-arrows '() '() (list tunnel-label)))
                     (ormap-strict (lambda (edge)
                                     (edge in-label inflowing-label tunnel-label))
                                   (hash-table-map (label-edges in-label)
                                                   cst:select-right))))))
           ;(when (or (label-cons? inflowing-label)
           ;          (and (label-cst? inflowing-label)
           ;               (number? (label-cst-value inflowing-label))
           ;               (or (= 1 (label-cst-value inflowing-label))
           ;                   (= 1 (label-cst-value inflowing-label)))))
           ;(printf "propagate ~a from ~a to ~a (type ~a)~n"
           ;        (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-simpled-edge1)
           ;        (syntax-object->datum (label-term out-label))
           ;        (syntax-object->datum (label-term in-label))
           ;        (label-type-var in-label))
           ;  )
           ;(ormap-strict (lambda (edge)
           ;                (edge in-label inflowing-label tunnel-label))
           ;              (hash-table-map (label-edges in-label)
           ;                              cst:select-right)))
           (lambda (out-label inflowing-label tunnel-label)
             (when tunnel-label
               ; coming out of tunnel, so set the out-label to the entrance of tunnel,
               ; and reset tunneling.
               ;(when (or (label-cons? inflowing-label)
               ;          (and (label-cst? inflowing-label)
               ;               (number? (label-cst-value inflowing-label))
               ;               (or (= 1 (label-cst-value inflowing-label))
               ;                   (= 2 (label-cst-value inflowing-label)))))
               ;  (printf "resetting tunnel for ~a: ~a~n" inflowing-label out-label);)
               (set! out-label tunnel-label))
             (let* ([out-set (label-set out-label)]
                    [arrows-in-set
                     (hash-table-get in-set inflowing-label cst:thunk-false)]
                    [arrows-out-set
                     (hash-table-get out-set inflowing-label cst:thunk-false)])
               (if arrows-in-set
                   ; the value has already flown before into this set, which means it has
                   ; already been propagated further down. So we just need to update the
                   ; in/out edges. Note that a side effect of this is that we never loop
                   ; indefinitely inside a cycle, which is mandatory if we generate things
                   ; like (listof number) as a recursive type.
                   (begin
                     (hash-table-put! in-set inflowing-label
                                      (make-arrows (cons out-label (arrows-in arrows-in-set))
                                                   (arrows-out arrows-in-set)
                                                   (list #f)))
                     (hash-table-put! out-set inflowing-label
                                      (make-arrows (arrows-in arrows-out-set)
                                                   (cons in-label (arrows-out arrows-out-set))
                                                   (list #f)))
                     #t)
                   ; first time this inflowing label is propagated to in-label, so update the
                   ; in/out edges and propagate further down.
                   (begin
                     (hash-table-put! in-set inflowing-label
                                      (make-arrows (list out-label)
                                                   '()
                                                   (list #f)))
                     (hash-table-put! out-set inflowing-label
                                      (make-arrows (arrows-in arrows-out-set)
                                                   (cons in-label (arrows-out arrows-out-set))
                                                   (list #f)))
                     ;(when (or (label-cons? inflowing-label)
                     ;          (and (label-cst? inflowing-label)
                     ;               (number? (label-cst-value inflowing-label))
                     ;               (or (= 1 (label-cst-value inflowing-label))
                     ;                   (= 2 (label-cst-value inflowing-label)))))
                     ;  (printf "propagate ~a from ~a to ~a (type ~a)~n"
                     ;          (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-simple-edge2)
                     ;          (syntax-object->datum (label-term out-label))
                     ;          (syntax-object->datum (label-term in-label))
                     ;          (label-type-var in-label))
                     ;  )
                     (ormap-strict (lambda (edge)
                                     (edge in-label inflowing-label #f))
                                   (hash-table-map (label-edges in-label)
                                                   cst:select-right)))))))
       in-label)))
  
  ; label edge -> void
  ; creates an edge from out-label to in-label and start the propagation for all the labels
  ; in out-label's set.
  ; Note: an edge is a function that updates the set of the in-label (and propagates further down
  ; the flow), so there's no need to have the in-label appear here explicitely as an argument.
  ; Note: if a function refers to a top level variable, and the function is applied twice and
  ; the top level variable refers both times to the same binding, we dont' want to end up with
  ; two parallel edges, so we have to test that.
  (define (add-edge-and-propagate-set-through-edge out-label new-edge)
    (let ([existing-edges-table (label-edges out-label)]
          [edge-func (car new-edge)]
          [in-label (cdr new-edge)])
      (unless (hash-table-get existing-edges-table in-label cst:thunk-false)
        (hash-table-put! existing-edges-table in-label edge-func)
        ; note: no need to return a boolean, because we never check this result in union-
        (hash-table-for-each (label-set out-label)
                             (lambda (label arrows)
                               (for-each (lambda (tunnel-label)
                                           (edge-func out-label label tunnel-label))
                                         (arrows-tunnel arrows)))))))
  
  ; sba-state edge label -> edge
  ; We must be able to take care of all the following different cases:
  ; (define-values (x) a)
  ; (define-values (x) (values a))
  ; (define-values (x) (values (values a)))
  ; (define-values (x) (values (values (values a))))
  ; ...
  ; with all the call to "values" being possibly inside functions...
  ; So we define extend-edge-for-values that recursively unpacks nested "values" by adding new
  ; unpacking edges on the fly when a label-values flows into a label that has an unpacking edge.
  ; The unpacking edge is created as a wrapper around a simple label-to-label edge simple-edge that
  ; we use for direct propagation of non-values labels.
  ; This is used in processing all values related forms (define-values, let-values, etc...)
  ; Note that for values, we only ever wrap the in-edges, not the out-edges (i.e. the edges
  ; that point towards a subexpression, not towards a context).
  (define (extend-edge-for-values sba-state simple-edge)
    (cons
     (lambda (out-label inflowing-label tunnel-label)
       (if (label-values? inflowing-label)
           ; we have something like (values a) flowing in. Now what flows into a is a list
           ; that contains the labels for the multiples values, so we have to extract that.
           (let ([label-list (hash-table-map (label-set (label-values-label inflowing-label))
                                             (lambda (label arrows)
                                               label))])
             (if (= (length label-list) 1)
                 (let ([values-label (car label-list)])
                   ; we do not expect an infinite list here, and even if we receive one it's
                   ; okay to flag an error and not propagate (even if originally the list
                   ; was of length one and we lost that information through, say, using apply)
                   ; because we try to prevent values from flowing in, not flowing out
                   ; (unlike what happens when we check for the number of values in the case
                   ; define-values, let-values, or letrec-values).
                   (if (= (label-list-length values-label) 1)
                       ; we have something like (define-values (x) (... (values a) ...)), so we add a
                       ; new direct edge from a to x. Of course this new edge has to be itself a recursive
                       ; unpacking edge, since some (values b) could later flow into a. Note that, since
                       ; our edges are independant of their origin, we can re-use the same simple edge.
                       ; Watch then the nice infinitely-looking recursion. We are just creating a
                       ; potentialy infinite number of unpacking edges, lazily. Also, since our edges
                       ; are closures already containing the target label (the one for x),
                       ; extend-edge-for-values doesn't need the target label as an explicit parameter.
                       ; Only the origin label (the one corresponding to some use of "values") ever
                       ; changes. This is just plain beautiful.
                       (let ([new-origin-label (label-cons-car values-label)])
                         (add-edge-and-propagate-set-through-edge
                          new-origin-label
                          (extend-edge-for-values sba-state simple-edge)))
                       ; (define-values (x) (... (values a b ...) ...))
                       (begin
                         (set-error-for-label sba-state
                                              inflowing-label
                                              'red
                                              (format "context expected 1 value, received ~a values"
                                                      (label-list-length values-label)))
                         #f)))
                 ; values contains more than one thing.  This is either an internal error,
                 ; or we have somehow ended up with an infinite list.  Since we trust ourselves,
                 ; we decide that it's an infinite list, and since we can't determine the
                 ; original length of the list we have to signal an error.
                 ; Question: do we still propagate or not?  After all, the length of the original
                 ; list might have been 1, in which case it would be correct to propagate.  On
                 ; the other hand most of the cases here can be expected to be error cases
                 ; (things like (apply values (list 1)) are not very common...) so propagating
                 ; would just trigger many more errors...  We flag an error anyway so we should
                 ; be fine.
                 (set-error-for-label sba-state
                                      inflowing-label
                                      'red
                                      (format "context expected 1 value, can't determine how many received"))
                 ;(error 'extend-edge-for-values "values didn't contain list: ~a"
                 ;       (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'extend-edge-for-values)
                 ;       ;(map (lambda (label)
                 ;       ;       (list (pp-type sba-state (get-type-from-label sba-state label) 'extend-edge-for-values)
                 ;       ;             (syntax-position (label-term label))
                 ;       ;             (syntax-object->datum (label-term label))))
                 ;       ;     label-list)
                 ;       )
                 ))
           ; (define-values (x) a) or equivalent (e.g. the result of analysing something like
           ; (define-values (x) (values (values (values a)))), after three levels of recursion).
           ((car simple-edge) out-label inflowing-label tunnel-label)))
     (cdr simple-edge)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DERIVATION
  
  ; sba-state syntax-object -> label
  ; create simple, basic label. Used directly during graph reconstruction for primitives, since
  ; only the outer label will be associated with the term position, not all the internal labels.
  (define (create-simple-label sba-state term)
    (let ([label (make-label #f #f #f #f #f term (make-hash-table) (make-hash-table))])
      ((sba-state-register-label-with-gui sba-state) label)
      label))
  
  (define (create-dummy-label term)
    (make-label #f #f #f #f #f term (make-hash-table) (make-hash-table)))
  
  ; create-simple-label is seldom used in the graph reconstruction from primitive type part
  ; of the code but used a lot in the graph derivation code, so rather than add a second
  ; argument to create-simple-label everywhere, it's easier to have this little specialized
  ; function for primitives...
  ; Note that such a label has prim? set to #t and that the associated term will, in practice,
  ; be the term into which the primitive label will initially flow.
  (define (create-simple-prim-label term)
    (make-label #f #f #f #f #t term (make-hash-table) (make-hash-table)))
  
  ; label -> void
  ; put a label in it's own set, for terms that are value sources
  (define (initialize-label-set-for-value-source label)
    (hash-table-put! (label-set label) label (make-arrows '() '() (list #f))))
  
  ; sba-state (listof booleans) (listof integer) (listof (listof label)) (listof label) label boolean -> edge
  ; The four first parameters simulate the surrounding case-lambda specification. We could
  ; wrap it inside a real case-lambda label, but we would have to create fake values for the
  ; other components of the structure...
  ; Note: we always create the args edges from left to right. We *need* this when we do
  ; the black magic for structures (see the create-make-struct-type-label function)
  (define (create-case-lambda-edge sba-state
                                   rest-arg?s-around req-args-around
                                   argss-labelss-around exps-labels-around
                                   label contra-union?)
    (cons
     (lambda (out-label inflowing-case-lambda-label tunnel-label)
       ; inflowing-case-lambda-label doesn't go anywhere, it's components are just connected to
       ; the rest of the graph (around), so out-label (which will be the op-label from which
       ; the case-lambda label is flowing out) is not used. I.e. op-label (out-label)
       ; is a sink for functions.
       (if (label-case-lambda? inflowing-case-lambda-label)
           (let ([top-around-thunk
                  (let loop-clauses-around
                    (; one thunk wrapped around this one for each around clause that's been
                     ; matched. If there's a matchinf error, it will be #f, and the test below
                     ; will be false, stopping the loop-clauses-around loop.
                     [around-thunk cst:dummy-thunk]
                     [rest-arg?s-around rest-arg?s-around]
                     [req-args-around req-args-around]
                     [argss-labelss-around argss-labelss-around]
                     [exps-labels-around exps-labels-around])
                    (if (null? rest-arg?s-around)
                        around-thunk
                        (let ([top-in-thunk
                               ; search match for current around clause, returning a thunk that
                               ; creates all the right edges, or #f.
                               (let loop-clauses-in
                                 ([rest-arg?s-in (label-case-lambda-rest-arg?s inflowing-case-lambda-label)]
                                  [req-args-in (label-case-lambda-req-args inflowing-case-lambda-label)]
                                  [argss-labelss-in (label-case-lambda-argss inflowing-case-lambda-label)]
                                  [exps-labels-in (label-case-lambda-exps inflowing-case-lambda-label)]
                                  [effects-in (label-case-lambda-effects inflowing-case-lambda-label)])
                                 (if (null? rest-arg?s-in)
                                     ; No match found.
                                     (begin
                                       (set-error-for-label
                                        sba-state
                                        label
                                        'red
                                        (format "procedure application: arity mismatch, given: ~a; ~a required arguments were given"
                                                (if (label-prim? inflowing-case-lambda-label)
                                                    ; this won't work if we use a primitive
                                                    ; in a higer-order way, but they can
                                                    ; always trace the case-lambda back,
                                                    ; so that should be good enough.
                                                    (unexpand (syntax-object->datum (label-term label)))
                                                    (unexpand
                                                     (syntax-object->datum
                                                      (label-term inflowing-case-lambda-label))))
                                                (car req-args-around)))
                                       #f)
                                     (let ([rest-arg?-in (car rest-arg?s-in)]
                                           [req-arg-in (car req-args-in)]
                                           [rest-arg?-around (car rest-arg?s-around)]
                                           [req-arg-around (car req-args-around)])
                                       ; case 2 is similiar to case 5 and case 3 similar to case 4,
                                       ; except that both case 4 and 5 don't go till they reach null.
                                       (cond
                                         [(and (or (and (not rest-arg?-in) (not rest-arg?-around))
                                                   (and rest-arg?-in rest-arg?-around))
                                               (= req-arg-in req-arg-around))
                                          ; exact one-to-one match between in and around, with or without
                                          ; rest args, it's the same
                                          (lambda ()
                                            ;(when (lookup-and-bind-top-level-vars
                                            ;       (car effects-in) (label-term term))
                                            ; make internal apps flow and top level vars looked up
                                            ((car effects-in))
                                            ;(set-car! app-thunks-in *dummy-thunk*)
                                            (let args-loop-in
                                              ([args-labels-in (car argss-labelss-in)]
                                               [args-labels-around (car argss-labelss-around)])
                                              (unless (null? args-labels-in)
                                                (add-edge-and-propagate-set-through-edge
                                                 (car args-labels-around)
                                                 (extend-edge-for-values
                                                  sba-state
                                                  (create-simple-edge (car args-labels-in))))
                                                (args-loop-in (cdr args-labels-in)
                                                              (cdr args-labels-around))))
                                            ; edge from body of clause to app term itself
                                            ; note that we do not detect multiple values here
                                            (add-edge-and-propagate-set-through-edge
                                             (car exps-labels-in)
                                             (create-simple-edge (car exps-labels-around))))]
                                         [(and rest-arg?-in (not rest-arg?-around)
                                               (<= req-arg-in req-arg-around))
                                          ; fixed number of args around and the in function can
                                          ; take them all. So we just have to create a label list for
                                          ; the rest argument.
                                          (lambda ()
                                            ;(when (lookup-and-bind-top-level-vars
                                            ;       (car effects-in) (label-term term))
                                            ; make internal apps flow
                                            ((car effects-in))
                                            ;(set-car! app-thunks-in *dummy-thunk*)
                                            (let args-loop-in
                                              ([args-labels-in (car argss-labelss-in)]
                                               [args-labels-around (car argss-labelss-around)])
                                              ; we know we have a rest arg, so the list is not null
                                              (if (null? (cdr args-labels-in))
                                                  ; create list for rest arg
                                                  (let* ([rest-arg-label (car args-labels-in)]
                                                         [rest-arg-term (label-term rest-arg-label)]
                                                         [args-labels-around-in-labellist
                                                          (let rest-loop-around ([args-labels-around
                                                                                  args-labels-around])
                                                            (if (null? args-labels-around)
                                                                (let ([null-label
                                                                       (make-label-cst
                                                                        #f #f #f #f #t
                                                                        rest-arg-term
                                                                        (make-hash-table)
                                                                        (make-hash-table)
                                                                        '())])
                                                                  (initialize-label-set-for-value-source
                                                                   null-label)
                                                                  ;(register-label-with-gui
                                                                  ; null-label)
                                                                  null-label)
                                                                (let ([cons-label
                                                                       (make-label-cons
                                                                        #f #f #f #f #t
                                                                        rest-arg-term
                                                                        (make-hash-table)
                                                                        (make-hash-table)
                                                                        (car args-labels-around)
                                                                        (rest-loop-around
                                                                         (cdr args-labels-around)))])
                                                                  (initialize-label-set-for-value-source
                                                                   cons-label)
                                                                  ;(register-label-with-gui
                                                                  ; cons-label)
                                                                  cons-label)))])
                                                    ; we know args-label-around-inlabellist is not
                                                    ; a multiple value...
                                                    (add-edge-and-propagate-set-through-edge
                                                     args-labels-around-in-labellist
                                                     (create-simple-edge rest-arg-label)))
                                                  ; normal args
                                                  (begin
                                                    (add-edge-and-propagate-set-through-edge
                                                     (car args-labels-around)
                                                     (extend-edge-for-values
                                                      sba-state
                                                      (create-simple-edge (car args-labels-in))))
                                                    (args-loop-in (cdr args-labels-in)
                                                                  (cdr args-labels-around)))))
                                            ; edge from body of clause to app term itself
                                            ; note that we do not detect multiple values here
                                            (add-edge-and-propagate-set-through-edge
                                             (car exps-labels-in)
                                             (create-simple-edge (car exps-labels-around))))]
                                         [(and (not rest-arg?-in) rest-arg?-around
                                               (>= req-arg-in req-arg-around))
                                          ; in fct takes a fixed number of args and there's some of
                                          ; them around in the rest argument => distribute  what
                                          ; is in the rest arg around by creating cons-distributing
                                          ; labels/edges. The problem is that we don't want to add
                                          ; any edges between in and around as long as we aren't sure
                                          ; we have the right number of arguments flowing into the
                                          ; rest argument. So we use a separate inner thunk to delay
                                          ; the creation of the edges for the regular arguments
                                          ; until we know the right number of args is actually flowing
                                          ; into the rest arg...
                                          (let ([inner-thunk
                                                 ; edge from body of clause to app term itself
                                                 ; note that we do not detect multiple values here
                                                 (lambda ()
                                                   (add-edge-and-propagate-set-through-edge
                                                    (car exps-labels-in)
                                                    (create-simple-edge (car exps-labels-around))))])
                                            (let args-loop-around
                                              ([args-labels-in (car argss-labelss-in)]
                                               [args-labels-around (car argss-labelss-around)])
                                              ; we know we have a rest arg, so the list is not null
                                              (if (null? (cdr args-labels-around))
                                                  ; distribute list for rest arg, if we can
                                                  ; note: we can create all the edges here in the let
                                                  ; directly, because nothing will flow into them
                                                  ; unless we add the arg-number-checking-edge to
                                                  ; rest-arg-label, i.e. not until the lambda in the
                                                  ; body of this let is applied (which will itself
                                                  ; only happen when the top level loop terminates).
                                                  (let* ([rest-arg-label (car args-labels-around)]
                                                         [rest-arg-term (label-term rest-arg-label)]
                                                         [splitting-rest-arg-label
                                                          (let rest-loop-in
                                                            ([args-labels-in args-labels-in])
                                                            (if (null? args-labels-in)
                                                                (let ([null-label
                                                                       (make-label-cst
                                                                        #f #f #f #f #t
                                                                        rest-arg-term
                                                                        (make-hash-table)
                                                                        (make-hash-table)
                                                                        '())])
                                                                  ; note that there's no need to type
                                                                  ; check here, because the only thing
                                                                  ; that ever flows in is '(), since we
                                                                  ; already checked the length below.
                                                                  ; (except in the case of an infinite
                                                                  ; list flowing in, in which case we don't
                                                                  ; want to type check anything anyway).
                                                                  ;(associate-label-with-type
                                                                  ; null-checking-label
                                                                  ; (make-type-cst '()))
                                                                  null-label)
                                                                (let* ([car-label (car args-labels-in)]
                                                                       [car-edge (create-simple-edge car-label)]
                                                                       [cdr-label (rest-loop-in (cdr args-labels-in))]
                                                                       [cdr-edge (create-simple-edge cdr-label)]
                                                                       [cons-label (create-simple-prim-label (label-term label))]
                                                                       [cons-edge
                                                                        (cons
                                                                         (lambda (out-label inflowing-label tunnel-label)
                                                                           ; cons sink => no use for
                                                                           ; out-label here.
                                                                           ; Note: we still have to test
                                                                           ; that we actually have a
                                                                           ; label-cons, in case the
                                                                           ; inflowing list is infinite,
                                                                           ; because then '() will flow in
                                                                           ; too.
                                                                           (when (label-cons? inflowing-label)
                                                                             (and 
                                                                              (add-edge-and-propagate-set-through-edge
                                                                               (label-cons-car inflowing-label)
                                                                               car-edge)
                                                                              (add-edge-and-propagate-set-through-edge
                                                                               (label-cons-cdr inflowing-label)
                                                                               cdr-edge))))
                                                                         ; cons sink
                                                                         (gensym))])
                                                                  ;(associate-label-with-type cons-label
                                                                  ;                           (make-type-cons
                                                                  ;                            (make-type-cst 'top) 
                                                                  ;                            (make-type-cst 'top)))
                                                                  (add-edge-and-propagate-set-through-edge
                                                                   cons-label cons-edge)
                                                                  cons-label)))]
                                                         [splitting-rest-arg-edge
                                                          (create-simple-edge splitting-rest-arg-label)]
                                                         [arg-number-checking-edge
                                                          (let ([inner-thunk inner-thunk])
                                                            (cons
                                                             (lambda (out-label inflowing-label tunnel-label)
                                                               (let ([rest-list-length (label-list-length inflowing-label)])
                                                                 (if (or (= rest-list-length +inf.0) ; infinite list
                                                                         (= (+ rest-list-length req-arg-around)
                                                                            req-arg-in))
                                                                     (begin
                                                                       ;(when (lookup-and-bind-top-level-vars
                                                                       ;       (car effects-in) (label-term term))
                                                                       ; make internal apps flow
                                                                       ((car effects-in))
                                                                       ;(set-car! app-thunks-in *dummy-thunk*)
                                                                       (add-edge-and-propagate-set-through-edge
                                                                        inflowing-label
                                                                        splitting-rest-arg-edge)
                                                                       (inner-thunk))
                                                                     (begin
                                                                       (set-error-for-label
                                                                        sba-state
                                                                        inflowing-case-lambda-label
                                                                        'red
                                                                        (format "possible arity error (might be a side effect of generating an infinite list): function ~a expected ~a arguments, received ~a"
                                                                                ; this would underline the primitive that generated the list
                                                                                ;(syntax-object->datum
                                                                                ; (label-term 
                                                                                ;  inflowing-label))
                                                                                (syntax-object->datum
                                                                                 (label-term
                                                                                  inflowing-case-lambda-label))
                                                                                req-arg-in
                                                                                (+ rest-list-length req-arg-around)
                                                                                ))
                                                                       #f))))
                                                             ; sink
                                                             (gensym)))])
                                                    (lambda ()
                                                      ; that's the only thing the top level loop will
                                                      ; have to do for this clause if all the clauses are
                                                      ; matched. Everything else will be done when args
                                                      ; flow into the rest arg.
                                                      (add-edge-and-propagate-set-through-edge
                                                       rest-arg-label
                                                       arg-number-checking-edge)))
                                                  ; normal args
                                                  (begin
                                                    (set! inner-thunk
                                                          (let ([inner-thunk inner-thunk])
                                                            (lambda ()
                                                              (add-edge-and-propagate-set-through-edge
                                                               (car args-labels-around)
                                                               (extend-edge-for-values
                                                                sba-state
                                                                (create-simple-edge (car args-labels-in))))
                                                              (inner-thunk))))
                                                    (args-loop-around (cdr args-labels-in)
                                                                      (cdr args-labels-around))))))]
                                         [(and rest-arg?-in rest-arg?-around
                                               (> req-arg-in req-arg-around))
                                          ; same problem here as in the previous case...
                                          (let ([inner-thunk
                                                 ; edge from body of clause to app term itself
                                                 ; note that we do not detect multiple values here
                                                 (lambda ()
                                                   (add-edge-and-propagate-set-through-edge
                                                    (car exps-labels-in)
                                                    (create-simple-edge (car exps-labels-around))))])
                                            (let args-loop-around
                                              ([args-labels-in (car argss-labelss-in)]
                                               [args-labels-around (car argss-labelss-around)])
                                              ; we know we have a rest arg, so the list is not null
                                              (if (null? (cdr args-labels-around))
                                                  ; distribute list for rest arg, if we can
                                                  ; note: we can create all the edges here in the let
                                                  ; directly, because nothing will flow into them
                                                  ; unless we add the arg-number-checking-edge to
                                                  ; rest-arg-label, i.e. not until the lambda in the
                                                  ; body of this let is applied (which will itself
                                                  ; only happen when the top level loop terminates).
                                                  (let* ([rest-arg-label (car args-labels-around)]
                                                         [rest-arg-term (label-term rest-arg-label)]
                                                         [splitting-rest-arg-label
                                                          (let rest-loop-in
                                                            ([args-labels-in args-labels-in])
                                                            (if (null? (cdr args-labels-in))
                                                                ; all the remaining values in the list of rest-arg-around
                                                                ; flow into rest-arg-in
                                                                (car args-labels-in)
                                                                (let* ([car-label (car args-labels-in)]
                                                                       [car-edge (create-simple-edge car-label)]
                                                                       [cdr-label (rest-loop-in (cdr args-labels-in))]
                                                                       [cdr-edge (create-simple-edge cdr-label)]
                                                                       [cons-label (create-simple-prim-label (label-term label))]
                                                                       [cons-edge
                                                                        (cons
                                                                         (lambda (out-label inflowing-label tunnel-label)
                                                                           ; cons sink => no use for
                                                                           ; out-label here.
                                                                           ; Note: we still have to test
                                                                           ; that we actually have a
                                                                           ; label-cons, in case the
                                                                           ; inflowing list is infinite.
                                                                           ; because then '() will flow in
                                                                           ; too.
                                                                           (when (label-cons? inflowing-label)
                                                                             (and 
                                                                              (add-edge-and-propagate-set-through-edge
                                                                               (label-cons-car inflowing-label)
                                                                               car-edge)
                                                                              (add-edge-and-propagate-set-through-edge
                                                                               (label-cons-cdr inflowing-label)
                                                                               cdr-edge))))
                                                                         ; cons sink
                                                                         (gensym))])
                                                                  ;(associate-label-with-type cons-label
                                                                  ;                           (make-type-cons
                                                                  ;                            (make-type-cst 'top) 
                                                                  ;                            (make-type-cst 'top)))
                                                                  (add-edge-and-propagate-set-through-edge
                                                                   cons-label cons-edge)
                                                                  cons-label)))]
                                                         [splitting-rest-arg-edge
                                                          (create-simple-edge splitting-rest-arg-label)]
                                                         [arg-number-checking-edge
                                                          (let ([inner-thunk inner-thunk])
                                                            (cons
                                                             (lambda (out-label inflowing-label tunnel-label)
                                                               (let ([rest-list-length (label-list-length inflowing-label)])
                                                                 (if (or (= rest-list-length +inf.0) ; infinite list
                                                                         (= (+ rest-list-length req-arg-around)
                                                                            req-arg-in))
                                                                     (begin
                                                                       ;(when (lookup-and-bind-top-level-vars
                                                                       ;       (car effects-in) (label-term term))
                                                                       ; make internal apps flow
                                                                       ((car effects-in))
                                                                       ;(set-car! app-thunks-in *dummy-thunk*)
                                                                       (add-edge-and-propagate-set-through-edge
                                                                        inflowing-label
                                                                        splitting-rest-arg-edge)
                                                                       (inner-thunk))
                                                                     (begin
                                                                       (set-error-for-label
                                                                        sba-state
                                                                        inflowing-case-lambda-label
                                                                        'red
                                                                        (format "possible arity error (might be a side effect of generating an infinite list): function ~a expected ~a arguments, received ~a"
                                                                                ; this would underline the primitive that generated the list
                                                                                ;(syntax-object->datum
                                                                                ; (label-term 
                                                                                ;  inflowing-label))
                                                                                (syntax-object->datum
                                                                                 (label-term
                                                                                  inflowing-case-lambda-label))
                                                                                req-arg-in
                                                                                (+ rest-list-length req-arg-around)
                                                                                ))
                                                                       #f))))
                                                             ; sink
                                                             (gensym)))])
                                                    (lambda ()
                                                      ; that's the only thing the top level loop will
                                                      ; have to do for this clause if all the clauses are
                                                      ; matched. Everything else will be done when args
                                                      ; flow into the rest arg.
                                                      (add-edge-and-propagate-set-through-edge
                                                       rest-arg-label
                                                       arg-number-checking-edge)))
                                                  ; normal args
                                                  (begin
                                                    (set! inner-thunk
                                                          (let ([inner-thunk inner-thunk])
                                                            (lambda ()
                                                              (add-edge-and-propagate-set-through-edge
                                                               (car args-labels-around)
                                                               (extend-edge-for-values
                                                                sba-state
                                                                (create-simple-edge (car args-labels-in))))
                                                              (inner-thunk))))
                                                    (args-loop-around (cdr args-labels-in)
                                                                      (cdr args-labels-around))))))]
                                         [(and rest-arg?-in rest-arg?-around
                                               (< req-arg-in req-arg-around))
                                          (lambda ()
                                            ;(when (lookup-and-bind-top-level-vars
                                            ;       (car effects-in) (label-term term))
                                            ; make internal apps flow
                                            ((car effects-in))
                                            ;(set-car! app-thunks-in *dummy-thunk*)
                                            (let args-loop-in
                                              ([args-labels-in (car argss-labelss-in)]
                                               [args-labels-around (car argss-labelss-around)])
                                              ; we know we have a rest arg, so the list is not null
                                              (if (null? (cdr args-labels-in))
                                                  ; create list for rest arg
                                                  (let* ([rest-arg-label (car args-labels-in)]
                                                         [rest-arg-term (label-term rest-arg-label)]
                                                         [args-labels-around-in-labellist
                                                          (let rest-loop-around ([args-labels-around
                                                                                  args-labels-around])
                                                            (if (null? (cdr args-labels-around))
                                                                ; everything in rest-arg-around will flow
                                                                ; into rest-arg-in, plus some other stuff
                                                                ; around the list.
                                                                (car args-labels-around)
                                                                (let ([cons-label
                                                                       (make-label-cons
                                                                        #f #f #f #f #t
                                                                        rest-arg-term
                                                                        (make-hash-table)
                                                                        (make-hash-table)
                                                                        (car args-labels-around)
                                                                        (rest-loop-around
                                                                         (cdr args-labels-around)))])
                                                                  (initialize-label-set-for-value-source
                                                                   cons-label)
                                                                  ;(register-label-with-gui
                                                                  ; cons-label)
                                                                  cons-label)))])
                                                    ; we know args-label-around-inlabellist is not
                                                    ; a multiple value...
                                                    (add-edge-and-propagate-set-through-edge
                                                     args-labels-around-in-labellist
                                                     (create-simple-edge rest-arg-label)))
                                                  ; normal args
                                                  (begin
                                                    (add-edge-and-propagate-set-through-edge
                                                     (car args-labels-around)
                                                     (extend-edge-for-values
                                                      sba-state
                                                      (create-simple-edge (car args-labels-in))))
                                                    (args-loop-in (cdr args-labels-in)
                                                                  (cdr args-labels-around)))))
                                            ; edge from body of clause to app term itself
                                            ; note that we do not detect multiple values here
                                            (add-edge-and-propagate-set-through-edge
                                             (car exps-labels-in)
                                             (create-simple-edge (car exps-labels-around))))]
                                         [else ; keep looking for a matching clause
                                          (loop-clauses-in
                                           (cdr rest-arg?s-in) (cdr req-args-in)
                                           (cdr argss-labelss-in) (cdr exps-labels-in)
                                           (cdr effects-in))]))))])
                          (if top-in-thunk
                              (loop-clauses-around (lambda ()
                                                     ; connect the current around clause
                                                     (top-in-thunk)
                                                     ; and all the other ones before it
                                                     (around-thunk))
                                                   (cdr rest-arg?s-around)
                                                   (cdr req-args-around)
                                                   (cdr argss-labelss-around)
                                                   (cdr exps-labels-around))
                              #f))))])
             (when top-around-thunk
               (top-around-thunk)))
           ; trying to apply something not a function
           ; Note: nothing was done, so there's nothing to undo
           (begin
             (set-error-for-label sba-state
                                  label
                                  'red
                                  (format "procedure application: expected procedure, given: ~a"
                                          (unexpand (syntax-object->datum
                                                     (label-term inflowing-case-lambda-label)))))
             #f)))
     ; function value sink => unique, fake destination
     (gensym)))
  
  
  ; (label -> boolean) label label edge -> edge
  ; The returned edge simulates an "if" based on the result of pred.
  ; our edges are origin-independant, so we can use the same one for both true and false.
  (define (create-self-modifying-edge pred true-label false-label join-edge)
    (letrec ([edge-fake-destination (gensym)]
             [dummy-edge (cons
                          (lambda (out-label inflowing-label tunnel-label)
                            ; sink edge, so no need for out-label
                            #t)
                          ; test value sink
                          edge-fake-destination)]
             [self-modifying-edge
              (cons
               (lambda (out-label inflowing-label tunnel-label)
                 ; sink edge, so no need for out-label
                 (if (pred inflowing-label)
                     (begin
                       (set! self-modifying-edge
                             (cons
                              (lambda (out-label inflowing-label tunnel-label)
                                ; sink edge, so no need for out-label
                                (when (not (pred inflowing-label))
                                  ; it would be more efficient to directly remove the edge.
                                  (set! self-modifying-edge dummy-edge)
                                  (add-edge-and-propagate-set-through-edge
                                   false-label join-edge)))
                              ; test value sink
                              edge-fake-destination))
                       (add-edge-and-propagate-set-through-edge
                        true-label join-edge))
                     (begin
                       (set! self-modifying-edge
                             (cons
                              (lambda (out-label inflowing-label tunnel-label)
                                ; sink edge, so no need for out-label
                                (when (pred inflowing-label)
                                  ; it would be more efficient to directly remove the edge.
                                  (set! self-modifying-edge dummy-edge)
                                  (add-edge-and-propagate-set-through-edge
                                   true-label join-edge)))
                              ; test value sink
                              edge-fake-destination))
                       (add-edge-and-propagate-set-through-edge
                        false-label join-edge))))
               ; test value sink
               edge-fake-destination)])
      self-modifying-edge))
  
  ; label label-struct-type -> boolean
  ; is a struct value a subtype of a struct type ?
  (define (is-subtype? label struct-type-label)
    (and (label-struct-value? label)
         (let loop ([type (label-struct-value-type label)])
           (if type
               (if (eq? type struct-type-label)
                   #t
                   (loop (label-struct-type-parent type)))
               ; no more parent
               #f))))
  
  ; sba-state syntax-object -> label
  ; create a label in which a case-lambda label flows, which, when applied, creates
  ; struct function labels of the right type.
  ; Note that we will return a case-lambda label that does strange things when something
  ; flows into it, in the sense that it will gather its different arguments using
  ; struct-label, and then create new case-lambda labels on the fly.
  ; Note also that we know that no multiple values can flow into the case-lambda label
  ; for make-struct-type, or any other for that matter, so we don't have to worry about that.
  ; (define-struct (bar foo) (d e f)) expands into
  ; (begin
  ;   (define-values
  ;     (struct:bar make-bar bar? bar-d set-bar-d! bar-e set-bar-e! bar-f set-bar-f!)
  ;     (let-values
  ;         (((type maker pred access mutate)
  ;           (#%app make-struct-type
  ;                  'bar
  ;                  (#%top . struct:foo)
  ;                  (#%datum . 3)
  ;                  (#%datum . 0)
  ;                  (#%datum . #f)
  ;                  null
  ;                  (#%datum . #f))))
  ;       (#%app values
  ;              type
  ;              maker
  ;              pred
  ;              (#%app make-struct-field-accessor access (#%datum . 0) 'd)
  ;              (#%app make-struct-field-mutator mutate (#%datum . 0) 'd)
  ;              (#%app make-struct-field-accessor access (#%datum . 1) 'e)
  ;              (#%app make-struct-field-mutator mutate (#%datum . 1) 'e)
  ;              (#%app make-struct-field-accessor access (#%datum . 2) 'f)
  ;              (#%app make-struct-field-mutator mutate (#%datum . 2) 'f))))
  ;   (define-syntaxes ...))
  ; which explains most of the names below...
  ; We only explicitely deal here with the
  ;           (#%app make-struct-type
  ;                  'bar
  ;                  (#%top . struct:foo)
  ;                  (#%datum . 3)
  ;                  (#%datum . 0)
  ;                  (#%datum . #f)
  ;                  null
  ;                  (#%datum . #f))))
  ; part, and let the rest of the anlysis deal with values, let-values, variable bindings, etc...
  (define (create-make-struct-type-label sba-state term)
    (let* (; We really use this label as a type shared between the different instances
           ; of the structure. It's also what we use to differentiate between two kinds of
           ; structures with the same name. The only reason it's a label instead of a type
           ; is because mzscheme treats it as a first class value and have it bound
           ; to struct:blablabla, so it needs to be a label to be able to flow...
           [struct-type-label (make-label-struct-type
                               #f #f #f #f #t
                               term
                               (make-hash-table)
                               (make-hash-table)
                               'uninitialized ; struct has no name (yet)
                               #f ; no parent by default
                               0 ; parent has no fields by default
                               0 ; struct has no fields by default
                               #f)]
           [maker-label (create-simple-prim-label term)]
           [maker-edge (create-simple-edge maker-label)]
           [pred-label (create-simple-prim-label term)]
           [pred-edge (create-simple-edge pred-label)]
           [access-label (create-simple-prim-label term)]
           [access-edge (create-simple-edge access-label)]
           [mutate-label (create-simple-prim-label term)]
           [mutate-edge (create-simple-edge mutate-label)]
           [null-label (make-label-cst #f #f #f #f #t term (make-hash-table) (make-hash-table) '())]
           [cons-label1 (make-label-cons #f #f #f #f #t term (make-hash-table) (make-hash-table)
                                         mutate-label null-label)]
           [cons-label2 (make-label-cons #f #f #f #f #t term (make-hash-table) (make-hash-table)
                                         access-label cons-label1)]
           [cons-label3 (make-label-cons #f #f #f #f #t term (make-hash-table) (make-hash-table)
                                         pred-label cons-label2)]
           [cons-label4 (make-label-cons #f #f #f #f #t term (make-hash-table) (make-hash-table)
                                         maker-label cons-label3)]
           [cons-label5 (make-label-cons #f #f #f #f #t term (make-hash-table) (make-hash-table)
                                         struct-type-label cons-label4)]
           [values-label (make-label-values #f #f #f #f #t term (make-hash-table) (make-hash-table)
                                            cons-label5)]
           [name-label (create-simple-prim-label term)]
           [name-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only symbol should flow in here
               (if (and (label-cst? inflowing-label)
                        (symbol? (label-cst-value inflowing-label)))
                   (begin
                     (set-label-struct-type-name! struct-type-label
                                                  (label-cst-value inflowing-label))
                     #t)
                   (begin
                     (set-error-for-label sba-state
                                          inflowing-label
                                          'red
                                          "make-struct-type expected symbol")
                     (set-label-struct-type-error?! struct-type-label #t)
                     #f)))
             (gensym))]
           [parent-label (create-simple-prim-label term)]
           [parent-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; if anything flows in here, it should be the struct label for the
               ; parent struct, or #f
               (if (or (label-struct-type? inflowing-label)
                       (and (label-cst? inflowing-label)
                            (not (label-cst-value inflowing-label))))
                   (begin
                     (when (label-struct-type? inflowing-label)
                       (set-label-struct-type-parent! struct-type-label
                                                      inflowing-label))
                     #t)
                   (begin
                     (set-error-for-label sba-state
                                          inflowing-label
                                          'red
                                          "make-struct-type expected structure type")
                     (set-label-struct-type-error?! struct-type-label #t)
                     #f)))
             (gensym))]
           [field-label (create-simple-prim-label term)]
           [field-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; inflowing label will tell use how many fields the struct will have
               (if (and (label-cst? inflowing-label)
                        (number? (label-cst-value inflowing-label)))
                   (begin
                     (let* ([parent (label-struct-type-parent struct-type-label)]
                            [parent-fields-nbr (if parent
                                                   (label-struct-type-total-fields-nbr
                                                    (label-struct-type-parent struct-type-label))
                                                   0)])
                       (set-label-struct-type-parent-fields-nbr! struct-type-label parent-fields-nbr)
                       (set-label-struct-type-total-fields-nbr! struct-type-label
                                                                (+ (label-cst-value inflowing-label)
                                                                   parent-fields-nbr)))
                     #t)
                   (begin
                     (set-error-for-label sba-state
                                          inflowing-label
                                          'red
                                          "make-struct-type expected number")
                     (set-label-struct-type-error?! struct-type-label #t)
                     #f)))
             (gensym))]
           [auto-field-label (create-simple-prim-label term)]
           [auto-field-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only 0 should flow in here
               (if (and (label-cst? inflowing-label)
                        (let ([value (label-cst-value inflowing-label)])
                          (and (number? value) (zero? value))))
                   #t
                   (begin
                     (set-error-for-label sba-state
                                          inflowing-label
                                          'red
                                          "auto-initialized structure fields not yet supported: expected 0")
                     (set-label-struct-type-error?! struct-type-label #t)
                     #f)))
             (gensym))]
           [auto-field-value-label (create-simple-prim-label term)]
           [auto-field-value-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only #f should flow in here
               (if (and (label-cst? inflowing-label)
                        (not (label-cst-value inflowing-label)))
                   #t
                   (begin
                     (set-error-for-label sba-state
                                          inflowing-label
                                          'red
                                          "auto-initialized structure fields not yet supported: expected #f")
                     (set-label-struct-type-error?! struct-type-label #t)
                     #f)))
             (gensym))]
           [properties-label (create-simple-prim-label term)]
           [properties-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only '() should flow in here
               (if (and (label-cst? inflowing-label)
                        (null? (label-cst-value inflowing-label)))
                   #t
                   (begin
                     (set-error-for-label sba-state
                                          inflowing-label
                                          'red
                                          "structure properties not yet supported: expected ()")
                     (set-label-struct-type-error?! struct-type-label #t)
                     #f)))
             (gensym))]
           [inspector-label (create-simple-prim-label term)]
           [inspector-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only #f should flow in here
               (if (and (label-cst? inflowing-label)
                        (not (label-cst-value inflowing-label)))
                   ; now, at this point, and since edges from actual to formal arguments
                   ; are created left to right when a function is applied, we know struct-label
                   ; has been complitely filled out. So we can do the black magic part, which
                   ; consists in creating case-lambdas on the fly, that will become the maker,
                   ; pred, access and mutate functions, and gather them in the multiple value
                   ; label.
                   (if (label-struct-type-error? struct-type-label)
                       ; nothing created, so nothing ever propagates down to
                       ; make-struct-field-accessor or make-struct-field-mutator
                       #f
                       (let* ([total-fields-nbr (label-struct-type-total-fields-nbr struct-type-label)]
                              ; maker
                              [maker-body-label (make-label-struct-value
                                                 #f #f #f #f #t
                                                 term
                                                 (make-hash-table)
                                                 (make-hash-table)
                                                 struct-type-label
                                                 (etc:build-list total-fields-nbr
                                                                 (lambda (_)
                                                                   (create-simple-prim-label term))))]
                              [maker-case-lambda-label (make-label-case-lambda
                                                        #f #f #f #f #t
                                                        term
                                                        (make-hash-table)
                                                        (make-hash-table)
                                                        struct-type-label ; never used
                                                        (list #f)
                                                        (list total-fields-nbr)
                                                        (list (label-struct-value-fields
                                                               maker-body-label))
                                                        (list maker-body-label)
                                                        (list cst:dummy-thunk))]
                              ; pred
                              [pred-true-label (make-label-cst #f #f #f #f #t
                                                               term
                                                               (make-hash-table)
                                                               (make-hash-table)
                                                               #t)]
                              [pred-false-label (make-label-cst #f #f #f #f #t
                                                                term
                                                                (make-hash-table)
                                                                (make-hash-table)
                                                                #f)]
                              [pred-body-label (create-simple-prim-label term)]
                              [pred-body-edge (create-simple-edge pred-body-label)]
                              [pred-arg-label (create-simple-prim-label term)]
                              [pred-arg-edge
                               (create-self-modifying-edge (lambda (label)
                                                             (is-subtype? label struct-type-label))
                                                           pred-true-label pred-false-label
                                                           pred-body-edge)]
                              [pred-case-lambda-label (make-label-case-lambda
                                                       #f #f #f #f #t
                                                       term
                                                       (make-hash-table)
                                                       (make-hash-table)
                                                       struct-type-label ; never used
                                                       (list #f)
                                                       (list 1)
                                                       (list (list pred-arg-label))
                                                       (list pred-body-label)
                                                       (list cst:dummy-thunk))]
                              ; access is a bit tricky: make-struct-field-accessor will
                              ; manually link access's second arg and wrap access inside another
                              ; case-lambda with one arg less. We just have to remember which
                              ; structure type access is about by setting the struct field to
                              ; struct-type-label (something we didn't really have to do for the
                              ; maker and pred, because the maker-body-label and pred-arg-edge
                              ; already explicitely refer to it, but we did it anyway, above,
                              ; just for consistency).
                              ; Note: no error checking on the input is done here. It will be done
                              ; by the wrapper.
                              [access-first-arg-label (create-simple-prim-label term)]
                              [access-second-arg-label (create-simple-prim-label term)]
                              [access-case-lambda-label (make-label-case-lambda
                                                         #f #f #f #f #t
                                                         term
                                                         (make-hash-table)
                                                         (make-hash-table)
                                                         struct-type-label
                                                         (list #f)
                                                         (list 2)
                                                         (list (list access-first-arg-label
                                                                     access-second-arg-label))
                                                         (list (create-simple-prim-label term))
                                                         (list cst:dummy-thunk))]
                              ; same problem with mutate
                              [mutate-first-arg-label (create-simple-prim-label term)]
                              [mutate-second-arg-label (create-simple-prim-label term)]
                              [mutate-third-arg-label (create-simple-prim-label term)]
                              [mutate-case-lambda-label (make-label-case-lambda
                                                         #f #f #f #f #t
                                                         term
                                                         (make-hash-table)
                                                         (make-hash-table)
                                                         struct-type-label
                                                         (list #f)
                                                         (list 2)
                                                         (list (list mutate-first-arg-label
                                                                     mutate-second-arg-label
                                                                     mutate-third-arg-label))
                                                         (list (create-simple-prim-label term))
                                                         (list cst:dummy-thunk))])
                         ; XXX should all the add-edge-and-propagate-set-through-edge be and-ed ?
                         ; maker
                         (initialize-label-set-for-value-source maker-body-label)            
                         (initialize-label-set-for-value-source maker-case-lambda-label)
                         (add-edge-and-propagate-set-through-edge
                          maker-case-lambda-label maker-edge)
                         ; pred
                         (initialize-label-set-for-value-source pred-true-label)
                         (initialize-label-set-for-value-source pred-false-label)
                         (add-edge-and-propagate-set-through-edge
                          pred-arg-label pred-arg-edge)
                         (initialize-label-set-for-value-source pred-case-lambda-label)
                         (add-edge-and-propagate-set-through-edge
                          pred-case-lambda-label pred-edge)
                         ; access
                         (initialize-label-set-for-value-source access-case-lambda-label)
                         (add-edge-and-propagate-set-through-edge
                          access-case-lambda-label access-edge)
                         ; mutate
                         (initialize-label-set-for-value-source mutate-case-lambda-label)
                         (add-edge-and-propagate-set-through-edge
                          mutate-case-lambda-label mutate-edge)
                         #t))
                   (begin
                     (set-error-for-label sba-state
                                          inflowing-label
                                          'red
                                          "structure inspectors not yet supported: expected #f")
                     (set-label-struct-type-error?! struct-type-label #t)
                     #f)))
             (gensym))]
           [make-struct-type-label (make-label-case-lambda
                                    #f #f #f #f #t term (make-hash-table) (make-hash-table) #f
                                    (list #f)
                                    (list 7)
                                    (list (list name-label
                                                parent-label
                                                field-label
                                                auto-field-label
                                                auto-field-value-label
                                                properties-label
                                                inspector-label))
                                    (list values-label)
                                    (list cst:dummy-thunk))])
      ; make-struct-type args
      (add-edge-and-propagate-set-through-edge name-label name-edge)
      (add-edge-and-propagate-set-through-edge parent-label parent-edge)
      (add-edge-and-propagate-set-through-edge field-label field-edge)
      (add-edge-and-propagate-set-through-edge auto-field-label auto-field-edge)
      (add-edge-and-propagate-set-through-edge auto-field-value-label auto-field-value-edge)
      (add-edge-and-propagate-set-through-edge properties-label properties-edge)
      (add-edge-and-propagate-set-through-edge inspector-label inspector-edge)
      (initialize-label-set-for-value-source struct-type-label)
      ; multiple values list
      (initialize-label-set-for-value-source null-label)
      (initialize-label-set-for-value-source cons-label1)
      (initialize-label-set-for-value-source cons-label2)
      (initialize-label-set-for-value-source cons-label3)
      (initialize-label-set-for-value-source cons-label4)
      (initialize-label-set-for-value-source cons-label5)
      (initialize-label-set-for-value-source values-label)
      (initialize-label-set-for-value-source make-struct-type-label)
      make-struct-type-label))
  
  ; sba-state syntax-object -> label
  ; Here again we rely heavily on the order in which actual arguments are connected
  ; to formal arguments (i.e. left to right). Note that the first arg of
  ; make-struct-field-accessor will be access, which is bound to the access defined
  ; by make-struct-type. This means that, if the define-struct is inside a lambda,
  ; we should make sure that, when the lambda is applied, make-struct-type is applied
  ; before make-struct-field-accessor. Hence the order in which the thunks are built
  ; in the #%app rule of create-label-from-term.
  (define (create-make-struct-field-accessor-label sba-state term)
    (let* (; WARNING: we assume that each occurence of make-struct-field-accessor is
           ; only used once in the program being analyzed, so set!-ing struct-label, access,
           ; and field-index is ok. This *will* break if the user starts using a
           ; function like:
           ; (lambda (index name)
           ;   (make-struct-field-accessor access index name))
           ; to create the different accessors, because the state will then be shared
           ; between several call places.
           ; Note that we could get back the struct-label and field-index when needed
           ; by fishing them out of the sets of the first and second args, but it's
           ; less painful to do it that way, and doing the fishing would still break
           ; the same way anyway...
           [struct-type-label #f]
           [access #f]
           [field-index #f]
           [body-label (create-simple-prim-label term)]
           [body-edge (create-simple-edge body-label)]
           [first-arg-label (create-simple-prim-label term)]
           [first-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only a case-lambda for a struct with a single arity-2 clause
               ; should flow in here. Note that, as in create-make-struct-type-label,
               ; we do the type checking as stuff flows in, instead of doing it
               ; post-analysis, just to make sure we don't screw our invariants when
               ; we finally run third-arg-edge...
               (if (and (label-case-lambda? inflowing-label)
                        (label-case-lambda-struct inflowing-label)
                        (= (length (label-case-lambda-rest-arg?s inflowing-label)) 1)
                        (= (length (car (label-case-lambda-argss inflowing-label))) 2))
                   (begin
                     (set! struct-type-label (label-case-lambda-struct inflowing-label))
                     (set! access inflowing-label)
                     #t)
                   (begin
                     (set-error-for-label sba-state
                                          inflowing-label
                                          'red
                                          (format "make-struct-field-accessor: expects type <accessor procedure that requires a field index> as 1st argument, given: ~a"
                                                  (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-make-struct-field-accessor-label1)))
                     #f)))
             (gensym))]
           [second-arg-label (create-simple-prim-label term)]
           [second-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only a number in the right range should flow in here
               (if struct-type-label
                   (if (and (label-cst? inflowing-label)
                            (let ([value (label-cst-value inflowing-label)])
                              (and (number? value)
                                   (exact? value)
                                   (<= 0 value))))
                       (let ([value (label-cst-value inflowing-label)])
                         (if (< value (- (label-struct-type-total-fields-nbr struct-type-label)
                                         (label-struct-type-parent-fields-nbr struct-type-label)))
                             (begin
                               (set! field-index
                                     (+ value (label-struct-type-parent-fields-nbr struct-type-label)))
                               #t)
                             (begin
                               (set-error-for-label
                                sba-state
                                inflowing-label
                                'red
                                (format "make-struct-field-accessor: slot index for ~a not in [0, ~a]: ~a"
                                        (pp-type sba-state (get-type-from-label sba-state struct-type-label) 'create-make-struct-field-accessor-label2)
                                        (- (label-struct-type-total-fields-nbr struct-type-label)
                                           (label-struct-type-parent-fields-nbr struct-type-label))
                                        (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-make-struct-field-accessor-label3)))
                               #f)))
                       (begin
                         (set! struct-type-label #f)
                         (set! access #f)
                         (set-error-for-label
                          sba-state
                          inflowing-label
                          'red
                          (format "make-struct-field-accessor: expects type <non-negative exact integer> as 2nd argument, given: ~a"
                                  (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-make-struct-field-accessor-label4)))
                         #f))
                   #f))
             (gensym))]
           [third-arg-label (create-simple-prim-label term)]
           [third-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only a symbol should flow in here
               (if field-index ; is not set if struct-label is not set...
                   (if (and (label-cst? inflowing-label)
                            (symbol? (label-cst-value inflowing-label)))
                       ; ready to wrap access... accessor is the result of applying
                       ; make-struct-field-accessor to access (i.e. it's the accessor
                       ; that will be bound to foo-a...)
                       (let* ([access-args (car (label-case-lambda-argss access))]
                              [access-body-edge (create-simple-edge (car (label-case-lambda-exps access)))]
                              [accessor-body-label (create-simple-prim-label term)]
                              [accessor-body-edge (create-simple-edge accessor-body-label)]
                              [accessor-arg-label (create-simple-prim-label term)]
                              [accessor-arg-edge
                               (cons
                                (lambda (out-label inflowing-label tunnel-label)
                                  ; name sink => no use for out-label
                                  (if (is-subtype? inflowing-label struct-type-label)
                                      (let ([result-label
                                             (list-ref (label-struct-value-fields
                                                        inflowing-label)
                                                       field-index)])
                                        ; we make the result flow into both the result of access and the result
                                        ; of the accessor
                                        (add-edge-and-propagate-set-through-edge
                                         result-label access-body-edge)
                                        (add-edge-and-propagate-set-through-edge
                                         result-label accessor-body-edge)
                                        #t)
                                      (begin
                                        (set-error-for-label
                                         sba-state
                                         ; we know we are inside a primitive, so we
                                         ; flag the entrance of the tunnel as the error.
                                         tunnel-label
                                         'red
                                         (format "accessor expects type ~a as 1st argument, given: ~a"
                                                 (pp-type sba-state (get-type-from-label sba-state struct-type-label) 'create-make-struct-field-accessor-label5)
                                                 (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-make-struct-field-accessor-label6)))
                                        #f)))
                                (gensym))]
                              [accessor-case-lambda-label (make-label-case-lambda
                                                           #f #f #f #f #t term (make-hash-table) (make-hash-table) #f
                                                           (list #f)
                                                           (list 1)
                                                           (list (list accessor-arg-label))
                                                           (list accessor-body-label)
                                                           (list cst:dummy-thunk))])
                         ; this is just to get the type for access right...
                         ; the structure flowing into the accessor flows into access's first arg
                         (add-edge-and-propagate-set-through-edge
                          accessor-arg-label
                          (create-simple-edge (car access-args)))
                         ; the index given to make-struct-field-accessor flows into the second arg
                         (add-edge-and-propagate-set-through-edge
                          second-arg-label
                          (create-simple-edge (cadr access-args)))
                         ; accessor
                         (add-edge-and-propagate-set-through-edge accessor-arg-label accessor-arg-edge)
                         (initialize-label-set-for-value-source accessor-case-lambda-label)
                         (add-edge-and-propagate-set-through-edge
                          accessor-case-lambda-label body-edge)
                         #t)
                       (begin
                         (set! struct-type-label #f)
                         (set! access #f)
                         (set! field-index #f)
                         (set-error-for-label
                          sba-state
                          inflowing-label
                          'red
                          (format "make-struct-field-accessor: expects type <symbol> as 3rd argument, given: ~a"
                                  (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-make-struct-field-accessor-label7)))
                         #f))
                   #f))
             (gensym))]
           [make-struct-field-accessor-label (make-label-case-lambda
                                              #f #f #f #f #t term (make-hash-table) (make-hash-table) #f
                                              (list #f)
                                              (list 3)
                                              (list (list first-arg-label
                                                          second-arg-label
                                                          third-arg-label))
                                              (list body-label)
                                              (list cst:dummy-thunk))])
      (add-edge-and-propagate-set-through-edge first-arg-label first-arg-edge)
      (add-edge-and-propagate-set-through-edge second-arg-label second-arg-edge)
      (add-edge-and-propagate-set-through-edge third-arg-label third-arg-edge)
      (initialize-label-set-for-value-source make-struct-field-accessor-label)
      make-struct-field-accessor-label))
  
  ; sba-state syntax-object -> label
  ; Here again we rely heavily on the order in which actual arguments are connected
  ; to formal arguments (i.e. left to right)
  (define (create-make-struct-field-mutator-label sba-state term)
    (let* (; WARNING: we assume that each occurence of make-struct-field-mutator is
           ; only used once in the program being analyzed, so set!-ing struct-label
           ; and field-index is ok. This *will* break if the user starts using a
           ; function like:
           ; (lambda (index name)
           ;   (make-struct-field-mutator mutate index name))
           ; to create the different mutators...
           ; Note that we could get back the struct-label and field-index when needed
           ; by fishing them out of the sets of the first and second args, but it's
           ; less painful to do it that way, and doing the fishing would still break
           ; the same way anyway...
           [struct-type-label #f]
           [mutate #f]
           [field-index #f]
           [body-label (create-simple-prim-label term)]
           [body-edge (create-simple-edge body-label)]
           [first-arg-label (create-simple-prim-label term)]
           [first-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only a case-lambda for a struct with a single arity-3 clause
               ; should flow in here. Note that, as in create-make-struct-type-label,
               ; we do the type checking as stuff flows in, instead of doing it
               ; post-analysis, just to make sure we don't screw our invariants when
               ; we finally run third-arg-edge...
               (if (and (label-case-lambda? inflowing-label)
                        (label-case-lambda-struct inflowing-label)
                        (= (length (label-case-lambda-rest-arg?s inflowing-label)) 1)
                        (= (length (car (label-case-lambda-argss inflowing-label))) 3))
                   (begin
                     (set! struct-type-label (label-case-lambda-struct inflowing-label))
                     (set! mutate inflowing-label)
                     #t)
                   (begin
                     (set-error-for-label
                      sba-state
                      inflowing-label
                      'red
                      (format "make-struct-field-mutator: expects type <mutator procedure that requires a field index> as 1st argument, given: ~a"
                              (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-make-struct-field-mutator-label1)))
                     #f)))
             (gensym))]
           [second-arg-label (create-simple-prim-label term)]
           [second-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only a number in the right range should flow in here
               (if struct-type-label
                   (if (and (label-cst? inflowing-label)
                            (let ([value (label-cst-value inflowing-label)])
                              (and (number? value)
                                   (exact? value)
                                   (<= 0 value))))
                       (let ([value (label-cst-value inflowing-label)])
                         (if (< value (- (label-struct-type-total-fields-nbr struct-type-label)
                                         (label-struct-type-parent-fields-nbr struct-type-label)))
                             (begin
                               (set! field-index 
                                     (+ value (label-struct-type-parent-fields-nbr struct-type-label)))
                               #t)
                             (begin
                               (set-error-for-label
                                sba-state
                                inflowing-label
                                'red
                                (format "make-struct-field-mutator: slot index for ~a not in [0, ~a]: ~a"
                                        (pp-type sba-state (get-type-from-label sba-state (struct-type-label)) 'create-make-struct-field-mutator-label2)
                                        (- (label-struct-type-total-fields-nbr struct-type-label)
                                           (label-struct-type-parent-fields-nbr struct-type-label))
                                        (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-make-struct-field-mutator-label3)))
                               #f)))
                       (begin
                         (set! struct-type-label #f)
                         (set! mutate #f)
                         (set-error-for-label
                          sba-state
                          inflowing-label
                          'red
                          (format "make-struct-field-mutator: expects type <non-negative exact integer> as 2nd argument, given: ~a"
                                  (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-make-struct-field-mutator-label4)))
                         #f))
                   #f))
             (gensym))]
           [third-arg-label (create-simple-prim-label term)]
           [third-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               ; only a symbol should flow in here
               (if field-index ; is not set if struct-label is not set...
                   (if (and (label-cst? inflowing-label)
                            (symbol? (label-cst-value inflowing-label)))
                       ; ready to wrap mutate... mutator is the result of applying
                       ; make-struct-field-mutator to mutate (i.e. it's the mutator
                       ; that will be bound to set-foo-a!...)
                       (let* ([mutate-args (car (label-case-lambda-argss mutate))]
                              [mutate-body-edge (create-simple-edge (car (label-case-lambda-exps mutate)))]
                              [mutator-case-lambda-label
                               (create-2args-mutator
                                sba-state
                                (lambda (inflowing-label)
                                  (is-subtype? inflowing-label struct-type-label))
                                cst:test-true
                                (lambda (inflowing-label)
                                  (list-ref (label-struct-value-fields
                                             inflowing-label)
                                            field-index))
                                cst:id
                                (pp-type sba-state (get-type-from-label sba-state struct-type-label) 'create-make-struct-field-mutator-label5)
                                "internal error 5: all types must be a subtype of top"
                                term)]
                              ; a mutator has only one clause
                              [mutator-args (car (label-case-lambda-argss mutator-case-lambda-label))])
                         ; this is just to get the type for mutate right...
                         ; the structure flowing into the mutator's first arg flows into
                         ; mutate's first arg
                         (add-edge-and-propagate-set-through-edge
                          (car mutator-args)
                          (create-simple-edge (car mutate-args)))
                         ; the index given to make-struct-field-mutator flows into mutate's second arg
                         (add-edge-and-propagate-set-through-edge
                          second-arg-label
                          (create-simple-edge (cadr mutate-args)))
                         ; the value flowing into the mutator's second args flows into
                         ; mutate's third arg
                         (add-edge-and-propagate-set-through-edge
                          (cadr mutator-args)
                          (create-simple-edge (caddr mutate-args)))
                         ; body
                         (add-edge-and-propagate-set-through-edge
                          (car (label-case-lambda-exps mutator-case-lambda-label))
                          mutate-body-edge)
                         ; mutator
                         (add-edge-and-propagate-set-through-edge
                          mutator-case-lambda-label body-edge)
                         #t)
                       (begin
                         (set! struct-type-label #f)
                         (set! mutate #f)
                         (set! field-index #f)
                         (set-error-for-label
                          sba-state
                          inflowing-label
                          'red
                          (format "make-struct-field-mutator: expects type <symbol> as 3rd argument, given: ~a"
                                  (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-make-struct-field-mutator-label7)))
                         #f))
                   #f))
             (gensym))]
           [make-struct-field-mutator-label (make-label-case-lambda
                                             #f #f #f #f #t term (make-hash-table) (make-hash-table) #f
                                             (list #f)
                                             (list 3)
                                             (list (list first-arg-label
                                                         second-arg-label
                                                         third-arg-label))
                                             (list body-label)
                                             (list cst:dummy-thunk))])
      (add-edge-and-propagate-set-through-edge first-arg-label first-arg-edge)
      (add-edge-and-propagate-set-through-edge second-arg-label second-arg-edge)
      (add-edge-and-propagate-set-through-edge third-arg-label third-arg-edge)
      (initialize-label-set-for-value-source make-struct-field-mutator-label)
      make-struct-field-mutator-label))
  
  ; sba-state (label -> boolean) (label -> boolean) (label -> label) (label -> label)  string string
  ; -> case-lambda-label
  ; creates a case-lambda label for a 2 args mutator.
  (define (create-2args-mutator sba-state
                                pred-first-arg pred-second-arg
                                accessor-first-arg accessor-second-arg
                                error-first-arg error-second-arg
                                term)
    (let* ([void-label (make-label-cst #f #f #f #f #f
                                       term
                                       (make-hash-table)
                                       (make-hash-table)
                                       cst:void)]
           [state-label (create-simple-prim-label term)]
           [state-edge (create-simple-edge state-label)]
           [mutator-body-label (create-simple-prim-label term)]
           [mutator-body-edge (create-simple-edge mutator-body-label)]
           [mutator-first-arg-label (create-simple-prim-label term)]
           [mutator-second-arg-label (create-simple-prim-label term)]
           [mutator-first-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               (if (pred-first-arg inflowing-label)
                   (add-edge-and-propagate-set-through-edge
                    state-label
                    (create-simple-edge (accessor-first-arg inflowing-label)))
                   (begin
                     (set-error-for-label
                      sba-state
                      ; we know we are inside a primitive, so we
                      ; flag the entrance of the tunnel as the error.
                      tunnel-label
                      'red
                      (format "mutator expects type ~a as 1st argument, given: ~a"
                              error-first-arg
                              (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-2args-mutator1)))
                     #f)))
             (gensym))]
           [mutator-second-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               (if (pred-second-arg inflowing-label)
                   (add-edge-and-propagate-set-through-edge
                    (accessor-second-arg inflowing-label)
                    state-edge)
                   (begin
                     (set-error-for-label
                      sba-state
                      ; we know we are inside a primitive, so we
                      ; flag the entrance of the tunnel as the error.
                      tunnel-label
                      'red
                      (format "mutator expects type ~a as 2nd argument, given: ~a"
                              error-second-arg
                              (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-2args-mutator2)))
                     #f)))
             (gensym))]
           [mutator-case-lambda-label (make-label-case-lambda
                                       #f #f #f #f #t term (make-hash-table) (make-hash-table) #f
                                       (list #f)
                                       (list 2)
                                       (list (list mutator-first-arg-label
                                                   mutator-second-arg-label))
                                       (list mutator-body-label)
                                       (list cst:dummy-thunk))])
      (initialize-label-set-for-value-source void-label)
      (add-edge-and-propagate-set-through-edge void-label mutator-body-edge)
      (add-edge-and-propagate-set-through-edge
       mutator-first-arg-label mutator-first-arg-edge)
      (add-edge-and-propagate-set-through-edge
       mutator-second-arg-label mutator-second-arg-edge)
      (initialize-label-set-for-value-source mutator-case-lambda-label)
      mutator-case-lambda-label))
  
  ; sba-state (label -> boolean) (label -> boolean) (label -> label) (label -> label)  string string string
  ; -> case-lambda-label
  ; creates a case-lambda label for a 3 args mutator.
  (define (create-3args-mutator sba-state
                                pred-first-arg pred-second-arg pred-third-arg
                                accessor-first-arg accessor-third-arg
                                error-first-arg error-second-arg error-third-arg
                                term)
    (let* ([void-label (make-label-cst #f #f #f #f #f
                                       term
                                       (make-hash-table)
                                       (make-hash-table)
                                       cst:void)]
           [state-label (create-simple-prim-label term)]
           [state-edge (create-simple-edge state-label)]
           [mutator-body-label (create-simple-prim-label term)]
           [mutator-body-edge (create-simple-edge mutator-body-label)]
           [mutator-first-arg-label (create-simple-prim-label term)]
           [mutator-second-arg-label (create-simple-prim-label term)]
           [mutator-third-arg-label (create-simple-prim-label term)]
           [mutator-first-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               (if (pred-first-arg inflowing-label)
                   (add-edge-and-propagate-set-through-edge
                    state-label
                    (create-simple-edge (accessor-first-arg inflowing-label)))
                   (begin
                     (set-error-for-label
                      sba-state
                      ; we know we are inside a primitive, so we
                      ; flag the entrance of the tunnel as the error.
                      tunnel-label
                      'red
                      (format "mutator expects type ~a as 1st argument, given: ~a"
                              error-first-arg
                              (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-3args-mutator1)))
                     #f)))
             (gensym))]
           [mutator-second-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               (if (pred-second-arg inflowing-label)
                   #t
                   (begin
                     (set-error-for-label
                      sba-state
                      ; we know we are inside a primitive, so we
                      ; flag the entrance of the tunnel as the error.
                      tunnel-label
                      'red
                      (format "mutator expects type ~a as 2nd argument, given: ~a"
                              error-second-arg
                              (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-3args-mutator2)))
                     #f)))
             (gensym))]
           [mutator-third-arg-edge
            (cons
             (lambda (out-label inflowing-label tunnel-label)
               ; name sink => no use for out-label
               (if (pred-third-arg inflowing-label)
                   (add-edge-and-propagate-set-through-edge
                    (accessor-third-arg inflowing-label)
                    state-edge)
                   (begin
                     (set-error-for-label
                      sba-state
                      ; we know we are inside a primitive, so we
                      ; flag the entrance of the tunnel as the error.
                      tunnel-label
                      'red
                      (format "mutator expects type ~a as 3rd argument, given: ~a"
                              error-third-arg
                              (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'create-3args-mutator3)))
                     #f)))
             (gensym))]
           [mutator-case-lambda-label (make-label-case-lambda
                                       #f #f #f #f #t term (make-hash-table) (make-hash-table) #f
                                       (list #f)
                                       (list 3)
                                       (list (list mutator-first-arg-label
                                                   mutator-second-arg-label
                                                   mutator-third-arg-label))
                                       (list mutator-body-label)
                                       (list cst:dummy-thunk))])
      (initialize-label-set-for-value-source void-label)
      (add-edge-and-propagate-set-through-edge void-label mutator-body-edge)
      (add-edge-and-propagate-set-through-edge
       mutator-first-arg-label mutator-first-arg-edge)
      (add-edge-and-propagate-set-through-edge
       mutator-second-arg-label mutator-second-arg-edge)
      (add-edge-and-propagate-set-through-edge
       mutator-third-arg-label mutator-third-arg-edge)
      (initialize-label-set-for-value-source mutator-case-lambda-label)
      mutator-case-lambda-label))
  
  ; sba-state (listof (syntax-object-listof syntax-object)) (listof (syntax-object-listof syntax-object))
  ; syntax-object (listof (cons symbol label)) -> case-lambda-label
  (define (create-case-lambda-label sba-state argss expss term gamma)
    (let* ([label (make-label-case-lambda
                   #f #f #f #f #f
                   term
                   (make-hash-table)
                   (make-hash-table)
                   #f
                   cst:dummy
                   cst:dummy
                   cst:dummy
                   cst:dummy
                   '())]
           [all-labels
            (list:foldr
             (lambda (args exps other-clauses-labels)
               (let ([rest-arg?s (vector-ref other-clauses-labels 0)]
                     [req-args (vector-ref other-clauses-labels 1)]
                     [argss-labels (vector-ref other-clauses-labels 2)]
                     [exps-labels (vector-ref other-clauses-labels 3)]
                     ; scheme list of syntax objects for body exps
                     [exps (syntax-e exps)])
                 ; we add one new element to each list each time we process a new clause,
                 ; so that the element for the current clause is always at the start of the
                 ; list, so we know where to find this element when we need it (we need to
                 ; update the top free vars for the current clause in the #%top case, and
                 ; the application thunk for the current clause in the #%app case).
                 (set-label-case-lambda-effects!
                  label
                  (cons cst:dummy-thunk (label-case-lambda-effects label)))
                 (kern:kernel-syntax-case
                  args #f
                  [(args ...)
                   (let* (; proper scheme list of syntax objects for arguments
                          [args (syntax-e (syntax (args ...)))]
                          [args-labels (map (lambda (term) (create-simple-label sba-state term)) args)]
                          [gamma-extended (extend-env gamma args args-labels)])
                     (vector (cons #f rest-arg?s)
                             (cons (length args) req-args)
                             (cons args-labels argss-labels)
                             (cons (list:foldl
                                    (lambda (exp _)
                                      (create-label-from-term sba-state exp gamma-extended label))
                                    cst:dummy
                                    exps)
                                   exps-labels)))]
                  [(first-arg . other-args-including-rest-arg)
                   (let* (; (syntax other-args-including-rest-arg) is either a (syntax
                          ; version of a) list of syntax objects (if there's strictly more
                          ; than one required argument), or a single syntax object (if
                          ; there's only one required argument). In both cases we want to
                          ; construct an improper list of syntax objects. syntax-e takes
                          ; care of that in the list case, cons takes care of that in the
                          ; other case.
                          [args (cons (syntax first-arg)
                                      (let* ([syntax-obj (syntax other-args-including-rest-arg)]
                                             [symbol-or-list-of-syntax-obj (syntax-e syntax-obj)])
                                        (if (symbol? symbol-or-list-of-syntax-obj)
                                            syntax-obj
                                            symbol-or-list-of-syntax-obj)))]
                          ; convert the improper list into a proper one.
                          [args (let loop ([args args])
                                  (if (pair? args)
                                      (cons (car args)
                                            (loop (cdr args)))
                                      (list args)))]
                          [args-labels (map (lambda (term) (create-simple-label sba-state term)) args)]
                          [gamma-extended (extend-env gamma args args-labels)])
                     (vector (cons #t rest-arg?s)
                             (cons (sub1 (length args)) req-args)
                             (cons args-labels argss-labels)
                             (cons (list:foldl
                                    (lambda (exp _)
                                      (create-label-from-term sba-state exp gamma-extended label))
                                    cst:dummy
                                    exps)
                                   exps-labels)))]
                  [rest-arg
                   (let* (; one syntax object for rest-arg
                          [rest-arg (syntax rest-arg)]
                          [rest-arg-label-list (list (create-simple-label sba-state rest-arg))]
                          [gamma-extended (extend-env gamma (list rest-arg) rest-arg-label-list)])
                     (vector (cons #t rest-arg?s)
                             (cons 0 req-args)
                             (cons rest-arg-label-list argss-labels)
                             (cons (list:foldl
                                    (lambda (exp _)
                                      (create-label-from-term sba-state exp gamma-extended label))
                                    cst:dummy
                                    exps)
                                   exps-labels)))]
                  )))
             (vector '()'()'()'())
             argss
             expss
             )])
      (set-label-case-lambda-rest-arg?s! label (vector-ref all-labels 0))
      (set-label-case-lambda-req-args! label (vector-ref all-labels 1))
      (set-label-case-lambda-argss! label (vector-ref all-labels 2))
      (set-label-case-lambda-exps! label (vector-ref all-labels 3))
      (initialize-label-set-for-value-source label)
      ((sba-state-register-label-with-gui sba-state) label)
      label))
  
  ; sba-state syntax-object (listof (cons symbol label)) label (listof label) -> label
  (define (create-top-level-label sba-state identifier gamma enclosing-lambda-label)
    (let* ([identifier-name (syntax-e identifier)]
           ; note that bound-label doesn't contain the #%top, but they have the same
           ; syntax source/line/column/position, so arrows and underlining will work
           ; the same, but it will make things a little bit simpler when doing a
           ; lookup-top-level-name in the #%app case (if we have to).
           [bound-label (create-simple-label sba-state identifier)])
      (if enclosing-lambda-label
          ; free var inside a lambda, so add it to the list of free variables, don't do
          ; any lookup now (will be done when the enclosing lambda is applied)
          (let* ([enclosing-lambda-effects (label-case-lambda-effects enclosing-lambda-label)]
                 [current-thunk (car enclosing-lambda-effects)])
            (set-car! enclosing-lambda-effects
                      (lambda ()
                        (current-thunk)
                        (lookup-and-bind-top-level-vars sba-state (list bound-label) identifier)
                        )))
          ; top level
          (lookup-and-bind-top-level-vars sba-state (list bound-label) identifier))
      bound-label))
  
  
  ; (label -> void) syntax-object (assoc-setof location-info label) -> label
  ; We must take sharing into account.  We can't count on using syntax-e and eq?
  ; because they don't preserve sharing (see the MzScheme manual) and using
  ; syntax-object->datum and eq? might mistakenly result in too much sharing, since
  ; some values like intergers, symbols, and '() are always eq?.  So we have to rely
  ; on source locations and so on.  And the reason we must take sharing into account
  ; is because otherwise things like '#0=(1 . #0#) will make this code fail to
  ; terminate.  Try the foolowing code in DrScheme to see why syntax-e and
  ; syntax-object->datum are not what we want:
  ;  (define-syntax lst
  ;    (syntax-rules ()
  ;      [(_ a b) #'(a a b)]))
  ;  (lst 1 1)
  ;  (define w1 #`#,(lst 1 1))
  ;  w1
  ;  (define w2 (syntax-e w1))
  ;  w2
  ;  (define w3 w2)
  ;  w3
  ;  (eq? (car w3) (cadr w3))
  ;  (eq? (car w3) (caddr w3))
  ;  (define w4 (syntax-object->datum w1))
  ;  w4
  ;  (eq? (car w4) (cadr w4))
  ;  (eq? (car w4) (caddr w4))
  ;
  ;  '(1 1 1)
  ;  (define x1 #''(1 1 1))
  ;  x1
  ;  (define x2 (syntax-e x1))
  ;  x2
  ;  (define x3 (syntax-e (cadr x2)))
  ;  x3
  ;  (eq? (car x3) (cadr x3))
  ;  (eq? (car x3) (caddr x3))
  ;  (define x4 (syntax-object->datum (cadr x2)))
  ;  x4
  ;  (eq? (car x4) (cadr x4))
  ;  (eq? (car x4) (caddr x4))
  ;  
  ;  '(#0=1 #0# 1)
  ;  (define y1 #''(#0=1 #0# 1))
  ;  y1
  ;  (define y2 (syntax-e y1))
  ;  y2
  ;  (define y3 (syntax-e (cadr y2)))
  ;  y3
  ;  (eq? (car y3) (cadr y3))
  ;  (eq? (car y3) (caddr y3))
  ;  (define y4 (syntax-object->datum (cadr y2)))
  ;  y4
  ;  (eq? (car y4) (cadr y4))
  ;  (eq? (car y4) (caddr y4))
  ;
  ;  '(#0=(1) #0# (1))
  ;  (define z1 #''(#0=(1) #0# (1)))
  ;  z1
  ;  (define z2 (syntax-e z1))
  ;  z2
  ;  (define z3 (syntax-e (cadr z2)))
  ;  z3
  ;  (eq? (car z3) (cadr z3))
  ;  (eq? (car z3) (caddr z3))
  ;  (define z4 (syntax-object->datum (cadr z2)))
  ;  z4
  ;  (eq? (car z4) (cadr z4))
  ;  (eq? (car z4) (caddr z4))
  ;
  (define (create-label-from-quote register-label-with-gui term-stx assoc-set)
    (let ([term-loc-info (list (syntax-source term-stx)
                               (syntax-position term-stx)
                               (syntax-span term-stx))])
      ;(printf "Q: ~a ~a ~a ~a~n" (syntax-object->datum term-stx) (syntax-e term-stx) term-stx (assoc-set-in? assoc-set term-loc-info))
      ;(printf "L: ~a~n" term-loc-info)
      (if (assoc-set-in? assoc-set term-loc-info)
          (assoc-set-get assoc-set term-loc-info)
          (let ([sexp-e (syntax-e term-stx)])
            (cond
              [(list? sexp-e)
               (let loop ([sexp-e sexp-e]
                          [top-label? #t])
                 (if (null? sexp-e)
                     (let ([null-label
                            (make-label-cst
                             #f #f #f #f #t
                             term-stx
                             (make-hash-table)
                             (make-hash-table)
                             sexp-e)])
                       (initialize-label-set-for-value-source null-label)
                       (register-label-with-gui null-label)
                       null-label)
                     (let ([cons-label
                            (make-label-cons
                             #f #f #f #f (not top-label?)
                             term-stx
                             (make-hash-table)
                             (make-hash-table)
                             #f
                             #f)])
                       ; the top-most cons-label in the list is the only one in
                       ; the list that might be associated with a #n name and
                       ; therefore the only one that might have a #n# sharing
                       ; reference somewhere else, so we need to remember it so
                       ; sharing is dealt with correctly.  We need to memoize it
                       ; before any recursive call so that we close the loop
                       ; correctly.
                       (when top-label?
                         (assoc-set-set assoc-set term-loc-info cons-label)
                         (register-label-with-gui cons-label))
                       (set-label-cons-car!
                        cons-label
                        (create-label-from-quote register-label-with-gui
                                                 (car sexp-e) assoc-set))
                       (set-label-cons-cdr!
                        cons-label
                        (loop (cdr sexp-e) #f))
                       (initialize-label-set-for-value-source cons-label)
                       cons-label)))]
              [(pair? sexp-e)
               (let ([cons-label
                      (make-label-cons
                       #f #f #f #f #f
                       term-stx
                       (make-hash-table)
                       (make-hash-table)
                       #f
                       #f)])
                 (assoc-set-set assoc-set term-loc-info cons-label)
                 (register-label-with-gui cons-label)
                 (set-label-cons-car!
                  cons-label
                  (create-label-from-quote register-label-with-gui
                                           (car sexp-e) assoc-set))
                 (set-label-cons-cdr!
                  cons-label
                  (create-label-from-quote register-label-with-gui
                                           (cdr sexp-e) assoc-set))
                 (initialize-label-set-for-value-source cons-label)
                 cons-label)]
              [else (let ([label (make-label-cst
                                  #f #f #f #f #f
                                  term-stx
                                  (make-hash-table)
                                  (make-hash-table)
                                  sexp-e)])
                      (assoc-set-set assoc-set term-loc-info label)
                      (initialize-label-set-for-value-source label)
                      (register-label-with-gui label)
                      label)])))))
  
  ; Builds a list of labels of length n, with all labels being the same.
  ; This function should be seldom called, so it's not being made tail recursive...
  (define (build-label-list label n)
    (if (<= n 0)
        '()
        (cons label (build-label-list label (sub1 n)))))
  
  ; given a label representing multiple values, connect the label for the different
  ; values to the different variables.  The tricky part is that the multiple values
  ; are potentially infinite, because of approximations.  E.g.
  ; (let-values ([(a b) (apply values (list 1 2))]) b)
  ; Because of the "apply", we can't actually determine how many multiple values
  ; we receive, so we have to try our best.
  ; sba-state label (listof label) integer symbol -> void
  (define (connect-value-labels-to-var-labels sba-state inflowing-label vars-labels vars-length term-name)
    (let ([values-labels
           (let* ([value-label-list (hash-table-map (label-set (label-values-label inflowing-label))
                                                    (lambda (label arrows) label))]
                  [value-label-list-length (length value-label-list)])
             (cond
               [(= value-label-list-length 1) (label-list->list sba-state (car value-label-list))]
               ; check for infinite list.  If we have an infinite list, then it's something
               ; like x = (union null (cons y x)) or x = (union (cons y x) null).  In either
               ; case (this case and the one below), we find y and create a list (list y y ...)
               ; with the right length so y (most likely a union of all the possible multiple
               ; values that flowed together when we lost track of the exact length of the
               ; list of multiple values) will flow in all vars-labels (therefore being *very*
               ; conservative since all possible values will flow into all possible bindings).
               ; Note that we don't actually check that the list is infinite (i.e. that the
               ; cons labels form a loop).  We could check that the cdr of the cons is eq? to
               ; (label-values-label inflowing-label) for example, but that doesn't always
               ; work because of loop unfolding (we does occur in practice).  So we just check
               ; that we have something vaguely resembling a loop at the outermost level and
               ; then we trust that the rest of the analysis and the primitive type
               ; descriptions are correct enough that we never end up here with something that
               ; resembles a list without being one.  In fact if the analysis is correct we
               ; should only ever see finite lists and infinite lists and nothing else, so
               ; since the first case above takes care of finite lists we can normally
               ; safely assume that in the two cases below we are dealing with infinite lists,
               ; even though we have no simple way to check that.  The last case is for extra
               ; checking so that if something goes really wrong we might at least learn about it...
               ; One good question is: should we propagate at all when we don't know whether
               ; we have an error or not?
               ; Note that we also assume that the car we get from the infinite list is all
               ; the possible cars we'll ever get, even in the presence of other cars in the
               ; infinite list that might come from loop unrolling!
               [(and (= value-label-list-length 2)
                     (let ([first-value-label (car value-label-list)]
                           [second-value-label (cadr value-label-list)])
                       (and (label-cst? first-value-label)
                            (null? (label-cst-value first-value-label))
                            (label-cons? second-value-label))))
                (set-error-for-label
                 sba-state
                 inflowing-label
                 'red
                 (format "~a: context expected ~a values, can't determine how many received"
                         term-name
                         vars-length))
                (build-label-list (label-cons-car (cadr value-label-list)) vars-length)]
               [(and (= value-label-list-length 2)
                     (let ([first-value-label (car value-label-list)]
                           [second-value-label (cadr value-label-list)])
                       (and (label-cst? second-value-label)
                            (null? (label-cst-value second-value-label))
                            (label-cons? first-value-label))))
                (set-error-for-label
                 sba-state
                 inflowing-label
                 'red
                 (format "~a: context expected ~a values, can't determine how many received"
                         term-name
                         vars-length))
                (build-label-list (label-cons-car (car value-label-list)) vars-length)]
               [else (error term-name "values didn't contain list: ~a"
                            (pp-type sba-state (get-type-from-label sba-state inflowing-label) 'let-values)
                            ;(map (lambda (label)
                            ;       (pp-type sba-state (get-type-from-label sba-state label)
                            ;                term-name))
                            ;     label-list)
                            )]))])
      (if (= (length values-labels) vars-length)
          ; we have something like
          ; (let-values ([(x y) (... (values a b) ...)]...) ...),
          ; so we add a new direct edge from a to x and b to y.
          ; Of course these new edges have to be themselves
          ; recursive unpacking edges, since some (values c)
          ; could later flow into either a or b.
          (ormap2-strict
           (lambda (new-origin-label var-label)
             (add-edge-and-propagate-set-through-edge
              new-origin-label
              (extend-edge-for-values
               sba-state
               (create-simple-edge var-label)))
             #t)
           values-labels vars-labels)
          ; (let-values ([(x y) (... (values a b c ...) ...)]
          ;             ...) ...)
          (begin
            (set-error-for-label
             sba-state
             inflowing-label
             'red
             (format "~a: context expected ~a values, received ~a values"
                     term-name
                     vars-length
                     (length values-labels)))
            #f))))
  
  ; sba-state syntax-object (listof (cons symbol label)) label -> label
  ; gamma is the binding-variable-name-to-label environment
  ; enclosing-lambda-label is the label for the enclosing lambda, if any. We
  ; need it to update its list of free variables if we find any. This means
  ; we have to create the label for a lambda before analyzing its body...
  (define (create-label-from-term sba-state term gamma enclosing-lambda-label)
    (kern:kernel-syntax-case
     term #f
     ; lambda and case-lambda are currently both core forms. This might change (dixit Matthew)
     [(lambda args exps ...)
      (let (; scheme lists of syntax object lists of syntax objects
            [argss (list (syntax args))]
            [expss (list (syntax (exps ...)))])
        (create-case-lambda-label sba-state argss expss term gamma))]
     [(case-lambda . ((args exps ...) ...))
      (let (; scheme lists of syntax object lists of syntax objects
            [argss (syntax-e (syntax (args ...)))]
            [expss (syntax-e (syntax ((exps ...) ...)))])
        (create-case-lambda-label sba-state argss expss term gamma))]
     [(#%app op actual-args ...)
      (let* ([app-label (create-simple-label sba-state term)]
             [op-term (syntax op)]
             [op-label (create-label-from-term sba-state op-term gamma enclosing-lambda-label)]
             [stx-actual-args (syntax (actual-args ...))]
             [actual-args-labels
              (map (lambda (actual-arg)
                     (create-label-from-term sba-state actual-arg gamma enclosing-lambda-label))
                   (syntax-e stx-actual-args))]
             [actual-args-length (length actual-args-labels)]
             [edge (create-case-lambda-edge
                    sba-state
                    (list #f)
                    (list actual-args-length)
                    (list actual-args-labels)
                    (list app-label)
                    op-label
                    #f)])
        ; If the app is inside a lambda, we delay the addition of the edge until the enclosing
        ; lambda is itself applied.
        (if enclosing-lambda-label
            (let* ([enclosing-lambda-effects (label-case-lambda-effects enclosing-lambda-label)]
                   ; has to be evaluated now, not inside the thunk, otherwise we might have an
                   ; infinite loop (if there's only one clause in the lambda) or complete
                   ; non-sense (if there's several clauses).
                   [current-thunk (car enclosing-lambda-effects)])
              (set-car! enclosing-lambda-effects
                        (lambda ()
                          ; the order in which we evaluate the thunks here is normally
                          ; insignificant, but it is *very* important to have it in this
                          ; order when we start having structs. Otherwise, if a define-struct
                          ; is inside a lambda, the application of make-struct-type might
                          ; occur after the application of make-struct-field-accessor, which
                          ; means access won't have been created by the time
                          ; make-struct-field-accessor is applied, which will make the
                          ; assumption (that args flow into make-struct-field-accessor in order,
                          ; from left to right) we made in create-make-struct-field-accessor-label
                          ; break.
                          (current-thunk)
                          (add-edge-and-propagate-set-through-edge op-label edge)
                          )))
            (add-edge-and-propagate-set-through-edge op-label edge))
        app-label)]
     [(#%datum . datum)
      (let ([label (make-label-cst
                    #f #f #f #f #f
                    term
                    (make-hash-table)
                    (make-hash-table)
                    (syntax-object->datum (syntax datum)))])
        (initialize-label-set-for-value-source label)
        ((sba-state-register-label-with-gui sba-state) label)
        label)]
     [(quote sexp)
      (create-label-from-quote (sba-state-register-label-with-gui sba-state)
                               (syntax sexp) (assoc-set-make 'equal))]
     [(define-values vars exp)
      (let* (; scheme list of syntax objects
             [vars (syntax-e (syntax vars))]
             [vars-length (length vars)]
             [exp-label (create-label-from-term sba-state (syntax exp) gamma enclosing-lambda-label)]
             [vars-labels (map (lambda (term) (create-simple-label sba-state term)) vars)]
             [define-label (make-label-cst
                            #f #f #f #f #f
                            term
                            (make-hash-table)
                            (make-hash-table)
                            'dummy-define-values)])
        ; don't add to top level before analysing exp-label, otherwise (define x x) will work.
        (for-each (lambda (var var-label)
                    (add-top-level-name sba-state var var-label))
                  vars vars-labels)
        ; We must be able to take care of all the following different cases:
        ; (define-values (x) a)
        ; (define-values (x) (values a))
        ; (define-values (x) (values (values a)))
        ; (define-values (x) (values (values (values a))))
        ; ...
        ; (define-values (x y) (values a b))
        ; (define-values (x y) (values (values a) (values b)))
        ; (define-values (x y) (values (values (values a)) (values (values b))))
        ; ...
        ; with all the call to "values" being possibly inside functions...
        ; So we use extend-edge-for-values that recursively unpacks nested "values" by adding
        ; new unpacking edges on the fly when a label-values flows into a label that has an
        ; unpacking edge.
        ; Note that when define-values defines more than one variable, we must first unpack
        ; the top level of "values", then start the recursion for each variable separately.
        (if (= vars-length 1)
            ; we have something like (define-values (x) (values (values (values a)))) so we
            ; can directly start the recursion.
            (let ([var-label (car vars-labels)])
              (add-edge-and-propagate-set-through-edge
               exp-label
               (extend-edge-for-values sba-state (create-simple-edge var-label))))
            ; we have something like (define-values (x y) (values (values (values a))
            ; (values (values b)))) so we first have to manually unpack the top-most "values",
            ; then start a recursion for each of the defined variables. So in effect we end
            ; up doing something equivalent to analysing
            ; (define-values (x) (values (values a)))
            ; (define-values (y) (values (values b)))
            ; in parallel.
            (let ([distributive-unpacking-edge
                   (cons
                    (lambda (out-label inflowing-label tunnel-label)
                      ; inflowing-label (the label corresponding to the top "values") doesn't
                      ; flow anywhere, it's just taken apart and its elements are connected to
                      ; the different variables. I.e. it's a sink for multiple values. So we
                      ; have no need for out-label here.
                      (if (label-values? inflowing-label)
                          (connect-value-labels-to-var-labels sba-state inflowing-label vars-labels vars-length 'define-values)
                          ; (define-values (x y) (... 1 ...))
                          (begin
                            (set-error-for-label
                             sba-state
                             define-label
                             'red
                             (format "define-values: context expected ~a values, received 1 non-multiple-values value"
                                     vars-length))
                            #f)))
                    ; multiple values sink => unique, fake destination
                    (gensym))])
              (add-edge-and-propagate-set-through-edge
               exp-label
               distributive-unpacking-edge)))
        ;(initialize-label-set-for-value-source define-label)
        ((sba-state-register-label-with-gui sba-state) define-label)
        define-label)]
     [(let-values ((vars exp) ...) body-exps ...)
      (let* ([let-values-label (create-simple-label sba-state term)]
             [gamma-extended
              (list:foldl
               ; syntax-obj syntax-obj -> (listof (cons symbol label))
               ; loop on each binding clause of the let-values, returning the corresponding
               ; extended environment
               (lambda (vars exp new-gamma)
                 (let* (; scheme list of syntax objects
                        [vars (syntax-e vars)]
                        [vars-length (length vars)]
                        [vars-labels (map (lambda (term) (create-simple-label sba-state term)) vars)]
                        ; analyse exp of clause in gamma, not gamma-extended...
                        [exp-label (create-label-from-term sba-state exp gamma enclosing-lambda-label)])
                   ; We must be able to take care of all the following different cases:
                   ; (let-values ([(x) a] ...) ...)
                   ; (let-values ([(x) (values a)] ...) ...)
                   ; (let-values ([(x) (values (values a))] ...) ...)
                   ; (let-values ([(x) (values (values (values a)))] ...) ...)
                   ; ...
                   ; (let-values ([(x y) (values a b)] ...) ...)
                   ; (let-values ([(x y) (values (values a) (values b))] ...) ...)
                   ; (let-values ([(x y) (values (values (values a)) (values (values b)))] ...) ...)
                   ; ...
                   ; with all the call to "values" being possibly inside functions...
                   ; So we use extend-edge-for-values that recursively unpacks nested "values" by
                   ; adding new unpacking edges on the fly when a label-values flows into a label
                   ; that has an unpacking edge.
                   ; Note that when let-values defines more than one variable, we must first
                   ; unpack the top level of "values", then start the recursion for each
                   ; variable separately.
                   (if (= vars-length 1)
                       ; we have something like
                       ; (let-values ([(x) (values (values (values a)))]) ...) so we can
                       ; directly start the recursion.
                       (let ([var-label (car vars-labels)])
                         (add-edge-and-propagate-set-through-edge
                          exp-label
                          (extend-edge-for-values sba-state (create-simple-edge var-label))))
                       ; we have something like
                       ; (let-values ([(x y) (values (values (values a)) (values (values b)))] ...) ...)
                       ; so we first have to manually unpack the top-most "values", then start a
                       ; recursion for each of the defined variables. So in effect we end up
                       ; doing something equivalent to analysing
                       ; (let-values ([(x) (values (values a))]
                       ;               (y) (values (values b))] ...) ...)
                       ; in parallel.
                       (let ([distributive-unpacking-edge
                              (cons
                               (lambda (out-label inflowing-label tunnel-label)
                                 ; inflowing-label (the label corresponding to the top "values")
                                 ; doesn't flow anywhere, it's just taken apart and its elements
                                 ; are connected to the different variables. I.e. it's a sink for
                                 ; multiple values. So we have no need for out-label here.
                                 (if (label-values? inflowing-label)
                                     (connect-value-labels-to-var-labels sba-state inflowing-label vars-labels vars-length 'let-values)
                                     ; (let-values ([(x y) (... 1 ...)] ...) ...)
                                     (begin
                                       (set-error-for-label
                                        sba-state
                                        let-values-label
                                        'red
                                        (format "let-values: context expected ~a values, received 1 non-multiple-values value"
                                                vars-length))
                                       #f)))
                               ; multiple values sink
                               (gensym))])
                         (add-edge-and-propagate-set-through-edge
                          exp-label
                          distributive-unpacking-edge)))
                   (extend-env new-gamma vars vars-labels)))
               gamma
               ; Scheme lists of syntax objects, one for each list of vars and one for each exp
               (syntax-e (syntax (vars ...)))
               (syntax-e (syntax (exp ...))))]
             [last-body-exp-label
              (list:foldl
               (lambda (exp _)
                 (create-label-from-term sba-state exp gamma-extended enclosing-lambda-label))
               cst:dummy
               (syntax-e (syntax (body-exps ...))))])
        (add-edge-and-propagate-set-through-edge
         last-body-exp-label
         (create-simple-edge let-values-label))
        let-values-label)]
     [(letrec-values ((vars exp) ...) body-exps ...)
      ; we simulate letrec by doing a let followed by a set!, except that we have to do that
      ; clause after clause.
      (let* ([letrec-values-label (create-simple-label sba-state term)]
             [varss-stx (map syntax-e (syntax-e (syntax (vars ...))))]
             [varss-labelss (map (lambda (single-clause-vars-stx)
                                   (map (lambda (var-stx)
                                          (let ([undefined-label (make-label-cst #f #f #f #f #f
                                                                                 var-stx
                                                                                 (make-hash-table)
                                                                                 (make-hash-table)
                                                                                 cst:undefined)]
                                                ;[binding-label (create-simple-label sba-state var-stx)]
                                                )
                                            (initialize-label-set-for-value-source undefined-label)
                                            ;(add-edge-and-propagate-set-through-edge
                                            ; undefined-label
                                            ; (create-simple-edge binding-label))
                                            ;binding-label
                                            undefined-label))
                                        single-clause-vars-stx))
                                 varss-stx)]
             [gamma-extended (list:foldl
                              (lambda (vars-stx vars-labels current-gamma)
                                (extend-env current-gamma vars-stx vars-labels))
                              gamma
                              varss-stx
                              varss-labelss)]
             [_
              ; process the clauses expressions, creating new labels for the vars and set!-ing
              ; gamma-extended as we go along, since the current labels for var contain the
              ; undefined value. We need to do that before analyzing the body.
              (let loop ([varss-stx varss-stx]
                         [exps (syntax-e (syntax (exp ...)))])
                (unless (null? exps)
                  ; process current clause
                  (let* ([exp-label (create-label-from-term sba-state (car exps) gamma-extended enclosing-lambda-label)]
                         [vars-stx (car varss-stx)]
                         [vars-length (length vars-stx)])
                    (if (= vars-length 1)
                        ; we have a clause like [(x) (values (values (values a)))] so we
                        ; can directly start the recursion.
                        (let* ([var-stx (car vars-stx)]
                               [var-label (create-simple-label sba-state var-stx)]
                               [var-name (syntax-e var-stx)])
                          (add-edge-and-propagate-set-through-edge
                           exp-label
                           (extend-edge-for-values sba-state (create-simple-edge var-label)))
                          (search-and-replace gamma-extended var-name var-label))
                        ; we have a clause like [(x y) (values (values (values a)) (values (values b)))]
                        ; so we first have to manually unpack the top-most "values", then start a
                        ; recursion for each of the defined variables. So in effect we end up doing
                        ; something equivalent to analysing the clauses
                        ; [(x) (values (values a))]
                        ; [(y) (values (values b))]
                        ; in parallel.
                        (let ([distributive-unpacking-edge
                               (cons
                                (lambda (out-label inflowing-label tunnel-label)
                                  ; inflowing-label (the label corresponding to the top "values") doesn't
                                  ; flow anywhere, it's just taken apart and its elements are connected to
                                  ; the different variables. I.e. it's a sink for multiple values. So we
                                  ; have no need for out-label here.
                                  (if (label-values? inflowing-label)
                                      (let ([vars-labels (map (lambda (var-stx) (create-simple-label sba-state var-stx)) vars-stx)])
                                        (connect-value-labels-to-var-labels sba-state inflowing-label vars-labels vars-length 'let-values)
                                        (for-each (lambda (var-stx var-label)
                                                    (search-and-replace gamma-extended (syntax-e var-stx) var-label))
                                                  vars-stx vars-labels))
                                      ; [(x y) (... 1 ...))]
                                      (begin
                                        (set-error-for-label
                                         sba-state
                                         letrec-values-label
                                         'red
                                         (format "letrec-values: context expected ~a values, received 1 non-multiple-values value"
                                                 vars-length))
                                        #f)))
                                ; multiple values sink => unique, fake destination
                                (gensym))])
                          (add-edge-and-propagate-set-through-edge
                           exp-label
                           distributive-unpacking-edge))))
                  ; process remaining clauses
                  (loop (cdr varss-stx) (cdr exps))))]
             [last-body-exp-label
              (list:foldl
               (lambda (exp _)
                 (create-label-from-term sba-state exp gamma-extended enclosing-lambda-label))
               cst:dummy
               (syntax-e (syntax (body-exps ...))))])
        (add-edge-and-propagate-set-through-edge
         last-body-exp-label
         (create-simple-edge letrec-values-label))
        letrec-values-label)]
     [(if test then else)
      (let*-values
          ([(test) (syntax test)]
           [(test-label) (create-label-from-term sba-state test gamma enclosing-lambda-label)]
           [(then-label else-label)
            (if (symbol? (syntax-e test))
                (let* ([test-name (syntax-e test)]
                       [binding-label (lookup-env test gamma)]
                       [new-then-binding-label (create-simple-prim-label term)]
                       [new-else-binding-label (create-simple-prim-label term)]
                       [new-then-gamma (extend-env gamma (list test) (list new-then-binding-label))]
                       [new-else-gamma (extend-env gamma (list test) (list new-else-binding-label))]
                       [then-normal-edge (create-simple-edge new-then-binding-label)]
                       [else-normal-edge (create-simple-edge new-else-binding-label)]
                       ; discards #f, passes the rest to then-normal-edge
                       [then-filtering-edge
                        (cons
                         (lambda (out-label inflowing-label tunnel-label)
                           (when (or (not (label-cst? inflowing-label))
                                     (label-cst-value inflowing-label))
                             ((car then-normal-edge) out-label inflowing-label tunnel-label)))
                         (cdr then-normal-edge))]
                       ; discards everything but #f and passes it to else-normal-edge
                       [else-filtering-edge
                        (cons
                         (lambda (out-label inflowing-label tunnel-label)
                           (when (and (label-cst? inflowing-label)
                                      (not (label-cst-value inflowing-label)))
                             ((car else-normal-edge) out-label inflowing-label tunnel-label)))
                         (cdr else-normal-edge))])
                  (if binding-label
                      (begin
                        (add-edge-and-propagate-set-through-edge binding-label then-filtering-edge)
                        (add-edge-and-propagate-set-through-edge binding-label else-filtering-edge)
                        (values
                         (create-label-from-term sba-state (syntax then) new-then-gamma enclosing-lambda-label)
                         (create-label-from-term sba-state (syntax else) new-else-gamma enclosing-lambda-label)))
                      (values
                       (create-label-from-term sba-state (syntax then) gamma enclosing-lambda-label)
                       (create-label-from-term sba-state (syntax else) gamma enclosing-lambda-label))))
                (values
                 (create-label-from-term sba-state (syntax then) gamma enclosing-lambda-label)
                 (create-label-from-term sba-state (syntax else) gamma enclosing-lambda-label)))]
           ; because of the (if test then) case below, else-label might be associated with
           ; the same position as the whole term, so we have to create the if-label after
           ; the else-label, so that the wrong label/position association created by the
           ; else-label is overwritten.
           [(if-label) (create-simple-label sba-state term)]
           [(if-edge) (create-simple-edge if-label)]
           ; that does the outgoing flow sensitivity
           [(test-edge) (create-self-modifying-edge (lambda (label)
                                                      ; XXX subtping should be used here
                                                      (or (not (label-cst? label))
                                                          (label-cst-value label)))
                                                    then-label else-label if-edge)])
        (add-edge-and-propagate-set-through-edge test-label test-edge)
        if-label)]
     [(if test then)
      (let* ([test (syntax test)]
             [test-label (create-label-from-term sba-state test gamma enclosing-lambda-label)]
             [then-label
              (if (symbol? (syntax-e test))
                  (let* ([test-name (syntax-e test)]
                         [binding-label (lookup-env test gamma)]
                         [new-then-binding-label (create-simple-prim-label term)]
                         [new-then-gamma (extend-env gamma (list test) (list new-then-binding-label))]
                         [then-normal-edge (create-simple-edge new-then-binding-label)]
                         ; discards #f, passes the rest to then-normal-edge
                         [then-filtering-edge
                          (cons
                           (lambda (out-label inflowing-label tunnel-label)
                             (when (or (not (label-cst? inflowing-label))
                                       (label-cst-value inflowing-label))
                               ((car then-normal-edge) out-label inflowing-label tunnel-label)))
                           (cdr then-normal-edge))])
                    (if binding-label
                        (begin
                          (add-edge-and-propagate-set-through-edge binding-label then-filtering-edge)
                          (create-label-from-term sba-state (syntax then) new-then-gamma enclosing-lambda-label))
                        (create-label-from-term sba-state (syntax then) gamma enclosing-lambda-label)))
                  (create-label-from-term sba-state (syntax then) gamma enclosing-lambda-label))]
             [else-label (let ([void-label (make-label-cst
                                            #f #f #f #f #t
                                            term
                                            (make-hash-table)
                                            (make-hash-table)
                                            cst:void)])
                           (initialize-label-set-for-value-source void-label)
                           ;(register-label-with-gui void-label)
                           void-label)]
             ; because of the (if test then) case below, else-label might be associated with
             ; the same position as the whole term, so we have to create the if-label after
             ; the else-label, so that the wrong label/position association created by the
             ; else-label is overwritten.
             [if-label (create-simple-label sba-state term)]
             [if-edge (create-simple-edge if-label)]
             [test-edge (create-self-modifying-edge (lambda (label)
                                                      (or (not (label-cst? label))
                                                          (label-cst-value label)))
                                                    then-label else-label if-edge)])
        (add-edge-and-propagate-set-through-edge test-label test-edge)
        if-label)]
     [(begin exp exps ...)
      (let ([begin-label (create-simple-label sba-state term)]
            [last-body-exp-label (list:foldl
                                  (lambda (exp _)
                                    (create-label-from-term sba-state exp gamma enclosing-lambda-label))
                                  cst:dummy
                                  (cons (syntax exp) (syntax-e (syntax (exps ...)))))])
        (add-edge-and-propagate-set-through-edge
         last-body-exp-label
         (create-simple-edge begin-label))
        begin-label)]
     [(begin0 exp exps ...)
      (let ([begin0-label (create-simple-label sba-state term)]
            [first-body-exp-label
             (create-label-from-term sba-state (syntax exp) gamma enclosing-lambda-label)])
        (for-each (lambda (exp)
                    (create-label-from-term sba-state exp gamma enclosing-lambda-label))
                  (syntax-e (syntax (exps ...))))
        (add-edge-and-propagate-set-through-edge
         first-body-exp-label
         (create-simple-edge begin0-label))
        begin0-label)]
     [(#%top . identifier)
      (let ([identifier (syntax identifier)])
        (create-top-level-label sba-state identifier gamma enclosing-lambda-label))]
     [(set! var exp)
      (let* ([var-stx (syntax var)]
             [var-name (syntax-e var-stx)]
             [var-label (create-simple-label sba-state var-stx)]
             [var-edge (create-simple-edge var-label)]
             [binding-label (lookup-env var-stx gamma)]
             [exp-label
              (create-label-from-term sba-state (syntax exp) gamma enclosing-lambda-label)]
             [set!-label (create-simple-label sba-state term)]
             [set!-edge (create-simple-edge set!-label)]
             [void-label (make-label-cst #f #f #f #f #f
                                         term
                                         (make-hash-table)
                                         (make-hash-table)
                                         cst:void)])
        (initialize-label-set-for-value-source void-label)
        (if binding-label
            ; lexical variable
            (let* ([binding-edge (create-simple-edge binding-label)]
                   [effect
                    (lambda ()
                      ;(search-and-replace gamma var-name var-label)
                      (add-edge-and-propagate-set-through-edge
                       exp-label var-edge)
                      (add-edge-and-propagate-set-through-edge
                       var-label binding-edge)
                      (add-edge-and-propagate-set-through-edge
                       void-label set!-edge)
                      )])
              (if enclosing-lambda-label
                  (let* ([enclosing-lambda-effects (label-case-lambda-effects enclosing-lambda-label)]
                         [current-thunk (car enclosing-lambda-effects)])
                    (set-car! enclosing-lambda-effects
                              (lambda ()
                                (current-thunk)
                                (effect))))
                  (effect)))
            (let ([effect
                   (lambda ()
                     ; delay the lookup until the effect takes place
                     ; if the name we want to set! is a primitive, we set! the label that
                     ; simulates the primitive's definition.
                     (let ([binding-label (or (lookup-top-level-name sba-state var-name)
                                              (let ([primitive-data
                                                     (lookup-primitive-data sba-state var-name)])
                                                (if primitive-data
                                                    (prim-data-label primitive-data)
                                                    #f)))])
                       (if (or binding-label
                               (eq? var-name 'make-struct-type)
                               (eq? var-name 'make-struct-field-accessor)
                               (eq? var-name 'make-struct-field-mutator)
                               (eq? var-name 'set-car!)
                               (eq? var-name 'set-cdr!)
                               (eq? var-name 'string-set!)
                               (eq? var-name 'string-fill!)
                               (eq? var-name 'vector-set!)
                               (eq? var-name 'vector-fill!))
                           ; top level var
                           (let ([binding-edge (create-simple-edge binding-label)]) 
                             ;(add-top-level-name var-stx var-label)
                             (add-edge-and-propagate-set-through-edge
                              exp-label var-edge)
                             (add-edge-and-propagate-set-through-edge
                              var-label binding-edge)
                             (add-edge-and-propagate-set-through-edge
                              void-label set!-edge)
                             )
                           (set-error-for-label sba-state
                                                set!-label
                                                'red
                                                (format "set!: cannot set undefined identifier: ~a" var-name)))))])
              (if enclosing-lambda-label
                  (let* ([enclosing-lambda-effects (label-case-lambda-effects enclosing-lambda-label)]
                         [current-thunk (car enclosing-lambda-effects)])
                    (set-car! enclosing-lambda-effects
                              (lambda ()
                                (current-thunk)
                                (effect))))
                  (effect))))
        set!-label)]
     [(quote-syntax foo ...)
      (let ([label (create-simple-label sba-state term)])
        (set-error-for-label sba-state
                             label
                             'red
                             (format "quote-syntax not yet implemented"))
        label)]
     [(with-continuation-mark foo ...)
      (let ([label (create-simple-label sba-state term)])
        (set-error-for-label sba-state
                             label
                             'red
                             (format "with-continuation-mark not yet implemented"))
        label)]
     [(define-syntaxes foo ...)
      (let ([label (create-simple-label sba-state term)])
        (set-error-for-label sba-state
                             label
                             'red
                             (format "define-syntaxes not yet implemented"))
        label)]
     [(module foo ...)
      (let ([label (create-simple-label sba-state term)])
        (set-error-for-label sba-state
                             label
                             'red
                             (format "module not yet implemented"))
        label)]
     [(require foo ...)
      (let ([label (create-simple-label sba-state term)])
        (set-error-for-label sba-state
                             label
                             'red
                             (format "require not yet implemented"))
        label)]
     [(require-for-syntax foo ...)
      (let ([label (create-simple-label sba-state term)])
        (set-error-for-label sba-state
                             label
                             'red
                             (format "require-for-syntax not yet implemented"))
        label)]
     [(provide foo ...)
      (let ([label (create-simple-label sba-state term)])
        (set-error-for-label sba-state
                             label
                             'red
                             (format "provide not yet implemented"))
        label)]
     [(#%plain-module-begin foo ...)
      (let ([label (create-simple-label sba-state term)])
        (set-error-for-label sba-state
                             label
                             'red
                             (format "#%plain-module-begin not yet implemented"))
        label)]
     [var
      ; we cannot directly return the binding label, because, even though it makes for a
      ; simpler graph and simpler types, it screws up the arrows
      (let* ([var-stx (syntax var)]
             ;[var-name (syntax-e var-stx)]
             [binding-label (lookup-env var-stx gamma)])
        (if binding-label
            ; lexical variable
            (let ([bound-label (create-simple-label sba-state term)])
              (if enclosing-lambda-label
                  ; we have to delay the binding, because there might be a set! in between the
                  ; analysis of the enclosing lambda and the time the lambda is applied.
                  ; Note that this means we have to redo a lookup later to get the right binder,
                  ; which will have changed if a set! has occured (explicitely, or because
                  ; the lambda is in a letrec clause (see letrec))
                  (let* ([enclosing-lambda-effects
                          (label-case-lambda-effects enclosing-lambda-label)]
                         [current-thunk (car enclosing-lambda-effects)])
                    (set-car! enclosing-lambda-effects
                              (lambda ()
                                (current-thunk)
                                (let ([binding-label (lookup-env var-stx gamma)])
                                  (add-edge-and-propagate-set-through-edge
                                   binding-label
                                   (extend-edge-for-values sba-state (create-simple-edge bound-label)))))))
                  (add-edge-and-propagate-set-through-edge
                   binding-label
                   (extend-edge-for-values sba-state (create-simple-edge bound-label))))
              bound-label)
            ; probably a top level var (like a primitive name) but without #%top (if it comes
            ; from a macro, or some strange stuff like that.
            (create-top-level-label sba-state var-stx gamma enclosing-lambda-label)
            ))]
     ))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TYPES
  
  ; each entry is of the form (type-name (listof direct-subtypes) scheme-predicate)
  ; top should appear first (see the subtype function below)
  ; note also that exact? and inexact? can only be used for numbers, so we have to do
  ; tests like complex? first.
  (define *basic-types* `((top (undefined void null boolean char symbol string eof-object env number port)
                               ,(lambda (_) #t))
                          (undefined () ,(lambda (v) (eq? v cst:undefined)))
                          (void () ,void?)
                          (null () ,null?)
                          (boolean () ,boolean?)
                          (char (letter) ,char?)
                          ; approximation
                          (letter () ,char?)
                          (symbol () ,symbol?)
                          (string () ,string?)
                          (eof-object () ,eof-object?)
                          ; no r5rs predicate, but no subtype anyway...
                          (env () ,(lambda (_) #f))
                          (number (exact-number inexact-number complex) ,number?)
                          (exact-number (exact-complex) ,(lambda (n) (and (number? n) (exact? n))))
                          (inexact-number (inexact-complex) ,(lambda (n) (and (number? n) (inexact? n))))
                          (complex (exact-complex inexact-complex real) ,complex?)
                          (exact-complex (exact-real) ,(lambda (n) (and (complex? n) (exact? n))))
                          (inexact-complex (inexact-real) ,(lambda (n) (and (complex? n) (inexact? n))))
                          (real (exact-real inexact-real rational) ,real?)
                          (exact-real (exact-rational) ,(lambda (n) (and (real? n) (exact? n))))
                          (inexact-real (inexact-rational) ,(lambda (n) (and (real? n) (inexact? n))))
                          (rational (exact-rational inexact-rational integer) ,rational?)
                          (exact-rational (exact-integer) ,(lambda (n) (and (rational? n) (exact? n))))
                          (inexact-rational (inexact-integer) ,(lambda (n) (and (rational? n) (inexact? n))))
                          (integer (exact-integer inexact-integer) ,integer?)
                          (exact-integer () ,(lambda (n) (and (integer? n) (exact? n))))
                          (inexact-integer () ,(lambda (n) (and (integer? n) (inexact? n))))
                          (port (input-port) ,port?)
                          (input-port () ,input-port?)
                          (output-port () ,output-port?)
                          (bottom () ,(lambda (_) #f))
                          ))
  
  (define *type-constructors* '(forall
                                cons listof
                                vector union values
                                case-lambda -> *-> rest
                                promise
                                rec-type
                                ))
  (define *all-type-keywords* (append (map car *basic-types*) *type-constructors*))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVE TYPE PARSER AND LOOKUP
  
  ; sba-state symbol -> (or/c prim-data #f)
  (define (lookup-primitive-data sba-state name)
    (hash-table-get (sba-state-primitive-types-table sba-state) name cst:thunk-false))
  
  ; sba-state string -> void
  (define (initialize-primitive-type-schemes sba-state filename)
    ; XXX should check for errors
    (let ([sexp (call-with-input-file filename read 'text)]
          [primitive-types-table (sba-state-primitive-types-table sba-state)])
      (unless (list? sexp)
        (raise-syntax-error
         'initialize-primitive-type-schemes
         (format "expected list at top level in file ~a, got: ~a"
                 filename sexp)))
      (for-each (lambda (prim-entry) 
                  (unless (and (list? prim-entry)
                               (= 2 (length prim-entry))
                               (symbol? (car prim-entry)))
                    (raise-syntax-error
                     'initialize-primitive-type-schemes
                     (format "expected `(,symbol type-scheme) entry in file ~a, got: ~a"
                             filename prim-entry))))
                sexp)
      (for-each (lambda (prim-entry)
                  (let ([primitive-name (car prim-entry)]
                        [primitive-type (cadr prim-entry)])
                    (when (hash-table-get primitive-types-table
                                          primitive-name cst:thunk-false)
                      (raise-syntax-error
                       'initialize-primitive-type-schemes
                       (format "found duplicate for primitive ~a in file ~a"
                               primitive-name filename)))
                    (hash-table-put! primitive-types-table primitive-name
                                     (make-prim-data (parse&check-type-scheme
                                                      primitive-type primitive-name filename)
                                                     (create-simple-prim-label #f)))))
                sexp)))
  
  ; sexp symbol tring -> type
  (define (parse&check-type-scheme sexp primitive-name filename)
    (if (and (list? sexp)
             (not (null? sexp))
             (eq? (car sexp) 'forall))
        (if (= (length sexp) 3)
            (let ([delta-flow (make-hash-table)]
                  [flow-vars&type^cs (cadr sexp)]
                  [type (caddr sexp)])
              (for-each
               (lambda (flow-var&type^C)
                 (if (and (list? flow-var&type^C)
                          (= (length flow-var&type^C) 2)
                          (symbol? (car flow-var&type^C)))
                     (let ([flow-var (car flow-var&type^C)]
                           [type^C (cadr flow-var&type^C)])
                       (when (memq flow-var *all-type-keywords*)
                         (raise-syntax-error
                          'parse&check-type-scheme
                          (format "flow variable ~a is already the name of a basic type or type constructor, in type scheme for primitive ~a in file ~a"
                                  flow-var primitive-name filename)))
                       (when (hash-table-get delta-flow flow-var cst:thunk-false)
                         (raise-syntax-error
                          'parse&check-type-scheme
                          (format "duplicate flow variable ~a in type scheme for primitive ~a in file ~a"
                                  flow-var primitive-name filename)))
                       (hash-table-put! delta-flow
                                        flow-var
                                        (cons (list #t #t (make-type-flow-var flow-var))
                                              ; type^cs do not contain flow vars, so we give an
                                              ; empty delta. If this function returns, we know
                                              ; the result is a constant type.
                                              (parse&check-type type^C
                                                                (make-hash-table)
                                                                '()
                                                                #t
                                                                primitive-name filename))))
                     (raise-syntax-error
                      'parse&check-type-scheme
                      (format "malformed type scheme clause for primitive ~a in file ~a: expected (symbol type), got ~a"
                              primitive-name filename flow-var&type^C))))
               flow-vars&type^cs)
              (let ([type (parse&check-type type delta-flow '() #t primitive-name filename)])
                (hash-table-for-each
                 delta-flow
                 (lambda (flow-var type-info)
                   (let ([no-contra-use (caar type-info)]
                         [no-co-use (cadar type-info)])
                     (cond
                       [(and no-contra-use no-co-use)
                        (raise-syntax-error
                         'parse&check-type-scheme
                         (format "unused flow variable ~a in type scheme for primitive ~a in file ~a"
                                 flow-var primitive-name filename))]
                       [no-contra-use
                        (raise-syntax-error
                         'parse&check-type-scheme
                         (format "no contravariant in-flow for flow variable ~a in type scheme for primitive ~a in file ~a"
                                 flow-var primitive-name filename))]
                       [no-co-use
                        (raise-syntax-error
                         'parse&check-type-scheme
                         (format "no covariant out-flow for flow variable ~a in type scheme for primitive ~a in file ~a"
                                 flow-var primitive-name filename))]
                       [else #t]))))
                (if (null? flow-vars&type^cs)
                    type
                    (make-type-scheme
                     (hash-table-map delta-flow (lambda (flow-var type-info) (caddar type-info)))
                     (hash-table-map delta-flow (lambda (flow-var type-info) (cdr type-info)))
                     type))))
            (raise-syntax-error 'parse&check-type-scheme
                                (format "malformed type scheme for primitive ~a in file ~a: expected (forall (flow-var-clause ...) type), got ~a"
                                        primitive-name filename sexp)))
        (parse&check-type sexp (make-hash-table) '() #t primitive-name filename)))
  
  ; sexp (hash-table-of symbol (cons (list boolean boolean type-var) type))
  ; (listof (cons symbol type-var))) boolean symbol string -> type
  (define (parse&check-type sexp delta-flow delta-type covariant? primitive-name filename)
    (if (list? sexp)
        (if (null? sexp)
            (make-type-cst '())
            (let ([type-kw (car sexp)])
              (cond
                [(eq? type-kw 'forall)
                 (raise-syntax-error
                  'parse&check-type
                  (format "type scheme inside type or other type scheme for primitive ~a in file ~a: ~a"
                          primitive-name filename sexp))]
                [(eq? type-kw 'case-lambda)
                 (let ([all-types            
                        (list:foldr
                         (lambda (clause other-clauses-types)
                           (if (and (list? clause)
                                    (= (length clause) 2))
                               (let* ([args (car clause)]
                                      [exp (cadr clause)]
                                      [exp-type (parse&check-type exp delta-flow delta-type
                                                                  covariant?
                                                                  primitive-name filename)]
                                      [rest-arg?s (vector-ref other-clauses-types 0)]
                                      [req-args (vector-ref other-clauses-types 1)]
                                      [argss-typess (vector-ref other-clauses-types 2)]
                                      [exps-types (vector-ref other-clauses-types 3)])
                                 (if (list? args)
                                     (let ([args-length (length args)])
                                       (if (and (pair? args) ; could be empty
                                                (eq? (car args) 'rest))
                                           ; list of (possibly complex) args with (possibly complex) rest arg
                                           (if (> args-length 1)
                                               (vector (cons #t rest-arg?s)
                                                       (cons (- args-length 2) req-args)
                                                       (cons (map (lambda (arg)
                                                                    (parse&check-type arg delta-flow delta-type
                                                                                      (not covariant?)
                                                                                      primitive-name filename))
                                                                  (cdr args))
                                                             argss-typess)
                                                       (cons exp-type exps-types))
                                               (raise-syntax-error
                                                'parse&check-type
                                                (format "missing rest argument in argument list for clause in case-lambda type in type scheme for primitive ~a in file ~a: expected (rest arg-type args-types ...), got ~a"
                                                        primitive-name filename args)))
                                           ; normal (possibly empty) list of (possibly complex) args
                                           (vector (cons #f rest-arg?s)
                                                   (cons args-length req-args)
                                                   (cons (map (lambda (arg)
                                                                (parse&check-type arg delta-flow delta-type
                                                                                  (not covariant?)
                                                                                  primitive-name filename))
                                                              args)
                                                         argss-typess)
                                                   (cons exp-type exps-types))))
                                     (raise-syntax-error
                                      'parse&check-type
                                      (format "malformed argument list for clause in case-lambda type in type scheme for primitive ~a in file ~a: expected (args-types ...), got ~a"
                                              primitive-name filename args))))
                               (raise-syntax-error
                                'parse&check-type
                                (format "malformed clause in case-lambda type in type scheme for primitive ~a in file ~a: expected (args-types exp-type), got ~a"
                                        primitive-name filename clause))))
                         (vector '()'()'()'())
                         (cdr sexp))])
                   (make-type-case-lambda (vector-ref all-types 0)
                                          (vector-ref all-types 1)
                                          (vector-ref all-types 2)
                                          (vector-ref all-types 3)))]
                [(eq? type-kw 'cons)
                 (if (= (length sexp) 3)
                     (make-type-cons (parse&check-type (cadr sexp) delta-flow delta-type covariant?
                                                       primitive-name filename)
                                     (parse&check-type (caddr sexp) delta-flow delta-type covariant?
                                                       primitive-name filename))
                     (raise-syntax-error
                      'parse&check-type
                      (format "malformed cons type in type scheme for primitive ~a in file ~a: ~a"
                              primitive-name filename sexp)))]
                [(eq? type-kw 'union)
                 (make-type-union (map (lambda (elt-sexp)
                                         (parse&check-type
                                          elt-sexp delta-flow delta-type covariant?
                                          primitive-name filename))
                                       (cdr sexp)))]
                [(eq? type-kw 'values)
                 (if (= (length sexp) 2)
                     (make-type-values (parse&check-type
                                        (cadr sexp) delta-flow delta-type covariant?
                                        primitive-name filename))
                     (raise-syntax-error
                      'parse&check-type
                      (format "malformed values type in type scheme for primitive ~a in file ~a: expected (values type), got ~a"
                              primitive-name filename sexp)))]
                [(eq? type-kw 'rec-type)
                 (if (= (length sexp) 3)
                     (let* ([clauses (cadr sexp)]
                            [clauses-type-vars-names&types
                             (map
                              (lambda (clause)
                                (if (and (list? clause)
                                         (= (length clause) 2))
                                    (let ([type-var-name (car clause)])
                                      (if (or (assq type-var-name delta-type)
                                              (hash-table-get delta-flow sexp cst:thunk-false))
                                          (raise-syntax-error
                                           'parse&check-type
                                           (format "recursive type variable ~a used twice or conflicts with flow variable name in type scheme for primitive ~a in file ~a"
                                                   type-var-name primitive-name filename))
                                          (cons type-var-name (make-type-var type-var-name #f #f))))))
                              clauses)]
                            [all-type-vars (append clauses-type-vars-names&types delta-type)]
                            [clauses-types
                             (map
                              (lambda (clause)
                                (parse&check-type
                                 (cadr clause) delta-flow all-type-vars covariant?
                                 primitive-name filename))
                              clauses)])
                       (make-type-rec (map cdr clauses-type-vars-names&types) clauses-types
                                      (parse&check-type
                                       (caddr sexp) delta-flow all-type-vars covariant?
                                       primitive-name filename)))
                     (raise-syntax-error
                      'parse&check-type
                      (format "malformed recursive type in type scheme primitive ~a in file ~a: ~a"
                              primitive-name filename sexp)))]
                [(eq? type-kw 'listof)
                 (if (= (length sexp) 2)
                     ; (listof T) = (rec ([alpha (union '() (cons T alpha))]) alpha)
                     (let ([listof-type-var (gensym)])
                       (parse&check-type
                        `(rec-type ([,listof-type-var (union () (cons ,(cadr sexp) ,listof-type-var))])
                                   ,listof-type-var)
                        delta-flow delta-type
                        covariant?
                        primitive-name filename))
                     (raise-syntax-error
                      'parse&check-type
                      (format "malformed listof type in type scheme for primitive ~a in file ~a: expected (listof type), got ~a"
                              primitive-name filename sexp)))]
                [(eq? type-kw 'vector)
                 (if (= (length sexp) 2)
                     (make-type-vector (parse&check-type
                                        (cadr sexp) delta-flow delta-type covariant?
                                        primitive-name filename))
                     (raise-syntax-error
                      'parse&check-type
                      (format "malformed vector type in type scheme for primitive ~a in file ~a: expected (vector type), got ~a"
                              primitive-name filename sexp)))]
                [(eq? type-kw 'promise)
                 (if (= (length sexp) 2)
                     (make-type-promise (parse&check-type
                                         (cadr sexp) delta-flow delta-type covariant?
                                         primitive-name filename))
                     (raise-syntax-error
                      'parse&check-type
                      (format "malformed promise type in type scheme for primitive ~a in file ~a: expected (promise type), got ~a"
                              primitive-name filename sexp)))]
                [(eq? type-kw 'rest)
                 (raise-syntax-error
                  'parse&check-type
                  (format "illegal use of rest in type scheme for primitive ~a in file ~a: ~a"
                          primitive-name filename sexp))]
                [else
                 (let* ([sexp-length (length sexp)]
                        [sexp-length-1 (sub1 sexp-length)]
                        [sexp-length-2 (sub1 sexp-length-1)]
                        [sexp-length-3 (sub1 sexp-length-2)])
                   (cond
                     [(and (>= sexp-length-2 0)
                           (eq? (list-ref sexp sexp-length-2) '->))
                      (let ([exp-sexp (list-ref sexp sexp-length-1)]
                            [list-head (list-head! sexp sexp-length-2
                                                   primitive-name filename)])
                        (parse&check-type
                         `(case-lambda [,list-head ,exp-sexp])
                         delta-flow delta-type covariant? primitive-name filename))]
                     [(and (>= sexp-length-3 0)
                           (eq? (list-ref sexp sexp-length-2) '*->))
                      (let ([exp-sexp (list-ref sexp sexp-length-1)]
                            [rest-sexp (list `(listof ,(list-ref sexp sexp-length-3)))]
                            [list-head (list-head! sexp sexp-length-3
                                                   primitive-name filename)])
                        (parse&check-type
                         `(case-lambda [,(cons 'rest (set-list-tail-cdr! list-head rest-sexp))
                                         ,exp-sexp])
                         delta-flow delta-type covariant? primitive-name filename))]
                     [else
                      (raise-syntax-error
                       'parse&check-type
                       (format "malformed constructed type in type scheme for primitive ~a in file ~a: ~a"
                               primitive-name filename sexp))]))]
                )))
        (cond
          [(pair? sexp)
           ; improper list
           (raise-syntax-error
            'parse&check-type
            (format "improper list found in type scheme for primitive ~a in file ~a: ~a"
                    primitive-name filename sexp))]
          [(memq sexp *type-constructors*)
           =>
           (lambda (_)
             (raise-syntax-error
              'parse&check-type-scheme
              (format "type variable ~a is already the name of a type constructor, in type scheme for primitive ~a in file ~a"
                      sexp primitive-name filename)))]
          [(hash-table-get delta-flow sexp cst:thunk-false)
           =>
           (lambda (type-info)
             (if covariant?
                 (set-car! (cdar type-info) #f)
                 (if (caar type-info)
                     (set-car! (car type-info) #f)
                     ; already used this flow variable in contravariant position
                     (raise-syntax-error
                      'parse&check-type
                      (format "flow variable ~a used several times in contravariant position in type scheme for primitive ~a in file ~a"
                              sexp primitive-name filename))))
             (caddar type-info))]
          [(assq sexp delta-type)
           =>
           ; gets (cons type-var-name type-var) => returns type-var
           cdr]
          ; [(memq sexp *basic-types*);XXX definition of *basic-types* has changed...
          ;  (make-type-cst sexp)]
          ; the following works for both basic types and any atomic value (which is
          ; then considered a basic type too). We know that flow var names and basic
          ; type names are disjoint, so there's no confusion between this case and
          ; the previous one.
          [else
           (cond
             [(eq? sexp 'boolean)
              (make-type-union (list (make-type-cst #t) (make-type-cst #f)))]
             [(eq? sexp 'void)
              (make-type-cst cst:void)]
             [(eq? sexp 'bottom)
              (make-type-empty)]
             [(eq? sexp 'undefined)
              (make-type-cst cst:undefined)]
             [else (make-type-cst sexp)])])))
  
  ; (listof alpha) number sexp symbol string -> (listof alpha)
  ; returns first n elements of l. We know from the way the function is called that we
  ; must always have n >= 0 and (length l) >= n.
  (define (list-head! l n primitive-name filename)
    (letrec ([chop (lambda (l n)
                     (if (= n 1)
                         (set-cdr! l '())
                         (chop (cdr l) (sub1 n))))])
      (cond
        [(zero? n) '()]
        [(>= n 1) (chop l n) l]
        [else (raise-syntax-error
               'list-head!
               (format "internal error 6 in type scheme for primitive ~a in file ~a"
                       primitive-name filename))])))
  
  ; (listof top) top -> improper-list
  ; glues rest-sexp as the cdr of the last element of list-head
  (define (set-list-tail-cdr! list-head rest-sexp)
    (letrec ([glue (lambda (l)
                     (if (null? (cdr l))
                         (set-cdr! l rest-sexp)
                         (glue (cdr l))))])
      (if (null? list-head)
          rest-sexp
          (begin
            (glue list-head)
            list-head))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TYPE ENVIRONMENT & MISC
  
  ; (hash-table-of symbol label) type-flow-var label -> label
  (define (add-flow-var-to-env env flow-var label)
    (hash-table-put! env flow-var label)
    env)
  
  ; (hash-table-of symbol label) type-flow-var -> label
  ; the type parser guarantees that the lookup will be succesfull
  (define (lookup-flow-var-in-env env flow-var)
    (hash-table-get env flow-var))
  
  ; like map, but over a list made of label-cons instead of cons
  (define (type-list-map f tl)
    (if (type-cons? tl)
        (cons (f (type-cons-car tl))
              (type-list-map f (type-cons-cdr tl)))
        (if (and (type-cst? tl)
                 (eq? (type-cst-type tl) '()))
            '()
            (error 'type-list-map "not a type list: ~a" tl))))
  
  ; symmetric of list-tail
  (define (list-head l n)
    (if (zero? n)
        '()
        (if (null? l)
            (error 'list-head "list too short")
            (cons (car l)
                  (list-head (cdr l) (sub1 n))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAPH RECONSTRUCTION FROM TYPE
  
  ; sba-state type (hash-table-of symbol label) label -> label
  ; analyse type scheme and creates flow var environment
  ; label is the label into which the final result will flow into. We need that
  ; mainly to report errors correctly.
  (define (reconstruct-graph-from-type-scheme sba-state type delta-flow label)
    (let ([term (label-term label)])
      (if (type-scheme? type)
          (begin
            (for-each (lambda (flow-var type^C)
                        (let ([label (create-simple-prim-label term)])
                          ;(associate-label-with-type label type^C)
                          (add-flow-var-to-env delta-flow flow-var (cons label type^C))))
                      (type-scheme-flow-vars type)
                      (type-scheme-type^cs type))
            (reconstruct-graph-from-type sba-state (type-scheme-type type) delta-flow '() label term #t #f))
          (reconstruct-graph-from-type sba-state type delta-flow '() label term #t #f))))
  
  ; sba-state type (hash-table-of type-flow-var (cons label type)) (listof (cons type-var label))
  ; label term boolean label -> label
  ; reconstructs a graph from type representing the primitive represented by label,
  ; using environment delta.
  ; delta-flow is the flow-var->label environment, delta-type is the type-var->label one.
  ; label is the label for the primitive whose type we are analyzing. It's just used for underlining
  ; errors. Same for term. covariant? is self-explanatory...
  ; contra-union? is a boolean telling whether the parent label we are dealing
  ; with is a union in contravariant position: since the flows are not filtered by types,
  ; everything that flows into a union will normally flow into the different componants
  ; of the union. We don't want that, because then things might flow into a label were they
  ; should flow into and trigger a false error. The best example is this is with lists: it's
  ; a recursive type that contains a union of the empty list and of a recursive cons. If a
  ; cons flows into a list label, the cons will flow in both parts of the union, and trigger
  ; an error when it flows into the empty label. So we have to do some filtering. This means
  ; that we are not going to create a simple edge between the union label and the empty label
  ; when we analyze the union type, but we are going to create a filtering edge between the
  ; union label and the empty label when we analyze the empty type. To do that we need to
  ; keep track of the parent union label.
  ; Note how we use associate-label-with-type to memorize type checking only in the contravariant
  ; case. The type to check in the covariant case is always top, since we assume internal
  ; correctness of the graph generation from a primitive type.
  (define (reconstruct-graph-from-type sba-state type delta-flow delta-type label term covariant? contra-union?)
    (if covariant?
        ; covariant cases
        (cond
          [(type-case-lambda? type)
           (let* ([all-labels
                   (list:foldr
                    (lambda (args-types exp-type other-clauses-labels)
                      (let ([argss-labelss (car other-clauses-labels)]
                            [exps-labels (cdr other-clauses-labels)])
                        (cons (cons (map (lambda (arg-type)
                                           (reconstruct-graph-from-type
                                            sba-state
                                            arg-type delta-flow delta-type label term #f #f))
                                         args-types)
                                    argss-labelss)
                              (cons (reconstruct-graph-from-type
                                     sba-state
                                     exp-type delta-flow delta-type label term #t #f)
                                    exps-labels))))
                    (cons '()'())
                    (type-case-lambda-argss type)
                    (type-case-lambda-exps type))]
                  [label (make-label-case-lambda
                          #f #f #f #f #t
                          term
                          (make-hash-table)
                          (make-hash-table)
                          #f
                          (type-case-lambda-rest-arg?s type)
                          (type-case-lambda-req-args type)
                          (car all-labels)
                          (cdr all-labels)
                          ;(map (lambda (_) '()) all-labels)
                          (map (lambda (_) cst:dummy-thunk) all-labels))])
             (initialize-label-set-for-value-source label)
             label)]
          [(type-cons? type)
           (let ([label (make-label-cons
                         #f #f #f #f #t
                         term
                         (make-hash-table)
                         (make-hash-table)
                         (reconstruct-graph-from-type
                          sba-state
                          (type-cons-car type) delta-flow delta-type label term #t #f)
                         (reconstruct-graph-from-type
                          sba-state
                          (type-cons-cdr type) delta-flow delta-type label term #t #f))])
             (initialize-label-set-for-value-source label)
             label)]
          [(type-vector? type)
           (let ([label (make-label-vector
                         #f #f #f #f #t
                         term
                         (make-hash-table)
                         (make-hash-table)
                         (reconstruct-graph-from-type
                          sba-state
                          (type-vector-element type) delta-flow delta-type label term #t #f))])
             (initialize-label-set-for-value-source label)
             label)]
          [(type-promise? type)
           (let ([label (make-label-promise
                         #f #f #f #f #t
                         term
                         (make-hash-table)
                         (make-hash-table)
                         (reconstruct-graph-from-type
                          sba-state
                          (type-promise-value type) delta-flow delta-type label term #t #f))])
             (initialize-label-set-for-value-source label)
             label)]
          [(type-flow-var? type)
           (car (lookup-flow-var-in-env delta-flow type))]
          [(type-var? type)
           (cdr (assq type delta-type))]
          [(type-cst? type)
           (let ([label (make-label-cst
                         #f #f #f #f #t
                         term
                         (make-hash-table)
                         (make-hash-table)
                         ; the type parser ensures that type-cst is only created for
                         ; non-list (i.e. atomic) types => 3, 'foo, 'int
                         (type-cst-type type))])
             (initialize-label-set-for-value-source label)
             label)]
          [(type-union? type)
           (let* ([elt-labels (map (lambda (elt-type)
                                     (reconstruct-graph-from-type
                                      sba-state
                                      elt-type delta-flow delta-type label term #t #f))
                                   (type-union-elements type))]
                  [union-label (create-simple-prim-label term)]
                  ; can return multiple values
                  [union-edge (create-simple-edge union-label)])
             (for-each (lambda (elt-label)
                         (add-edge-and-propagate-set-through-edge elt-label union-edge))
                       elt-labels)
             union-label)]
          [(type-values? type)
           (let* ([values-content-label (reconstruct-graph-from-type
                                         sba-state
                                         (type-values-type type) delta-flow delta-type label term #t #f)]
                  [values-label (make-label-values
                                 #f #f #f #f #t
                                 term
                                 (make-hash-table)
                                 (make-hash-table)
                                 values-content-label)])
             (initialize-label-set-for-value-source values-label)
             values-label)]
          [(type-rec? type)
           (let* ([clauses-vars-types&labels (map (lambda (type-var)
                                                    (cons type-var (create-simple-prim-label term)))
                                                  (type-rec-vars type))]
                  [all-var-labels (append clauses-vars-types&labels delta-type)]
                  [clauses-types-labels (map (lambda (clause-type)
                                               (reconstruct-graph-from-type
                                                sba-state
                                                clause-type delta-flow all-var-labels label term #t #f))
                                             (type-rec-types type))])
             ; note: we never check whether all clauses are used. If they are not, they'll be
             ; garbage collected after we return from here.
             (for-each (lambda (clause-var-type&label clause-type-label)
                         (add-edge-and-propagate-set-through-edge
                          clause-type-label
                          (create-simple-edge (cdr clause-var-type&label))))
                       clauses-vars-types&labels clauses-types-labels)
             (reconstruct-graph-from-type sba-state (type-rec-body type) delta-flow all-var-labels label term #t #f))]
          [(type-empty? type)
           (create-simple-prim-label term)]
          [else (error 'reconstruct-graph-from-type "unknown covariant type for primitive ~a: ~a"
                       (syntax-e term) type)]
          )
        ;
        ; contravariant cases
        ;
        (cond
          [(type-case-lambda? type)
           (let* ([rest-arg?s-around (type-case-lambda-rest-arg?s type)]
                  [req-args-around (type-case-lambda-req-args type)]
                  [argss-labelss-around (map (lambda (args-types)
                                               (map (lambda (arg-type)
                                                      (reconstruct-graph-from-type
                                                       sba-state
                                                       arg-type delta-flow delta-type label term #t #f))
                                                    args-types))
                                             (type-case-lambda-argss type))]
                  [exps-labels-around (map (lambda (exp-type)
                                             (reconstruct-graph-from-type
                                              sba-state
                                              exp-type delta-flow delta-type label term #f #f))
                                           (type-case-lambda-exps type))]
                  [case-lambda-label (create-simple-prim-label term)]
                  [case-lambda-edge (create-case-lambda-edge
                                     sba-state
                                     rest-arg?s-around
                                     req-args-around
                                     argss-labelss-around
                                     exps-labels-around
                                     case-lambda-label
                                     contra-union?)])
             (unless contra-union?
               (associate-label-with-type
                sba-state
                case-lambda-label 
                (make-type-case-lambda
                 rest-arg?s-around
                 req-args-around
                 (map (lambda (args-labels rest-arg?)
                        (if rest-arg?
                            (list:foldr
                             (lambda (arg-label other-args)
                               (if (null? other-args)
                                   ; rest arg => listof
                                   (let ([fake-type-var (make-type-var (gensym) #f #f)])
                                     (cons (make-type-rec
                                            (list fake-type-var)
                                            (list (make-type-union
                                                   (list
                                                    (make-type-cst '())
                                                    (make-type-cons
                                                     (make-type-empty)
                                                     fake-type-var))))
                                            fake-type-var)
                                           '()))
                                   (cons (make-type-empty)
                                         other-args)))
                             '()
                             args-labels)
                            (map (lambda (arg-label)
                                   (make-type-empty))
                                 args-labels)))
                      argss-labelss-around rest-arg?s-around)
                 (map (lambda (exp-label)
                        (make-type-cst 'top))
                      exps-labels-around))
                delta-flow))
             (add-edge-and-propagate-set-through-edge case-lambda-label case-lambda-edge)
             case-lambda-label)]
          [(type-cons? type)
           (let* ([car-label (reconstruct-graph-from-type
                              sba-state
                              (type-cons-car type) delta-flow delta-type label term #f #f)]
                  [car-edge (create-simple-edge car-label)]
                  [cdr-label (reconstruct-graph-from-type
                              sba-state
                              (type-cons-cdr type) delta-flow delta-type label term #f #f)]
                  [cdr-edge (create-simple-edge cdr-label)]
                  [cons-label (create-simple-prim-label term)]
                  [cons-edge
                   (cons
                    (if contra-union?
                        ; non-error-checking edge
                        (lambda (out-label inflowing-label tunnel-label)
                          ; cons sink => no use for out-label here
                          (if (label-cons? inflowing-label)
                              (and (add-edge-and-propagate-set-through-edge
                                    (label-cons-car inflowing-label)
                                    car-edge)
                                   (add-edge-and-propagate-set-through-edge
                                    (label-cons-cdr inflowing-label)
                                    cdr-edge))
                              #f))
                        ; error checking edge
                        (lambda (out-label inflowing-label tunnel-label)
                          ; cons sink => no use for out-label here
                          (if (label-cons? inflowing-label)
                              (and (add-edge-and-propagate-set-through-edge
                                    (label-cons-car inflowing-label)
                                    car-edge)
                                   (add-edge-and-propagate-set-through-edge
                                    (label-cons-cdr inflowing-label)
                                    cdr-edge))
                              ; XXX should we do this here because we can, or in check-primitive-types
                              ; because that's where it should be done... ? We don't have access to
                              ; term anymore in check-primitive-types (yet)... See the commented call to
                              ; associate-label-with-type below.
                              (begin
                                (set-error-for-label sba-state
                                                     label
                                                     'red
                                                     (format "primitive expects argument of type <pair>; given ~a"
                                                             (pp-type sba-state (get-type-from-label
                                                                                 sba-state
                                                                                 inflowing-label)
                                                                      'type-cons)))
                                #f))))
                    ; cons sink
                    (gensym))])
             (unless contra-union?
               (associate-label-with-type sba-state
                                          cons-label
                                          (make-type-cons
                                           (make-type-cst 'top)
                                           (make-type-cst 'top))
                                          delta-flow))
             (add-edge-and-propagate-set-through-edge cons-label cons-edge)
             cons-label)]
          [(type-vector? type)
           (let* ([element-label (reconstruct-graph-from-type
                                  sba-state
                                  (type-vector-element type) delta-flow delta-type label term #f #f)]
                  [element-edge (create-simple-edge element-label)]
                  [vector-label (create-simple-prim-label term)]
                  [vector-edge
                   (cons
                    (if contra-union?
                        ; non-error-checking edge
                        (lambda (out-label inflowing-label tunnel-label)
                          ; vector sink => no use for out-label here
                          (if (label-vector? inflowing-label)
                              (add-edge-and-propagate-set-through-edge
                               (label-vector-element inflowing-label)
                               element-edge)
                              #f))
                        ; error checking edge
                        (lambda (out-label inflowing-label tunnel-label)
                          ; vector sink => no use for out-label here
                          (if (label-vector? inflowing-label)
                              (add-edge-and-propagate-set-through-edge
                               (label-vector-element inflowing-label)
                               element-edge)
                              ; XXX should we do this here because we can, or in check-primitive-types
                              ; because that's where it should be done... ? We don't have access to
                              ; term anymore in check-primitive-types (yet)... See the commented call to
                              ; associate-label-with-type below.
                              (begin
                                (set-error-for-label sba-state
                                                     label
                                                     'red
                                                     (format "primitive expects argument of type <vector>; given ~a"
                                                             (pp-type sba-state (get-type-from-label
                                                                                 sba-state inflowing-label)
                                                                      'type-vector)))
                                #f))))
                    ; vector sink
                    (gensym))])
             (unless contra-union?
               (associate-label-with-type sba-state
                                          vector-label
                                          (make-type-vector (make-type-cst 'top))
                                          delta-flow))
             (add-edge-and-propagate-set-through-edge vector-label vector-edge)
             vector-label)]
          [(type-promise? type)
           (let* ([element-label (reconstruct-graph-from-type
                                  sba-state
                                  (type-promise-value type) delta-flow delta-type label term #f #f)]
                  [element-edge (create-simple-edge element-label)]
                  [promise-label (create-simple-prim-label term)]
                  [promise-edge
                   (cons
                    (if contra-union?
                        ; non-error-checking edge
                        (lambda (out-label inflowing-label tunnel-label)
                          ; promise sink => no use for out-label here
                          (if (label-promise? inflowing-label)
                              (add-edge-and-propagate-set-through-edge
                               (label-promise-value inflowing-label)
                               element-edge)
                              #f))
                        ; error checking edge
                        (lambda (out-label inflowing-label tunnel-label)
                          ; promise sink => no use for out-label here
                          (if (label-promise? inflowing-label)
                              (add-edge-and-propagate-set-through-edge
                               (label-promise-value inflowing-label)
                               element-edge)
                              ; XXX should we do this here because we can, or in check-primitive-types
                              ; because that's where it should be done... ? We don't have access to
                              ; term anymore in check-primitive-types (yet)... See the commented call to
                              ; associate-label-with-type below.
                              (begin
                                (set-error-for-label sba-state
                                                     label
                                                     'red
                                                     (format "primitive expects argument of type <promise>; given ~a"
                                                             (pp-type sba-state (get-type-from-label
                                                                                 sba-state inflowing-label)
                                                                      'type-promise)))
                                #f))))
                    ; promise sink
                    (gensym))])
             (unless contra-union?
               (associate-label-with-type sba-state
                                          promise-label
                                          (make-type-promise (make-type-cst 'top))
                                          delta-flow))
             (add-edge-and-propagate-set-through-edge promise-label promise-edge)
             promise-label)]
          [(type-flow-var? type)
           (let* ([label&type^C (lookup-flow-var-in-env delta-flow type)]
                  [label (car label&type^C)])
             (unless contra-union?
               (associate-label-with-type sba-state label (cdr label&type^C) delta-flow))
             label)]
          [(type-var? type)
           (cdr (assq type delta-type))]
          [(type-cst? type)
           (let* ([cst-label (make-label-cst
                              #f #f #f #f #t
                              term
                              (make-hash-table)
                              (make-hash-table)
                              ; the type parser ensures that type-cst is only created for
                              ; non-list (i.e. atomic) types => 3, 'foo, 'int
                              (type-cst-type type))])
             ; propagation to such a label always works, so post checking is necessary
             ; note that propagation always works because we don't do any type-based
             ; filtering. This means that if the cst is inside a union, the propagation to
             ; the union will always work, and the error detection will only happen after
             ; the fact (which might be ok, since a label flowing into a cst doesn't go
             ; anywhere else) XXX ?
             (unless contra-union?
               (associate-label-with-type sba-state cst-label type delta-flow))
             cst-label)]
          [(type-values? type)
           (let* ([values-content-label (reconstruct-graph-from-type
                                         sba-state
                                         (type-values-type type) delta-flow delta-type label term #f #f)]
                  [values-content-edge (create-simple-edge values-content-label)]
                  [values-label (create-simple-prim-label term)]
                  [values-edge
                   (cons
                    (if contra-union?
                        ; non-error-checking edge
                        (lambda (out-label inflowing-label tunnel-label)
                          ; values sink => no use for out-label here
                          (if (label-values? inflowing-label)
                              ; the label-list of multiple values that might flow in might contain
                              ; more than one value, but that's ok.
                              (add-edge-and-propagate-set-through-edge
                               (label-values-label inflowing-label)
                               values-content-edge)
                              ; we are in contravariant position, so the value x that flows out
                              ; is unique and equivalent to (values x). So we simulate that. Note
                              ; that multiple values are in fact label-lists of labels inside a
                              ; values label, so we have to simulate the label-list part...
                              (let* ([null-label (make-label-cst #f #f #f #f #t
                                                                 term
                                                                 (make-hash-table)
                                                                 (make-hash-table)
                                                                 '())]
                                     [cons-label (make-label-cons #f #f #f #f #t
                                                                  term
                                                                  (make-hash-table)
                                                                  (make-hash-table)
                                                                  inflowing-label
                                                                  null-label)])
                                (initialize-label-set-for-value-source null-label)
                                (initialize-label-set-for-value-source cons-label)
                                (add-edge-and-propagate-set-through-edge
                                 cons-label
                                 values-content-edge))))
                        ; error checking edge
                        (lambda (out-label inflowing-label tunnel-label)
                          ; values sink => no use for out-label here
                          (if (label-values? inflowing-label)
                              (add-edge-and-propagate-set-through-edge
                               (label-values-label inflowing-label)
                               values-content-edge)
                              (let* ([null-label (make-label-cst #f #f #f #f #t
                                                                 term
                                                                 (make-hash-table)
                                                                 (make-hash-table)
                                                                 '())]
                                     [cons-label (make-label-cons #f #f #f #f #t
                                                                  term
                                                                  (make-hash-table)
                                                                  (make-hash-table)
                                                                  inflowing-label
                                                                  null-label)])
                                (initialize-label-set-for-value-source null-label)
                                (initialize-label-set-for-value-source cons-label)
                                (add-edge-and-propagate-set-through-edge
                                 cons-label
                                 values-content-edge)))))
                    ; vector sink
                    (gensym))])
             ; useless, when you think about it...
             ;(unless contra-union?
             ;  (associate-label-with-type values-label
             ;                             (make-type-values (make-type-cst 'top))
             ;                             delta-flow))
             (add-edge-and-propagate-set-through-edge values-label values-edge)
             values-label)]
          [(type-union? type)
           (let* ([elt-labels (map (lambda (elt-type)
                                     ; reconstruct without error checking
                                     ; XXX this does not work in the case of a flow var,
                                     ; because associate-label-with-type has already been done.
                                     (reconstruct-graph-from-type
                                      sba-state
                                      elt-type delta-flow delta-type label term #f #t))
                                   (type-union-elements type))]
                  [union-label (create-simple-prim-label term)]
                  [union-label-in-between (create-simple-prim-label term)]
                  [simple-non-error-checking-edge (create-simple-edge union-label-in-between)]
                  [error-checking-edge
                   (cons
                    (lambda (out-label inflowing-label tunnel-label)
                      (if ((car simple-non-error-checking-edge) out-label inflowing-label tunnel-label)
                          (begin
                            #t)
                          (begin
                            (set-error-for-label sba-state
                                                 label
                                                 'red
                                                 (format "value ~a not a subtype of union ~a inside application of ~a"
                                                         (pp-type sba-state (get-type-from-label
                                                                             sba-state inflowing-label)
                                                                  'type-union1)
                                                         ;(syntax-object->datum
                                                         ; (label-term inflowing-label))
                                                         (pp-type sba-state type 'type-union2)
                                                         (syntax-object->datum term)))
                            ; stop error up-propagation
                            #t)))
                    (cdr simple-non-error-checking-edge))])
             ; edges can't propagate multiple values
             (for-each (lambda (elt-label)
                         (add-edge-and-propagate-set-through-edge
                          union-label-in-between
                          (extend-edge-for-values sba-state (create-simple-edge elt-label))))
                       elt-labels)
             (if contra-union?
                 ; union inside a union, so forget about checking at this level
                 union-label-in-between
                 (begin
                   (add-edge-and-propagate-set-through-edge
                    union-label
                    (extend-edge-for-values sba-state error-checking-edge))
                   union-label)))]
          [(type-rec? type)
           (let* ([clauses-vars&labels (map (lambda (type-var)
                                              (cons type-var (create-simple-prim-label term)))
                                            (type-rec-vars type))]
                  [all-var-labels (append clauses-vars&labels delta-type)]
                  [clauses-types-labels (map (lambda (clause-type)
                                               (reconstruct-graph-from-type
                                                sba-state
                                                clause-type delta-flow all-var-labels label term #f #f))
                                             (type-rec-types type))]
                  [rec-body-label (reconstruct-graph-from-type
                                   sba-state
                                   (type-rec-body type) delta-flow all-var-labels label term #f #f)]
                  [rec-label (create-simple-prim-label term)])
             ; note: we never check whether all clauses are used. If they are not, they'll be
             ; garbage collected after we return from here.
             (for-each (lambda (clause-var-type&label clause-type-label)
                         (add-edge-and-propagate-set-through-edge
                          (cdr clause-var-type&label)
                          (create-simple-edge clause-type-label)))
                       clauses-vars&labels clauses-types-labels)
             (unless contra-union?
               (associate-label-with-type sba-state rec-body-label type delta-flow))
             ; note: if type is the type corresponding, say, to a list, then if (list 1 2 3)
             ; flows into rec-label, then rec-body-label will contain (list 1 2 3), (list 2 3),
             ; (list 3), and ().
             (add-edge-and-propagate-set-through-edge
              rec-label
              (extend-edge-for-values sba-state (create-simple-edge rec-body-label)))
             rec-label)]
          [(type-empty? type)
           (let ([empty-label (create-simple-prim-label term)])
             ; propagation to such a label always works, so post checking is necessary
             ; note that propagation always works because we don't do any type-based
             ; filtering.
             (unless contra-union?
               (associate-label-with-type sba-state empty-label type delta-flow))
             empty-label)]
          [else (error 'reconstruct-graph-from-type "unknown contravariant type for primitive ~a: ~a"
                       (syntax-e term) type)]
          )))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POST ANALYSIS TYPE CHECKING FOR PRIMITIVES
  
  ; sba-state label type (hash-table-of type-flow-var (cons label type)) -> void
  ; Note that we don't store the type but only the handle.
  (define (associate-label-with-type sba-state label type delta-flow)
    (hash-table-put! (sba-state-label->types sba-state)
                     label
                     (cons (hc:hashcons-type (sba-state-hashcons-tbl sba-state)
                                             (subst-vals/flow-vars type delta-flow))
                           delta-flow)))
  
  ; sba-state -> void
  ; post analysis checking of primitives inputs and outputs
  (define (check-primitive-types sba-state)
    (hash-table-for-each
     (sba-state-label->types sba-state)
     (lambda (label expected-type&delta)
       (subtype sba-state
                (get-type-from-label sba-state label)
                (car expected-type&delta)
                (cdr expected-type&delta) #t label))))
  
  ; (hashtableof symbol (cons (listof symbol) (top -> boolean)) symbol -> (listof symbol)
  ; computes the complete (closed) list of subtypes for type-entry
  (define (close-subtypes table type-name)
    (let ([new-type-entry (hash-table-get table type-name cst:thunk-false)])
      (if new-type-entry
          (car new-type-entry)
          (let* ([original-type-entry (cdr (assq type-name *basic-types*))]
                 [new-type-entry
                  (cons (list:foldl
                         (lambda (type-name type-names-list-so-far)
                           (merge-lists (close-subtypes table type-name)
                                        type-names-list-so-far))
                         ; not strickly necessary for the way we use the function later
                         (list type-name)
                         (car original-type-entry))
                        (cadr original-type-entry))])
            (hash-table-put! table type-name new-type-entry)
            (car new-type-entry)))))
  
  (define/contract subt
    (sba-state? hc:hashcons-table? handle? handle? any/c set? ;(listof (cons/p handle? handle?))
                . -> . boolean?)
    (let ([subtyping-table
           (let ([table (make-hash-table)])
             ; the entry for 'top should appear first. Everything will get
             ; put into the table as we process this one.
             (close-subtypes table (caar *basic-types*))
             table)]
          [memo-table (make-hash-table 'equal)])
      (lambda (sba-state hashcons-tbl t1-handle t2-handle delta-flow trace)
        (let* ([t1 (hc:get-type hashcons-tbl t1-handle)]
               [t2 (hc:get-type hashcons-tbl t2-handle)]
               [subt (lambda (handle1 handle2)
                       (set-set trace (cons t1-handle t2-handle))
                       ;		       (subt sba-state hashcons-tbl handle1 handle2 delta-flow (cons (cons t1-handle t2-handle) trace)))
                       (let ([v (subt sba-state hashcons-tbl handle1 handle2 delta-flow trace)])
                         (set-remove trace (cons t1-handle t2-handle))
                         v))]
               [get-list-of-handle
                (lambda (handle)
                  (let* ([fake-type-var (make-type-var (gensym) #f #f)]
                         [type (make-type-rec  (list fake-type-var)
                                               (list (make-type-union (list (make-type-cst '())
                                                                            (make-type-cons handle fake-type-var))))
                                               fake-type-var)])
                    (hc:hashcons-type hashcons-tbl type)))])
          (if (hash-table-get memo-table (cons t1-handle t2-handle) cst:thunk-false)
              (hash-table-get memo-table (cons t1-handle t2-handle) cst:thunk-false)
              (let ([subtype-value
                     (or
                      ; basic cases
                      (or (= t1-handle t2-handle)
                          (and (type-cst? t2) (eq? (type-cst-type t2) 'top))
                          ;(and (type-cst? t1) (eq? (type-cst-type t1) 'bottom))
                          (type-empty? t1) ; bottom
                          (set-in? trace (cons t1-handle t2-handle)))
                      ; constants
                      (and (type-cst? t1) (type-cst? t2)
                           (let* ([t1 (type-cst-type t1)]
                                  [t2 (type-cst-type t2)]
                                  [t2-entry (hash-table-get subtyping-table t2 cst:thunk-false)])
                             (if t2-entry
                                 ; t2 is a symbolic type (like number)
                                 (if (or (memq t1 (car t2-entry)) ; for symbolic t1: real <= number
                                         ((cdr t2-entry) t1))     ; for scheme t1: 3 <= number
                                     #t #f)
                                 ; t2 is an immediate type (like 5), and we already know t1 is not bottom
                                 ; so t1 and t2 have to be equal
                                 (eq? t1 t2))))
                      ; cons
                      (and (type-cons? t1) (type-cons? t2)
                           (subt (type-cons-car t1) (type-cons-car t2))
                           (subt (type-cons-cdr t1) (type-cons-cdr t2)))
                      ; vector
                      (and (type-vector? t1) (type-vector? t2)
                           (subt (type-vector-element t1) (type-vector-element t2)))
                      ; case-lambda
                      (and (type-case-lambda? t1) (type-case-lambda? t2)
                           (util:ormap4-vector
                            (lambda (t1-rest-arg? t1-req-arg t1-args t1-exp)
                              (if t1-rest-arg?
                                  (util:andmap4-vector
                                   (lambda (t2-rest-arg? t2-req-arg t2-args t2-exp)
                                     (if t2-rest-arg?
                                         ; both t1 and t2 have rest args
                                         (or (and (< t1-req-arg t2-req-arg)
                                                  (subt t1-exp t2-exp)
                                                  ; contravariant
                                                  (util:andmap2-vector-interval subt t2-args t1-args 0 t1-req-arg)
                                                  (let ([t1-rest-arg (vector-ref t1-args t1-req-arg)])
                                                    (and
                                                     (util:andmap-vector-interval
                                                      (lambda (t2-arg)
                                                        ; contravariant
                                                        (subt (get-list-of-handle t2-arg) t1-rest-arg))
                                                      t2-args t1-req-arg t2-req-arg)
                                                     (subt (vector-ref t2-args t2-req-arg) ; t2-rest-arg
                                                           t1-rest-arg))))
                                             (and (= t1-req-arg t2-req-arg)
                                                  (subt t1-exp t2-exp)
                                                  ; contravariant
                                                  (util:andmap2-vector subt t2-args t1-args))
                                             (and (> t1-req-arg t2-req-arg)
                                                  (subt t1-exp t2-exp)
                                                  ; contravariant
                                                  (util:andmap2-vector-interval subt t2-args t1-args 0 t2-req-arg)
                                                  (let ([t2-rest-arg (vector-ref t2-args t2-req-arg)])
                                                    (and
                                                     (util:andmap-vector-interval
                                                      (lambda (t1-arg)
                                                        (subt t2-rest-arg (get-list-of-handle t1-arg)))
                                                      t1-args t2-req-arg t1-req-arg)
                                                     (subt t2-rest-arg
                                                           (vector-ref t1-args t1-req-arg) ; t1-rest-arg
                                                           )))))
                                         ;; t1 has rest-args, t2 has NO rest-args
                                         (and (<= t1-req-arg t2-req-arg)
                                              (subt t1-exp t2-exp)
                                              ; t1 has a rest arg, t2 has a fixed number of args
                                              ; so we need to check all the required args, and
                                              ; check the rest arg specially, since the rest arg
                                              ; of t1 is automatically wrapped inside a list.
                                              ; contravariant
                                              (util:andmap2-vector-interval subt t2-args t1-args 0 t1-req-arg)
                                              ; contravariant
                                              (subt (hc:hashcons-type hashcons-tbl
                                                                      (list:foldr make-type-cons
                                                                                  (make-type-cst '())
                                                                                  (util:interval->list t2-args t1-req-arg
                                                                                                       (vector-length t2-args))))
                                                    (vector-ref t1-args t1-req-arg) ; rest arg
                                                    ))))
                                   (type-case-lambda-rest-arg?s t2)
                                   (type-case-lambda-req-args t2)
                                   (type-case-lambda-argss t2)
                                   (type-case-lambda-exps t2))
                                  ; t1 has no rest-args
                                  (util:andmap4-vector
                                   (lambda (t2-rest-arg? t2-req-arg t2-args t2-exp)
                                     (if t2-rest-arg?
                                         (and (>= t1-req-arg t2-req-arg)
                                              (subt t1-exp t2-exp)
                                              ; t1 has a fixed number of args, t2 has a rest arg
                                              ; so we need to check all the required args, and
                                              ; check the rest arg specially, since the rest arg
                                              ; of t2 is automatically wrapped inside a list.
                                              ; contravariant
                                              (util:andmap2-vector-interval subt t2-args t1-args 0 t2-req-arg)
                                              ; contravariant
                                              (let ([t2-rest-arg (vector-ref t2-args t2-req-arg)])
                                                (util:andmap-vector-interval
                                                 (lambda (t1-arg)
                                                   (subt t2-rest-arg (get-list-of-handle t1-arg)))
                                                 t1-args t2-req-arg (vector-length t1-args))))
                                         ; t1 and t2 have a fixed number of args
                                         (and (= t1-req-arg t2-req-arg)
                                              (subt t1-exp t2-exp)
                                              ; contravariant
                                              (util:andmap2-vector subt t2-args t1-args))))
                                   (type-case-lambda-rest-arg?s t2)
                                   (type-case-lambda-req-args t2)
                                   (type-case-lambda-argss t2)
                                   (type-case-lambda-exps t2))))
                            (type-case-lambda-rest-arg?s t1)
                            (type-case-lambda-req-args t1)
                            (type-case-lambda-argss t1)
                            (type-case-lambda-exps t1)))
                      ; the order of the following two rules matters, because, for
                      ; example: (union 1 2) is a subtype of (union 1 2 3), but is
                      ; not a subtype of either 1, 2 or 3. On the other hand both 1
                      ; and 2 are subtypes of (union 1 2 3), so if both t1 and t2
                      ; are unions, we have to split t1 first.
                      (and (type-union? t1)
                           (andmap (lambda (t1-elt) (subt t1-elt t2-handle)) (type-union-elements t1)))
                      (and (type-union? t2)
                           (ormap (lambda (t2-elt) (subt t1-handle t2-elt)) (type-union-elements t2)))
                      ; multiple values
                      (and (type-values? t1) (type-values? t2)
                           (subt (type-values-type t1) (type-values-type t2)))
                      (and (type-promise? t1) (type-promise? t2)
                           (subt (type-promise-value t1) (type-promise-value t2)))
                      (and (type-struct-type? t1) (type-struct-type? t2)
                           ; can't use strutural equivalence here because of genericity
                           (or (eq? (type-struct-type-type-label t1) (type-struct-type-type-label t2))
                               (let ([t1-parent-label (label-struct-type-parent
                                                       (type-struct-type-type-label t1))])
                                 (if t1-parent-label
                                     (subt (get-type-from-label sba-state t1-parent-label)
                                           t2-handle)
                                     #f))))
                      (and (type-struct-value? t1) (type-struct-type? t2)
                           (subt (get-type-from-label sba-state (type-struct-value-type-label t1))
                                 t2-handle))
                      (and (type-flow-var? t2)
                           (let ([label&type (lookup-flow-var-in-env delta-flow t2)])
                             (subt t1 (hc:hashcons-type hashcons-tbl (cdr label&type)))))
                      ;; Subt works on previously hashcons values,
                      ;; i.e. all cycles are implicit in the table
                      (and (or (type-flow-var? t1) (type-flow-var? t2)
                               (type-var? t1) (type-var? t2)
                               (type-rec? t1) (type-rec? t2)
                               (type-scheme? t1) (type-scheme? t2))
                           (error 'subt "Unexpected non-hashconsed types: ~a ~a" t1 t2)))])
                (hash-table-put! memo-table (cons t1-handle t2-handle) subtype-value)
                subtype-value))))))
  
  ; called when t2 is a (flow var free) type instead of a handle
  (define/contract subtype-type
    (sba-state? handle? hc:hashcons-type? any/c boolean? (or/c false/c label?) . -> . boolean?)
    (lambda (sba-state t1-handle t2 delta-flow error? label)
      (subtype sba-state t1-handle
               (hc:hashcons-type (sba-state-hashcons-tbl sba-state) t2)
               delta-flow error? label)))
  
  (define/contract subtype
    (sba-state? handle? handle? any/c boolean? (or/c false/c label?) . -> . boolean?)
    (lambda (sba-state t1-handle t2-handle delta-flow error? label)
      (if (subt sba-state (sba-state-hashcons-tbl sba-state) t1-handle t2-handle delta-flow (set-make 'equal))
          #t
          (begin
            (when error?
              (set-error-for-label sba-state
                                   label
                                   'red
                                   (format "~a not a subtype of ~a"
                                           (pp-type sba-state t1-handle 'subtype)
                                           (pp-type sba-state t2-handle delta-flow))))
            #f))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GUI INTERFACE
  
  ; sba-state -> symbol
  (define (create-type-var-name sba-state)
    (let ([new-counter (sba-state-type-var-counter sba-state)])
      (set-sba-state-type-var-counter! sba-state (add1 new-counter))
      (string->symbol (string-append "a" (number->string new-counter)))))
  
  ; label -> positive-int
  ; returns start location of term associated with label
  (define (get-mzscheme-position-from-label label)
    (syntax-position (label-term label)))
  
  ; label -> boolean
  (define (is-label-atom? label)
    (let ([stx-l (syntax-e (label-term label))])
      (or (not (pair? stx-l)) ; identifier
          (let ([term-type (syntax-e (car stx-l))])
            (or (eq? term-type '#%datum)
                (eq? term-type '#%top)
                (eq? term-type 'quote))))))
  
  ; label -> (or/c number #f)
  (define (get-span-from-label label)
    (syntax-span (label-term label)))
  
  ; sba-state label (or/c 'red 'green 'orange) string -> void
  (define (set-error-for-label sba-state label gravity message)
    (err:error-table-set (sba-state-errors sba-state)
                         (list label)
                         gravity
                         message))
  
  ; sba-state label -> (listof sba-error)
  ; extracts error messages.
  (define (get-errors-from-label sba-state label)
    (err:error-table-get (sba-state-errors sba-state) label))
  
  ; label -> exact-non-negative-integer
  (define (get-source-from-label label)
    (syntax-source (label-term label)))
  
  ; label sba-state -> void
  (define (add-type-var-to-label label sba-state)
    (unless (label-type-var label)
      (set-label-type-var! label (make-type-var (create-type-var-name sba-state) #f #f))))
  
  ; label set -> void
  ; if the reachable set has already been computed, re-use it, otherwise compute it
  ; (unless we detected a cycle).
  (define (reachL label set)
    (if (and (label-type-var label)
             (type-var-reach (label-type-var label)))
        (set-union set (type-var-reach (label-type-var label)) 'first)
        (unless (set-in? set label)
          (set-set set label)
          (reachU label set))))
  
  ; label set -> void
  ; a label for a value constructor has itself in its own value set, so reachL above might
  ; already have put that label in set, but a simple label does not appear in its own set
  ; so we still have to add the content of its set to set.  Hence the #f below.
  (define (reachU label set)
    (hash-table-for-each (label-set label)
                         (lambda (value-label arrows)
                           (set-set set value-label #f)
                           (reachT value-label set))))
  ; label set -> void
  (define (reachT label set)
    (cond
      ;[(label-cst? label) cst:void]
      [(label-cons? label) (begin (reachL (label-cons-car label) set)
                                  (reachL (label-cons-cdr label) set))]
      [(label-vector? label) (reachL (label-vector-element label) set)]
      [(label-promise? label) (reachL (label-promise-value label) set)]
      [(label-values? label) (reachL (label-values-label label) set)]
      [(label-case-lambda? label) (for-each (lambda (args-labels exp-label)
                                              (for-each (lambda (l) (reachL l set)) args-labels)
                                              (reachL exp-label set))
                                            (label-case-lambda-argss label)
                                            (label-case-lambda-exps label))]
      [(label-struct-value? label) (begin (reachL (label-struct-value-type label) set)
                                          (for-each (lambda (l) (reachL l set)) (label-struct-value-fields label)))]
      ;[(label-struct-type? label) cst:void]
      ;[else cst:void]
      ))
  
  ; label sba-state -> (set-of label)
  ; reachable lables from a given label
  ; we know the label has a type var because add-type-var-to-label has been called
  ; already in get-type-from-label.
  ; Note that this is the only place where we set the type-var-reach, so if reachL above
  ; find a set, we know that set already contains everything we need.
  (define (reachable-labels-from-label label)
    (let ([set (type-var-reach (label-type-var label))])
      (if set
          set
          (let ([set (set-make)])
            (reachL label set)
            (set-type-var-reach! (label-type-var label) set)
            set))))
  
  ; label -> (or/c type-var handle)
  ; the label better have a type-var...
  (define (get-handle-or-type-var label)
    (let* ([type-var (label-type-var label)]
           [handle (type-var-handle type-var)])
      (if handle
          ;(begin (printf ".")
          handle
          ;)
          type-var)))
  
  ; label (listof labels) -> type-rec
  (define (typeL label reachable-labels)
    (make-type-rec (map label-type-var reachable-labels)
                   (map typeU reachable-labels)
                   (get-handle-or-type-var label)))
  
  ; label -> type-union
  ; label-set should move from a hash-table to an assoc-set, then we can use
  ; assoc-set-cardinality instead of going through the list twice.
  (define (typeU label)
    (let* ([union-content (hash-table-map (label-set label) (lambda (label arrows) (typeT label)))]
           [union-length (length union-content)])
      (cond
        [(= union-length 0) (make-type-empty)]
        [(= union-length 1) (car union-content)]
        [else (make-type-union union-content)])))
  
  ; label -> type
  (define (typeT label)
    (cond
      [(label-cst? label) (make-type-cst (label-cst-value label))]
      [(label-cons? label) (make-type-cons (get-handle-or-type-var (label-cons-car label))
                                           (get-handle-or-type-var (label-cons-cdr label)))]
      [(label-vector? label) (make-type-vector (get-handle-or-type-var (label-vector-element label)))]
      [(label-promise? label) (make-type-promise (get-handle-or-type-var (label-promise-value label)))]
      [(label-values? label) (make-type-values (get-handle-or-type-var (label-values-label label)))]
      [(label-case-lambda? label) (make-type-case-lambda (label-case-lambda-rest-arg?s label)
                                                         (label-case-lambda-req-args label)
                                                         (map (lambda (args)
                                                                (map get-handle-or-type-var args))
                                                              (label-case-lambda-argss label))
                                                         (map get-handle-or-type-var (label-case-lambda-exps label)))]
      [(label-struct-value? label) (make-type-struct-value (label-struct-value-type label)
                                                           (map get-handle-or-type-var (label-struct-value-fields label)))]
      [(label-struct-type? label) (make-type-struct-type label)]
      [else (error 'typeT "unknown label: ~a" label)]))
  
  ; sba-state label -> type
  ; computes type for label, computes the corresponding handle, and memoize it
  (define (get-type-from-label sba-state label)
    (add-type-var-to-label label sba-state)
    (or (type-var-handle (label-type-var label))
        (let* (;[_ (begin (print-struct #t)(printf "T: ~a ~a ~a " (type-var-name (label-type-var label))
               ;                                   (syntax-position (label-term label))
               ;                                   (syntax-object->datum (label-term label))))]
               ;[start (current-milliseconds)]
               [reachable-labels (set-map (reachable-labels-from-label label)
                                          (lambda (l) (add-type-var-to-label l sba-state) l))]
               ;[_ (begin (print-struct #t)(printf "R: ~a~n" (map (lambda (l) (type-var-name (label-type-var l))) reachable-labels)))]
               ;[_ (printf "~a " (- (current-milliseconds) start))]
               ;[start (current-milliseconds)]
               [reconstructed-type (typeL label reachable-labels)]
               ;[_ (begin (print-struct #t)(printf "T: ~a~n" (ppp-type reconstructed-type 'blah)))]
               ;[_ (printf " ~a~n" (- (current-milliseconds) start))]
               ;[start (current-milliseconds)]
               [handle (hc:hashcons-type (sba-state-hashcons-tbl sba-state) reconstructed-type)]
               ;[_ (printf "HC-Time= ~a~n" (- (current-milliseconds) start))]
               )
          ; XXX memoization
          (set-type-var-handle! (label-type-var label) handle)
          handle)))
  
  ; type (or/c (hash-table-of type-flow-var (cons label type)) symbol) -> string
  ; type pretty printer
  ; delta-flow is the flow variable environment, or a symbol if no flow environment
  ; was available at the time of the call.
  (define (pp-type sba-state type delta-flow)
    (let ([pretty-string (hc:handle->string (sba-state-hashcons-tbl sba-state) type
                                            (lambda (h1 h2)
                                              (subtype sba-state h1 h2 #f #f #f)))
                         ])
      ;(printf "H: ~a~nP: ~a~n~n" type foo)
      pretty-string))
  
  ;  (require (prefix string: (lib "string.ss")))
  ;  (define (ppp-type type delta-flow)
  ;      (cond
  ;        [(type-empty? type) "_"]
  ;        [(type-cst? type)
  ;         ; can be a complex sexp if (quote sexp) is in the input
  ;         (string:expr->string (type-cst-type type))]
  ;        ;      (let ([val (type-cst-type type)])
  ;        ;        (cond
  ;        ;          [(number? val) (number->string val)]
  ;        ;          [(symbol? val) (symbol->string val)]
  ;        ;          [(string? val) (string-append "\"" val "\"")]
  ;        ;          [(void? val) "void"]
  ;        ;          [else (error 'ppp-type "unknown datum: ~a" val)]))]
  ;        [(type-cons? type)
  ;         (string-append "(cons "
  ;                        (ppp-type (type-cons-car type) delta-flow) " "
  ;                        (ppp-type (type-cons-cdr type) delta-flow) ")")]
  ;        [(type-vector? type)
  ;         (string-append "(vector " (ppp-type (type-vector-element type) delta-flow) ")")]
  ;        [(type-promise? type)
  ;         (string-append "(promise "
  ;                        ; skipping the thunk inside the promise (we know it's always a
  ;                        ; thunk because delay is a macro...) Note that the promise might
  ;                        ; be empty, for now, so we have to test that...
  ;                        (let ([promise-value-type (type-promise-value type)])
  ;                          (if (type-case-lambda? promise-value-type)
  ;                              (ppp-type (car (type-case-lambda-exps promise-value-type)) delta-flow)
  ;                              (ppp-type promise-value-type delta-flow)))
  ;                        ")")]
  ;        [(type-case-lambda? type)
  ;         (string-append
  ;          "(case-lambda "
  ;          (list:foldr
  ;           (lambda (rest-arg? formal-args-types body-exp-type str)
  ;             (string-append
  ;              "["
  ;              (list:foldr
  ;               (lambda (formal-arg-type str)
  ;                 (string-append
  ;                  (ppp-type formal-arg-type delta-flow)
  ;                  " "
  ;                  str))
  ;               ""
  ;               formal-args-types)
  ;              (if rest-arg?
  ;                  "*-> "
  ;                  "-> ")
  ;              (ppp-type body-exp-type delta-flow)
  ;              "]"
  ;              ;(if (string=? str "")
  ;              ;  ""
  ;              ;  " ")
  ;              str))
  ;           ""
  ;           (type-case-lambda-rest-arg?s type)
  ;           (type-case-lambda-argss type)
  ;           (type-case-lambda-exps type))
  ;          ")")]
  ;        [(type-var? type)
  ;         (symbol->string (type-var-name type))]
  ;        [(type-flow-var? type)
  ;         (error 'ppp-type "flow var: ~a~n" (type-flow-var-name type))
  ;         (ppp-type (cdr (lookup-flow-var-in-env delta-flow type)) delta-flow)]
  ;        [(type-union? type)
  ;         (string-append
  ;          "(union "
  ;          (list:foldr
  ;           (lambda (union-element str)
  ;             (string-append
  ;              (ppp-type union-element delta-flow)
  ;              (if (string=? str ")")
  ;                  ""
  ;                  " ")
  ;              str))
  ;           ")"
  ;           (type-union-elements type)))]
  ;        [(type-values? type)
  ;         (let ([values-type (type-values-type type)])
  ;           (cond
  ;             [(type-empty? values-type)
  ;              (ppp-type values-type delta-flow)]
  ;             [(and (type-cst? values-type) (eq? (type-cst-type values-type) 'top))
  ;              (ppp-type values-type delta-flow)]
  ;             [else
  ;              (let* ([values-types-list (type-list-map cst:id (type-values-type type))]
  ;                     [values-types-list-length (length values-types-list)])
  ;                (cond
  ;                  [(zero? values-types-list-length)
  ;                   (ppp-type (make-type-empty) delta-flow)]
  ;                  [(= values-types-list-length 1)
  ;                   (ppp-type (car values-types-list) delta-flow)]
  ;                  [else (string-append
  ;                         "(values "
  ;                         (list:foldr
  ;                          (lambda (type str)
  ;                            (string-append (ppp-type type delta-flow)
  ;                                           (if (string=? str ")")
  ;                                               ""
  ;                                               " ")
  ;                                           str))
  ;                          ")"
  ;                          values-types-list))]))]))]
  ;        [(type-rec? type)
  ;         (string-append
  ;          "(rec-type ("
  ;          (list:foldr
  ;           (lambda (var type str)
  ;             (string-append
  ;              "["
  ;              (symbol->string (type-var-name var))
  ;              " "
  ;              ; poor man's type beautifier
  ;              (if (and (type-union? type)
  ;                       (= (length (type-union-elements type)) 2)
  ;                       (or (and (type-cst? (car (type-union-elements type)))
  ;                                (null? (type-cst-type (car (type-union-elements type))))
  ;                                (type-cons? (cadr (type-union-elements type)))
  ;                                (type-var? (type-cons-cdr (cadr (type-union-elements type))))
  ;                                (eq? (type-var-name (type-cons-cdr (cadr (type-union-elements type))))
  ;                                     (type-var-name var)))
  ;                           (and (type-cst? (cadr (type-union-elements type)))
  ;                                (null? (type-cst-type (cadr (type-union-elements type))))
  ;                                (type-cons? (car (type-union-elements type)))
  ;                                (type-var? (type-cons-cdr (car (type-union-elements type))))
  ;                                (eq? (type-var-name (type-cons-cdr (car (type-union-elements type))))
  ;                                     (type-var-name var)))))
  ;                  (string-append
  ;                   "(listof "
  ;                   (ppp-type (if (type-cst? (car (type-union-elements type)))
  ;                                (type-cons-car (cadr (type-union-elements type)))
  ;                                (type-cons-car (car (type-union-elements type))))
  ;                            delta-flow)
  ;                   ")")
  ;                  (ppp-type type delta-flow))
  ;              (if (string=? str ") ")
  ;                  "]"
  ;                  "] ")
  ;              str))
  ;           ") "
  ;           (type-rec-vars type)
  ;           (type-rec-types type))
  ;          (ppp-type (type-rec-body type) delta-flow)
  ;          ")")]
  ;        [(type-struct-value? type)
  ;         (string-append
  ;          "#(struct:"
  ;          (symbol->string (label-struct-type-name (type-struct-value-type-label type)))
  ;          " "
  ;          (list:foldr
  ;           (lambda (elt-type str)
  ;             (string-append
  ;              (ppp-type elt-type delta-flow)
  ;              (if (string=? str ")")
  ;                  ""
  ;                  " ")
  ;              str))
  ;           ")"
  ;           (type-struct-value-types type)))]
  ;        [(type-struct-type? type)
  ;         (string-append
  ;          "#<struct-type:"
  ;          (symbol->string (label-struct-type-name (type-struct-type-type-label type)))
  ;          ">")]
  ;        [else (error 'ppp-type "unknown type: ~a" type)]))
  
  
  ; label (listof label) -> (listof label)
  ; returns list of labels from which labels in label's set went in
  ; the trace is necessary to prevent the search for original parents to loop forever when
  ; inside recursive code generated by a macro. Despite that the search might still use
  ; exponential time when only using the trace because it explores all possibles paths at
  ; all possible labels when exploring the graph recursively. The running time is tremendously
  ; helped by adding the memoization: we compute the result set for a given label only once
  ; and always reuse that result in the future without ever searching the piece of graph
  ; behind the label again.
  ; Note: could probably be made even faster if the trace was a set and we kept it around
  ; until the final result is computed, so as not to re-explore pieces of graphs we have
  ; just seen but are reaching through another path.  Should be good enough for noe since
  ; we memoize the final result anyway.
  (define (get-parents-from-label label trace)
    (if (label-parents label)
        (label-parents label)
        (if (memq label trace)
            '()
            (let ([result (set-make)])
              (for-each
               (lambda (unfiltered-parents-for-current-set-element)
                 (let* ([direct-parents-without-primitive-labels
                         (list:filter (lambda (label)
                                        (not (label-prim? label)))
                                      unfiltered-parents-for-current-set-element)]
                        [direct-or-indirect-original-parents
                         (list:foldr
                          (lambda (direct-parent original-parents-so-far)
                            (if (gui-registerable? direct-parent)
                                (cons direct-parent original-parents-so-far)
                                (merge-lists (get-parents-from-label direct-parent (cons label trace))
                                             original-parents-so-far)))
                          '()
                          direct-parents-without-primitive-labels)])
                   (for-each (lambda (parent)
                               (set-set result parent #f))
                             direct-or-indirect-original-parents)))
               (hash-table-map (label-set label)
                               (lambda (label arrows)
                                 (arrows-in arrows))))
              (let ([final-result (set-map result cst:id)])
                (set-label-parents! label final-result)
                final-result)))))
  
  ; label (listof label) -> (listof label)
  ; should be abstracted with the above...
  ; differences are arrows-in vs arrows-out and
  ; label-parents/set-label-parents! vs label-children/set-label-children!
  (define (get-children-from-label label trace)
    (if (label-children label)
        (label-children label)
        (if (memq label trace)
            '()
            (let ([result (set-make)])
              (for-each
               (lambda (unfiltered-children-for-current-set-element)
                 (let* ([direct-children-without-primitive-labels
                         (list:filter (lambda (label)
                                        (not (label-prim? label)))
                                      unfiltered-children-for-current-set-element)]
                        [direct-or-indirect-original-children
                         (list:foldr
                          (lambda (direct-child original-children-so-far)
                            (if (gui-registerable? direct-child)
                                (cons direct-child original-children-so-far)
                                (merge-lists (get-children-from-label direct-child (cons label trace))
                                             original-children-so-far)))
                          '()
                          direct-children-without-primitive-labels)])
                   (for-each (lambda (child)
                               (set-set result child #f))
                             direct-or-indirect-original-children)))
               (hash-table-map (label-set label)
                               (lambda (label arrows)
                                 (arrows-out arrows))))
              (let ([final-result (set-map result cst:id)])
                (set-label-children! label final-result)
                final-result)))))
  
  
  ; (listof label) -> (listof (list label label string))
  ; not really fast but good enough for now.
  ; XXX should combine this with the above get-parents/children
  (define (get-arrows-from-labels labels)
    (delete-duplicates
     (append! (apply append! (map (lambda (label)
                                    (map (lambda (parent)
                                           (list parent label "blue"))
                                         (list:filter (lambda (parent)
                                                        (not (memq parent labels)))
                                                      (get-parents-from-label label '()))))
                                  labels))
              (apply append! (map (lambda (label)
                                    (map (lambda (child)
                                           (list label child "blue"))
                                         (list:filter (lambda (child)
                                                        (not (memq child labels)))
                                                      (get-children-from-label label '()))))
                                  labels)))))
  
  ; (listof (cons top (cons top (listof top)))) -> (listof (cons top (cons top (listof top))))
  (define (delete-duplicates l)
    (if (null? l)
        l
        (let ([elt (car l)])
          (cons elt
                (delete-duplicates
                 (let ([elt-s (car elt)]
                       [elt-e (cadr elt)])
                   (list:filter (lambda (other-elt)
                                  (or (not (eq? elt-s (car other-elt)))
                                      (not (eq? elt-e (cadr other-elt)))))
                                (cdr l))))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DRIVER
  
  ;    ; port value -> void
  ;    (define (sba-driver port source)
  ;      (let ([start (current-milliseconds)])
  ;        (read-and-analyze port source)
  ;        (check-primitive-types)
  ;        (printf "time: ~a ms~n" (- (current-milliseconds) start)))
  ;      )
  ;  
  ;    ; port value -> void
  ;    ; read and analyze, one syntax object at a time
  ;    (define (read-and-analyze port source)
  ;      (let ([stx-obj (read-syntax source port)])
  ;        ;(unless (eof-object? stx-obj)
  ;        ;  (begin (printf "sba-driver in: ~a~n" (syntax-object->datum stx-obj))
  ;        ;         (printf "sba-driver analyzed: ~a~n~n" (syntax-object->datum (expand stx-obj)))
  ;        ;         (printf "sba-driver out: ~a~n~n" (create-label-from-term sba-state (expand stx-obj) '() #f)))
  ;        ;  (read-and-analyze port source))))
  ;        (if (eof-object? stx-obj)
  ;          '()
  ;          (cons (create-label-from-term sba-state (expand stx-obj) '() #f)
  ;                (read-and-analyze port source)))))
  ;    
  ;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PERFORMANCE TEST
  ;    
  ;    ; (: test-i (nothing -> void))
  ;    ; parse expression interactively
  ;    (define (test-i)
  ;      (sba-driver (current-input-port) 'interactive))
  ;    
  ;    ; (: test-f (string -> (listof Ast)))
  ;    (define (test-f filename)
  ;      (let ([port (open-input-file filename)])
  ;        (sba-driver port filename)
  ;        (close-input-port port)))
  ;    
  ;  (let* ([path (build-path (collection-path "mrflow") "tests")]
  ;         [files (list:filter (lambda (file)
  ;                               (and (> (string-length file) 3)
  ;                                    (string=? "test-real"
  ;                                              (substring file 0 9))
  ;                                    (string=? "test-realbig"
  ;                                              (substring file 0 12))))
  ;                             (list:sort (directory-list path) string<=?)
  ;                             )]
  ;         )
  ;    (initialize-primitive-type-schemes XXX)
  ;    (for-each (lambda (file)
  ;                (printf "~a: " file)
  ;                (test-f (build-path path file))
  ;                ;		  (test-f file)
  ;                )
  ;              files))
  
  (define/contract subst-vals/flow-vars (type? any/c . -> . type?)
    (lambda (type delta-flow)
      (let subst ([type type])
        (match type
          [(? handle? type) type]
          [($ type-case-lambda rest-arg?s req-args argss exps)
           (let* ([argss ((if (list? argss) util:map2deep util:for-each-vov!) subst argss)]
                  [exps ((if (list? exps) map util:for-each-vector!) subst exps)])
             (make-type-case-lambda rest-arg?s req-args argss exps))]
          [($ type-cons hd tl)
           (make-type-cons (subst hd) (subst tl))]
          [($ type-cst ty) type]
          [($ type-empty) type]
          [($ type-promise value)
           (make-type-promise (subst value))]
          [($ type-rec vars types body)
           (make-type-rec vars (map subst types) (subst body))]
          [($ type-struct-type label) type]
          [($ type-struct-value label types)
           (make-type-struct-value label (map subst types))]
          [($ type-union elements)
           (make-type-union (map subst elements))]
          [($ type-values type)
           (make-type-values (subst type))]
          [($ type-var name reach handle) type]
          [($ type-vector element)
           (make-type-vector (subst element))]
          [($ type-flow-var name) (cdr (lookup-flow-var-in-env delta-flow type))]
          [_ (error 'subst-vals/flow-vars "Unmatched type ~a" type)]))))
  
  ) ; end module constraints-gen-and-prop
