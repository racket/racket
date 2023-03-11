#lang racket/base

;; ============================================================
;; Compile-time

(module ct racket/base
  (provide (struct-out attr)
           (struct-out stxclass)
           (struct-out scopts)
           (struct-out conventions)
           (struct-out literalset)
           (struct-out lse:lit)
           (struct-out lse:datum-lit)
           (struct-out eh-alternative-set)
           (struct-out eh-alternative)
           (struct-out den:lit)
           (struct-out den:datum-lit)
           (struct-out den:delayed)
           prop:syntax-class
           has-stxclass-prop?
           stxclass-prop-ref
           alt-stxclass-mapping
           log-syntax-parse-error
           log-syntax-parse-warning
           log-syntax-parse-info
           log-syntax-parse-debug
           syntax-parse-logger
           prop:pattern-expander
           pattern-expander?
           pattern-expander-proc
           syntax-local-syntax-parse-pattern-introduce)

  (define-logger syntax-parse)

  ;; == from rep-attr.rkt
  (define-struct attr (name depth syntax?) #:prefab)

  ;; == from rep-data.rkt

  ;; A Stxclass is (stxclass Symbol Arity SAttrs Id Bool SCOpts Id/#f)
  (define-struct stxclass
    (name         ;; Symbol
     arity        ;; Arity (defined in kws.rkt)
     attrs        ;; (Listof SAttr)
     parser       ;; Id, reference to parser (see parse.rkt for parser signature)
     splicing?    ;; Bool
     opts         ;; scopts
     inline       ;; Id/#f, reference to a predicate
     ) #:prefab)

  (define-values [prop:syntax-class has-stxclass-prop? stxclass-prop-ref]
    (make-struct-type-property 'syntax-class))

  ;; alt-stxclass-mapping : (Boxof (Listof (cons Identifier Stxclass)))
  ;; Maps existing bindings (can't use syntax-local-value mechanism) to stxclasses.
  ;; Uses alist to avoid residual dependence on syntax/id-table.
  (define alt-stxclass-mapping (box null))

  ;; A SCOpts is (scopts Nat Bool Bool String/#f)
  ;; These are passed on to var patterns.
  (define-struct scopts
    (attr-count   ;; Nat
     commit?      ;; Bool
     delimit-cut? ;; Bool
     desc         ;; String/#f, String = known constant description
     ) #:prefab)

  ;; A Conventions is (conventions Id (-> (Listof ConventionRule)))
  ;; A ConventionRule is (list Regexp DeclEntry)
  (define-struct conventions (get-procedures get-rules) #:transparent)

  ;; A LiteralSet is (literalset (Listof LiteralSetEntry))
  ;; An LiteralSetEntry is one of
  ;; - (make-lse:lit Symbol Id Expr)
  ;; - (make-lse:datum-lit Symbol Symbol)
  (define-struct literalset (literals) #:transparent)
  (define-struct lse:lit (internal external phase) #:transparent)
  (define-struct lse:datum-lit (internal external) #:transparent)

  ;; An EH-alternative-set is (eh-alternative-set (Listof EH-alternative))
  ;; An EH-alternative is(eh-alternative RepConstraint (Listof SAttr) Id)
  (define-struct eh-alternative-set (alts))
  (define-struct eh-alternative (repc attrs parser))

  (define-struct den:lit (internal external input-phase lit-phase) #:transparent)
  (define-struct den:datum-lit (internal external) #:transparent)
  (define-struct den:delayed (parser class))

  ;; == Pattern expanders

  (define-values (prop:pattern-expander pattern-expander? get-proc-getter)
    (make-struct-type-property 'pattern-expander))

  (define (pattern-expander-proc pat-expander)
    (define get-proc (get-proc-getter pat-expander))
    (get-proc pat-expander))

  (define (syntax-local-syntax-parse-pattern-introduce stx)
    (syntax-local-introduce stx)))
(require (for-syntax (submod "." ct)))
(provide (for-syntax (all-from-out (submod "." ct))))

;; ============================================================
;; Run-time

(module progress racket/base
  (require racket/list
           "minimatch.rkt")
  (provide ps-empty
           ps-add-car
           ps-add-cdr
           ps-add-stx
           ps-add-unbox
           ps-add-unvector
           ps-add-unpstruct
           ps-add-opaque
           ps-add-post
           ps-add
           (struct-out ord)

           ps-pop-opaque
           ps-pop-ord
           ps-pop-post
           ps-context-syntax
           ps-difference

           (struct-out failure)
           failure*

           expect?
           (struct-out expect:thing)
           (struct-out expect:atom)
           (struct-out expect:literal)
           (struct-out expect:message)
           (struct-out expect:disj)
           (struct-out expect:proper-pair)

           es-add-thing
           es-add-message
           es-add-atom
           es-add-literal
           es-add-proper-pair)

  ;; FIXME: add phase to expect:literal

  ;; == Failure ==

  ;; A Failure is (failure PS ExpectStack)

  ;; A FailureSet is one of
  ;; - Failure
  ;; - (cons FailureSet FailureSet)

  ;; A FailFunction = (FailureSet -> Answer)
  (define-struct failure (progress expectstack) #:prefab)

  ;; failure* : PS ExpectStack/#f -> Failure/#t
  (define (failure* ps es) (if es (failure ps es) #t))

  ;; == Progress ==

  ;; Progress (PS) is a non-empty list of ProgressFrames (PF).

  ;; A ProgressFrame (PF) is one of
  ;; - stx     ;; "Base" frame, or ~parse/#:with term
  ;; - 'car    ;; car of pair; also vector->list, unbox, struct->list, etc
  ;; - nat     ;; Represents that many repeated cdrs
  ;; - 'post   ;; late/post-traversal check
  ;; - #s(ord group index) ;; ~and subpattern, only comparable w/in group
  ;; - 'opaque

  ;; The error-reporting context (ie, syntax-parse #:context arg) is always
  ;; the final frame.

  ;; All non-stx frames (eg car, cdr) interpreted as applying to nearest following
  ;; stx frame.

  ;; A stx frame is introduced
  ;; - always at base (that is, by syntax-parse)
  ;;   - if syntax-parse has #:context arg, then two stx frames at bottom:
  ;;     (list to-match-stx context-stx)
  ;; - by #:with/~parse
  ;; - by #:fail-*/#:when/~fail & stx

  ;; Interpretation: later frames are applied first.
  ;; eg, (list 'car 1 stx)
  ;;     means ( car of ( cdr once of stx ) )
  ;;     NOT apply car, then apply cdr once, then stop

  (define-struct ord (group index) #:prefab)

  (define (ps-empty stx ctx)
    (if (eq? stx ctx)
        (list stx)
        (list stx ctx)))
  (define (ps-add-car parent)
    (cons 'car parent))
  (define (ps-add-cdr parent [times 1])
    (if (zero? times)
        parent
        (match (car parent)
          [(? exact-positive-integer? n)
           (cons (+ times n) (cdr parent))]
          [_
           (cons times parent)])))
  (define (ps-add-stx parent stx)
    (cons stx parent))
  (define (ps-add-unbox parent)
    (ps-add-car parent))
  (define (ps-add-unvector parent)
    (ps-add-car parent))
  (define (ps-add-unpstruct parent)
    (ps-add-car parent))
  (define (ps-add-opaque parent)
    (cons 'opaque parent))
  (define (ps-add parent frame)
    (cons frame parent))
  (define (ps-add-post parent)
    (cons 'post parent))

  ;; ps-context-syntax : Progress -> Syntax
  (define (ps-context-syntax ps)
    ;; Bottom frame is always syntax
    (last ps))

  ;; ps-difference : Progress Progress -> Nat
  ;; Returns N s.t. B = (ps-add-cdr^N A)
  (define (ps-difference a b)
    (define-values (a-cdrs a-base)
      (match a
        [(cons (? exact-positive-integer? a-cdrs) a-base)
         (values a-cdrs a-base)]
        [_ (values 0 a)]))
    (define-values (b-cdrs b-base)
      (match b
        [(cons (? exact-positive-integer? b-cdrs) b-base)
         (values b-cdrs b-base)]
        [_ (values 0 b)]))
    (unless (eq? a-base b-base)
      (error 'ps-difference "INTERNAL ERROR: ~e does not extend ~e" b a))
    (- b-cdrs a-cdrs))

  ;; ps-pop-opaque : Progress -> Progress
  ;; Used to continue with progress from opaque head pattern.
  (define (ps-pop-opaque ps)
    (match ps
      [(cons (? exact-positive-integer? n) (cons 'opaque ps*))
       (ps-add-cdr ps* n)]
      [(cons 'opaque ps*)
       ps*]
      [_ (error 'ps-pop-opaque "INTERNAL ERROR: opaque frame not found: ~e" ps)]))

  ;; ps-pop-ord : Progress -> Progress
  (define (ps-pop-ord ps)
    (match ps
      [(cons (? exact-positive-integer? n) (cons (? ord?) ps*))
       (ps-add-cdr ps* n)]
      [(cons (? ord?) ps*)
       ps*]
      [_ (error 'ps-pop-ord "INTERNAL ERROR: ord frame not found: ~e" ps)]))

  ;; ps-pop-post : Progress -> Progress
  (define (ps-pop-post ps)
    (match ps
      [(cons (? exact-positive-integer? n) (cons 'post ps*))
       (ps-add-cdr ps* n)]
      [(cons 'post ps*)
       ps*]
      [_ (error 'ps-pop-post "INTERNAL ERROR: post frame not found: ~e" ps)]))


  ;; == Expectations ==

  ;; There are multiple types that use the same structures, optimized for
  ;; different purposes.

  ;; -- During parsing, the goal is to minimize/consolidate allocations.

  ;; An ExpectStack (during parsing) is one of
  ;; - (expect:thing Progress String Boolean String/#f ExpectStack)
  ;; - (expect:thing Progress #f     #f      String/#f ExpectStack)
  ;; * (expect:message String ExpectStack)
  ;; * (expect:atom Datum ExpectStack)
  ;; * (expect:literal Identifier ExpectStack)
  ;; * (expect:proper-pair FirstDesc ExpectStack)
  ;; * #t

  ;; The *-marked variants can only occur at the top of the stack (ie, not
  ;; in the next field of another Expect). The top of the stack contains
  ;; the most specific information.

  ;; An ExpectStack can also be #f, which means no failure tracking is
  ;; requested (and thus no more ExpectStacks should be allocated).

  ;; -- During reporting, the goal is ease of manipulation.

  ;; An ExpectList (during reporting) is (listof Expect).

  ;; An Expect is one of
  ;; - (expect:thing #f String #t String/#f StxIdx)
  ;; * (expect:message String StxIdx)
  ;; * (expect:atom Datum StxIdx)
  ;; * (expect:literal Identifier StxIdx)
  ;; * (expect:proper-pair FirstDesc StxIdx)
  ;; * (expect:disj (NEListof Expect) StxIdx)
  ;; - '...

  ;; A StxIdx is (cons Syntax Nat)

  ;; That is, the next link is replaced with the syntax+index of the term
  ;; being complained about. An expect:thing's progress is replaced with #f.

  ;; An expect:disj never contains a '... or another expect:disj.

  ;; We write ExpectList when the most specific information comes first and
  ;; RExpectList when the most specific information comes last.

  (struct expect:thing (term description transparent? role next) #:prefab)
  (struct expect:message (message next) #:prefab)
  (struct expect:atom (atom next) #:prefab)
  (struct expect:literal (literal next) #:prefab)
  (struct expect:disj (expects next) #:prefab)
  (struct expect:proper-pair (first-desc next) #:prefab)

  (define (expect? x)
    (or (expect:thing? x)
        (expect:message? x)
        (expect:atom? x)
        (expect:literal? x)
        (expect:disj? x)
        (expect:proper-pair? x)))

  (define (es-add-thing ps description transparent? role next)
    (if (and next (or description (not transparent?)))
        (expect:thing ps description transparent? role next)
        next))

  (define (es-add-message message next)
    (if (and next message)
        (expect:message message next)
        next))

  (define (es-add-atom atom next)
    (and next (expect:atom atom next)))

  (define (es-add-literal literal next)
    (and next (expect:literal literal next)))

  (define (es-add-proper-pair first-desc next)
    (and next (expect:proper-pair first-desc next)))

  ;; A FirstDesc is one of
  ;; - #f                   -- unknown, multiple possible, etc
  ;; - string               -- description
  ;; - (list 'any)
  ;; - (list 'literal symbol)
  ;; - (list 'datum datum)
  (void))
(require (submod "." progress))
(provide (all-from-out (submod "." progress)))

;; ------------------------------------------------------------

(module state racket/base
  (provide unwind-to
           maybe-add-state-undo
           current-state
           current-state-writable?
           state-cons!
           track-literals)

  (define (unwind-to undos base)
    ;; PRE: undos = (list* proc/hash ... base)
    (unless (eq? undos base)
      (let ([top-undo (car undos)])
        (cond [(procedure? top-undo) (top-undo)]
              [(hash? top-undo) (current-state top-undo)]))
      (unwind-to (cdr undos) base)))

  (define (maybe-add-state-undo init-state new-state undos)
    (if (eq? init-state new-state)
        undos
        (cons init-state undos)))

  ;; To make adding undos to rewind current-state simpler, only allow updates
  ;; in a few contexts:
  ;; - literals (handled automatically)
  ;; - in ~do/#:do blocks (sets current-state-writable? = #t)

  (define current-state (make-parameter (hasheq)))
  (define current-state-writable? (make-parameter #f))

  (define (state-cons! key value)
    (define state (current-state))
    (current-state (hash-set state key (cons value (hash-ref state key null)))))

  (define (track-literals who v #:introduce? [introduce? #t])
    (unless (syntax? v)
      (raise-argument-error who "syntax?" v))
    (let* ([literals (hash-ref (current-state) 'literals '())])
      (if (null? literals)
          v
          (let ([literals* (if (and introduce? (syntax-transforming?) (list? literals))
                               (for/list ([literal (in-list literals)])
                                 (if (identifier? literal)
                                     (syntax-local-introduce literal)
                                     literal))
                               literals)]
                [old-val (syntax-property v 'disappeared-use)])
            (syntax-property v 'disappeared-use
                             (if old-val
                                 (cons literals* old-val)
                                 literals*)))))))
(require (submod "." state))
(provide (all-from-out (submod "." state)))

;; ------------------------------------------------------------

(module 3d-stx racket/base
  (require (only-in '#%flfxnum flvector? fxvector?)
           (only-in '#%extfl extflonum? extflvector?))
  (provide 2d-stx?)

  ;; Checks for 3D syntax (syntax that contains unwritable values, etc)

  (define INIT-FUEL #e1e6)

  ;; TO DO:
  ;; - extension via proc (any -> list/#f),
  ;;   value considered good if result is list, all values in list are good

  ;; 2d-stx? : Any -> Boolean
  ;; Would (write (compile `(quote-syntax ,x))) succeed?
  ;; If traverse-syntax? is #t, recurs into existing syntax
  ;; If traverse-syntax? is #f, assumes existing stxobjs are 2d, and only
  ;; checks if *new* 3d syntax would be created.
  (define (2d-stx? x
                   #:traverse-syntax? [traverse-syntax? #t]
                   #:irritant [irritant-box #f])
    (check-datum x
                 #:syntax-mode (if traverse-syntax? 'compound 'atomic)
                 #:allow-impersonators? #f
                 #:allow-mutable? 'no-hash/prefab
                 #:allow-unreadable-symbols? #t
                 #:allow-cycles? #t
                 #:irritant irritant-box))

  ;; check-datum : Any -> Boolean
  ;; where StxMode = (U 'atomic 'compound #f)
  ;; Returns nat if x is "good", #f if "bad"
  ;; If irritant-b is a box, the first bad subvalue found is put in the box.
  ;; If visited-t is a hash, it is used to detect cycles.
  (define (check-datum x
                       #:syntax-mode [stx-mode #f]
                       #:allow-impersonators? [allow-impersonators? #f]
                       #:allow-mutable? [allow-mutable? #f]
                       #:allow-unreadable-symbols? [allow-unreadable? #f]
                       #:allow-cycles? [allow-cycles? #f]
                       #:irritant [irritant-b #f])
    ;; Try once with some fuel. If runs out of fuel, try again with cycle checking.
    (define (run fuel visited-t)
      (check* x fuel visited-t
              stx-mode allow-impersonators? allow-mutable? allow-unreadable? allow-cycles?
              irritant-b))
    (let ([result (run INIT-FUEL #f)])
      (cond [(not (equal? result 0)) ;; nat>0 or #f
             (and result #t)]
            [else
             ;; (eprintf "out of fuel, restarting\n")
             (and (run +inf.0 (make-hasheq)) #t)])))

  ;; check* : Any Nat/+inf.0 StxMode Boolean Boolean Boolean Box -> Nat/#f
  ;; Returns #f if bad, positive nat if good, 0 if ran out of fuel
  ;; If bad, places bad subvalue in irritant-b, if box
  (define (check* x0 fuel0 visited-t
                  stx-mode allow-impersonators? allow-mutable? allow-unreadable? allow-cycles?
                  irritant-b)
    (define no-mutable? (not allow-mutable?))
    (define no-mutable-hash/prefab? (or no-mutable? (eq? allow-mutable? 'no-hash/prefab)))
    (define no-cycle? (not allow-cycles?))
    (define no-impersonator? (not allow-impersonators?))
    (define (loop x fuel)
      (if (and fuel (not (zero? fuel)))
          (loop* x fuel)
          fuel))
    (define (loop* x fuel)
      (define (bad) (when irritant-b (set-box! irritant-b x)) #f)
      (define-syntax-rule (with-mutable-check mutable? body ...) ;; don't use for hash or prefab
        (cond [(and no-mutable? mutable?)
               (bad)]
              [else
               body ...]))
      (define-syntax-rule (with-cycle-check body ...)
        (cond [(and visited-t (hash-ref visited-t x #f))
               => (lambda (status)
                    (cond [(and no-cycle? (eq? status 'traversing))
                           (bad)]
                          [else
                           fuel]))]
              [else
               (when visited-t
                 (hash-set! visited-t x 'traversing))
               (begin0 (begin body ...)
                 (when visited-t
                   (hash-remove! visited-t x)))]))
      ;; (eprintf "-- checking ~s, fuel ~s\n" x fuel)
      (cond
        ;; Immutable compound
        [(and visited-t (list? x))
         ;; space optimization: if list (finite), no need to store all cdr pairs in cycle table
         ;; don't do unless visited-t present, else expands fuel by arbitrary factors
         (with-cycle-check
           (for/fold ([fuel (sub1 fuel)]) ([e (in-list x)] #:break (not fuel))
             (loop e fuel)))]
        [(pair? x)
         (with-cycle-check
           (let ([fuel (loop (car x) (sub1 fuel))])
             (loop (cdr x) fuel)))]
        ;; Atomic
        [(or (null? x)
             (boolean? x)
             (number? x)
             (char? x)
             (keyword? x)
             (regexp? x)
             (byte-regexp? x)
             (extflonum? x))
         fuel]
        [(symbol? x)
         (cond [(symbol-interned? x)
                fuel]
               [(symbol-unreadable? x)
                (if allow-unreadable? fuel (bad))]
               [else ;; uninterned
                (if (eq? allow-unreadable? #t) fuel (bad))])]
        ;; Mutable flat
        [(or (string? x)
             (bytes? x))
         (with-mutable-check (not (immutable? x))
           fuel)]
        [(or (fxvector? x)
             (flvector? x)
             (extflvector? x))
         (with-mutable-check (not (immutable? x))
           fuel)]
        ;; Syntax
        [(syntax? x)
         (case stx-mode
           ((atomic) fuel)
           ((compound) (loop (syntax-e x) fuel))
           (else (bad)))]
        ;; Impersonators and chaperones
        [(and no-impersonator? (impersonator? x))  ;; else continue to chaperoned type
         (bad)]
        [(and no-impersonator? (chaperone? x))  ;; else continue to impersonated type
         (bad)]
        [else
         (with-cycle-check
           (cond
             ;; Mutable (maybe) compound
             [(vector? x)
              (with-mutable-check (not (immutable? x))
                (for/fold ([fuel fuel]) ([e (in-vector x)] #:break (not fuel))
                  (loop e fuel)))]
             [(box? x)
              (with-mutable-check (not (immutable? x))
                (loop (unbox x) (sub1 fuel)))]
             [(prefab-struct-key x)
              => (lambda (key)
                   (cond [(and no-mutable-hash/prefab? (mutable-prefab-key? key))
                          (bad)]
                         [else
                          ;; traverse key, since contains arbitrary auto-value
                          (let ([fuel (loop key fuel)])
                            (loop (struct->vector x) fuel))]))]
             [(hash? x)
              (cond [(and no-mutable-hash/prefab? (not (immutable? x)))
                     (bad)]
                    [else
                     (for/fold ([fuel fuel]) ([(k v) (in-hash x)] #:break (not fuel))
                       (let ([fuel (loop k fuel)])
                         (loop v fuel)))])]
             ;; Bad
             [else
              (bad)]))]))
    (loop x0 fuel0))

  ;; mutable-prefab-key? : prefab-key -> Boolean
  (define (mutable-prefab-key? key)
    ;; A prefab-key is either
    ;;  - symbol
    ;;  - (list* symbol maybe-nat maybe-list maybe-vector prefab-key)
    ;; where mutable fields indicated by vector
    ;; This code is probably overly general; racket seems to normalize keys.
    (let loop ([k key])
      (and (pair? k)
           (or (and (vector? (car k))
                    (positive? (vector-length (car k))))
               (loop (cdr k)))))))

;; ------------------------------------------------------------

(module parse-stxparam racket/base
  (require (for-syntax racket/base)
           racket/stxparam)
  (provide this-syntax
           this-role
           this-context-syntax)

  ;; this-syntax
  ;; Bound to syntax being matched inside of syntax class
  (define-syntax-parameter this-syntax
    (lambda (stx)
      (raise-syntax-error #f "used out of context: not within a syntax class" stx)))

  (define-syntax-parameter this-role
    (lambda (stx)
      (raise-syntax-error #f "used out of context: not within a syntax class" stx)))

  ;; this-context-syntax
  ;; Bound to (expression that extracts) context syntax (bottom frame in progress)
  (define-syntax-parameter this-context-syntax
    (lambda (stx)
      (raise-syntax-error #f "used out of context: not within a syntax class" stx))))

(require (submod "." parse-stxparam))
(provide (all-from-out (submod "." parse-stxparam)))

;; ------------------------------------------------------------

(module attribute racket/base
  (require (for-syntax racket/base
                       racket/private/sc
                       (only-in (submod ".." ct) make-attr))
           racket/private/promise
           racket/private/template)
  (provide (for-syntax attribute-mapping attribute-mapping?)
           attribute
           attribute-binding
           check-attr-value)

  (define-syntax (attribute stx)
    (syntax-case stx ()
      [(attribute name)
       (identifier? #'name)
       (let ([mapping (syntax-local-value #'name (lambda () #f))])
         (unless (syntax-pattern-variable? mapping)
           (raise-syntax-error #f "not bound as a pattern variable" stx #'name))
         (let ([var (syntax-mapping-valvar mapping)])
           (let ([attr (syntax-local-value var (lambda () #f))])
             (unless (attribute-mapping? attr)
               (raise-syntax-error #f "not bound as an attribute" stx #'name))
             (syntax-property (attribute-mapping-var attr)
                              'disappeared-use
                              (list (syntax-local-introduce #'name))))))]))

  ;; (attribute-binding id)
  ;; mostly for debugging/testing
  (define-syntax (attribute-binding stx)
    (syntax-case stx ()
      [(attribute-bound? name)
       (identifier? #'name)
       (let ([value (syntax-local-value #'name (lambda () #f))])
         (if (syntax-pattern-variable? value)
             (let ([value (syntax-local-value (syntax-mapping-valvar value) (lambda () #f))])
               (if (attribute-mapping? value)
                   #`(quote #,(make-attr (attribute-mapping-name value)
                                         (attribute-mapping-depth value)
                                         (if (attribute-mapping-check value) #f #t)))
                   #'(quote #f)))
             #'(quote #f)))]))

  ;; check-attr-value : Any d:Nat b:Boolean Syntax/#f -> (Listof^d (if b Syntax Any))
  (define (check-attr-value v0 depth0 stx? ctx)
    (define (bad kind v)
      (raise-syntax-error #f (format "attribute contains non-~s value\n  value: ~e" kind v) ctx))
    (define (depthloop depth v)
      (if (zero? depth)
          (baseloop v)
          (let listloop ([v v] [root? #t])
            (cond [(null? v) null]
                  [(pair? v) (let ([new-car (depthloop (sub1 depth) (car v))]
                                   [new-cdr (listloop (cdr v) #f)])
                               (cond [(and (eq? (car v) new-car) (eq? (cdr v) new-cdr)) v]
                                     [else (cons new-car new-cdr)]))]
                  [(promise? v) (listloop (force v) root?)]
                  [(and root? (eq? v #f)) (begin (signal-absent-pvar) (bad 'list v))]
                  [else (bad 'list v)]))))
    (define (baseloop v)
      (cond [(promise? v) (baseloop (force v))]
            [(not stx?) v]
            [(syntax? v) v]
            [(eq? v #f) (begin (signal-absent-pvar) (bad 'syntax v))]
            [else (bad 'syntax v)]))
    (depthloop depth0 v0)))

(require (submod "." attribute))
(provide (all-from-out (submod "." attribute)))

;; ------------------------------------------------------------

(module parse-util racket/base
  (require (for-syntax racket/base)
           racket/list
           racket/lazy-require
           syntax/stx
           (submod ".." 3d-stx)
           (submod ".." state)
           (submod ".." progress))
  (provide list->values
           stx-list-take
           stx-list-drop/cx
           datum->syntax/with-clause
           check-literal*
           error/null-eh-match
           illegal-cut-error
           name->too-few/once
           name->too-few
           name->too-many
           normalize-context
           syntax-patterns-fail
           predicate-ellipsis-parser)

  (define (list->values n vs)
    (apply values (if n (take vs n) vs)))

  ;; stx-list-take : Stx Nat -> Syntax
  (define (stx-list-take stx n)
    (datum->syntax #f
                   (let loop ([stx stx] [n n])
                     (if (zero? n)
                         null
                         (cons (stx-car stx)
                               (loop (stx-cdr stx) (sub1 n)))))))

  ;; stx-list-drop/cx : Stx Syntax Nat -> (values Stx Syntax)
  (define (stx-list-drop/cx x cx n)
    (let loop ([x x] [cx cx] [n n])
      (if (zero? n)
          (values x
                  (if (syntax? x) x cx))
          (loop (stx-cdr x)
                (if (syntax? x) x cx)
                (sub1 n)))))

  ;; datum->syntax/with-clause : Any -> Syntax
  (define (datum->syntax/with-clause x)
    (cond [(syntax? x) x]
          [(2d-stx? x #:traverse-syntax? #f)
           (datum->syntax #f x #f)]
          [else
           (error 'datum->syntax/with-clause
                  (string-append
                   "implicit conversion to 3D syntax\n"
                   " right-hand side of #:with clause or ~~parse pattern would be 3D syntax\n"
                   "  value: ~e")
                  x)]))

  ;; check-literal* : Id Phase Phase (Listof Phase) Syntax -> Void
  (define (check-literal* id used-phase mod-phase ok-phases/ct-rel ctx)
    (unless (or (memv (and used-phase (- used-phase mod-phase))
                      ok-phases/ct-rel)
                (identifier-binding id used-phase))
      (raise-syntax-error
       #f
       (format "literal is unbound in phase ~a (phase ~a relative to the enclosing module)"
               used-phase
               (and used-phase (- used-phase mod-phase)))
       ctx id)))

  ;; error/null-eh-match : -> (escapes)
  (define (error/null-eh-match)
    (error 'syntax-parse "an ellipsis-head pattern matched an empty sequence"))

  (define (illegal-cut-error . _)
    (error 'syntax-parse "illegal use of cut"))

  (define (name->too-few/once name)
    (and name (format "missing required occurrence of ~a" name)))
  (define (name->too-few name)
    (and name (format "too few occurrences of ~a" name)))
  (define (name->too-many name)
    (and name (format "too many occurrences of ~a" name)))

  ;; normalize-context : Symbol Any Syntax -> (list Symbol/#f Syntax)
  (define (normalize-context who ctx stx)
    (cond [(syntax? ctx)
           (list #f ctx)]
          [(symbol? ctx)
           (list ctx stx)]
          [(eq? ctx #f)
           (list #f stx)]
          [(and (list? ctx)
                (= (length ctx) 2)
                (or (symbol? (car ctx)) (eq? #f (car ctx)))
                (syntax? (cadr ctx)))
           ctx]
          [else (error who "bad #:context argument\n  expected: ~s\n  given: ~e"
                       '(or/c syntax? symbol? #f (list/c (or/c symbol? #f) syntax?))
                       ctx)]))

  (lazy-require
   ["runtime-report.rkt"
    (call-current-failure-handler)])

  ;; syntax-patterns-fail : (list Symbol/#f Syntax) -> (Listof (-> Any)) FailureSet -> escapes
  (define ((syntax-patterns-fail ctx) undos fs)
    (unwind-to undos null)
    (call-current-failure-handler ctx fs))

  ;; == specialized ellipsis parser
  ;; returns (values 'ok attr-values) or (values 'fail failure)

  (define (predicate-ellipsis-parser x cx pr es pred? desc rl)
    (let ([elems (stx->list x)])
      (if (and elems (list? elems) (andmap pred? elems))
          (values 'ok elems)
          (let loop ([x x] [cx cx] [i 0])
            (cond [(syntax? x)
                   (loop (syntax-e x) x i)]
                  [(pair? x)
                   (if (pred? (car x))
                       (loop (cdr x) cx (add1 i))
                       (let* ([pr (ps-add-cdr pr i)]
                              [pr (ps-add-car pr)]
                              [es (es-add-thing pr desc #t rl es)])
                         (values 'fail (failure pr es))))]
                  [else ;; not null, because stx->list failed
                   (let ([pr (ps-add-cdr pr i)]
                         #|
                         ;; Don't extend es! That way we don't get spurious "expected ()"
                         ;; that *should* have been cancelled out by ineffable pair failures.
                         |#)
                     (values 'fail (failure* pr es)))]))))))

(require (submod "." parse-util))
(provide (all-from-out (submod "." parse-util)))
