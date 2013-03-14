#lang racket/base

(require racket/list
         racket/match
         racket/contract
         racket/class
         racket/unit
         "syntax-property.rkt")

; CONTRACTS

(define varref-set? (listof identifier?))
(define binding-set? (or/c varref-set? (symbols 'all)))
(define (arglist? v)
  (or (null? v)
      (identifier? v)
      (and (pair? v)
           ((flat-contract-predicate (cons/c identifier? arglist?)) v))        
      (and (syntax? v) (null? (syntax-e v)))
      (and (syntax? v) 
           ((flat-contract-predicate (cons/c identifier? arglist?)) (syntax-e v)))))

#;(provide/contract
   [varref-set-remove-bindings (-> varref-set? varref-set? varref-set?)]
   [binding-set-varref-set-intersect (-> binding-set? varref-set? binding-set?)]
   [binding-set-union (-> (listof binding-set?) binding-set?)]
   [varref-set-union (-> (listof varref-set?) varref-set?)]
   [skipto/auto (syntax? (symbols 'rebuild 'discard) 
                         (syntax? . -> . syntax?)
                         . -> . 
                         syntax?)]
   [in-closure-table (-> any/c boolean?)]
   [sublist (-> number? number? list? list?)]
   [attach-info (-> syntax? syntax? syntax?)]
   [transfer-info (-> syntax? syntax? syntax?)]
   [arglist->ilist (-> arglist? any)]
   [arglist-flatten (-> arglist? (listof identifier?))])

(provide
 skipto/auto
 sublist
 attach-info
 transfer-info
 arglist->ilist
 arglist-flatten
 binding-set-union
 binding-set-pair-union
 varref-set-union
 varref-set-pair-union
 varref-set-remove-bindings
 binding-set-varref-set-intersect
 step-result?
 step-maybe-result?
 (struct-out before-after-result)
 (struct-out before-error-result)
 (struct-out error-result)
 (struct-out finished-stepping)
 (struct-out runaway-process)
 list-take
 list-partition
 (struct-out closure-record)
 *unevaluated* 
 struct-flag
 multiple-highlight
 flatten-take
 get-lifted-var
 get-arg-var
 begin0-temp
 zip
 let-counter
 syntax-pair-map
 make-queue ; -> queue
 queue-push ; queue val -> 
 queue-pop ; queue -> val
 queue-length ; queue -> num
 rebuild-stx ; datum syntax -> syntax
 break-kind? ; predicate
 varref-set? ; predicate
 binding-set? ; predicate
 ; get-binding-name
 ; bogus-binding?
 ; get-lifted-gensym
 ; expr-read
 ; set-expr-read!
 values-map
 a...b ; a list of numbers from a to b
 reset-profiling-table ; profiling info
 get-set-pair-union-stats ; profiling info
 re-intern-identifier
 finished-xml-box-table
 language-level->name
 saved-code-inspector
 
 
 (struct-out annotated-proc)
 
 view-controller^
 stepper-frame^
 )

  
  ; A step-result is either:
  ; (make-before-after-result finished-exps exp redex reduct)
  ; or (make-before-error-result finished-exps exp redex err-msg)
  ; or (make-error-result finished-exps err-msg)
  ; or (make-finished-result finished-exps)
  
  (struct before-after-result (pre-exps post-exps kind pre-src post-src) #:prefab)
  (struct before-error-result (pre-exps err-msg pre-src) #:prefab)
  (struct error-result (err-msg) #:prefab)
  (struct finished-stepping () #:prefab)
  (struct runaway-process (sema) #:prefab)
  
  (define step-result? (or/c before-after-result? before-error-result? 
                             error-result? finished-stepping?))
  (define step-maybe-result? (or/c step-result? runaway-process?))
  
  ; the closure record is placed in the closure table

  (define-struct closure-record (name mark constructor? lifted-index))

  ; bogus-binding is used so that we can create legal bindings for temporary variables
  
  (define (create-bogus-binding name)
    (let* ([gensymed-name (gensym name)]
           [binding (datum->syntax #'here gensymed-name)])
      binding))
  
  ; make-binding-source creates a pool of bindings, indexed by arbitrary keys. These bindings
  ; not eq? to any other bindings[*], but a client can always get the same binding by
  ; invoking the resulting procedure with the same key (numbers work well). make-binding-source
  ; also takes a string which will be part of the printed representation of the binding's
  ; name; this makes debugging easier.
  ; [*] actually, this is not true if you don't use a one-to-one function as the binding-maker
  ; make-gensym-source : (string -> (key -> binding))
  
  (define (make-binding-source id-string binding-maker key-displayer)
    (let ([assoc-table (make-weak-hash)])
      (lambda (key)
        (let ([maybe-fetch (hash-ref assoc-table key (lambda () #f))])
          (or maybe-fetch
              (begin
                (let* ([new-binding (binding-maker 
                                     (string-append id-string (key-displayer key) "-"))])
                  (hash-set! assoc-table key new-binding)
                  new-binding)))))))
  
  
  ; get-arg-var maintains a list of bindings associated with the non-negative
  ; integers.  These symbols are used in the elaboration of applications; the nth
  ; in the application is evaluated and stored in a variable whose name is the nth
  ; gensym supplied by get-arg-var.
  
  (define get-arg-var
    (make-binding-source "arg" create-bogus-binding number->string))
  
  ; test cases: (returns #t on success)
;  (printf "test of get-arg-binding: ~a\n"
;          (let* ([arg3 (get-arg-var 3)]
;                 [arg2 (get-arg-var 2)]
;                 [arg1 (get-arg-var 1)]
;                 [arg2p (get-arg-var 2)])
;            (and (not (eq? arg3 arg2))
;                 (not (eq? arg3 arg1))
;                 (not (eq? arg3 arg2p))
;                 (not (eq? arg2 arg1))
;                 (eq? arg2 arg2p)
;                 (not (eq? arg1 arg2p)))))

  (define begin0-temp (create-bogus-binding "begin0-temp"))
  
  ; get-lifted-var maintains the mapping between let-bindings and the syntax object
  ; which is used to capture its index at runtime.
  ; unfortunately, it can't use "make-binding-source" because you need to compare the items 
  ; with module-variable=?, which means that hash tables won't work.
  
  ; my weak-assoc lists are lists of two-element lists, where the first one is in a weak box.
  ; furthermore, the whole thing is in a box, to allow it to be banged when needed.
  
  (define (weak-assoc-add boxed-lst key value)
       (set-box! boxed-lst (cons (list (make-weak-box key) value) (unbox boxed-lst))))
  
  (define (weak-assoc-search boxed-lst key eq-fun)
    (let* ([lst (unbox boxed-lst)]
           [found-val #f]
           [stripped (let loop ([remaining lst])
                       (if (null? remaining)
                           null
                           (let* ([first (car remaining)]
                                  [first-key (weak-box-value (car first))])
                             (if first-key
                                 (if (eq-fun key first-key)
                                     (begin 
                                       (set! found-val (cadr first))
                                       remaining)
                                     (cons first
                                           (loop (cdr remaining))))
                                 (loop (cdr remaining))))))])
      (set-box! boxed-lst stripped)
      found-val))
  
  ; test cases:
  ;  (define wa (box null))
  ;  (define-struct test ())
  ;  (weak-assoc-add wa 3 4)
  ;  (weak-assoc-add wa 9 10)
  ;  (= (weak-assoc-search wa 3 =) 4)
  ;  (= (weak-assoc-search wa 9 =) 10)
  ;  (= (weak-assoc-search wa 3 =) 4)
  ;  (= (length (unbox wa)) 2)
  ;  (define my-struct (make-test))
  ;  (weak-assoc-add wa my-struct 14)
  ;  (= (length (unbox wa)) 3)
  ;  (= (weak-assoc-search wa my-struct eq?) 14)
  ;  (set! my-struct #f)
  ;  (collect-garbage)
  ;  (= (length (unbox wa)) 3)
  ;  (= (weak-assoc-search wa 3 =) 4)
  ;  (= (length (unbox wa)) 2)
  
  (define lifted-index 0)
  (define (next-lifted-symbol str)
    (let ([index lifted-index]) 
      (set! lifted-index (+ lifted-index 1))
      (datum->syntax #'here (string->symbol (string-append str (number->string index))))))
 
  (define get-lifted-var
   (let ([assoc-table (box null)])
      (lambda (stx)
        (let ([maybe-fetch (weak-assoc-search assoc-table stx free-identifier=?)])
          (or maybe-fetch
              (begin
                (let* ([new-binding (next-lifted-symbol
                                     (string-append "lifter-" (format "~a" (syntax->datum stx)) "-"))])
                  (weak-assoc-add assoc-table stx new-binding)
                  new-binding)))))))
  
  ; gensyms needed by many modules:


  ; multiple-highlight is used to indicate multiple highlighted expressions
  (define multiple-highlight (gensym "multiple-highlight-"))
  
  ; *unevaluated* is the value assigned to temps before they are evaluated. It's not a symbol so
  ; it won't need quoting in the source.  Bit of a hack, I know.
  (define-struct *unevaluated-struct* ())
  (define *unevaluated* (make-*unevaluated-struct*))
 
  ; struct-flag : uninterned symbol
  (define struct-flag (gensym "struct-flag-"))
  
  ; list-partition takes a list and a number, and returns a 2vals containing 2 lists; the first one contains the
  ; first n elements of the list, and the second contains the remainder.  If n is greater than
  ; the length of the list, the exn:application:mismatch exception is raised.
  
  (define (list-partition lst n)
    (if (= n 0)
        (vector null lst)
        (if (null? lst)
            (list-ref lst 0) ; cheap way to generate exception
            (match-let* ([(vector first rest) (list-partition (cdr lst) (- n 1))])
              (vector (cons (car lst) first) rest)))))
  
;  (define expr-read read-getter)
;  (define set-expr-read! read-setter)
  
  (define (list-take n a-list)
    (if (= n 0)
        null
        (cons (car a-list) (list-take (- n 1) (cdr a-list)))))
  
  (define (flatten-take n a-list)
    (apply append (list-take n a-list)))
  
  (define-values (closure-table-put! closure-table-lookup in-closure-table)
    (let ([closure-table (make-weak-hash)])
      (values
       (lambda (key value)
	 (hash-set! closure-table key value)
	 key)                                  ; this return allows a run-time-optimization
       (lambda args ; key or key & failure-thunk
         (apply hash-ref closure-table args))
       (lambda (key)
         (let/ec k
           (hash-ref closure-table key (lambda () (k #f)))
           #t)))))
  
  ;(begin (closure-table-put! 'foo 'bar)
  ;       (and (eq? (in-closure-table 'blatz) #f)
  ;            (eq? (in-closure-table 'foo) #t)))
 
  
  ;; arglist : for our puposes, an ilist is defined like this:
  ;; arglist : (or/c identifier? null? (cons identifier? arglist?) (syntax (cons identifier? arglist?))
  ;; ... where an ilist val can be anything _except_ a pair or null

  ;; arglist->ilist : turns an (possibly improper) arglist into a (possibly improper) list of syntax objects

  (define (arglist->ilist arglist)
    (let loop ([ilist arglist])
      (cond [(identifier? ilist)
             ilist]
            [(pair? ilist)
             (cons (car ilist)
                   (loop (cdr ilist)))]
            [(and (syntax? ilist) (pair? (syntax-e ilist)))
             (loop (syntax-e ilist))]
            [(null? ilist) null])))

  ;; arglist-flatten : produces a list containing the elements of the ilist

  (define (arglist-flatten arglist)
    (let loop ([ilist arglist])
      (cond [(identifier? ilist)
             (cons ilist null)]
            [(or (null? ilist) (and (syntax? ilist) (null? (syntax-e ilist))))
             null]
            [(pair? ilist)
             (cons (car ilist) (loop (cdr ilist)))]
            [(and (syntax? ilist)
                  (pair? (syntax-e ilist)))
             (loop (syntax-e ilist))])))

  ;; zip : (listof 'a) (listof 'b) (listof 'c) ...
  ;;       -> (listof (list 'a 'b 'c ...))
  ;; zip reshuffles lists of items into a list of item-lists.  Look at the
  ;; contract, okay?

  (define zip
    (lambda args
      (apply map list args)))

  (define let-counter
    (stepper-syntax-property #'let-counter 'stepper-binding-type 'stepper-temp))


  ; syntax-pair-map (using the def'ns of the Racket docs):

  (define (syntax-pair-map pair fn)
    (cons (fn (car pair))
          (cond [(syntax? (cdr pair))
                 (fn (cdr pair))]
                [(pair? (cdr pair))
                 (syntax-pair-map (cdr pair) fn)]
                [(null? (cdr pair))
                 null])))

  (define (make-queue)
    (box null))

  (define (queue-push queue new)
    (set-box! queue (append (unbox queue) (list new))))

  (define (queue-pop queue)
    (if (null? (unbox queue))
      (error 'queue-pop "no elements in queue")
      (let ([first (car (unbox queue))])
        (set-box! queue (cdr (unbox queue)))
        first)))

  (define (queue-length queue)
    (length (unbox queue)))

  (define saved-code-inspector (variable-reference->module-declaration-inspector
                                (#%variable-reference)))
  
  (define (rebuild-stx new old)
    (datum->syntax old new old old))

  (define break-kind?
    (symbols 'normal-break 'normal-break/values 'result-exp-break
             'result-value-break 'double-break 'late-let-break
             'expr-finished-break 'define-struct-break))

  ; functional update package

  ;; a trace is one of
  ;; (cons 'car trace)
  ;; (cons 'cdr trace)
  ;; (cons 'syntax-e trace)
  ;; (cons 'both (list trace trace))
  ;; null
  
  (define (swap-args 2-arg-fun)
    (lambda (x y)
      (2-arg-fun y x)))

  (define second-arg (lambda (dc y) y))
  
  (define (up-mapping traversal fn)
    (unless (symbol? fn)
      (error 'up-mapping "expected symbol for stepper traversal, given: ~v" fn))
    (case traversal
      [(rebuild) (case fn 
                   [(car) (lambda (stx new) (cons new (cdr stx)))]
                   [(cdr) (lambda (stx new) (cons (car stx) new))]
                   [(syntax-e) (swap-args rebuild-stx)]
                   [(both-l both-r) (lambda (stx a b) (cons a b))]
                   [else (error 'up-mapping "unexpected symbol in up-mapping (1): ~v" fn)])]
      [(discard) (case fn
                   [(car) second-arg]
                   [(cdr) second-arg]
                   [(syntax-e) second-arg]
                   [(both-l) (lambda (stx a b) a)]
                   [(both-r) (lambda (stx a b) b)]
                   [else (error 'up-mapping "unexpected symbol in up-mapping (2): ~v" fn)])]))
  
  (define (down-mapping fn)
    (case fn
      [(car) car]
      [(cdr) cdr]
      [(syntax-e) syntax-e]
      [else (error 'down-mapping "called on something other than 'car, 'cdr, & 'syntax-e: ~v" fn)]))

  (define (update fn-list val fn traversal)
    (if (null? fn-list)
        (fn val)
        (let ([up (up-mapping traversal (car fn-list))])
          (case (car fn-list)
            [(both-l both-r) (up val 
                                 (update (cadr fn-list) (car val) fn traversal)
                                 (update (caddr fn-list) (cdr val) fn traversal))]
            [else (let ([down (down-mapping (car fn-list))])
                    (up val (update (cdr fn-list) (down val) fn traversal)))]))))
  

    #;(display (equal? (update '(cdr cdr car both-l (car) (cdr))
                           `(a . (b ((1) c . 2) d))
                           (lambda (x) (+ x 1))
                           'rebuild)
                   `(a . (b ((2) c . 3) d))))
  
  
  ;; skipto/auto : syntax?
  ;;               (symbols 'rebuild 'discard)
  ;;               (syntax? . -> . syntax?)
  ;; "skips over" part of a tree to find a subtree indicated by the
  ;; stepper-skipto property, and applies the transformer to it.  
  ;; If the traversal argument is 'rebuild, the
  ;; result of transformation is embedded again in the same tree.  if the
  ;; traversal argument is 'discard, the result of the transformation is the
  ;; result of this function
  (define (skipto/auto stx traversal transformer)
    (cond [(or (stepper-syntax-property stx 'stepper-skipto)
	       (stepper-syntax-property stx 'stepper-skipto/discard))
           =>
           (lambda (x) (update x stx 
                               (lambda (y) 
                                 (skipto/auto y traversal transformer)) 
                               traversal))]
          [else (transformer stx)]))

  ;  small test case:
  #;(display (equal? (syntax->datum
                    (skipto/auto (stepper-syntax-property #`(a #,(stepper-syntax-property #`(b c)
                                                                          'stepper-skipto
                                                                          '(syntax-e cdr car)))
                                                  'stepper-skipto
                                                  '(syntax-e cdr car))
                                 'discard
                                 (lambda (x) x)))
                   'c))
  

  ; BINDING-/VARREF-SET FUNCTIONS

  ; note: a BINDING-SET which is not 'all may be used as a VARREF-SET.
  ; this is because they both consist of syntax objects, and a binding
  ; answers true to bound-identifier=? with itself, just like a varref
  ; in the scope of that binding would.

  ; binding-set-union: (listof BINDING-SET) -> BINDING-SET
  ; varref-set-union: (listof VARREF-SET) -> VARREF-SET

  (define profiling-table (make-hash))
  (define (reset-profiling-table)
    (set! profiling-table (make-hash)))

  (define (get-set-pair-union-stats)
    (hash-map profiling-table (lambda (k v) (list k (unbox v)))))

  ;; test cases :
  ;; (profiling-table-incr 1 2)
  ;; (profiling-table-incr 2 3)
  ;; (profiling-table-incr 2 1)
  ;; (profiling-table-incr 1 2)
  ;; (profiling-table-incr 2 1)
  ;;
  ;; (equal? (get-set-pair-union-stats)
  ;         `(((2 . 3) 1) ((2 . 1) 2) ((1 . 2) 2)))

  ;; until this remove* goes into list.rkt?

  (define (set-pair-union a-set b-set comparator)
    (cond [(null? b-set) a-set]
          [(null? a-set) b-set]
          [else (append (remove* a-set b-set comparator) a-set)]))

  (define (varref-set-pair-union a-set b-set)
    (set-pair-union a-set b-set free-identifier=?))

  (define (binding-set-pair-union a-set b-set)
    (cond [(eq? a-set 'all) 'all]
          [(eq? b-set 'all) 'all]
          [else (set-pair-union a-set b-set eq?)]))

  (define (pair-union->many-union fn)
    (lambda (args)
      (foldl fn null args)))

  (define binding-set-union
    (pair-union->many-union binding-set-pair-union))

  (define varref-set-union
    (pair-union->many-union varref-set-pair-union))

  ; binding-set-varref-set-intersect : BINDING-SET VARREF-SET -> BINDING-SET
  ; return the subset of varrefs that appear in the bindings

  (define (binding-set-varref-set-intersect bindings varrefs)
    (cond [(eq? bindings 'all) varrefs]
          [else (filter (lambda (varref)
                          (ormap (lambda (binding)
                                   (bound-identifier=? binding varref))
                                 bindings))
                        varrefs)]))

  ; varref-set-remove-bindings : VARREF-SET (BINDING-SET - 'all) -> VARREF-SET
  ; remove bindings from varrefs

  (define (varref-set-remove-bindings varrefs bindings)
    (cond [(eq? bindings 'all)
           (error 'varref-set-remove-bindings "binding-set 'all passed as second argument, first argument was: ~s" varrefs)]
          [else (remove* bindings varrefs bound-identifier=?)]))

  ;; sublist returns the list beginning with element <begin> and ending just
  ;; before element <end>.
  ;; (-> number? number? list? list?)
  (define (sublist begin end lst)
    (if (= end 0)
        null
        (if (= begin 0)
            (cons (car lst)
                  (sublist 0 (- end 1) (cdr lst)))
            (sublist (- begin 1) (- end 1) (cdr lst)))))

  ;; take info from source expressions to reconstructed expressions

  (define (attach-info to-exp from-exp)
    (let* ([attached (syntax-property to-exp 'stepper-properties (append (or (syntax-property from-exp 'stepper-properties)
                                                                             null)
                                                                         (or (syntax-property to-exp 'stepper-properties)
                                                                             null)))]
           [attached (syntax-property attached 'user-source (syntax-source from-exp))]
           [attached (syntax-property attached 'user-position (syntax-position from-exp))]
           [attached (syntax-property attached 'user-origin (syntax-property from-exp 'origin))])
      attached))

  ;; transfer info from reconstructed expressions to other reconstructed
  ;; expressions

  (define (transfer-info to-exp from-exp)
    (let* ([attached (syntax-property to-exp 'stepper-properties (append (or (syntax-property from-exp 'stepper-properties)
                                                                             null)
                                                                         (or (syntax-property to-exp 'stepper-properties)
                                                                             null)))]
           [attached (syntax-property attached 'user-source (syntax-property from-exp 'user-source))]
           [attached (syntax-property attached 'user-position (syntax-property from-exp 'user-position))]
           [attached (syntax-property attached 'user-origin (syntax-property from-exp 'user-origin))])
      attached))

  (define (values-map fn . lsts)
    (apply values
           (apply map list
                  (apply map (lambda args
                               (call-with-values (lambda () (apply fn args))
                                   list))
                         lsts))))
  
  ; produces the list of numbers from a to b (inclusive)
  (define (a...b a b)
    (if (= a b)
        (list a)
        (cons a (a...b (+ a 1) b))))
  
  ;; re-intern-identifier : (identifier? -> identifier?)
  ;; re-intern-identifier : some identifiers are uninterned, which breaks
  ;; test cases.  re-intern-identifier takes an identifier to a string
  ;; and back again to make in into an interned identifier.
  (define (re-intern-identifier identifier)
    #`#,(string->symbol (symbol->string (syntax-e identifier))))
  
  
  (provide/contract [syntax->hilite-datum 
                     ((syntax?) (#:ignore-highlight? boolean?) . ->* . any)]) ; sexp with explicit tags
  
  ;; syntax->hilite-datum : takes a syntax object with zero or more
  ;; subexpressions tagged with the 'stepper-highlight', 'stepper-xml-hint', and 'stepper-xml-value-hint' syntax-properties
  ;; and turns it into a datum, where expressions with the named
  ;; properties result in (hilite <datum>), (xml-box <datum>), (scheme-box <datum>) and (splice-box <datum>) rather than <datum>. It also
  ;; re-interns all identifiers.  In cases where a given expression has more than one of these, they appear in the order
  ;; listed.  That is, an expression with both highlight and xml-box annotations will result it (hilite (xml-box <datum>))
  ;; 
  ;; this procedure is useful in checking the output of the stepper.
  
  (define (syntax->hilite-datum stx #:ignore-highlight? [ignore-highlight? #f])
    (let ([datum (syntax-case stx ()
                   [(a . rest) (cons (syntax->hilite-datum #`a) (syntax->hilite-datum #`rest))]
                   [id
                    (identifier? stx)
                    (string->symbol (symbol->string (syntax-e stx)))]
                   [else (if (syntax? stx)
                             (syntax->datum stx)
                             stx)])])
      (let* ([it (case (stepper-syntax-property stx 'stepper-xml-hint)
                   [(from-xml-box) `(xml-box ,datum)]
                   [(from-scheme-box) `(scheme-box ,datum)]
                   [(from-splice-box) `(splice-box ,datum)]
                   [else datum])]
             [it (case (stepper-syntax-property stx 'stepper-xml-value-hint)
                   [(from-xml-box) `(xml-box-value ,it)]
                   [else it])]
             [it (if (and (not ignore-highlight?)
                          (stepper-syntax-property stx 'stepper-highlight))
                     `(hilite ,it)
                     it)])
        it)))
  
  ;; finished-xml-box-table : this table tracks values that are the result
  ;; of evaluating xml boxes.  These values should be rendered as xml boxes,
  ;; and not as simple lists.
  
  (define finished-xml-box-table (make-weak-hash))
  
  (provide/contract [syntax->interned-datum (syntax? ; input
                                                    . -> .
                                                    any)]) ; sexp 
  
  ;; syntax->interned-datum : like syntax->datum, except
  ;; that it re-interns all identifiers.  Useful in checking whether
  ;; two sexps will have the same printed representation.
  
  (define (syntax->interned-datum stx)
    (syntax-case stx ()
      [(a . rest) (cons (syntax->interned-datum #`a) (syntax->interned-datum #`rest))]
      [id
       (identifier? stx)
       (string->symbol (symbol->string (syntax-e stx)))]
      [else (if (syntax? stx)
                (syntax->datum stx)
                stx)]))
  
  
  ;; the xml-snip-creation@ unit accepts the xml-snip% and scheme-snip% classes and
  ;; provides functions which map a "spec" to an xml-snip.
  ;; An xml-spec is (listof xml-spec-elt)
  ;; An xml-spec-elt is either
  ;;  - a string,
  ;;  - (cons/c 'scheme-box scheme-spec), or
  ;;  - (cons/c 'splice-box scheme-spec)
  ;;
  ;; A scheme-spec is (listof scheme-spec-elt)
  ;; A scheme-spec-elt is either
  ;;  - a string, or
  ;;  - (cons ... oh crud.
  #;(define xml-snip-creation@
    (unit/sig (create-xml-snip create-scheme-snip create-splice-snip)
      (import (xml-snip% scheme-snip%))
      
      (define (construct-xml-box spec)
        (let* ([new-xml-box (instantiate xml-snip% () 
                              [eliminate-whitespace-in-empty-tags? #t])] ;  need to check what the languages themselves do here
               [xml-editor (send new-xml-box get-editor)])
          (for-each
           (match-lambda
            [`(scheme-box ,@(schemeboxspec ...)) (send new-xml-box insert (construct-scheme-box #f schemeboxspec))]
            [`(splice-box ,@(spliceboxspec ...)) (send new-xml-box insert (construct-scheme-box #f spliceboxspec))]
            [(? string? text) (send xml-editor insert text)])
           spec)
          new-xml-box))
      
      (define (construct-scheme-box splice? spec)
        (let* ([new-scheme-box (instantiate scheme-snip% () [splice? splice?])]
               [scheme-editor (send new-scheme-box get-editor)])
          (for-each 
           (match-lambda
            [`(xml-box ,@(xmlspec ...)) (send scheme-editor insert (construct-xml-box xmlspec))]
            [(? string? text) (send scheme-editor insert text)])
           spec)))))
  
  
  (define (language-level->name language)
    (car (last-pair (send language get-language-position))))
  
  
  ;; per Robby's suggestion: rather than using a hash table for 
  ;; lambdas, just use an applicable structure instead.
  
  ;; An annotated procedure is represented at runtime by
  ;; an applicable structure that stores stepper information.
  (struct annotated-proc (base info)
    #:property prop:procedure
    (struct-field-index base))
  
  
  (define-signature view-controller^ (go))
  (define-signature stepper-frame^ (stepper-frame%))
