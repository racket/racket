#lang racket/base

(require (for-syntax racket/base)
         (for-syntax stepper/private/syntax-property))

  ;; ~ = lazy (or delayed)
  ;; ! = strict (or forced)
  ;; (See below for app-related names)

  ;; --------------------------------------------------------------------------
  ;; Syntax utilities

  ;; taken & modified from swindle/misc.rkt
  (provide defsubst) ; useful utility
  (define-syntax (defsubst-process stx)
    (syntax-case stx ()
      [(_ name (acc ...))
       #'(define-syntax (name stx)
           (syntax-case stx () acc ...))]
      [(_ name (acc ...) id subst . more) (identifier? #'id)
       #'(defsubst-process
           name (acc ...
                 (id (identifier? #'id) #'subst)
                 ((id x (... ...)) #'(subst x (... ...))))
           . more)]
      [(_ name (acc ...) n+a subst . more)
       #'(defsubst-process name (acc ... (n+a #'subst)) . more)]))
  (define-syntax defsubst
    (syntax-rules ()
      [(_ (name . args) subst . more)
       (defsubst-process name () (name . args) subst . more)]
      [(_ name subst . more)
       (defsubst-process name () name subst . more)]))

  ;; utility for defining ~foo but make it look like #<procedure:foo>
  (define-syntax (define* stx)
    (syntax-case stx ()
      [(_ ~name val) (identifier? #'~name)
       (let* ([~str (symbol->string (syntax-e #'~name))]
              [str  (string->symbol (regexp-replace #rx"^[~*]" ~str ""))])
         (with-syntax ([name (datum->syntax #'~name str #'~name)])
           #'(define ~name (let ([name val]) (mark-lazy name)))))]
      [(_ (~name . xs) body ...) (identifier? #'~name)
       #'(define* ~name (lambda xs body ...))]))

  ;; --------------------------------------------------------------------------
  ;; Delay/force etc

  (require "force.rkt")

  (provide ~)

  ;; the exposed `!' (and other similar !s) must be a special form in the lazy
  ;; language -- but this is achieved through the lazy #%app (~!%app below)
  ;; that treats it (and the others) specially: uses mzscheme's application
  (define-for-syntax strict-names
    (syntax->list #'(! !! !list !!list !values !!values)))

  ;; --------------------------------------------------------------------------
  ;; Stepper utility fns

  (define-for-syntax (stepper-hide-operator stx)
    (stepper-syntax-property stx 'stepper-skipto (append skipto/cdr skipto/second)))
  (define-for-syntax (stepper-add-lazy-op-prop stx)
    (stepper-syntax-property stx 'lazy-op #t))
  
  (define-syntax (hidden-car stx)
    (syntax-case stx ()
      [(_ arg) (stepper-hide-operator (syntax/loc stx (car arg)))]))
  
  (define-syntax (hidden-cdr stx)
    (syntax-case stx ()
      [(_ arg) (stepper-hide-operator (syntax/loc stx (cdr arg)))]))
  
  (define-syntax (hidden-! stx)
    (syntax-case stx ()
      [(_ arg) (stepper-hide-operator (syntax/loc stx (! arg)))]))
  
  (define-syntax (mark-as-lazy-op stx)
    (syntax-case stx ()
      [(_ arg) 
       (identifier? #'arg)
       (stepper-add-lazy-op-prop (syntax/loc stx arg))]
      [(_ arg) #'arg]))
    
  (define-syntax (hidden-~ stx)
    (syntax-case stx ()
      [(_ arg) (stepper-hide-operator (syntax/loc stx (~ arg)))]))
  
  ;; --------------------------------------------------------------------------
  ;; Determine laziness

  (define-values (lazy-proc lazy-proc?)
    (let-values ([(type make pred ref set)
                  (make-struct-type
                   'lazy-proc #f 1 0 #f null (current-inspector) 0)])
      (values make pred)))
  (defsubst (lazy? x) (if (lazy-proc? x) #t (struct-constructor-procedure? x)))
  ;; a version that works on any value
  (defsubst (mark-lazy x) (if (procedure? x) (lazy-proc x) x))

  ;; a few primitive constructors
  (define ~cons   (lazy-proc cons))
  (define ~list   (lazy-proc list))
  (define ~list*  (lazy-proc list*))
  (define ~vector (lazy-proc vector))
  (define ~box    (lazy-proc box))
  ;; values is special, see below

  ;; --------------------------------------------------------------------------
  ;; Implicit begin & multiple values

  ;; This is used for implicit body begins.  It is slightly complex since it
  ;; should still be possible to use it for splicing up macro contents, so
  ;; definitions are used with a normal begin.  The actual body turns into one
  ;; promise that, when forced, forces each of its expressions and returns the
  ;; last value.  This effectively ties evaluation of all expressions in one
  ;; package, so (~begin foo bar) will always evaluate `foo' when the value of
  ;; `bar' is forced.
  (define-syntax ~begin
    (let ([ids (syntax->list
                #'(~define ~define-values define-syntax define-syntaxes
                   define-struct struct require provide))])
      (define (definition? stx)
        (ormap (lambda (id) (free-identifier=? id stx)) ids))
      (lambda (stx)
        (syntax-case stx ()
          ;; optimize simple cases
          [(_) #'(begin)]
          [(_ expr) #'expr]
          [(_ expr ...)
           (let loop ([exprs #'(expr ...)] [defs '()])
             (syntax-case exprs ()
               [((head . rest) expr ...)
                (definition? #'head)
                (loop #'(expr ...) (cons #'(head . rest) defs))]
               ;; only definitions
               [() #`(begin #,@(reverse defs))]
               ;; single expr
               [(expr) #`(begin #,@(reverse defs) expr)]
               [(expr ...)
                #`(begin #,@(reverse defs) (hidden-~ (begin (hidden-! expr) ...)))]))]))))

  ;; redefined to use lazy-proc and ~begin
  (define-syntax (~lambda stx)
    (syntax-case stx ()
      [(_ args body0 body ...)
       (let ([n (syntax-local-name)])
         (with-syntax ([lam (syntax-property
                             (syntax/loc stx
                               (lambda args (~begin body0 body ...)))
                             'inferred-name n)])
           (syntax/loc stx (lazy-proc lam))))]))
  (provide (rename-out [~lambda λ]))
  
;  (defsubst
;    (~define (f . xs) body0 body ...) (define f (~lambda xs body0 body ...))
;    (~define v x) (define v x))
  ;; STC: define ~define to add stepper-properties
  ;; had to duplicate some stuff from ~lambda
  (define-syntax (~define stx)
    (define (attach-inferred-name stx fn-name-stx)
      (syntax-property
       (stepper-syntax-property
        (stepper-syntax-property
         stx
         'stepper-define-type 'shortened-proc-define)
        'stepper-proc-define-name fn-name-stx)
       'inferred-name fn-name-stx))
    ; duplicated some stuff from ~lambda so I could add stepper-properties
    (syntax-case stx ()
      [(_ (f . args) body0 body ...)
       (quasisyntax/loc stx
         (define f 
           (lazy-proc
            #,(attach-inferred-name
               #'(lambda args (~begin body0 body ...))
               #'f)
            )))]
      [(_ name expr) #'(define name expr)]))
  
  (defsubst
    (~let [(x v) ...] body0 body ...)
      (let ([x v] ...) (~begin body0 body ...))
    (~let name [(x v) ...] body0 body ...)
      (let name [(x v) ...] (~begin body0 body ...)))
  (defsubst (~let* [(x v) ...] body0 body ...)
    (let* ([x v] ...) (~begin body0 body ...)))
  (defsubst (~letrec [(x v) ...] body0 body ...)
    (letrec ([x v] ...) (~begin body0 body ...)))

  ;; parameterize should force its arguments
  (defsubst (~parameterize ([param val] ...) body ...)
    ;; like ~begin, delaying the whole thing is necessary to tie the evaluation
    ;; to whenever the value is actually forced
    (hidden-~ (parameterize ([param (hidden-! val)] ...) (~begin body ...))))

  ;; Multiple values are problematic: Racket promises can use multiple
  ;; values, but to carry that out `call-with-values' should be used in all
  ;; places that deal with multiple values, which will make the whole thing
  ;; much slower (about twice in tight loops) -- but multiple values are rarely
  ;; used (spceifically, students never use them).  So `values' is redefined to
  ;; produce a first-class tuple-holding struct, and `split-values' turns that
  ;; into multiple values.
  ;; STC: add inspector for lazy stepper
  (struct multiple-values (values) #:inspector (make-inspector))
  (define (split-values x)
    (let ([x (! x)])
      (if (multiple-values? x) (apply values (multiple-values-values x)) x)))
  (define-syntax (hidden-split-values stx)
    (syntax-case stx ()
      [(_ arg) (stepper-hide-operator (syntax/loc stx (split-values arg)))]))
  ;; Force and split resulting values.
  (define (!values x)
    (split-values (! x)))
  ;; Similar, but forces the actual values too.
  (define (!!values x)
    (let ([x (! x)])
      (if (multiple-values? x)
        (apply values (map ! (multiple-values-values x)))
        x)))

  (define* ~values
    (case-lambda [(x) x] [xs (multiple-values xs)]))

  ;; Redefine multiple-value constructs so they split the results
  (defsubst (~define-values (v ...) body)
    (define-values (v ...) (hidden-split-values body)))
  (defsubst (~let-values ([(x ...) v] ...) body ...)
    (let-values ([(x ...) (split-values v)] ...) (~begin body ...)))
  (defsubst (~let*-values ([(x ...) v] ...) body ...)
    (let*-values ([(x ...) (split-values v)] ...) (~begin body ...)))
  (defsubst (~letrec-values ([(x ...) v] ...) body ...)
    (letrec-values ([(x ...) (split-values v)] ...) (~begin body ...)))

  ;; Redefine things that return multiple values.
  ;; (todo: only stuff necessary for the datatypes are done, more needed)
  (define* (~make-struct-type . args)
    (let ([args (!!list args)])
      (call-with-values (lambda () (apply make-struct-type args)) ~values)))

  ;; --------------------------------------------------------------------------
  ;; Applications

  ;; Basic names:
  ;; `app':    syntax, calls a function over given arguments
  ;; `apply':  function, last argument is a list of arguments to the function
  ;; Conventions:
  ;; `!*---':  forces args when needed (depending on the function)
  ;;           doesn't force the function (internal use only)
  ;; `!---':   forces function, and forces args when needed
  ;; `~!---':  adds a delay wrapper to the application (uses the above)
  ;;           (this is a macro in the `apply' case too)
  ;; `~!*---': like the previous, but does not force the function (internal)
  ;; Provided stuff:
  ;; `~!%app': provided as `#%app' -- similar to `~!app' but treats a few
  ;;           application kinds as special (mostly all kinds of forces)
  ;; `!apply': provided as `apply' (no need to provide `~!apply', since all
  ;;           function calls are delayed by `#%app')

  (define (extract-if-lazy-proc f)
    (or (procedure-extract-target f) f))
  (define-syntax (!*app stx)
    (syntax-case stx ()
      [(_ f x ...)
       (let ([$$ (lambda (stx)
                   (stepper-syntax-property 
                    stx
                    'stepper-skipto
                    (append skipto/cddr
                            `(both-l () (car)))))]
             [$  (lambda (stx)
                   (stepper-syntax-property
                    stx
                    'stepper-skipto
                    (append skipto/cdr 
                            skipto/first)))])
         (with-syntax ([(y ...) (generate-temporaries #'(x ...))])
           ;; use syntax/loc for better errors etc
           (with-syntax ([lazy   (syntax/loc stx ((extract-if-lazy-proc p) y ...))]
                         [strict (syntax/loc stx (p (hidden-! y) ...))])
             (quasisyntax/loc stx
               ((lambda (p y ...)
                  #,($$ #'(if (lazy? p) lazy strict)))
                f x ...)
               #;(let ([p f] [y x] ...)
                 ;; #,($$ #`(if (lazy? p) lazy strict))
                 (if (lazy? p) lazy strict))))))]))

  (defsubst (!app   f x ...) (!*app (hidden-! (mark-as-lazy-op f)) x ...))
  (defsubst (~!*app f x ...) (hidden-~ (!*app f x ...)))
  (defsubst (~!app  f x ...) (hidden-~ (!app f x ...)))

  (define-for-syntax (toplevel?)
    (memq (syntax-local-context)
          '(top-level module module-begin))) ; not sure about module-begin

  ;; What happens when encoutering a toplevel non-definition expression?
  (provide toplevel-forcer)
  (define toplevel-forcer (make-parameter !))

  (provide (rename-out [~!%app #%app])) ; all applications are delayed
  (define-syntax (~!%app stx) ; provided as #%app
    #;(define (unwinder stx rec)
      (syntax-case stx (!)
        [(let-values ([(_p) (_app ! f)] [(_y) x] ...) _body)
         (with-syntax ([(f x ...) (rec #'(f x ...))])
           #'(f x ...))]))
    #;(define (stepper-annotate stx)
      (let* ([stx (stepper-syntax-property stx 'stepper-hint unwinder)]
             [stx (stepper-syntax-property stx 'stepper-skip-double-break #t)])
        stx))
    (syntax-case stx (~)
      ;; the usual () shorthand for null
      [(_) #'null]
      [(_ ~ x) (syntax/loc stx (~ x))] ; not really needed
      [(_ f x ...)
       (cond [(let ([f #'f])
               (and (identifier? f)
                    (ormap (lambda (s) (free-identifier=? f s))
                           strict-names)))
              ;; strict function => special forms => use plain application
              (syntax/loc stx (f x ...))]
             [(toplevel?)
              ;; toplevel expressions are always forced
              (syntax/loc stx ((toplevel-forcer) (!app f x ...)))]
             [else (syntax/loc stx (~!app f x ...))])]))

  (define (!*apply f . xs)
    (let ([xs (!list (apply list* xs))])
      (apply f (if (lazy? f) xs (map ! xs)))))
  (define* (!apply f . xs)
    (let ([f (! f)] [xs (!list (apply list* xs))])
      (apply f (if (lazy? f) xs (map ! xs)))))
  (defsubst (~!*apply f . xs) (hidden-~ (!*apply f . xs)))
  (defsubst (~!apply  f . xs) (hidden-~ (!apply  f . xs)))

  (provide (rename-out [!apply apply])) ; can only be used through #%app => delayed

  ;; do the same special treatment for toplevel variable expressions
  (provide (rename-out [!top #%top]))
  (define-syntax (!top stx)
    (syntax-case stx ()
      [(_ . id) (if (toplevel?) #'(! (#%top . id)) #'(#%top . id))]))

  ;; used for explicitly strict/lazy calls
  (defsubst (strict-call f x ...) (hidden-~ (f (! x) ...)))
  (defsubst (lazy-call f x ...) (hidden-~ (f x ...)))

  ;; --------------------------------------------------------------------------
  ;; Special forms that are now functions

  ;; Since these things are rarely used as functions, they are defined as
  ;; macros that expand to the function form when used as an expression.

  (define* *if
    (case-lambda [(e1 e2 e3) (if (! e1) e2 e3)]
                 [(e1 e2   ) (when (! e1) e2   )]))
  (defsubst (~if e1 e2 e3) (hidden-~ (if (hidden-! e1) e2 e3))
            (~if e1 e2   ) (hidden-~ (if (hidden-! e1) e2   ))
            ~if *if)

  (define* (*and . xs)
    (let ([xs (!list xs)])
      (or (null? xs)
          (let loop ([x (car xs)] [xs (cdr xs)])
            (if (null? xs) x (and (! x) (loop (car xs) (cdr xs))))))))
  (define-syntax !and
    (syntax-rules ()
      [(_) (and)]
      [(_ x ... y) (and (hidden-! x) ... y)]))
  (defsubst (~and x ...) (hidden-~ (!and x ...)) ~and *and)

  (define* (*or . xs)
    (let ([xs (!list xs)])
      (and (pair? xs)
           (let loop ([x (car xs)] [xs (cdr xs)])
             (if (null? xs) x (or (! x) (loop (car xs) (cdr xs))))))))
  (define-syntax !or
    (syntax-rules ()
      [(_) (or)]
      [(_ x ... y) (or (hidden-! x) ... y)]))
  (defsubst (~or x ...) (hidden-~ (!or x ...)) ~or *or)

  ;; --------------------------------------------------------------------------
  ;; Special forms that are still special forms since they use ~begin

  (defsubst (~begin0 x y ...) ; not using ~begin, but equivalent
    (hidden-~ (let ([val (hidden-! x)]) (hidden-! y) ... val)))

  (defsubst (~when   e x ...) (hidden-~ (when   (hidden-! e) (~begin x ...))))
  (defsubst (~unless e x ...) (hidden-~ (unless (hidden-! e) (~begin x ...))))

  ;; --------------------------------------------------------------------------
  ;; Misc stuff

  ;; Just for fun...
  (defsubst (~set! id expr) (hidden-~ (set! id (hidden-! expr))))
  ;; The last ! above is needed -- without it:
  ;;   (let ([a 1] [b 2]) (set! a (add1 b)) (set! b (add1 a)) a)
  ;; goes into an infinite loop.  (Thanks to Jos Koot)

  (define* (~set-mcar! mpair val) (~ (set-mcar! (! mpair) val)))
  (define* (~set-mcdr! mpair val) (~ (set-mcdr! (! mpair) val)))
  (define* (~vector-set! vec i val) (~ (vector-set! (! vec) (! i) val)))
  (define* (~set-box! box val) (~ (set-box! (! box) val)))

  ;; not much to do with these besides inserting strictness points and ~begin
  ; for stepper: change else to #t test, add new error else branch
  (define-syntax (~cond stx)
    (syntax-case stx ()
      [(_ clause ...) ; stepper needs the loc of the full clause
       (with-syntax
           ([(new-clause ...)
             (map 
              (λ (c)
                (with-syntax ([(test body ...) c])
                  (with-syntax
                      ([new-test
                        (syntax-case #'test (else)
                          [else ; for stepper
                           (stepper-syntax-property #'#t 'stepper-else #t)]
                          [x (syntax/loc #'x (hidden-! x))])])
                    (syntax/loc c (new-test (~begin body ...))))))
              (syntax->list #'(clause ...)))]
            [new-else-body (syntax/loc stx (error 'cond "should not get here"))])
         (quasisyntax/loc stx
           (hidden-~ 
            #,(syntax/loc stx
                (cond
                  new-clause ...
                  [else new-else-body])))))]))
  (defsubst (~case v [keys body ...] ...)
    (hidden-~ (case (hidden-! v) [keys (~begin body ...)] ...)))

  ;; Doing this will print the whole thing, but problems with infinite things
  (define* (~error . args) (apply error (!! args)))

  ;; I/O shows the whole thing
  (define* (~printf fmt . args) (apply printf (! fmt) (!! args)))
  (define* (~fprintf p fmt . args) (apply fprintf (! p) (! fmt) (!! args)))
  (define* (~display x . port)  (apply display (!! x) (!!list port)))
  (define* (~write   x . port)  (apply write   (!! x) (!!list port)))
  (define* (~print   x . port)  (apply print   (!! x) (!!list port)))

  ;; --------------------------------------------------------------------------
  ;; Equality functions

  ;; All of these try to stop if the promises are the same.

  (define* (~eq? . args)
    (or (apply eq? (!list args)) (apply eq? (!!list args))))

  (define* (~eqv? . args)
    (or (apply eqv? (!list args)) (apply eqv? (!!list args))))

  ;; for `equal?' we must do a recursive scan
  (define (equal2? x y)
    (cond [(pair? x) (and (pair? y)
                          (~equal? (car x) (car y))
                          (~equal? (cdr x) (cdr y)))]
          [(vector? x)
           (let ([k (vector-length x)])
             (and (vector? y)
                  (= k (vector-length y))
                  (let loop ([i 0])
                    (or (= i k)
                        (and (~equal? (vector-ref x i) (vector-ref y i))
                             (loop (add1 i)))))))]
          [(struct? x)
           (and (struct? y)
                (let-values ([(xtype xskipped?) (struct-info x)]
                             [(ytype yskipped?) (struct-info y)])
                  (and xtype ytype (not xskipped?) (not yskipped?)
                       (eq? xtype ytype)
                       (let*-values ([(name initk autok ref set imms spr skp?)
                                      (struct-type-info xtype)]
                                     [(k) (+ initk autok)])
                         (let loop ([i 0])
                           (or (= i k) (and (~equal? (ref x i) (ref y i))
                                            (loop (add1 i)))))))))]
          [(box? x) (and (box? y) (~equal? (unbox x) (unbox y)))]
          [else #f]))
  (define* (~equal? x y . args)
    (let ([args (!list args)])
      (if (pair? args)
        (and (~equal? x y) (apply ~equal? x (cdr args)))
        (or (equal? x y)
            (let ([x (! x)] [y (! y)])
              (or (equal? x y) (equal2? x y)))))))

  ;; --------------------------------------------------------------------------
  ;; List functions

  (define* (~list?  x) (list?  (!list x))) ; must force the whole list
  (define* (~length l) (length (!list l))) ; for these

  (define* (~car    x) (car (! x))) ; these are for internal use: ~!app will do
  (define* (~cdr    x) (cdr (! x))) ; this job when using this language
  (define* (~caar   x) (car (! (car (! x)))))
  (define* (~cadr   x) (car (! (cdr (! x)))))
  (define* (~cdar   x) (cdr (! (car (! x)))))
  (define* (~cddr   x) (cdr (! (cdr (! x)))))
  (define* (~caaar  x) (car (! (~caar x))))
  (define* (~caadr  x) (car (! (~cadr x))))
  (define* (~cadar  x) (car (! (~cdar x))))
  (define* (~caddr  x) (car (! (~cddr x))))
  (define* (~cdaar  x) (cdr (! (~caar x))))
  (define* (~cdadr  x) (cdr (! (~cadr x))))
  (define* (~cddar  x) (cdr (! (~cdar x))))
  (define* (~cdddr  x) (cdr (! (~cddr x))))
  (define* (~caaaar x) (car (! (~caaar x))))
  (define* (~caaadr x) (car (! (~caadr x))))
  (define* (~caadar x) (car (! (~cadar x))))
  (define* (~caaddr x) (car (! (~caddr x))))
  (define* (~cadaar x) (car (! (~cdaar x))))
  (define* (~cadadr x) (car (! (~cdadr x))))
  (define* (~caddar x) (car (! (~cddar x))))
  (define* (~cadddr x) (car (! (~cdddr x))))
  (define* (~cdaaar x) (cdr (! (~caaar x))))
  (define* (~cdaadr x) (cdr (! (~caadr x))))
  (define* (~cdadar x) (cdr (! (~cadar x))))
  (define* (~cdaddr x) (cdr (! (~caddr x))))
  (define* (~cddaar x) (cdr (! (~cdaar x))))
  (define* (~cddadr x) (cdr (! (~cdadr x))))
  (define* (~cdddar x) (cdr (! (~cddar x))))
  (define* (~cddddr x) (cdr (! (~cdddr x))))

  (define* (~list-ref l k)
    (let ([k (! k)])
      (unless (exact-nonnegative-integer? k)
        (raise-type-error 'list-ref "non-negative exact integer" 1 l k))
      (let loop ([k k] [l (! l)])
        (cond [(not (pair? l))
               (raise-type-error 'list-ref "proper list" l)]
              [(zero? k) (car l)]
              [else (loop (sub1 k) (! (cdr l)))]))))
  (define* (~list-tail l k)
    (let ([k (! k)])
      (unless (exact-nonnegative-integer? k)
        (raise-type-error 'list-tail "non-negative exact integer" 1 l k))
      (let loop ([k k] [l l]) ; don't force here -- unlike list-ref
        (cond [(zero? k) l]
              [else (let ([l (! l)])
                      (unless (pair? l)
                        (raise-type-error 'list-tail "list" l))
                      (loop (sub1 k) (cdr l)))]))))

  (define* (~append . xs)
    (let ([xs (!list xs)])
      (cond [(null? xs) '()]
            [(null? (cdr xs)) (car xs)]
            [else (let ([ls (~ (apply ~append (cdr xs)))])
                    (let loop ([l (! (car xs))])
                      (if (null? l)
                        ls
                        (cons (car l) (~ (loop (! (cdr l))))))))])))

  ;; useful utility for many list functions below
  (define (!cdr l) (! (cdr l)))

  (define-syntax (deflistiter stx)
    (syntax-case stx (extra: null ->)
      [(deflistiter (?~name ?proc ?args ... ?l . ?ls)
         null -> ?base
         ?loop -> ?step-single ?step-multiple)
       #'(deflistiter (?~name ?proc ?args ... ?l . ?ls)
           extra:
           null -> ?base
           ?loop -> ?step-single ?step-multiple)]
      [(deflistiter (?~name ?proc ?args ... ?l . ?ls)
         extra: [?var ?init] ...
         null -> ?base
         ?loop -> ?step-single ?step-multiple)
       (with-syntax ([?name (let* ([x (symbol->string (syntax-e #'?~name))]
                                   [x (regexp-replace #rx"^~" x "")]
                                   [x (string->symbol x)])
                              (datum->syntax #'?~name x #'?~name))])
         #'(define* ?~name
             (case-lambda
               [(?proc ?args ... ?l)
                (let ([?proc (hidden-! ?proc)])
                  (let ?loop ([?l (hidden-! ?l)] [?var ?init] ...)
                    (if (null? ?l)
                      ?base
                      ?step-single)))]
               [(?proc ?args ... ?l . ?ls)
                (let ([?proc (hidden-! ?proc)])
                  (let ?loop ([?ls (cons (hidden-! ?l) (!!list ?ls))] [?var ?init] ...)
                    (if (ormap null? ?ls)
                      (if (andmap null? ?ls)
                        ?base
                        (error '?name "all lists must have same size"))
                      ?step-multiple)))])))]))

  ;; These use the `*' version of app/ly, to avoid forcing the function over
  ;; and over -- `deflistiter' forces it on entry
  (deflistiter (~map proc l . ls)
    null -> '()
    loop -> (cons (~!*app proc (car l)) (~ (loop (! (cdr l)))))
            (cons (~!*apply proc (map car ls)) (~ (loop (map !cdr ls)))))
  (deflistiter (~for-each proc l . ls)
    null -> (void)
    loop -> (begin (! (!*app proc (car l))) (loop (! (cdr l))))
            (begin (! (!*apply proc (map car ls))) (loop (map !cdr ls))))
  (deflistiter (~andmap proc l . ls)
    null -> #t
    loop -> (and (! (!*app proc (car l))) (loop (! (cdr l))))
            (and (! (!*apply proc (map car ls))) (loop (map !cdr ls))))
  (deflistiter (~ormap proc l . ls)
    null -> #f
    loop -> (or (! (!*app proc (car l))) (loop (! (cdr l))))
            (or (! (!*apply proc (map car ls))) (loop (map !cdr ls))))
  (deflistiter (foldl proc init l . ls)
    extra: [acc init]
    null -> acc
    loop ->
      (~ (loop (! (cdr l)) (~!*app proc (car l) acc)))
      (~ (loop (map !cdr ls)
               (~!*apply proc (append (map car ls) (list acc))))))
  (deflistiter (foldr proc init l . ls)
    null -> init
    loop ->
      (~!*app proc (car l) (~ (loop (! (cdr l)))))
      (~!*apply proc (append (map car ls) (list (~ (loop (map !cdr ls)))))))

  (define (do-member name = elt list) ; no currying for procedure names
    ;; `elt', `=', and `name' are always forced values
    (let loop ([list (! list)])
      (cond [(null? list) #f]
            [(not (pair? list)) (error name "not a proper list: ~e" list)]
            [(= elt (! (car list))) list]
            [else (loop (! (cdr list)))])))
  (define* (~member elt list) (do-member 'member ~equal? (! elt) list))
  (define* (~memq   elt list) (do-member 'memq   ~eq?    (! elt) list))
  (define* (~memv   elt list) (do-member 'memv   ~eqv?   (! elt) list))

  (define (do-assoc name = key alist) ; no currying for procedure names
    ;; `key', `=', and `name' are always forced values
    (let loop ([alist (! alist)])
      (cond [(null? alist) #f]
            [(not (pair? alist)) (error name "not a proper list: ~e" alist)]
            [else (let ([cell (! (car alist))])
                    (cond [(not (pair? cell))
                           (error name "non-pair found in list: ~e" cell)]
                          [(= (! (car cell)) key) cell]
                          [else (loop (! (cdr alist)))]))])))
  (define* (~assoc key alist) (do-assoc 'assoc ~equal? (! key) alist))
  (define* (~assq  key alist) (do-assoc 'assq  ~eq?    (! key) alist))
  (define* (~assv  key alist) (do-assoc 'assv  ~eqv?   (! key) alist))

  (define* (~reverse list)
    (let ([list (!list list)])
      (reverse list)))

  ;; --------------------------------------------------------------------------
  ;; Extra functionality that is useful for lazy list stuff

  (define* (take n l)
    (let ([n0 (! n)])
      (unless (exact-nonnegative-integer? n0)
        (raise-type-error 'take "non-negative exact integer" 0 n0 l))
      (let loop ([n n0] [l l])
        (if (zero? n)
          '()
          (let ([l (! l)])
            (cond [(null? l)
                   ;; it would be fine to force the whole list (since we now
                   ;; know it's finite), but doing so means keeping a reference
                   ;; to its head, which can lead to memory leaks.
                   (error 'take "index ~e too large for input list" n0)]
                  [(pair? l) (cons (car l) (~ (loop (sub1 n) (cdr l))))]
                  [else (error 'take "not a proper list: ~e" l)]))))))

  ;; not like Haskell's `cycle' that consumes a list
  (define* (cycle . l)
    (letrec ([r (~ (~append (! l) r))])
      r))

  ;; --------------------------------------------------------------------------
  ;; mzlib/list functionality

  ;; These are a hack, they're not the same due to different error
  ;; messages (and they work with improper lists too).
  (define* (rest x) (~cdr x))
  (define* (first   x) (~car    x))
  (define* (second  x) (~cadr   x))
  (define* (third   x) (~caddr  x))
  (define* (fourth  x) (~cadddr x))
  (define* (fifth   x) (~car    (~cddddr x)))
  (define* (sixth   x) (~cadr   (~cddddr x)))
  (define* (seventh x) (~caddr  (~cddddr x)))
  (define* (eighth  x) (~cadddr (~cddddr x)))
  (define* (cons? x) (pair? (! x)))
  (define* empty null)
  (define* (empty? x) (null? (! x)))

  (require (only-in racket/list [last-pair !last-pair]))
  (define* (last-pair list) (!last-pair (!list list)))

  (define (do-remove name item list =)
    (let ([= (! =)])
      (let loop ([list (! list)])
        (cond [(null? list) list]
              [(not (pair? list))
               (error name "not a proper list: ~e" list)]
              [(!*app = item (car list)) (cdr list)]
              [else (cons (car list) (~ (loop (! (cdr list)))))]))))
  (define* remove
    (case-lambda [(item list  ) (do-remove 'remove item list ~equal?)]
                 [(item list =) (do-remove 'remove item list =)]))
  (define* (remq item list)     (do-remove 'remq   item list ~eq?))
  (define* (remv item list)     (do-remove 'remv   item list ~eqv?))

  (define (do-remove* name items list =)
    (let ([= (! =)] [items (!list items)])
      (let loop ([list (! list)])
        (cond [(null? list) list]
              [(not (pair? list))
               (error name "not a proper list: ~e" list)]
              [else
               (let ([xs (~ (loop (! (cdr list))))])
                 (if (memf (lambda (item) (!*app = item (car list))) items)
                   xs
                   (cons (car list) xs)))]))))
  (define* remove*
    (case-lambda [(items list  ) (do-remove* 'remove* items list ~equal?)]
                 [(items list =) (do-remove* 'remove* items list =)]))
  (define* (remq* items list)    (do-remove* 'remq*   items list ~eq?))
  (define* (remv* items list)    (do-remove* 'remv*   items list ~eqv?))

  (define* (memf pred list)
    (let ([pred (! pred)])
      (let loop ([list (! list)])
        (cond [(null? list) #f]
              [(not (pair? list)) (error 'memf "not a proper list: ~e" list)]
              [(!*app pred (car list)) list]
              [else (loop (! (cdr list)))]))))

  (define* (assf pred alist)
    (let ([pred (! pred)])
      (let loop ([alist (! alist)])
        (cond [(null? alist) #f]
              [(not (pair? alist)) (error 'assf "not a proper list: ~e" alist)]
              [else (let ([cell (! (car alist))])
                      (cond [(not (pair? cell))
                             (error 'assf "non-pair found in list: ~e" cell)]
                            [(!*app pred (car cell)) cell]
                            [else (loop (! (cdr alist)))]))]))))

  (define* (filter pred list)
    (let ([pred (! pred)])
      (let loop ([list (! list)])
        (cond [(null? list) list]
              [(pair? list)
               (let ([x (car list)]
                     [xs (~ (loop (! (cdr list))))])
                 (if (! (!*app pred x)) (cons x xs) xs))]
              [else (error 'filter "not a proper list: ~e" list)]))))

  (require (only-in racket/base [sort !sort]))
  (define* (sort list less?)
    (let ([less? (! less?)])
      (!sort (!list list) (lambda (x y) (! (!*app less? x y))))))

  ;; --------------------------------------------------------------------------
  ;; mzlib/etc functionality

  (require (only-in racket/bool boolean=? symbol=?))
  (define* true  #t)
  (define* false #f)

  (define* (identity x) x)
  ;; no need for dealing with multiple values since students don't use them
  (define* (compose . fs)
    (let ([fs (!list fs)])
      (cond [(null? fs) identity]
            [(null? (cdr fs)) (car fs)]
            [else (let ([fs (reverse fs)])
                    (lambda xs
                      (let loop ([fs (cdr fs)]
                                 [x  (~!apply (car fs) xs)])
                        (if (null? fs)
                          x
                          (loop (cdr fs) (~!app (car fs) x))))))])))

  (define* (build-list n f)
    (let ([n (! n)] [f (! f)])
      (unless (exact-nonnegative-integer? n)
        (error 'build-list "~s must be an exact integer >= 0" n))
      (unless (procedure? f)
        (error 'build-list "~s must be a procedure" f))
      (let loop ([i 0])
        (if (>= i n)
          '()
          (cons (~ (f i)) (~ (loop (add1 i))))))))

  ;; --------------------------------------------------------------------------
  ;; Provide everything except some renamed stuff

  (define-syntax (provide-strict-names stx)
    #`(provide #,@strict-names))
  (provide-strict-names)

  (define-syntax (renaming-provide stx)
    (syntax-case stx ()
      [(_ id ...)
       (with-syntax
           ([(~id ...)
             (map (lambda (id)
                    (let* ([str (symbol->string (syntax-e id))]
                           [~id (string->symbol (string-append "~" str))])
                      (datum->syntax id ~id id)))
                  (syntax->list #'(id ...)))])
         #'(provide (except-out (all-from-out racket/base) module #%app apply #%top λ
                                id ...)
                    (rename-out [~id id] ...)))]))
  (renaming-provide
   lambda define let let* letrec parameterize
   values define-values let-values let*-values letrec-values make-struct-type
   cons list list* vector box
   if and or begin begin0 when unless
   set! set-mcar! set-mcdr! vector-set! set-box!
   cond case error printf fprintf display write print
   eq? eqv? equal?
   list? length list-ref list-tail append map for-each andmap ormap
   member memq memv assoc assq assv reverse
   caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar
   caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr
   cddaar cddadr cdddar cddddr)

  (provide
   ;; multiple values (see above)
   split-values
   ;; explicit strict/lazy calls
   strict-call lazy-call
   ;; `list' stuff
   first second third fourth fifth sixth seventh eighth rest cons? empty empty?
   foldl foldr last-pair remove remq remv remove* remq* remv* memf assf filter
   sort
   ;; `etc' stuff
   true false boolean=? symbol=? identity compose build-list
   ;; extra stuff for lazy Scheme
   take cycle)


#|
;; Some tests
(cadr (list (/ 1 0) 1 (/ 1 0))) -> 1
(foldl + 0 '(1 2 3 4)) -> 10
(foldl (lambda (x y) y) 0 (list (/ 1 0) (/ 2 0) (/ 3 0))) -> 0
(foldl (lambda (x y) y) 0 (cons (/ 1 0) (cons (/ 2 0) '()))) -> 0
(foldr + 0 '(1 2 3 4)) -> 10
(foldr (lambda (x y) y) 0 (list (/ 1 0) (/ 2 0) (/ 3 0))) -> 0
(foldr (lambda (x y) y) 0 (cons (/ 1 0) (cons (/ 2 0) '()))) -> 0
(define ones (cons 1 ones))
(take 5 (foldr cons '() ones)) -> (1 1 1 1 1)
(define a (list (/ 1 0) 2 (/ 3 0)))
(caadr (map list a)) -> 2
(cadr (map + a a)) -> 4
(andmap even? '(1 2 3 4)) -> #f
(ormap even? '(1 2 3 4)) -> #t
(ormap even? '(1 21 3 41)) -> #f
(andmap even? (list 1 2 3 (/ 4 0))) -> #f
|#
