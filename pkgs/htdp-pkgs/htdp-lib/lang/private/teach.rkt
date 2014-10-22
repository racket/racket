;; Implements the syntactic forms for the HtDP teaching languages. The
;; reader-level aspects of the language (e.g., case-sensitivity) are
;; not implemented here, and the procedures are in a separate
;; module.

;; To a first approximation, this module is one big error-checking
;; routine. In other words, most of the syntax implementations are
;; straightforward, but error-checking is complex.

;; Error-message conventions:
;;  - Report errors, somewhat anthropomorphically, in terms of "expected"
;;    versus "found" syntax.
;;  - Report errors according to a left-to-right reading; e.g., the
;;    error in `(define 1 2 3)' is "expected an identifier, found 1",
;;    not "expected two parts after `define', but found three".
;;  - The error message should always explain what is wrong, not simply
;;    state a fact. For example, "f defined previously, so it cannot
;;    be re-defined here" is a good error message; in contrast, "found
;;    second definition of f here" doesn't say what's wrong with a second
;;    definition.

;; Left-to-right reporting sometimes requires an explicit expression
;; check before reporting some other error. For example, in the
;; expression (cond [true + 1 2]), the reported error should ideally
;; be for a misuse of "+", not that there are two extra parts in the
;; clause. This check requires local-expanding, so it doesn't work
;; when checking top-level forms like `define' (because not all of the
;; definitions are ready, yet). For other cases, ensure that the
;; expansion is in an expression position (not the top level) and use
;; the `local-expand-for-error' function instead of `local-expand' to
;; help declare that the expansion is merely for left-to-right error
;; reporting. As always, avoid using `local-expand' if there's no
;; error.

#lang mzscheme

(require mzlib/etc
         mzlib/list
         mzlib/math
         mzlib/pconvert-prop
         scheme/match
         racket/undefined
         "set-result.rkt"
         (only racket/base define-struct)
         racket/struct-info
         deinprogramm/signature/signature-english
         (all-except deinprogramm/signature/signature signature-violation)
         (all-except lang/private/signature-syntax property)
         (rename lang/private/signature-syntax signature:property property)
         (all-except deinprogramm/quickcheck/quickcheck property)
         (rename deinprogramm/quickcheck/quickcheck quickcheck:property property)
         test-engine/racket-tests
         scheme/class
         "../posn.rkt"
         (only lang/private/teachprims
               beginner-equal? beginner-equal~? teach-equal?
               advanced-cons advanced-list*))

(require "rewrite-error-message.rkt")

(require-for-syntax "teachhelp.rkt"
                    "rewrite-error-message.rkt"
                    "teach-shared.rkt"
                    "rewrite-error-message.rkt"
                    racket/syntax
                    syntax/kerncase
                    syntax/stx
                    syntax/struct
                    syntax/context
                    mzlib/include
                    scheme/list
                    (rename racket/base racket:define-struct define-struct)
                    (only racket/base syntax->datum datum->syntax)
                    (rename racket/base kw-app #%app)
                    racket/struct-info
                    stepper/private/syntax-property
                    test-engine/racket-tests)

(define-for-syntax EXPECTED-HEADER
  "expected a variable name, or a function name and its variables (in parentheses), but ~a")

(define-for-syntax EXPECTED-MATCH-PATTERN
  "expected a pattern--answer clause after the expression following `match', but nothing's there")

(define-for-syntax EXPECTED-FUNCTION-NAME
  "expected a function after the open parenthesis, but found a structure name")

(define-for-syntax EXPECTED-MUTABLE
  "expected a mutable variable after set!, but found a variable that cannot be modified: ~a")

(define-for-syntax EXPECTED-1
  "expected at least one binding (in parentheses) after the function name, but found none")

(define-for-syntax EXPECTED-1-FIELD
  "expected at least one field name (in parentheses) after the structure name, but nothing's there")

(define-for-syntax EXPECTED-CLAUSE
  "expected a clause with a question and an answer, but found a clause with only one part")

(define-for-syntax EXPECTED-GOOD-CLAUSE
  "expected a clause with a question and an answer, but found a clause with ~a parts")

(define-for-syntax EXPECTED-MORE-THAN-NOTHING
  (string-append
   "expected a clause with at least one choice (in parentheses) and an answer after the expression,"
   " but nothing's there"))

(define-for-syntax EXPECTED-SYMBOL-OR-NUMBER+
  "expected a symbol (without its quote) or a number as a choice, but found ~a")

(define-for-syntax EXPECTED-SYMBOL-OR-NUMBER
  "expected a symbol (without its quote) or a number as a choice, but nothing's there")

(define-for-syntax EXPECTED-CLAUSE-WITH-CHOICE
  "expected a clause with at least one choice (in parentheses) and an answer, but found ~a")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-time helpers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; verify-boolean is inserted to check for boolean results:
(define (verify-boolean b where)
  (if (or (eq? b #t) (eq? b #f))
      b
      (raise
       (make-exn:fail:contract
        (format "~a: question result is not true or false: ~e" where b)
        (current-continuation-marks)))))

(define (identifier-is-bound? id)
  (or (identifier-binding id)
      ;; identifier-binding returns #f for variable bound at the top-level,
      ;; check explicitly:
      (and (namespace-variable-value (syntax-e id) #t (lambda () #f)) #t)))

;; Wrapped around top-level definitions to disallow re-definition:
(define (check-top-level-not-defined who id)
  (when (identifier-is-bound? id)
    (raise-syntax-error #f "this name was defined previously and cannot be re-defined" id)))

(define (top/check-defined id)
  (namespace-variable-value (syntax-e id) #t (lambda () (raise-not-bound-error id))))

;; For quasiquote and shared:
(require (rename "teachprims.rkt" the-cons advanced-cons))
(require (only   "teachprims.rkt" cyclic-list?))

;; Referenced to ensure that evaluating `lambda' always
;; produces a new closure (instead of using a closure
;; that's allocated once)
(define make-lambda-generative 5)

;; A consistent pattern for stepper-skipto:
(define-for-syntax (stepper-ignore-checker stx)
  (stepper-syntax-property stx 'stepper-skipto '(syntax-e cdr syntax-e cdr car)))

(define-for-syntax (map-with-index proc . lists)
  (let loop ([i 0] [lists lists] [rev-result '()])
    (if (null? (car lists))
        (reverse rev-result)
        (loop (+ 1 i)
              (map cdr lists)
              (cons (apply proc i (map car lists)) rev-result)))))

;; build-struct-names is hard to handle
(define-for-syntax (make-struct-names name fields stx)
  (apply (lambda (struct: constructor predicate . rest)
           (let loop ([rest rest]
                      [getters '()]
                      [setters '()])
             (if (null? rest)
                 (values struct: constructor predicate (reverse getters) (reverse setters))
                 (loop (cddr rest)
                       (cons (car rest) getters)
                       (cons (cadr rest) setters)))))
         (build-struct-names name fields #f #f stx)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax implementations
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-syntax-set/provide
  (lambda (stx)
    (syntax-case stx ()
      [(_ (id ...) defn ...)
       (with-syntax ([(plain-id ...)
                      (map (lambda (id)
                             (if (identifier? id)
                                 id
                                 (stx-car (stx-cdr id))))
                           (syntax->list (syntax (id ...))))]
                     [provided-identifiers
                      (datum->syntax-object stx 'provided-identifiers)])
         (syntax
          (begin
            (provide plain-id ...)
            (define-syntax-set (id ...) 
              (define provided-identifiers (quote-syntax (id ...)))
              defn ...))))])))


;; The implementation of form X is defined below as X/proc. The
;; reason for this is to allow the implementation of Y to re-use the
;; implementation of X (expanding to a use of X would mangle syntax
;; error messages), while preserving the binding of X as the one for
;; the syntax definition (so that quasiquote can recognize unquote,
;; etc.).

(define-syntax-set/provide (beginner-define
                             beginner-define-struct
                             beginner-lambda
                             beginner-app     beginner-app-continue
                             beginner-top     beginner-top-continue
                             beginner-cond
                             beginner-else
                             beginner-if
                             beginner-and
                             beginner-or
                             beginner-quote/expr
                             beginner-require
                             beginner-dots
                             beginner-true
                             beginner-false
                             
                             intermediate-define
                             intermediate-define-struct
                             intermediate-pre-lambda
                             intermediate-local
                             intermediate-letrec
                             intermediate-let
                             intermediate-let*
                             intermediate-recur
                             intermediate-app
                             intermediate-quote/expr
                             intermediate-quasiquote/expr
                             intermediate-unquote
                             intermediate-unquote-splicing
                             intermediate-time
                             
                             intermediate-lambda-define
                             intermediate-lambda
                             
                             advanced-define
                             advanced-lambda
                             advanced-app
                             advanced-set!  advanced-set!-continue
                             advanced-when
                             advanced-unless
                             advanced-define-struct
                             advanced-define-datatype
                             advanced-let
                             advanced-recur
                             advanced-begin
                             advanced-begin0
                             advanced-case
                             advanced-match
                             advanced-shared
                             advanced-delay
                             
                             define-wish)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; compile-time helpers
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Raise a syntax error:
  (define (teach-syntax-error form stx detail msg . args)
    (let ([form (or form (first (flatten (syntax->datum stx))))]
          [msg (apply format msg args)])
      (if detail
          (raise-syntax-error form msg stx detail)
          (raise-syntax-error form msg stx))))
  
  
  (define (teach-syntax-error* form stx details msg . args)
    (let ([exn (with-handlers ([exn:fail:syntax?
                                (lambda (x) x)])
                 (apply teach-syntax-error form stx #f msg args))])
      (raise
       (make-exn:fail:syntax
        (exn-message exn)
        (exn-continuation-marks exn)
        details))))
  
  (define (binding-in-this-module? b)
    (and (list? b)
         (module-path-index? (car b))
         (let-values ([(path base) (module-path-index-split (car b))])
           (and (not path) (not base)))))
  
  ;; The syntax error when a form's name doesn't follow a "("
  (define (bad-use-error name stx)
    (teach-syntax-error
     name
     stx
     #f
     "expected an open parenthesis before ~a, but found none" name))
  
  ;; Use for messages "expected ..., found <something else>"
  (define (something-else v)
    (let ([v (syntax-e v)])
      (cond
        [(number? v) "a number"]
        [(string? v) "a string"]
        [(list? v) "a part"]
        [(struct? v) "an image"]
        [else "something else"])))
  
  (define (ordinal n)
    (cond
      [(or (<= 11 n 13)
           (zero? (modulo n 10))
           (<= 4 (modulo n 10) 9))
       (format "~ath" n)]
      [(= 1 (modulo n 10))
       (format "~ast" n)]
      [(= 2 (modulo n 10))
       (format "~and" n)]
      [(= 3 (modulo n 10))
       (format "~ard" n)]))
  
  
  ;; At the top level, wrap `defn' to first check for
  ;;  existing definitions of the `names'. The `names'
  ;;  argument is a syntax list of identifiers.
  ;; In a module context, just check the binding
  ;;  at compile time.
  ;; In either context, if `assign?' is true, then
  ;;  generate an unevaluated assignment that makes
  ;;  the identifier mutable.
  (define (check-definitions-new who stx names defn assign)
    (cond
      [(eq? (syntax-local-context) 'top-level)
       (with-syntax ([defn defn]
                     [who who])
         (with-syntax ([(check ...)
                        (map (lambda (name)
                               (with-syntax ([name name])
                                 ;; Make sure each check has the
                                 ;; source location of the original
                                 ;; expression:
                                 (syntax/loc stx
                                   (check-top-level-not-defined 'who #'name))))
                             names)])
           (stepper-syntax-property 
            (syntax/loc stx
              (begin
                check ...
                defn)) 
            'stepper-skipto 
            (cons 'syntax-e
                  (let loop ([l names])
                    (if (null? l)
                        `(syntax-e cdr car)
                        (cons 'cdr (loop (cdr l)))))))))]
      [(memq (syntax-local-context) '(module module-begin))
       (for-each (lambda (name)
                   (let ([b (identifier-binding name)])
                     (when b
                       (teach-syntax-error
                        (syntax-e name)
                        name
                        #f
                        "this name was defined previously and cannot be re-defined"))))
                 names)
       (if assign
           (with-syntax ([(name ...) (if (eq? assign #t)
                                         names
                                         assign)]
                         [made-up (gensym)]
                         [defn defn])
             (with-syntax ([made-up-defn 
                            (stepper-syntax-property 
                             (syntax (define made-up (lambda () (advanced-set! name 10) ...)))
                             'stepper-skip-completely
                             #t)])
               (syntax/loc stx
                 (begin
                   made-up-defn ;; (define made-up (lambda () (advanced-set! name 10) ...))
                   defn))))
           defn)]
      [else defn]))
  
  ;; Same as above, but for one name
  (define (check-definition-new who stx name defn assign)
    (check-definitions-new who stx (list name) defn assign))
  
  ;; Check context for a `define' before even trying to
  ;; expand
  (define-struct expanding-for-intermediate-local ())
  (define (ok-definition-context)
    (let ([ctx (syntax-local-context)])
      (or (memq ctx '(top-level module module-begin))
          (and (pair? ctx)
               (expanding-for-intermediate-local? (car ctx))))))
  
  (define (local-expand-for-error stx ctx stops)
    ;; This function should only be called in an 'expression
    ;;  context. In case we mess up, avoid bogus error messages.
    (when (memq (syntax-local-context) '(expression))
      (local-expand stx ctx stops)))
  
  (define (ensure-expression stx k)
    (if (memq (syntax-local-context) '(expression))
        (k)
        (stepper-syntax-property #`(begin0 #,stx) 'stepper-skipto skipto/second)))
  
  ;; Use to generate nicer error messages than direct pattern
  ;; matching. The `where' argument is an English description
  ;; of the portion of the larger expression where a single
  ;; sub-expression was expected.
  (define (check-single-expression who where stx exprs will-bind)
    (when (null? exprs)
      (teach-syntax-error
       who
       stx
       #f
       "expected an expression ~a, but nothing's there"
       where))
    (unless (null? (cdr exprs))
      ;; In case it's erroneous, to ensure left-to-right reading, let's
      ;;  try expanding the first expression. We have to use
      ;;  `will-bind' to avoid errors for unbound ids that will actually
      ;;  be bound. Since they're used as stopping points, we may miss
      ;;  some errors after all. It's worth a try, though. We also
      ;;  have to stop at advanced-set!, in case it's used with
      ;;  one of the identifiers in will-bind.
      (when will-bind
        (local-expand-for-error (car exprs) 'expression (cons #'advanced-set!
                                                              will-bind)))
      ;; First expression seems ok, report an error for 2nd and later:
      (teach-syntax-error
       who
       stx
       (cadr exprs)
       "expected only one expression ~a, but found ~a extra part~a"
       where
       (sub1 (length exprs))
       (if (> (length exprs) 2) "s" ""))))
  
  (define (check-single-result-expr exprs where enclosing-expr will-bind)
    (check-single-expression where
                             "for the function body"
                             enclosing-expr
                             exprs
                             will-bind))
  
  (define keyword-list
    (append (syntax->list provided-identifiers)
            (syntax->list (quote-syntax 
                           (#%datum
                            #%top
                            empty true false
                            )))))
  
  (define (identifier/non-kw? stx)
    (and (identifier? stx)
         (not (ormap (lambda (x) (module-identifier=? stx x))
                     keyword-list))))
  
  (define (something-else/kw stx)
    (if (identifier? stx)
        "a keyword"
        (something-else stx)))
  
  (define (make-name-inventer)
    ;; Normally we'd use (make-syntax-introducer) because gensyming makes
    ;;  identifiers that play badly with exporting. But we don't have
    ;;  to worry about exporting in the teaching languages, while we do
    ;;  have to worry about mangled names.
    (lambda (id)
      (datum->syntax-object id 
                            (string->uninterned-symbol (symbol->string (syntax-e id)))
                            id)))
  
  (define (wrap-func-definitions first-order? kinds names argcs k)
    (if first-order?
        (let ([name2s (map (make-name-inventer) names)])
          (values (quasisyntax
                   (begin
                     #,@(map
                         (lambda (name name2 kind argc)
                           #`(define-syntax #,name 
                               (make-first-order-function '#,kind 
                                                          #,argc
                                                          (quote-syntax #,name2) 
                                                          (quote-syntax #%app))))
                         names name2s kinds argcs)
                     #,(k name2s)))
                  name2s))
        (values (k names)
                names)))
  
  
  ;; Racket's true and false are defined as macros (for performance perhaps?),
  ;; but this dodge *SL's redefinition of #%app and set!. Without these
  ;; beginner-true/proc and beginner-false/proc here, (true) would throw a
  ;; professional error message not suitable for beginners.
  (define (make-constant-expander val)
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! id rhs) (syntax/loc stx (set! val rhs))]
         [(id . args) 
          (teach-syntax-error
           '|function call|
           #'stx
           #'id 
           "expected a function after the open parenthesis, but found ~a"
           (syntax-e #'id))]
         [_ (datum->syntax stx val stx)]))))
  
  (define beginner-true/proc (make-constant-expander #t))
  (define beginner-false/proc (make-constant-expander #f))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define (beginner)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (define/proc first-order? assign? stx lambda-stx)
    
    (define (wrap-func-definition name argc k)
      (wrap-func-definitions first-order? 
                             '(procedure) (list name) (list argc)
                             (lambda (names)
                               (k (car names)))))
    
    (define (check-function-defn-ok stx)
      (when first-order?
        (when (eq? 'top-level (syntax-local-context))
          (teach-syntax-error
           'define
           stx
           #f
           "function definitions are not allowed in the interactions window; ~
              they must be in the definitions window"))))
    
    (unless (or (ok-definition-context)
                (identifier? stx))
      (teach-syntax-error
       'define
       stx
       #f
       "found a definition that is not at the top level"))
    
    (syntax-case stx ()
      ;; Constant or lambda def:
      [(_ name expr)
       (identifier/non-kw? (syntax name))
       (let ([lam (syntax expr)])
         (check-defined-lambda lam)
         (syntax-case* (syntax expr) (beginner-lambda) (lambda (a b)
                                                         (module-identifier=? a lambda-stx))
           ;; Well-formed lambda def:
           [(beginner-lambda arg-seq lexpr ...)
            (begin
              (check-function-defn-ok stx)
              (let-values ([(defn bind-names)
                            (wrap-func-definition
                             #'name
                             (length (syntax->list #'arg-seq))
                             (lambda (name)
                               (with-syntax ([name name])
                                 (quasisyntax/loc 
                                     stx 
                                   (define name
                                     #,(stepper-syntax-property
                                        (syntax-track-origin
                                         #`(lambda arg-seq 
                                             #,(stepper-syntax-property #`make-lambda-generative 
                                                                        'stepper-skip-completely #t)
                                             lexpr ...)
                                         lam
                                         (syntax-local-introduce (car (syntax-e lam))))
                                        'stepper-define-type
                                        'lambda-define))))))])
                (check-definition-new
                 'define
                 stx
                 #'name
                 defn
                 (and assign? bind-names))))]
           ;; Constant def
           [_else
            (check-definition-new
             'define
             stx
             (syntax name)
             (quasisyntax/loc stx (define name expr))
             (and assign? (list (syntax name))))]))]
      ;; Function definition:
      [(_ name-seq expr ...)
       (syntax-case (syntax name-seq) () [(name ...) #t][_else #f])
       ;; name-seq is at least a sequence
       (let ([names (syntax->list (syntax name-seq))])
         (check-function-defn-ok stx)
         (when (null? names)
           (teach-syntax-error
            'define
            stx
            #f
            "expected a name for the function, but nothing's there"))
         (let loop ([names names][pos 0])
           (unless (null? names)
             (unless (identifier/non-kw? (car names))
               (teach-syntax-error
                'define
                stx
                (car names)
                "expected ~a, but found ~a"
                (cond
                  [(zero? pos) "the name of the function"]
                  [else "a variable"])
                (something-else/kw (car names))))
             (loop (cdr names) (add1 pos))))
         (when (null? (cdr names))
           (teach-syntax-error
            'define
            stx
            (syntax name-seq)
            "expected at least one variable after the function name, but found none"))
         (let ([dup (check-duplicate-identifier (cdr names))])
           (when dup
             (teach-syntax-error
              'define
              stx
              dup
              "found a variable that is used more than once: ~a"
              (syntax-e dup))))
         (check-single-result-expr (syntax->list (syntax (expr ...)))
                                   #f
                                   stx
                                   ;; can't local-expand function body, because
                                   ;;  not all top-level defns are ready:
                                   #f)
         
         (let-values ([(defn bind-names)
                       (wrap-func-definition
                        (car (syntax-e #'name-seq))
                        (length (cdr (syntax->list #'name-seq)))
                        (lambda (fn)
                          (with-syntax ([fn fn]
                                        [args (cdr (syntax-e #'name-seq))])
                            (quasisyntax/loc stx
                              (define fn
                                #,(stepper-syntax-property
                                   (stepper-syntax-property
                                    ;; this is so signature blame can report a
                                    ;; position for the procedure
                                    (syntax/loc stx (lambda args expr ...))
                                    'stepper-define-type
                                    'shortened-proc-define)
                                   'stepper-proc-define-name
                                   #`fn))))))])
           (check-definition-new 
            'define
            stx
            (car names)
            defn
            (and assign? bind-names))))]
      ;; Constant/lambda with too many or too few parts:
      [(_ name expr ...)
       (identifier/non-kw? (syntax name))
       (let ([exprs (syntax->list (syntax (expr ...)))])
         (check-single-expression 'define
                                  (format "after the variable name ~a"
                                          (syntax-e (syntax name)))
                                  stx
                                  exprs
                                  ;; can't local-expand RHS, because
                                  ;;  not all top-level defns are ready:
                                  #f))]
      ;; Bad name/header:
      [(_ non-name expr ...)
       (teach-syntax-error
        'define
        stx
        (syntax non-name)
        EXPECTED-HEADER
        (string-append "found " (something-else/kw (syntax non-name))))]
      ;; Missing name:
      [(_)
       (teach-syntax-error
        'define
        stx
        #f
        EXPECTED-HEADER
        "nothing's there")]
      [_else
       (bad-use-error 'define stx)]))
  
  (define (beginner-define/proc stx)
    (define/proc #t #f stx #'beginner-lambda))
  
  (define (define-wish/proc stx)
    (syntax-case stx ()
      [(_ name) 
       (define/proc #t #f 
         #`(define (#,#'name x) 
             (begin 
               (send (send (get-test-engine) get-info) add-wish-call (quote #,#'name))
               (raise (exn:fail:wish 
                       (format "wished for function ~a not implemented" (quote #,#'name))
                       (current-continuation-marks) (quote #,#'name) x)))) #'lambda)]
      [(_ name default-value)
       (define/proc #t #f
         #`(define (#,#'name x) 
             (begin 
               (send (send (get-test-engine) get-info) add-wish-call (quote #,#'name))
               #,#'default-value))
         #'lambda)]))               
  
  (define (intermediate-define/proc stx)
    (define/proc #f #f stx #'intermediate-pre-lambda))
  
  (define (intermediate-lambda-define/proc stx)
    ;; no special treatment of intermediate-lambda:
    (define/proc #f #f stx #'beginner-lambda))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; lambda (beginner; only works with define)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (beginner-lambda/proc stx)
    (syntax-case stx ()
      [(_ . rest)
       (teach-syntax-error
        'lambda
        stx
        #f
        "found a lambda that is not a function definition")]
      [_else
       (bad-use-error 'lambda stx)]))
  
  (define (intermediate-pre-lambda/proc stx)
    (beginner-lambda/proc stx))
  
  (define (check-defined-lambda rhs)
    (syntax-case rhs ()
      [(lam . _)
       (and (identifier? #'lam)
            (or (module-identifier=? #'lam #'beginner-lambda)
                (module-identifier=? #'lam #'intermediate-pre-lambda)))
       (syntax-case rhs ()
         [(lam arg-seq lexpr ...)
          (syntax-case (syntax arg-seq) () [(arg ...) #t][_else #f])
          (let ([args (syntax->list (syntax arg-seq))])
            (for-each (lambda (arg)
                        (unless (identifier/non-kw? arg)
                          (teach-syntax-error
                           'lambda
                           rhs
                           arg
                           "expected a variable, but found ~a"
                           (something-else/kw arg))))
                      args)
            (when (null? args)
              (teach-syntax-error
               'lambda
               rhs
               (syntax arg-seq)
               "expected at least one variable after lambda, but found none"))
            (let ([dup (check-duplicate-identifier args)])
              (when dup
                (teach-syntax-error
                 'lambda
                 rhs
                 dup
                 "found a variable that is used more than once: ~a"
                 (syntax-e dup))))
            (check-single-result-expr (syntax->list (syntax (lexpr ...)))
                                      #f
                                      rhs
                                      args)
            'ok)]
         ;; Bad lambda because bad args:
         [(lam args . _)
          (teach-syntax-error
           'lambda
           rhs
           (syntax args)
           "expected at least one variable (in parentheses) after lambda, but found ~a"
           (something-else (syntax args)))]
         ;; Bad lambda, no args:
         [(lam)
          (teach-syntax-error
           'lambda
           rhs
           #f
           "expected at least one variable (in parentheses) after lambda, but nothing's there")]
         [_else 'ok])]
      [_else 'ok]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define-struct (beginner)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (do-define-struct stx first-order? setters?)
    
    (unless (or (ok-definition-context)
                (identifier? stx))
      (teach-syntax-error
       'define-struct
       stx
       #f
       "found a definition that is not at the top level"))
    
    (syntax-case stx ()
      ;; First, check for a struct name:
      [(_ name . __)
       (not (identifier/non-kw? (syntax name)))
       (teach-syntax-error
        'define-struct
        stx
        (syntax name)
        "expected the structure name after define-struct, but found ~a"
        (something-else/kw (syntax name)))]
      ;; Main case (`rest' is for nice error messages):
      [(_ name_ (field_ ...) . rest)
       (let ([name (syntax name_)]
             [fields (syntax->list (syntax (field_ ...)))]
             [ht (make-hash-table)])
         (for-each
          (lambda (field)
            (unless (identifier? field)
              (teach-syntax-error
               'define-struct
               stx
               field
               "expected a field name, but found ~a"
               (something-else field)))
            (let ([sym (syntax-e field)])
              (when (hash-table-get ht sym (lambda () #f))
                (teach-syntax-error
                 'define-struct
                 stx
                 field
                 "found a field name that is used more than once: ~a"
                 sym))
              (hash-table-put! ht sym #t)))
          fields)
         (let ([rest (syntax->list (syntax rest))])
           (unless (null? rest)
             (teach-syntax-error
              'define-struct
              stx
              (car rest)
              "expected nothing after the field names, but found ~a extra part~a"
              (length rest)
              (if (> (length rest) 1) "s" ""))))
         [define-values (struct: constructor-name predicate-name getter-names setter-names)
           (make-struct-names name fields stx)]
         [define field# (length fields)]
         [define signature-name (car (generate-temporaries (list name)))]
         [define parametric-signature-name (datum->syntax name (format-symbol "~a-of" name))]
         [define to-define-names (list* constructor-name predicate-name
                                        (if setters?
                                            (append getter-names setter-names)
                                            getter-names))]
         [define proc-names to-define-names]
         (define fields-without-location 
           (map (λ (x) (datum->syntax x (syntax->datum x) #f)) (syntax->list #'(field_ ...))))
         (with-syntax ([compile-info 
                        (kw-app build-struct-expand-info name fields #f (not setters?) #t null null
                                #:omit-struct-type? #t)]
                       [(field_/no-loc ...) fields-without-location])
           (define-values (defn0 bind-names)
             (wrap-func-definitions 
              first-order? 
              (list* 'constructor 
                     'predicate
                     (map (lambda (x) 'selector) (cddr proc-names)))
              proc-names
              (list* (- (length proc-names) 2)
                     1
                     (map (lambda (x) 1) (cddr proc-names)))
              (lambda (def-proc-names)
                (with-syntax ([(def-proc-name ...) def-proc-names]
                              [(proc-name ...) proc-names]
                              [(getter-id ...) getter-names])
                  (define defns
                    #`(define-values (#;#,signature-name #,parametric-signature-name def-proc-name ...)
                        (let ()
                          (define-values (type-descriptor
                                          raw-constructor
                                          raw-predicate
                                          raw-generic-access
                                          raw-generic-mutate)
                            (make-struct-type
                             'name_
                             #f
                             #,field# 1
                             #f ; auto-v
                             (list
                              (cons prop:print-convert-constructor-name
                                    '#,constructor-name)
                              (cons prop:print-converter
                                    (lambda (r recur)
                                      (list '#,constructor-name
                                            #,@(map-with-index
                                                (lambda (i _)
                                                  #`(recur (raw-generic-access r #,i)))
                                                fields))))
                              (cons prop:custom-print-quotable
                                    'never)
                              (cons prop:custom-write
                                    ;; Need a transparent-like printer, but hide auto field.
                                    ;; This simplest way to do that is to create an instance
                                    ;; of a transparet structure with the same name and field values.
                                    (let-values ([(struct:plain make-plain plain? plain-ref plain-set)
                                                  (make-struct-type 'name_ #f #,field# 0 #f null #f)])
                                      (lambda (r port mode)
                                        (let ((v (make-plain
                                                  #,@(map-with-index (lambda (i _)
                                                                       #`(raw-generic-access r #,i))
                                                                     fields))))
                                          (cond
                                            [(eq? mode #t) (write v port)]
                                            [(eq? mode #f) (display v port)]
                                            [else (print v port mode)])))))
                              (cons prop:equal+hash
                                    (list
                                     (lambda (r1 r2 equal?)
                                       (and #,@(map-with-index 
                                                (lambda (i field-spec)
                                                  #`(equal? (raw-generic-access r1 #,i)
                                                            (raw-generic-access r2 #,i)))
                                                fields)))
                                     (make-equal-hash 
                                      (lambda (r i) (raw-generic-access r i)) #,field#) 
                                     (make-equal2-hash
                                      (lambda (r i) (raw-generic-access r i)) #,field#)))
                              (cons prop:lazy-wrap
                                    (make-lazy-wrap-info
                                     (lambda args (apply #,constructor-name args))
                                     (list #,@(map-with-index
                                               (lambda (i _) 
                                                 #`(lambda (r) (raw-generic-access r #,i)))
                                               fields))
                                     (list #,@(map-with-index
                                               (lambda (i _)
                                                 #`(lambda (r v) (raw-generic-mutate r #,i v)))
                                               fields))
                                     (lambda (r)
                                       (raw-generic-access r #,field#))
                                     (lambda (r v)
                                       (raw-generic-mutate r #,field# v)))))
                             ;; give `check-struct-wraps!' access
                             (make-inspector)))
                          
                          #,@(map-with-index (lambda (i name field-name)
                                               #`(define #,name
                                                   (let ([raw (make-struct-field-accessor
                                                               raw-generic-access
                                                               #,i
                                                               '#,field-name)])
                                                     (lambda (r)
                                                       (raw r)))))
                                             getter-names
                                             fields)
                          #,@(map-with-index (lambda (i name field-name)
                                               #`(define #,name 
                                                   (let ([raw (make-struct-field-mutator
                                                               raw-generic-mutate
                                                               #,i
                                                               '#,field-name)])
                                                     (lambda (r v)
                                                       (raw r v)))))
                                             setter-names
                                             fields)
                          (define #,predicate-name raw-predicate)
                          (define #,constructor-name raw-constructor)
                          
                          (define #,signature-name (signature (predicate raw-predicate)))
                          
                          #,(if setters?
                                #`(define (#,parametric-signature-name field_ ...)
                                    (signature
                                     (combined 
                                      (at name_ (predicate raw-predicate))
                                      (at field_ (signature:property getter-id field_/no-loc)) ...)))
                                #`(define (#,parametric-signature-name field_ ...)
                                    (let* ((sigs (list field_/no-loc ...))
                                           (sig
                                            (make-lazy-wrap-signature 'name_ #t
                                                                      type-descriptor
                                                                      raw-predicate
                                                                      sigs
                                                                      #'name_)))
                                      (let ((arbs (map signature-arbitrary sigs)))
                                        (when (andmap values arbs)
                                          (set-signature-arbitrary! 
                                           sig
                                           (apply arbitrary-record
                                                  #,constructor-name 
                                                  (list #,@getter-names)
                                                  arbs))))
                                      sig)))
                          
                          (values #;#,signature-name #,parametric-signature-name proc-name ...))))
                  ;; --- IN ---
                  (stepper-syntax-property defns 'stepper-black-box-expr stx)))))
           ;; --------------------------------------------------------------------------------
           (define struct-name-size (string-length (symbol->string (syntax-e #'name_))))
           (define struct-name/locally-introduced (syntax-local-introduce #'name_))
           
	   (define signature-name-directive #f)
           (define parametric-signature-name-directive #f)

           (define struct-name-to-maker-directive
             (vector (syntax-local-introduce constructor-name)
                     5
                     struct-name-size
                     struct-name/locally-introduced
                     0
                     struct-name-size))
           
           (define struct-name-to-predicate-directive
             (vector (syntax-local-introduce predicate-name)
                     0
                     struct-name-size
                     struct-name/locally-introduced
                     0
                     struct-name-size))
           
           (define (struct->selector-directive selector)
             (define selector-name/locally-introduced (syntax-local-introduce selector))
             (vector selector-name/locally-introduced
                     0
                     struct-name-size
                     struct-name/locally-introduced
                     0
                     struct-name-size))
           
           (define (field->selector-directive field selector)
             (define field-name/locally-introduced (syntax-local-introduce field))
             (define field-name-size (string-length (symbol->string (syntax-e field))))
             (define selector-name/locally-introduced (syntax-local-introduce selector))
             (vector selector-name/locally-introduced
                     (+ struct-name-size 1)
                     field-name-size
                     field-name/locally-introduced
                     0
                     field-name-size))
           
           (define (struct->setter-directive setter)
             (define setter-name/locally-introduced (syntax-local-introduce setter))
             (vector setter-name/locally-introduced
                     ;; set-name_-field!
                     ;; 012|--->|
                     (string-length "set-")
                     struct-name-size
                     struct-name/locally-introduced
                     0
                     struct-name-size))
           
           (define (field->setter-directive field setter)
             (define field-name/locally-introduced (syntax-local-introduce field))
             (define field-name-size (string-length (symbol->string (syntax-e field))))
             (define setter-name/locally-introduced (syntax-local-introduce setter))
             (vector setter-name/locally-introduced
                     ;; set-name_-field!
                     ;; 012|4...X|--->
                     (+ (string-length "set-") struct-name-size 1)
                     field-name-size
                     field-name/locally-introduced
                     0
                     field-name-size))
           
           (define all-directives
             (list* signature-name-directive
                    parametric-signature-name-directive
                    struct-name-to-maker-directive
                    struct-name-to-predicate-directive
                    (map field->selector-directive fields getter-names)
                    (map struct->selector-directive getter-names)
                    (map field->setter-directive fields setter-names)
                    (map struct->setter-directive setter-names)))
           
           ;; --------------------------------------------------------------------------------
           (define defn1 defn0)
           (define defn2 
             (quasisyntax/loc stx
               (begin
                 #,(stepper-syntax-property
                    #`(define-syntaxes (name_) 
                        (let ()
                          (racket:define-struct info ()
                                                #:super struct:struct-info
                                                ;; support `signature'
                                                #:property 
                                                prop:procedure
                                                (lambda (_ stx)
                                                  (syntax-case stx ()
                                                    [(self . args)
                                                     (raise-syntax-error
                                                      #f
                                                      EXPECTED-FUNCTION-NAME
                                                      stx
                                                      #'self)]
                                                    [else
						      (raise-syntax-error
                                                      #f
                                                      (format "structure type; do you mean make-~a"
							(syntax-e #'name_))
						      stx
                                                      stx)
						      #;#'#,signature-name
						      ])))
                          ;; support `shared'
                          (make-info (lambda () compile-info))))
                    'stepper-skip-completely
                    #t)
                 #,defn1)))
           (define defn3
             (check-definitions-new 'define-struct
                                    stx 
                                    (list* name parametric-signature-name to-define-names)
                                    defn2
                                    (and setters? bind-names)))
           (define defn4
             (syntax-property defn3 'disappeared-use (list struct-name/locally-introduced)))
           (syntax-property defn4 'sub-range-binders all-directives)))]
      [(_ name_ something . rest)
       (teach-syntax-error
        'define-struct
        stx
        (syntax something)
        "expected at least one field name (in parentheses) after the structure name, but found ~a"
        (something-else (syntax something)))]
      [(_ name_)
       (teach-syntax-error
        'define-struct
        stx
        #f
        EXPECTED-1-FIELD)]
      [(_)
       (teach-syntax-error
        'define-struct
        stx
        #f
        "expected the structure name after define-struct, but nothing's there")]
      [_else (bad-use-error 'define-struct stx)]))
  
  (define (beginner-define-struct/proc stx)
    (do-define-struct stx #t #f))
  
  (define (intermediate-define-struct/proc stx)
    (do-define-struct stx #f #f))
  
  (define (advanced-define-datatype/proc stx)
    (unless (or (ok-definition-context)
                (identifier? stx))
      (teach-syntax-error
       'define-datatype
       stx
       #f
       "found a definition that is not at the top level"))
    
    (syntax-case stx ()
      
      ;; First, check for a datatype name:
      [(_ name . __)
       (not (identifier/non-kw? (syntax name)))
       (teach-syntax-error
        'define-datatype
        stx
        (syntax name)
        "expected a datatype type name after `define-datatype', but found ~a"
        (something-else/kw (syntax name)))]
      
      [(_ name (variant field ...) ...)
       
       (let ([find-duplicate
              (λ (stxs fail-k)
                (define ht (make-hash-table))
                (for-each
                 (λ (s)
                   (define sym (syntax-e s))
                   (when (hash-table-get ht sym (λ () #f))
                     (fail-k s))
                   (hash-table-put! ht sym #t))
                 (syntax->list stxs)))])
         (for-each 
          (λ (v)
            (unless (identifier/non-kw? v)
              (teach-syntax-error
               'define-datatype
               stx
               v
               "expected a variant name, found ~a"
               (something-else/kw v))))
          (syntax->list #'(variant ...)))
         (find-duplicate #'(variant ...)
                         (λ (v-stx)
                           (define v (syntax-e v-stx))
                           (teach-syntax-error
                            'define-datatype
                            stx
                            v-stx
                            "found a variant name that is used more than once: ~a"
                            v)))              
         
         (for-each
          (λ (vf)
            (with-syntax ([(variant field ...) vf])
              (for-each
               (λ (f)
                 (unless (identifier? f)
                   (teach-syntax-error
                    'define-datatype
                    stx
                    f
                    "in variant `~a': expected a field name, found ~a"
                    (syntax-e #'variant)
                    (something-else f))))
               (syntax->list #'(field ...)))
              (find-duplicate #'(field ...)
                              (λ (f-stx)
                                (teach-syntax-error
                                 'define-datatype
                                 stx
                                 f-stx
                                 "in variant `~a': found a field name that is used more than once: ~a"
                                 (syntax-e #'variant)
                                 (syntax-e f-stx))))))
          (syntax->list #'((variant field ...) ...))))
       
       (with-syntax ([(name? variant? ...)
                      (map (lambda (stx)
                             (datum->syntax stx (format-symbol "~a?" (syntax->datum stx)) stx))
                           (syntax->list #'(name variant ...)))])
         ;; Here we are using an explicit loop and the "/proc" functions instead of producing 
         ;; a syntax with "..." to preserve the syntax location information.
         (with-syntax ([the-definition 
                        (advanced-define/proc 
                         (syntax/loc stx (define (name? x) (or (variant? x) ...))))]
                       [(the-struct-definitions ...)
                        (map
                         (lambda (v)
                           (syntax-case v ()
                             [(variant field ...)
                              (advanced-define-struct/proc
                               (syntax/loc stx (define-struct variant (field ...))))]))
                         (syntax->list #'((variant field ...) ...)))])
           (syntax/loc stx (begin the-definition the-struct-definitions ...))))]
      [(_ name_ (variant field ...) ... something . rest)
       (teach-syntax-error
        'define-datatype
        stx
        (syntax something)
        "expected a variant after the datatype type name in `define-datatype', ~
         but found ~a"
        (something-else (syntax something)))]
      [(_)
       (teach-syntax-error
        'define-datatype
        stx
        #f
        "expected a datatype type name after `define-datatype', but nothing's there")]
      [_else (bad-use-error 'define-datatype stx)]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; application (beginner and intermediate)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; For beginner:
  
  ;; #%app should never happen in beginner. Primitive operations and
  ;; defined functions turn into syntax that handle application
  ;; forms.  The only vaguely legitimate application would involve a
  ;; poorly implemented teachpack that exports functions instead of
  ;; primitive operators. Also, #%app is unavoidable in the REPL.
  
  ;; An #%app might happen "temporarily" if it appears at the top
  ;; level before the corresponding function definition. To provide
  ;; a good error message, we need to wait, and that's what
  ;; beginner-app-delay does.
  
  (define-values (beginner-app/proc intermediate-app/proc)
    (let ([mk-app
           (lambda (lex-ok?)
             (lambda (stx)
               (syntax-case stx ()
                 [(_ rator rand ...)
                  (let* ([fun (syntax rator)]
                         [binding (and (identifier? fun)
                                       (identifier-binding fun))]
                         [lex? (eq? 'lexical binding)]
                         [bad-app (lambda (what)
                                    (teach-syntax-error
                                     '|function call|
                                     stx
                                     fun
                                     "expected a function after the open parenthesis, but found ~a"
                                     what))])
                    (unless (and (identifier? fun) (or lex-ok? (not lex?)))
                      (bad-app (if lex?
                                   "a variable"
                                   (something-else fun))))
                    ;; The following check disallows calling thunks.
                    ;; It's disabled because we need to allow calls to
                    ;; primitive thunks.
                    '(when (null? (syntax->list (syntax (rand ...))))
                       (teach-syntax-error
                        '|function call|
                        stx
                        #f
                        "expected an argument after the function, but nothing's there"))
                    (cond
                      [(and (not lex-ok?) (binding-in-this-module? binding))
                       ;; An application of something defined as a constant
                       (bad-app "a variable")]
                      [(or lex-ok? (and binding (not (binding-in-this-module? binding))))
                       (with-syntax ([new-rator (syntax-property #'rator 'was-in-app-position #t)])
                         (syntax/loc stx (#%app new-rator rand ...)))]
                      [else
                       ;; We don't know what rator is, yet, and it might be local:
                       (with-syntax ([new-rator (syntax-property #'rator 'was-in-app-position #t)])
                         (quasisyntax/loc 
                             stx 
                           (#%app values #,(quasisyntax/loc
                                               stx
                                             (beginner-app-continue new-rator rand ...)))))]))]
                 [(_)
                  (teach-syntax-error
                   '|function call|
                   stx
                   #f
                   "expected a function after the open parenthesis, but nothing's there")]
                 [_else (bad-use-error '|function call| stx)])))])
      (values (mk-app #f) (mk-app #t))))
  
  
  
  (define (beginner-app-continue/proc stx)
    (syntax-case stx ()
      [(_ rator rand ...)
       (let* ([fun #'rator]
              [binding (identifier-binding fun)])
         (if binding
             ;; Now defined in the module:
             (if (set!-transformer? (syntax-local-value fun (lambda () #f)))
                 ;; Something that takes care of itself:
                 (syntax/loc stx (rator rand ...))
                 ;; Something for which we probably need to report an error,
                 ;;  but let beginner-app take care of it:
                 (syntax/loc stx (beginner-app rator rand ...)))
             
             ;; Something undefined; let beginner-top take care of it:
             (with-syntax ([new-rator (syntax-property #'rator 'was-in-app-position #t)])
               (syntax/loc stx (#%app new-rator rand ...)))))]))
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; top-level variables (beginner)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (beginner-top/proc stx)
    (syntax-case stx ()
      [(_ . id)
       (if (not (identifier-binding #'id))
           (if (syntax-source-module #'id)
               ;; If we're in a module, we'll need to check that the name
               ;;  is bound but it might be defined later in the module, so
               ;; delay the check.
               (stepper-ignore-checker 
                (syntax/loc stx (#%app values (beginner-top-continue id))))
               
               ;; identifier-finding only returns useful information when inside a module. 
               ;; At the top-level we need to  do the check at runtime. Also, note that at 
               ;; the top level there is no need for stepper annotations
               (syntax/loc stx (#%app top/check-defined #'id)))
           
           (syntax/loc stx (#%top . id)))]))
  
  
  (define (beginner-top-continue/proc stx)
    (syntax-case stx ()
      [(_ id)
       (if (not (identifier-binding #'id))
           ;; If there's still no binding, it's an "unknown name" error.
           (raise-not-bound-error #'id)
           
           ;; Don't use #%top here; id might have become bound to something
           ;;  that isn't a value.
           #'id)]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; cond
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (beginner-cond/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_)
          (teach-syntax-error
           'cond
           stx
           #f
           "expected a clause after cond, but nothing's there")]
         [(_ clause ...)
          (let* ([clauses (syntax->list (syntax (clause ...)))]
                 [check-preceding-exprs
                  (lambda (stop-before)
                    (let/ec k
                      (for-each 
                       (lambda (clause)
                         (if (eq? clause stop-before)
                             (k #t)
                             (syntax-case clause ()
                               [(question answer)
                                (begin
                                  (unless
                                      (and (identifier? (syntax question))
                                           (module-identifier=? (syntax question) #'beginner-else))
                                    (local-expand-for-error (syntax question) 'expression null))
                                  (local-expand-for-error (syntax answer) 'expression null))])))
                       clauses)))])
            (let ([checked-clauses
                   (map
                    (lambda (clause)
                      (syntax-case clause (beginner-else)
                        [(beginner-else answer)
                         (let ([lpos (memq clause clauses)])
                           (when (not (null? (cdr lpos)))
                             (teach-syntax-error
                              'cond
                              stx
                              clause
                              "found an else clause that isn't the last clause ~
                                    in its cond expression"))
                           (with-syntax ([new-test (stepper-syntax-property #'#t 'stepper-else #t)])
                             (syntax/loc clause (new-test answer))))]
                        [(question answer)
                         (with-syntax ([verified (stepper-ignore-checker 
                                                  #'(verify-boolean question 'cond))])
                           (syntax/loc clause (verified answer)))]
                        [()
                         (check-preceding-exprs clause)
                         (teach-syntax-error
                          'cond
                          stx
                          clause
                          "expected a clause with a question and an answer, but found an empty part")]
                        [(question?)
                         (check-preceding-exprs clause)
                         (teach-syntax-error
                          'cond
                          stx
                          clause
                          EXPECTED-CLAUSE)]
                        [(question? answer? ...)
                         (check-preceding-exprs clause)
                         (let ([parts (syntax->list clause)])
                           ;; to ensure the illusion of left-to-right checking, make sure 
                           ;; the question and first answer (if any) are ok:
                           (unless (and (identifier? (car parts))
                                        (module-identifier=? (car parts) #'beginner-else))
                             (local-expand-for-error (car parts) 'expression null))
                           (unless (null? (cdr parts))
                             (local-expand-for-error (cadr parts) 'expression null))
                           ;; question and answer (if any) are ok, raise a count-based exception:
                           (teach-syntax-error*
                            'cond
                            stx
                            parts
                            EXPECTED-GOOD-CLAUSE
                            (length parts)))]
                        [_else
                         (teach-syntax-error
                          'cond
                          stx
                          clause
                          "expected a clause with a question and an answer, but found ~a"
                          (something-else clause))]))
                    clauses)])
              ;; Add `else' clause for error (always):
              (define error-call (syntax/loc stx (error 'cond "all question results were false")))
              [define clauses
                (append checked-clauses 
                        (list 
                         (with-syntax ([error-call error-call]) 
                           (syntax [else error-call]))))]
              (with-syntax ([clauses clauses])
                (syntax/loc stx (cond . clauses)))))]
         [_else (bad-use-error 'cond stx)]))))
  
  (define beginner-else/proc
    (make-set!-transformer
     (lambda (stx)
       (define (bad expr)
         (teach-syntax-error
          'else
          expr
          #f
          "not allowed here, because this is not a question in a clause"))
       (syntax-case stx (set! x)
         [(set! e expr) (bad #'e)]
         [(e . expr) (bad #'e)]
         [e (bad stx)]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; if
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (beginner-if/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_ test then else)
          (with-syntax ([new-test (stepper-ignore-checker (syntax (verify-boolean test 'if)))])
            (syntax/loc stx
              (if new-test
                  then
                  else)))]
         [(_ . rest)
          (let ([n (length (syntax->list (syntax rest)))])
            (teach-syntax-error
             'if
             stx
             #f
             "expected a question and two answers, but ~a"
             (cond [(zero? n) "nothing's there"]
                   [(= n 1) "found only 1 part"]
                   [(= n 2) "found only 2 parts"]
                   [else (format "found ~a parts" n)])))]
         [_else (bad-use-error 'if stx)]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; or, and
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-values (beginner-or/proc beginner-and/proc)
    (let ([mk
           (lambda (where)
             (let ([stepper-tag (case where
                                  [(or) 'comes-from-or]
                                  [(and) 'comes-from-and])])
               (with-syntax ([swhere where])
                 (lambda (stx)
                   (ensure-expression
                    stx
                    (lambda ()
                      (syntax-case stx ()
                        [(_ . clauses)
                         (let ([n (length (syntax->list (syntax clauses)))])
                           (when (n . < . 2)
                             (teach-syntax-error
                              where
                              stx
                              #f
                              (argcount-error-message #f 2 n #t)))
                           (let loop ([clauses-consumed 0]
                                      [remaining (syntax->list #`clauses)])
                             (if (null? remaining)
                                 (case where
                                   [(or) #`#f]
                                   [(and) #`#t])
                                 (stepper-syntax-property
                                  (stepper-syntax-property
                                   (quasisyntax/loc 
                                       stx
                                     (if #,(stepper-ignore-checker 
                                            (quasisyntax/loc stx
                                              (verify-boolean #,(car remaining) 'swhere)))
                                         #,@(case where
                                              [(or) 
                                               #`(#t
                                                  #,(loop (+ clauses-consumed 1) (cdr remaining)))]
                                              [(and)
                                               #`(#,(loop (+ clauses-consumed 1) (cdr remaining))
                                                  #f)])))
                                   'stepper-hint
                                   stepper-tag)
                                  'stepper-and/or-clauses-consumed
                                  clauses-consumed))))]
                        [_else (bad-use-error where stx)])))))))])
      (values (mk 'or) (mk 'and))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; quote (symbols)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (beginner-quote/expr/proc stx)
    (syntax-case stx ()
      [(_ expr)
       (let ([id-or-null (syntax expr)])
         (unless (or (identifier? id-or-null)
                     (null? (syntax-e id-or-null)))
           (teach-syntax-error
            'quote
            stx
            #f
            "expected the name of a symbol or () after the quote, but found ~a"
            (something-else id-or-null)))
         (syntax/loc stx (quote expr)))]
      [_else (bad-use-error 'quote stx)]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; require
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (check-string-form stx s)
    (unless (regexp-match #rx#"^[-a-zA-Z0-9_. ]+(/+[-a-zA-Z0-9_. ]+)*$" (syntax-e s))
      (teach-syntax-error
       'require
       stx
       s
       (cond
         [(string=? "" (syntax-e s))
          "a module-naming string cannot be empty"]
         [(regexp-match #rx"^/" (syntax-e s))
          "a module-naming string cannot start with a slash"]
         [(regexp-match #rx"/$" (syntax-e s))
          "a module-naming string cannot end with a slash"]
         [else
          "a module-naming string can contain only a-z, A-Z, 0-9, -, _, ., space, and slash"]))))
  
  (define (version-number? n)
    (and (number? n) (exact? n) (integer? n) (n . >= . 0)))
  
  (define (beginner-require/proc stx)
    (when (identifier? stx)
      (bad-use-error 'require stx))
    (unless (memq (syntax-local-context) '(top-level module module-begin))
      (teach-syntax-error
       'define
       stx
       #f
       "found a module require that is not at the top level"))
    (syntax-case stx (lib planet)
      [(_ s)
       (string? (syntax-e #'s))
       (begin
         (check-string-form stx #'s)
         (stepper-syntax-property
          #'(require s)
          'stepper-black-box-expr
          stx))]
      [(_ id)
       (identifier? #'id)
       (begin
         (unless (module-path? (syntax-e #'id))
           (teach-syntax-error
            'require
            stx
            #'id
            "bad syntax for a module path"))
         (stepper-syntax-property
          #'(require id)
          'stepper-black-box-expr
          stx))]
      [(_ (lib . rest))
       (let ([s (syntax->list #'rest)])
         (unless ((length s) . >= . 1)
           (teach-syntax-error
            'require
            stx
            #f
            "expected at least one string with lib, found only ~a parts"
            (length s)))
         (for-each (lambda (v)
                     (unless (string? (syntax-e v))
                       (teach-syntax-error
                        'require
                        stx
                        v
                        "expected a string for a lib path, found ~a"
                        (something-else v)))
                     (check-string-form stx v))
                   s)
         ;; use the original `lib', so that it binds correctly:
         (syntax-case stx ()
           [(_ ms) (stepper-syntax-property
                    #'(require ms)
                    'stepper-black-box-expr
                    stx)]))]
      [(_ (planet . rest))
       (let ([go
              (λ ()
                ;; use the original `planet', so that it binds correctly:
                (syntax-case stx ()
                  [(_ ms) (stepper-syntax-property
                           #'(require ms)
                           'stepper-black-box-expr
                           stx)]))])
         (syntax-case stx (planet)
           [(_ (planet s1 (s2 s3 n1 n2)))
            (and (string? (syntax-e #'s1))
                 (string? (syntax-e #'s2))
                 (string? (syntax-e #'s3))
                 (version-number? (syntax-e #'n1))
                 (version-number? (syntax-e #'n2)))
            (begin
              (check-string-form stx #'s1)
              (check-string-form stx #'s2)
              (check-string-form stx #'s3)
              (go))]
           [(_ (planet a))
            (or (string? (syntax-e #'a))
                (symbol? (syntax-e #'a)))
            (go)]
           [_else
            (teach-syntax-error
             'require
             stx
             #f
             (string-append
              "not a valid planet path; should be:"
              " (require (planet STRING (STRING STRING NUMBER NUMBER)))"
              " (require (planet STRING)) or (require (planet SYMBOL))"))]))]
      [(_ thing)
       (teach-syntax-error
        'require
        stx
        #'thing
        "expected a module name as a string, a `lib' form, or a `planet' form, found ~a"
        (something-else #'thing))]
      [(_)
       (teach-syntax-error
        'require
        stx
        #f
        "expected a module name after `require', but found nothing")]
      [(_ . rest)
       (teach-syntax-error
        'require
        stx
        #f
        "expected a single module name after `require', but found ~a parts"
        (length (syntax->list #'rest)))]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; dots (.. and ... and .... and ..... and ......)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Syntax Identifier -> Expression
  ;; Produces an expression which raises an error reporting unfinished code.
  (define (dots-error stx name)
    (quasisyntax/loc stx
      (error (quote (unsyntax name))
             "expected a finished expression, but found a template")))
  
  ;; Expression -> Expression
  ;; Transforms unfinished code (... and the like) to code
  ;; raising an appropriate error.
  (define beginner-dots/proc
    (make-set!-transformer
     (lambda (stx)
       
       ;; this ensures that coverage happens; it lifts a constant
       ;; expression to the top level, but one that has the source location of the dots expression
       (syntax-local-lift-expression (datum->syntax #'here 1 stx))
       
       (syntax-case stx (set!)
         [(set! form expr) (dots-error stx (syntax form))]
         [(form . rest) (dots-error stx (syntax form))]
         [form (dots-error stx stx)]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; local
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (intermediate-local/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_ (definition ...) . exprs)
          (let ([defns (syntax->list (syntax (definition ...)))]
                ;; The following context value lets teaching-language definition
                ;;  forms know that it's ok to expand in this internal
                ;;  definition context.
                [int-def-ctx (build-expand-context (make-expanding-for-intermediate-local))])
            (let* ([partly-expand (lambda (d)
                                    (local-expand
                                     d
                                     int-def-ctx
                                     (kernel-form-identifier-list)))]
                   [partly-expanded-defns
                    (map partly-expand defns)]
                   [flattened-defns
                    (let loop ([l partly-expanded-defns][origs defns])
                      (apply
                       append
                       (map (lambda (d orig)
                              (syntax-case d (begin define-values define-syntaxes)
                                ;; we don't have to check for ill-formed `define-values'
                                ;; or `define-syntaxes', because only macros can generate
                                ;; them
                                [(begin defn ...)
                                 (let ([l (map partly-expand (syntax->list (syntax (defn ...))))])
                                   (loop l l))]
                                [(define-values . _)
                                 (list d)]
                                [(define-syntaxes . _)
                                 (list d)]
                                [_else
                                 (teach-syntax-error
                                  'local
                                  stx
                                  orig
                                  "expected a definition, but found ~a"
                                  (something-else orig))]))
                            l origs)))]
                   [val-defns
                    (apply
                     append
                     (map (lambda (partly-expanded)
                            (syntax-case partly-expanded (define-values)
                              [(define-values (id ...) expr)
                               (list partly-expanded)]
                              [_else
                               null]))
                          flattened-defns))]
                   [stx-defns
                    (apply
                     append
                     (map (lambda (partly-expanded)
                            (syntax-case partly-expanded (define-syntaxes)
                              [(define-syntaxes (id ...) expr)
                               (list partly-expanded)]
                              [_else
                               null]))
                          flattened-defns))]
                   [get-ids (lambda (l)
                              (apply
                               append
                               (map (lambda (partly-expanded)
                                      (syntax-case partly-expanded ()
                                        [(_ (id ...) expr)
                                         (syntax->list (syntax (id ...)))]))
                                    l)))]
                   [val-ids (get-ids val-defns)]
                   [stx-ids (get-ids stx-defns)])
              (let ([dup (check-duplicate-identifier (append val-ids stx-ids))])
                (when dup
                  (teach-syntax-error
                   'local
                   stx
                   dup
                   "~a was defined locally more than once"
                   (syntax-e dup)))
                (let ([exprs (syntax->list (syntax exprs))])
                  (check-single-expression 'local
                                           "after the local definitions"
                                           stx
                                           exprs
                                           (append val-ids stx-ids)))
                (with-syntax ([((d-v (def-id ...) def-expr) ...) val-defns]
                              [(stx-def ...) stx-defns])
                  (with-syntax ([(((tmp-id def-id/prop) ...) ...)
                                 ;; Generate tmp-ids that at least look like the defined
                                 ;;  ids, for the purposes of error reporting, etc.:
                                 (map (lambda (def-ids)
                                        (map (lambda (def-id)
                                               (list
                                                (stepper-syntax-property
                                                 (datum->syntax-object
                                                  #f
                                                  (string->uninterned-symbol
                                                   (symbol->string (syntax-e def-id))))
                                                 'stepper-orig-name
                                                 def-id)
                                                (syntax-property
                                                 def-id
                                                 'bind-as-variable
                                                 #t)))
                                             (syntax->list def-ids)))
                                      (syntax->list (syntax ((def-id ...) ...))))])
                    (with-syntax ([(mapping ...)
                                   (let ([mappers
                                          (syntax->list
                                           (syntax
                                            ((define-syntaxes (def-id/prop ...)
                                               (values
                                                (redirect-identifier-to
                                                 (quote-syntax set!)
                                                 (quote-syntax tmp-id))
                                                ...))
                                             ...)))])
                                     (map syntax-track-origin
                                          mappers
                                          val-defns
                                          (syntax->list (syntax (d-v ...)))))])
                      (stepper-syntax-property
                       (quasisyntax/loc stx
                         (let ()
                           (#%stratified-body
                            (define #,(gensym) 1) ; this ensures that the expansion of 'local' looks
                            ; roughly the same, even if the local has no defs.
                            mapping ...
                            stx-def ...
                            (define-values (tmp-id ...) def-expr)
                            ...
                            . exprs)))
                       'stepper-hint
                       'comes-from-local)))))))]
         [(_ def-non-seq . __)
          (teach-syntax-error
           'local
           stx
           (syntax def-non-seq)
           "expected at least one definition (in square brackets) after local, but found ~a"
           (something-else (syntax def-non-seq)))]
         [(_)
          (teach-syntax-error
           'local
           stx
           #f
           "expected at least one definition (in square brackets) after local, but nothing's there")]
         [_else (bad-use-error 'local stx)]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; letrec and let (intermediate)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; For the `let' forms, match the definitely correct patterns, and
  ;; put all error checking in `bad-let-form'.
  
  (define (intermediate-letrec/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_ ([name rhs-expr] ...) expr)
          (let ([names (syntax->list (syntax (name ...)))])
            (and (andmap identifier/non-kw? names)
                 (not (check-duplicate-identifier names))))
          (with-syntax ([(tmp-id ...)
                         ;; Generate tmp-ids that at least look like the defined
                         ;;  ids, for the purposes of error reporting, etc.:
                         (map (lambda (name)
                                (stepper-syntax-property
                                 (datum->syntax-object
                                  #f
                                  (string->uninterned-symbol
                                   (symbol->string (syntax-e name))))
                                 'stepper-orig-name
                                 name))
                              (syntax->list #`(name ...)))]
                        [(rhs-expr ...) (map allow-local-lambda 
                                             (syntax->list (syntax (rhs-expr ...))))])
            (quasisyntax/loc stx
              (#%stratified-body
               (define-syntaxes (name) (redirect-identifier-to
                                        (quote-syntax set!)
                                        (quote-syntax tmp-id)))
               ...
               (define-values (tmp-id) rhs-expr)
               ...
               expr)))]
         [_else (bad-let-form 'letrec stx stx)]))))
  
  (define (intermediate-let/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_ ([name rhs-expr] ...) expr)
          (let ([names (syntax->list (syntax (name ...)))])
            (and (andmap identifier/non-kw? names)
                 (not (check-duplicate-identifier names))))
          (with-syntax ([(tmp-id ...)
                         ;; Generate tmp-ids that at least look like the defined
                         ;;  ids, for the purposes of error reporting, etc.:
                         (map (lambda (name)
                                (stepper-syntax-property
                                 (datum->syntax-object
                                  #f
                                  (string->uninterned-symbol
                                   (symbol->string (syntax-e name))))
                                 'stepper-orig-name
                                 name))
                              (syntax->list #`(name ...)))]
                        [(rhs-expr ...) (map allow-local-lambda 
                                             (syntax->list (syntax (rhs-expr ...))))])
            (quasisyntax/loc stx
              (let-values ([(tmp-id) rhs-expr] ...)
                #,(stepper-syntax-property
                   #`(let-syntaxes ([(name) (redirect-identifier-to
                                             (quote-syntax set!t)
                                             (quote-syntax tmp-id))]
                                    ...)
                                   expr)
                   'stepper-skipto
                   (append
                    ;; body of let-values:
                    skipto/third
                    ;; body of let-values:
                    skipto/third)))))]
         [_else (bad-let-form 'let stx stx)]))))
  
  (define (intermediate-let*/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_ () expr)
          (stepper-syntax-property
           #`(let () expr)
           'stepper-skipto
           skipto/third)]
         [(_ ([name0 rhs-expr0] [name rhs-expr] ...) expr)
          (let ([names (syntax->list (syntax (name0 name ...)))])
            (andmap identifier/non-kw? names))
          (with-syntax ([rhs-expr0 (allow-local-lambda (syntax rhs-expr0))])
            (stepper-syntax-property
             (quasisyntax/loc stx
               (intermediate-let ([name0 rhs-expr0])
                                 #,(quasisyntax/loc stx 
                                     (intermediate-let* ([name rhs-expr]
                                                         ...)
                                                        expr))))
             'stepper-hint
             'comes-from-let*))]
         [_else (bad-let-form 'let* stx stx)]))))
  
  ;; Helper function: allows `intermediate-pre-lambda' instead
  ;; of rejecting it:
  (define (allow-local-lambda stx)
    (syntax-case stx (intermediate-pre-lambda)
      [(intermediate-pre-lambda . rest)
       (begin
         (check-defined-lambda stx)
         ;; pattern-match again to pull out the formals:
         (syntax-case stx ()
           [(_ formals . rest)
            (quasisyntax/loc stx (lambda formals #,(stepper-syntax-property
                                                    #`make-lambda-generative
                                                    'stepper-skip-completely
                                                    #t)
                                   . rest))]))]
      [_else stx]))
  
  ;; Helper function:
  (define (bad-let-form who stx orig-stx)
    (syntax-case stx ()
      [(_ (binding ...) . exprs)
       (let ([bindings (syntax->list (syntax (binding ...)))])
         (for-each (lambda (binding)
                     (syntax-case binding ()
                       [(something . exprs)
                        (not (identifier/non-kw? (syntax something)))
                        (teach-syntax-error
                         who
                         orig-stx
                         (syntax something)
                         "expected a variable for the binding, but found ~a"
                         (something-else/kw (syntax something)))]
                       [(name expr)
                        (void)]
                       [(name . exprs)
                        (check-single-expression who
                                                 (format "after the name ~a"
                                                         (syntax-e (syntax name)))
                                                 binding
                                                 (syntax->list (syntax exprs))
                                                 #f)]
                       [_else
                        (teach-syntax-error
                         who
                         orig-stx
                         binding
                         "expected a binding with a variable and an expression, but found ~a"
                         (something-else binding))]))
                   bindings)
         (unless (eq? who 'let*)
           (let ([dup (check-duplicate-identifier (map (lambda (binding)
                                                         (syntax-case binding ()
                                                           [(name . _) (syntax name)]))
                                                       bindings))])
             (when dup
               (teach-syntax-error
                who
                orig-stx
                dup
                "~a was defined locally more than once"
                (syntax-e dup)))))
         (let ([exprs (syntax->list (syntax exprs))])
           (check-single-expression who 
                                    "after the bindings"
                                    orig-stx
                                    exprs
                                    #f)))]
      [(_ binding-non-seq . __)
       (teach-syntax-error
        who
        orig-stx
        (syntax binding-non-seq)
        "expected at least one binding (in parentheses) after ~a, but found ~a"
        who
        (something-else (syntax binding-non-seq)))]
      [(_)
       (teach-syntax-error
        who
        orig-stx
        #f
        "expected at least one binding (in parentheses) after ~a, but nothing's there"
        who)]
      [_else
       (bad-use-error who stx)]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; recur (intermediate and advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; We can defer some error reporting to `bad-let-form',
  ;; but not all of it.
  
  (define-values (intermediate-recur/proc advanced-recur/proc)
    (let ([mk
           (lambda (empty-ok?)
             (lambda (stx)
               (ensure-expression
                stx
                (lambda ()
                  (syntax-case stx ()
                    [(_ fname ([name rhs-expr] ...) expr)
                     (and (identifier/non-kw? (syntax fname))
                          (let ([names (syntax->list (syntax (name ...)))])
                            (and (andmap identifier/non-kw? names)
                                 (or empty-ok? (pair? names))
                                 (not (check-duplicate-identifier names)))))
                     (stepper-syntax-property
                      (quasisyntax/loc stx
                        ((intermediate-letrec ([fname
                                                #,(stepper-syntax-property
                                                   (stepper-syntax-property 
                                                    #`(lambda (name ...)
                                                        expr)
                                                    'stepper-define-type
                                                    'shortened-proc-define)
                                                   'stepper-proc-define-name
                                                   #`fname)])
                                              fname)
                         rhs-expr ...))
                      'stepper-hint
                      'comes-from-recur)]
                    [(_form fname empty-seq . rest)
                     (and (not empty-ok?)
                          (identifier/non-kw? (syntax fname))
                          (null? (syntax-e (syntax empty-seq))))
                     (teach-syntax-error
                      'recur
                      stx
                      (syntax empty-seq)
                      EXPECTED-1)]
                    [(_form fname . rest)
                     (identifier/non-kw? (syntax fname))
                     (bad-let-form 'recur (syntax (_form . rest)) stx)]
                    [(_form fname . rest)
                     (teach-syntax-error
                      'recur
                      stx
                      #f
                      "expected a function name after recur, but found ~a"
                      (something-else/kw (syntax fname)))]
                    [(_form)
                     (teach-syntax-error
                      'recur
                      stx
                      #f
                      "expected a function name after recur, but nothing's there")]
                    [_else
                     (bad-use-error 'recur stx)])))))])
      (values (mk #f) (mk #t))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; lambda (intermediate)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (intermediate-lambda/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_ arg-seq lexpr ...)
          (syntax-case (syntax arg-seq) () [(arg ...) #t][_else #f])
          (let ([args (syntax->list (syntax arg-seq))])
            (for-each (lambda (arg)
                        (unless (identifier/non-kw? arg)
                          (teach-syntax-error
                           'lambda
                           stx
                           arg
                           "expected a variable, but found ~a"
                           (something-else/kw arg))))
                      args)
            (when (null? args)
              (teach-syntax-error
               'lambda
               stx
               (syntax arg-seq)
               "expected at least one variable after lambda, but found none"))
            (let ([dup (check-duplicate-identifier args)])
              (when dup
                (teach-syntax-error
                 'lambda
                 stx
                 dup
                 "found a variable that is used more than once: ~a"
                 (syntax-e dup))))
            (check-single-expression 'lambda
                                     "for the function body"
                                     stx
                                     (syntax->list (syntax (lexpr ...)))
                                     args)
            (syntax/loc stx (lambda arg-seq lexpr ...)))]
         ;; Bad lambda because bad args:
         [(_ args . __)
          (teach-syntax-error
           'lambda
           stx
           (syntax args)
           "expected at least one variable (in parentheses) after lambda, but found ~a"
           (something-else (syntax args)))]
         [(_)
          (teach-syntax-error
           'lambda
           stx
           #f
           "expected at least one variable (in parentheses) after lambda, but nothing's there")]
         [_else
          (bad-use-error 'lambda stx)]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; quote (intermediate)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define intermediate-quote/expr/proc
    (lambda (stx)
      (syntax-case stx ()
        [(_ expr ...)
         (begin
           (check-single-expression 'quote
                                    "after quote"
                                    stx
                                    (syntax->list (syntax (expr ...)))
                                    ;; Don't expand expr!
                                    #f)
           (syntax (quote expr ...)))]
        [_else (bad-use-error 'quote stx)])))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; quasiquote (intermediate)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; This quasiquote uses the right cons, and perhaps provides more
  ;; suitable error messages. The "right" cons is actually advanced-cons,
  ;; because it works with shared:
  
  (define (intermediate-quasiquote/expr/proc stx)
    (let loop ([stx (syntax-case stx ()
                      [(_ stx) (syntax stx)]
                      [(_ . any)
                       (teach-syntax-error
                        'quasiquote
                        stx
                        #f
                        "misuse of quasiquote")]
                      [_else (bad-use-error 'quasiquote stx)])]
               [depth 0])
      (syntax-case stx (intermediate-unquote intermediate-unquote-splicing intermediate-quasiquote)
        [(intermediate-unquote x)
         (if (zero? depth)
             (syntax x)
             (with-syntax ([x (loop (syntax x) (sub1 depth))]
                           [uq (stx-car stx)])
               (syntax/loc stx (list (quote uq) x))))]
        [intermediate-unquote
         (teach-syntax-error
          'quasiquote
          stx
          #f
          "misuse of unquote within a quasiquoting backquote")]
        [((intermediate-unquote-splicing x) . rest)
         (if (zero? depth)
             (with-syntax ([rest (loop (syntax rest) depth)])
               (syntax (append x rest)))
             (with-syntax ([x (loop (syntax x) (sub1 depth))]
                           [rest (loop (syntax rest) depth)]
                           [uq-splicing (stx-car (stx-car stx))])
               (syntax/loc stx (the-cons/matchable (list (quote uq-splicing) x) rest))))]
        [intermediate-unquote-splicing
         (teach-syntax-error
          'quasiquote
          stx
          #f
          "misuse of ,@ or unquote-splicing within a quasiquoting backquote")]
        [(intermediate-quasiquote x)
         (with-syntax ([x (loop (syntax x) (add1 depth))]
                       [qq (stx-car stx)])
           (syntax/loc stx (list (quote qq) x)))]
        [(a . b)
         (with-syntax ([a (loop (syntax a) depth)]
                       [b (loop (syntax b) depth)])
           (syntax/loc stx (the-cons/matchable a b)))]
        [any
         (syntax/loc stx (quote any))])))
  
  (define (intermediate-unquote/proc stx)
    (teach-syntax-error
     'unquote
     stx
     #f
     "misuse of a comma or unquote, not under a quasiquoting backquote"))
  
  (define (intermediate-unquote-splicing/proc stx)
    (teach-syntax-error
     'unquote-splicing
     stx
     #f
     "misuse of ,@ or unquote-splicing, not under a quasiquoting backquote"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; time
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (intermediate-time/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_ . exprs)
          (check-single-expression 'time 
                                   "after time"
                                   stx
                                   (syntax->list (syntax exprs))
                                   null)
          (stepper-syntax-property 
           (syntax/loc stx (time . exprs))
           'stepper-skipto
           (append 
            ;; let-values-bindings
            skipto/second
            ;; rhs of first binding
            skipto/first
            skipto/second
            ;; 2nd term of application:
            skipto/cdr
            skipto/second
            ;; lambda-body:
            skipto/cddr
            skipto/first))]
         [_else
          (bad-use-error 'time stx)]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (advanced-define/proc stx)
    ;; Handle the case that doesn't fit into intermediate, then dispatch to 
    ;; the common code that it also used by beginner/intermediate.
    (syntax-case stx ()
      [(_ (name) expr)
       (and (identifier/non-kw? (syntax name))
            (ok-definition-context))
       (check-definition-new
        'define
        stx
        (syntax name)
        (syntax/loc stx (define (name) expr))
        (list #'name))]
      [(_ (name) expr ...)
       (and (identifier/non-kw? (syntax name))
            (ok-definition-context))
       (check-single-result-expr (syntax->list (syntax (expr ...)))
                                 #f
                                 stx
                                 (list #'name))]
      [(_ . rest)
       ;; Call transformer define/proc.
       ;; Note that we call the transformer instead of producing
       ;; new syntax object that is an `intermediate-define' form;
       ;; that's important for syntax errors, so that they
       ;; report `advanced-define' as the source.
       (define/proc #f #t stx #'beginner-lambda)]
      [_else
       (bad-use-error 'define stx)]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; lambda (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (advanced-lambda/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_  (name ...) . exprs)
          (let ([names (syntax->list (syntax (name ...)))])
            (for-each (lambda (name)
                        (unless (identifier/non-kw? name)
                          (teach-syntax-error
                           'lambda
                           stx
                           name
                           "expected a variable, but found ~a"
                           (something-else/kw name))))
                      names)
            (let ([dup (check-duplicate-identifier names)])
              (when dup
                (teach-syntax-error
                 'lambda
                 stx
                 dup
                 "found a variable that is used more than once: ~a"
                 (syntax-e dup))))
            (check-single-expression 'lambda 
                                     "for the function body"
                                     stx
                                     (syntax->list (syntax exprs))
                                     names)
            (syntax/loc stx (lambda (name ...) . exprs)))]
         [(_ arg-non-seq . exprs)
          (teach-syntax-error
           'lambda
           stx
           (syntax arg-non-seq)
           "expected at least one variable (in parentheses) after lambda, but found ~a"
           (something-else (syntax arg-non-seq)))]
         [(_)
          (teach-syntax-error
           'lambda
           stx
           #f
           "expected at least one variable (in parentheses) after lambda, but nothing's there")]
         [_else
          (bad-use-error 'lambda stx)]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; application (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (advanced-app/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_ rator rand ...)
          (with-syntax ([new-rator (syntax-property #'rator 'was-in-app-position #t)])
            (syntax/loc stx (#%app new-rator rand ...)))]
         [(_)
          (teach-syntax-error
           '|function call|
           stx
           #f
           "expected a function after the open parenthesis, but nothing's there")]
         [_else (bad-use-error '|function call| stx)]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; set! (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; We disallow set!s on lambda-bound variables, which we recognize
  ;; as lexically-bound variables that are not bound to
  ;; set!-transformer syntax values.
  
  (define-values (advanced-set!/proc advanced-set!-continue/proc)
    (let ([proc
           (lambda (continuing?)
             (lambda (stx)
               (ensure-expression
                stx
                (lambda ()
                  (syntax-case stx ()
                    [(_ id expr ...)
                     (identifier? (syntax id))
                     (let ([exprs (syntax->list (syntax (expr ...)))])
                       ;; Check that id isn't syntax, and not lexical.
                       ;; First try syntax:
                       (let ([binding (syntax-local-value (syntax id) (lambda () #f))])
                         ;; If it's a transformer binding, then it can take care of itself...
                         (cond
                           [(set!-transformer? binding)
                            ;; no lex check wanted
                            (void)]
                           [(not binding)
                            ;; Now try lexical:
                            (when (eq? 'lexical (identifier-binding (syntax id)))
                              (teach-syntax-error
                               'set!
                               stx
                               (syntax id)
                               EXPECTED-MUTABLE
                               (syntax-e #'id)))]
                           [else 
                            (teach-syntax-error
                             'set!
                             stx
                             (syntax id)
                             "expected a variable after set!, but found a ~a" (syntax-e #'id))]))
                       ;; If we're in a module, we'd like to check here whether
                       ;;  the identier is bound, but we need to delay that check
                       ;;  in case the id is defined later in the module. So only
                       ;;  do this in continuing mode:
                       (when continuing?
                         (let ([binding (identifier-binding #'id)])
                           (cond
                             [(and (not binding)
                                   (syntax-source-module #'id))
                              (teach-syntax-error
                               #f
                               #'id
                               #f
                               "this variable is not defined")]
                             [(and (list? binding)
                                   (or (not (module-path-index? (car binding)))
                                       (let-values ([(path rel) 
                                                     (module-path-index-split (car binding))])
                                         path)))
                              (teach-syntax-error
                               'set!
                               #'id
                               #f
                               EXPECTED-MUTABLE
                               (syntax-e #'id))])))
                       ;; Check the RHS
                       (check-single-expression 'set!
                                                "for the new value"
                                                stx
                                                exprs
                                                null)
                       
                       (if continuing?
                           (stepper-syntax-property
                            (quasisyntax/loc stx
                              (begin
                                #,(datum->syntax #'here 
                                                 `(set! ,#'id ,@(syntax->list #'(expr ...))) 
                                                 stx)
                                set!-result))
                            'stepper-skipto
                            (append skipto/cdr
                                    skipto/first))
                           (stepper-ignore-checker 
                            (quasisyntax/loc stx
                              (#%app values 
                                     #,(advanced-set!-continue/proc
                                        (syntax/loc stx (_ id expr ...))))))))]
                    [(_ id . __)
                     (teach-syntax-error
                      'set!
                      stx
                      (syntax id)
                      "expected a variable after set!, but found ~a"
                      (something-else (syntax id)))]
                    [(_)
                     (teach-syntax-error
                      'set!
                      stx
                      #f
                      "expected a variable after set!, but nothing's there")]
                    [_else (bad-use-error 'set! stx)])))))])
      (values (proc #f)
              (proc #t))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; when and unless (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-values (advanced-when/proc advanced-unless/proc)
    (let ([mk
           (lambda (who target-stx)
             (lambda (stx)
               (ensure-expression
                stx
                (lambda ()
                  (syntax-case stx ()
                    [(_)
                     (teach-syntax-error
                      who
                      stx
                      #f
                      "expected a question and an answer, but nothing's there")]
                    [(_ q)
                     (teach-syntax-error
                      who
                      stx
                      #'q
                      "expected a question and an answer, but found only one part")]
                    [(_ q a)
                     (with-syntax ([who who]
                                   [target target-stx])
                       (syntax/loc stx (target (verify-boolean q 'who) a)))]
                    [(_ . parts)
                     (teach-syntax-error*
                      who
                      stx
                      (syntax->list #'parts)
                      "expected a question and an answer, but found ~a parts" 
                      (length (syntax->list #'parts)))]
                    [_else
                     (bad-use-error who stx)])))))])
      (values (mk 'when (quote-syntax when))
              (mk 'unless (quote-syntax unless)))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; define-struct (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (advanced-define-struct/proc stx)
    (do-define-struct stx #f #t))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; let (advanced)       >> mz errors in named case <<
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (advanced-let/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_form name . rest)
          (identifier/non-kw? (syntax name))
          (begin
            (bad-let-form 'let (syntax (_form . rest)) stx)
            (syntax/loc stx (let name . rest)))]
         [(_ name)
          (identifier/non-kw? (syntax name))
          (teach-syntax-error
           'let
           stx
           #f
           "expected at least one binding (in parentheses) after ~a, but nothing's there"
           (syntax->datum (syntax name)))]
         [(_form name . rest)
          (identifier/non-kw? (syntax name))
          (bad-let-form 'let (syntax (_form . rest)) stx)]
         [(_ . rest)
          (syntax/loc stx (intermediate-let . rest))]
         [_else
          (bad-use-error 'let stx)]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; begin (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Mask out the top-level begin
  (define (advanced-begin/proc stx)
    (syntax-case stx ()
      [(_)
       (teach-syntax-error
        'begin
        stx
        #f
        "expected at least one expression after begin, but nothing's there")]
      [(_ e ...)
       (stepper-syntax-property (syntax/loc stx (let () e ...))
                                'stepper-hint
                                'comes-from-begin)]
      [_else
       (bad-use-error 'begin stx)]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; begin0 (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (advanced-begin0/proc stx)
    (syntax-case stx ()
      [(_)
       (teach-syntax-error
        'begin0
        stx
        #f
        "expected at least one expression after begin0, but nothing's there")]
      [(_ e ...)
       (syntax/loc stx (begin0 e ...))]
      [_else
       (bad-use-error 'begin0 stx)]))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; case
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (advanced-case/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_)
          (teach-syntax-error
           'case
           stx
           #f
           "expected an expression after case, but nothing's there")]
         [(_ expr)
          (teach-syntax-error
           'case
           stx
           #f
           EXPECTED-MORE-THAN-NOTHING)]
         [(_ v-expr clause ...)
          (let ([clauses (syntax->list (syntax (clause ...)))])
            (for-each
             (lambda (clause)
               (syntax-case clause (beginner-else)
                 [(beginner-else answer ...)
                  (let ([lpos (memq clause clauses)])
                    (when (not (null? (cdr lpos)))
                      (teach-syntax-error
                       'case
                       stx
                       clause
                       "found an else clause that isn't the last clause ~
                                    in its case expression"))
                    (let ([answers (syntax->list (syntax (answer ...)))])
                      (check-single-expression 'case
                                               "for the answer in the case clause"
                                               clause
                                               answers
                                               null)))]
                 [(choices answer ...)
                  (let ([choices (syntax choices)]
                        [answers (syntax->list (syntax (answer ...)))])
                    (syntax-case choices ()
                      [(elem ...)
                       (let ([elems (syntax->list (syntax (elem ...)))])
                         (for-each (lambda (e)
                                     (let ([v (syntax-e e)])
                                       (unless (or (number? v)
                                                   (symbol? v))
                                         (teach-syntax-error
                                          'case
                                          stx
                                          e
                                          EXPECTED-SYMBOL-OR-NUMBER+
                                          (something-else e)))))
                                   elems))]
                      [_else (teach-syntax-error
                              'case
                              stx
                              choices
                              "expected at least one choice (in parentheses), but found ~a"
                              (something-else choices))])
                    (when (stx-null? choices)
                      (teach-syntax-error
                       'case
                       stx
                       choices
                       EXPECTED-SYMBOL-OR-NUMBER))
                    (check-single-expression 'case
                                             "for the answer in the case clause"
                                             clause
                                             answers
                                             null))]
                 [()
                  (teach-syntax-error
                   'case
                   stx
                   clause
                   EXPECTED-CLAUSE-WITH-CHOICE
                   "an empty part")]
                 [_else
                  (teach-syntax-error
                   'case
                   stx
                   clause
                   EXPECTED-CLAUSE-WITH-CHOICE
                   (something-else clause))]))
             clauses)
            ;; Add `else' clause for error, if necessary:
            (let ([clauses
                   (let loop ([clauses clauses])
                     (cond
                       [(null? clauses)
                        (list
                         (syntax/loc stx
                           [else (error 'cases "the expression matched none of the choices")]))]
                       [(syntax-case (car clauses) (beginner-else)
                          [(beginner-else . _) (syntax/loc (car clauses) (else . _))]
                          [_else #f])
                        => 
                        (lambda (x) (cons x (cdr clauses)))]
                       [else (cons (car clauses) (loop (cdr clauses)))]))])
              (with-syntax ([clauses clauses])
                (syntax/loc stx (case v-expr . clauses)))))]
         [_else (bad-use-error 'case stx)]))))
  
  ;; match (advanced)
  (define (advanced-match/proc stx)
    (ensure-expression
     stx
     (lambda ()
       (syntax-case stx ()
         [(_)
          (teach-syntax-error
           'match
           stx
           #f
           "expected an expression after `match', but nothing's there")]
         [(_ expr)
          (teach-syntax-error
           'match
           stx
           #f
           EXPECTED-MATCH-PATTERN)]
         [(_ v-expr clause ...)
          (let ([clauses (syntax->list (syntax (clause ...)))])
            (for-each
             (lambda (clause)
               (syntax-case clause ()
                 [(pattern answer ...)
                  (let ([pattern (syntax pattern)]
                        [answers (syntax->list (syntax (answer ...)))])
                    (check-single-expression 'match
                                             "for the answer in a `match' clause"
                                             clause
                                             answers
                                             null))]
                 [()
                  (teach-syntax-error
                   'match
                   stx
                   clause
                   "expected a pattern--answer clause, but found an empty clause")]
                 [_else
                  (teach-syntax-error
                   'match
                   stx
                   clause
                   "expected a pattern--answer clause, but found ~a"
                   (something-else clause))]))
             clauses)
            
            (letrec
                ([check-and-translate-qqp
                  (λ (qqp)
                    (syntax-case qqp (intermediate-unquote intermediate-unquote-splicing)
                      [(intermediate-unquote p)
                       (quasisyntax/loc qqp
                         (unquote #,(check-and-translate-p #'p)))]
                      [(intermediate-unquote-splicing p)
                       (quasisyntax/loc qqp
                         (unquote-splicing #,(check-and-translate-p #'p)))]
                      [(qqpi ...)
                       (quasisyntax/loc qqp
                         (#,@(map check-and-translate-qqp (syntax->list #'(qqpi ...)))))]
                      [_
                       qqp]))]
                 [check-and-translate-p
                  (λ (p)
                    (syntax-case p (struct 
                                     posn
                                     beginner-true
                                     beginner-false
                                     empty
                                     intermediate-quote
                                     intermediate-quasiquote
                                     advanced-cons
                                     list
                                     advanced-list*
                                     vector
                                     box)
                      [beginner-true
                        (syntax/loc p
                          #t)]
                      [beginner-false
                        (syntax/loc p
                          #f)]
                      [empty
                       (syntax/loc p
                         (list))]
                      [(intermediate-quote qp)
                       (syntax/loc p
                         (quote qp))]
                      [(intermediate-quasiquote qqp)
                       (quasisyntax/loc p
                         (quasiquote #,(check-and-translate-qqp #'qqp)))]
                      [(advanced-cons p1 p2)
                       (quasisyntax/loc p
                         (cons #,(check-and-translate-p #'p1)
                               #,(check-and-translate-p #'p2)))]
                      [(list pi ...)
                       (quasisyntax/loc p
                         (list #,@(map check-and-translate-p (syntax->list #'(pi ...)))))]
                      [(advanced-list* pi ...)
                       (quasisyntax/loc p
                         (list* #,@(map check-and-translate-p (syntax->list #'(pi ...)))))]
                      [(struct posn (pi ...))
                       (quasisyntax/loc p
                         (struct posn-id #,(map check-and-translate-p (syntax->list #'(pi ...)))))]
                      [(struct struct-id (pi ...))
                       (quasisyntax/loc p
                         (struct struct-id #,(map check-and-translate-p (syntax->list #'(pi ...)))))]
                      [(vector pi ...)
                       (quasisyntax/loc p
                         (vector #,@(map check-and-translate-p (syntax->list #'(pi ...)))))]
                      [(box p1)
                       (quasisyntax/loc p
                         (box #,(check-and-translate-p #'p1)))]
                      [_
                       (let ([v (syntax->datum p)])
                         (if (or (and (symbol? v)
                                      (not (member v '(true false empty))))
                                 (number? v)
                                 (string? v)
                                 (char? v))
                             p
                             (teach-syntax-error
                              'match
                              stx
                              p
                              "expected a pattern, but found ~a"
                              (something-else p))))]))])
              (let ([clauses
                     (map (λ (c)
                            (syntax-case c ()
                              [(p e)
                               (quasisyntax/loc c
                                 (#,(check-and-translate-p #'p) e))]))
                          clauses)])
                (with-syntax ([clauses clauses])
                  (syntax/loc stx
                    (match v-expr . clauses))))))]
         [_else (bad-use-error 'match stx)]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; delay (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define advanced-delay/proc
    (lambda (stx)
      (ensure-expression
       stx
       (lambda ()
         (syntax-case stx ()
           [(_ expr ...)
            (begin
              (check-single-expression 'delay
                                       "after delay"
                                       stx
                                       (syntax->list (syntax (expr ...)))
                                       null)
              (syntax (delay expr ...)))]
           [_else (bad-use-error 'delay stx)])))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; shared (advanced)
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; We do all the initial syntax checks, and otherwise
  ;; let "shared-body.rkt" implement the form.
  
  (define advanced-shared/proc
    (lambda (stx)
      (ensure-expression
       stx
       (lambda ()
         ;; Helper for the main implementation
         (define (make-check-cdr name)
           (with-syntax ([name name])
             (syntax (unless (cyclic-list? (cdr name))
                       (raise-type-error
                        'cons
                        "list"
                        1
                        (car name)
                        (cdr name))))))
         
         ;; Check the syntax before letting the main implementation go:
         (syntax-case stx ()
           [(_ (binding ...) . exprs)
            (let ([bindings (syntax->list (syntax (binding ...)))])
              (for-each
               (lambda (binding)
                 (syntax-case binding ()
                   [(id . exprs)
                    (identifier/non-kw? (syntax id))
                    (check-single-expression 'shared
                                             "after the binding name"
                                             binding
                                             (syntax->list (syntax exprs))
                                             #f)]
                   [(a . rest)
                    (not (identifier/non-kw? (syntax a)))
                    (teach-syntax-error
                     'shared
                     stx
                     (syntax a)
                     "expected a variable for the binding, but found ~a"
                     (something-else/kw (syntax a)))]
                   [()
                    (teach-syntax-error
                     'shared
                     stx
                     #f
                     "expected a variable for a binding, but nothing's there")]
                   [_else
                    (teach-syntax-error
                     'shared
                     stx
                     binding
                     "expected a binding with a variable and an expression, but found ~a"
                     (something-else binding))]))
               bindings)
              (check-single-expression 'shared
                                       "after the bindings"
                                       stx
                                       (syntax->list (syntax exprs))
                                       #f)
              (let ([dup (check-duplicate-identifier (map (lambda (binding)
                                                            (syntax-case binding ()
                                                              [(name . _) (syntax name)]))
                                                          bindings))])
                (when dup
                  (teach-syntax-error
                   'shared
                   stx
                   dup
                   "found a variable that is used more than once: ~a"
                   (syntax-e dup)))))]
           [(_ bad-bind . exprs)
            (teach-syntax-error
             'shared
             stx
             (syntax bad-bind)
             "expected at least one binding (in parentheses) after shared, but found ~a"
             (something-else (syntax bad-bind)))]
           [(_)
            (teach-syntax-error
             'shared
             stx
             #f
             "expected at least one binding (in parentheses) after shared, but nothing's there")]
           [_else (bad-use-error 'shared stx)])
         
         ;; The main implementation
         (shared/proc stx make-check-cdr))))))

;; ----------------------------------------
;; Utilities for `define-struct':

(define (make-equal-hash generic-access field-count)
  (lambda (r recur)
    (let loop ((i 0)
               (factor 1)
               (hash 0))
      (if (= i field-count)
          hash
          (loop (+ 1 i)
                (* factor 33)
                (+ hash (* factor (recur (generic-access r i)))))))))

(define (make-equal2-hash generic-access field-count)
  (lambda (r recur)
    (let loop ((i 0)
               (factor 1)
               (hash 0))
      (if (= i field-count)
          hash
          (loop (+ 1 i)
                (* factor 33)
                (+ hash (* factor 
                           (recur (generic-access r (- field-count i 1))))))))))

;; ----------------------------------------
;; Extend quote forms to work with `match':

(provide beginner-quote
         intermediate-quote
         intermediate-quasiquote)

(define-match-expander beginner-quote
  (syntax-local-value #'beginner-quote/expr)
  (syntax-local-value #'beginner-quote/expr))

(define-match-expander intermediate-quote
  (syntax-local-value #'intermediate-quote/expr)
  (syntax-local-value #'intermediate-quote/expr))

(define-match-expander intermediate-quasiquote
  ;; Match expander:
  (let ([qq (syntax-local-value #'intermediate-quasiquote/expr)])
    (lambda (stx)
      ;; Call expression version for checking:
      (qq stx)
      ;; But then just use `scheme/base' quasiquote and unquotes:
      (quasisyntax/loc stx
        (quasiquote
         #,(let loop ([stx (syntax-case stx ()
                             [(_ stx) (syntax stx)])]
                      [depth 0])
             (syntax-case stx (intermediate-unquote 
                               intermediate-unquote-splicing
                               intermediate-quasiquote)
               [(intermediate-unquote x)
                (if (zero? depth)
                    (syntax (unquote x))
                    (with-syntax ([x (loop (syntax x) (sub1 depth))])
                      (syntax/loc stx (unquote x))))]
               [((intermediate-unquote-splicing x) . rest)
                (if (zero? depth)
                    (with-syntax ([rest (loop (syntax rest) depth)])
                      (syntax/loc stx ((unquote-splicing x) . rest)))
                    (with-syntax ([x (loop (syntax x) (sub1 depth))]
                                  [rest (loop (syntax rest) depth)])
                      (syntax/loc stx ((unquote-splicing x) . rest))))]
               [(intermediate-quasiquote x)
                (with-syntax ([x (loop (syntax x) (add1 depth))]
                              [qq (stx-car stx)])
                  (syntax/loc stx (quasiquote x)))]
               [(a . b)
                (with-syntax ([a (loop (syntax a) depth)]
                              [b (loop (syntax b) depth)])
                  (syntax/loc stx (a . b)))]
               [any stx]))))))
  ;; Expression expander:
  (syntax-local-value #'intermediate-quasiquote/expr))

(define-match-expander the-cons/matchable
  ;; For match (no cdr check needed for deconstruction):
  (lambda (stx)
    (syntax-case stx ()
      [(_ a b) (syntax/loc stx (cons a b))]))
  ;; For expressions (cdr check via `the-cons'):
  (lambda (stx)
    (with-syntax
        ([the-cons/tagged (stepper-syntax-property
                           #'the-cons
                           'stepper-prim-name
                           #'cons)])
      (syntax-case stx ()
        [(_ a b) (syntax/loc stx (the-cons/tagged a b))]))))

(provide signature :
         -> mixed one-of predicate combined)

(provide Integer Number Rational Real Natural 
         Boolean True False
         String Char Symbol Empty-list
         Any Unspecific
         cons-of)

(define Integer (signature/arbitrary arbitrary-integer (predicate integer?)))
(define Number (signature/arbitrary arbitrary-real (predicate number?)))
(define Rational (signature/arbitrary arbitrary-rational (predicate rational?)))
(define Real (signature/arbitrary arbitrary-real (predicate real?)))

(define (natural? x)
  (and (integer? x)
       (not (negative? x))))

(define Natural (signature/arbitrary arbitrary-natural (predicate natural?)))

(define Boolean (signature/arbitrary arbitrary-boolean (predicate boolean?)))

(define True (signature (one-of #f)))
(define False (signature (one-of #f)))

(define String (signature/arbitrary arbitrary-printable-ascii-string (predicate string?)))
(define Char (signature/arbitrary arbitrary-printable-ascii-string (predicate char?)))
(define Symbol (signature/arbitrary arbitrary-symbol (predicate symbol?)))
(define Empty-list (signature (one-of empty)))

(define Any (signature Any %Any))

(define Unspecific (signature Unspecific %Unspecific))

(define (cons-of car-sig cdr-sig)
  (make-pair-signature #t car-sig cdr-sig))

; QuickCheck

(provide for-all ==>
         check-property
         expect expect-within expect-member-of expect-range
         Property)

(define-syntax (for-all stx)
  (syntax-case stx ()
    ((_ (?clause ...) ?body)
     (with-syntax
         ((((?id ?arb) ...)
           (map (lambda (pr)
                  (syntax-case pr ()
                    ((?id ?signature)
                     (identifier? #'?id)
                     (with-syntax
                         ((?error-call
                           (syntax/loc #'?signature (error "Signature does not have a generator"))))
                       #'(?id
                          (or (signature-arbitrary (signature ?signature))
                              ?error-call))))
                    (_
                     (raise-syntax-error #f "incorrect `for-all' clause - should have form (id contr)"
                                         pr))))
                (syntax->list #'(?clause ...)))))
       
       (stepper-syntax-property #'(quickcheck:property 
                                   ((?id ?arb) ...) ?body)
                                'stepper-skip-completely
                                #t)))
    ((_ ?something ?body)
     (raise-syntax-error #f "no clauses of them form (id contr)"
                         stx))
    ((_ ?thing1 ?thing2 ?thing3 ?things ...)
     (raise-syntax-error #f "too many operands"
                         stx))))

(define-syntax (check-property stx)
  (unless (memq (syntax-local-context) '(module top-level))
    (raise-syntax-error
     #f "`check-property' must be at top level" stx))
  (syntax-case stx ()
    ((_ ?prop)
     (stepper-syntax-property
      (check-expect-maker stx #'check-property-error #'?prop '() 
                          'comes-from-check-property)
      'stepper-replace
      #'#t))
    (_ (raise-syntax-error #f "`check-property' expects a single operand"
                           stx))))

(define (check-property-error test src-info test-info)
  (let ((info (send test-info get-info)))
    (send info add-check)
    (with-handlers ((exn:fail?
                     (lambda (e)
                       (send info property-error e src-info)
                       (raise e))))
      (call-with-values
       (lambda ()
         (with-handlers
             ((exn:assertion-violation?
               (lambda (e)
                 ;; minor kludge to produce comprehensible error message
                 (if (eq? (exn:assertion-violation-who e) 'coerce->result-generator)
                     (raise (make-exn:fail (string-append "Value must be property or boolean: "
                                                          ((error-value->string-handler)
                                                           (car (exn:assertion-violation-irritants e))
                                                           100))
                                           (exn-continuation-marks e)))
                     (raise e)))))
           (quickcheck-results (test))))
       (lambda (ntest stamps result)
         (if (check-result? result)
             (begin
               (send info property-failed result src-info)
               #f)
             #t))))))

(define (expect v1 v2)
  (quickcheck:property () (teach-equal? v1 v2)))

(define (ensure-real who n val)
  (unless (real? val)
    (raise
     (make-exn:fail:contract
      (string->immutable-string
       (format "~a argument ~e for `~a' is not a real number." n val who))
      (current-continuation-marks)))))

(define (expect-within v1 v2 epsilon)
  (ensure-real 'expect-within "Third" epsilon)
  (quickcheck:property () (beginner-equal~? v1 v2 epsilon)))

(define (expect-range val min max)
  (ensure-real 'expect-range "First" val)
  (ensure-real 'expect-range "Second" min)
  (ensure-real 'expect-range "Third" max)
  (quickcheck:property () (and (<= min val) (<= val max))))

(define (expect-member-of val . candidates)
  (quickcheck:property () (ormap (lambda (cand) (teach-equal? val cand)) candidates)))

(define Property
  (signature (predicate (lambda (x) (or (boolean? x) (property? x))))))
