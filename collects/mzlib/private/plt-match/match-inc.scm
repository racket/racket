;; This library is usedby match.ss and plt-match.ss

;;! (function match:syntax-err
;;          (form (match:syntax-err object message . detail) -> void)
;;          (contract (any string . any) -> void)
;;          (example (match:syntax-err (syntax here) "Bad error" (vector))
;;                   -> void)
;;          (contract object -> (normally a syntax object that
;;                               that helps determine the source location
;;                               of the error)))
;; This function is used to report malformed match expressions.
(define match:syntax-err (lambda (obj msg . detail)
                           (apply
                            raise-syntax-error
                            'match
                            msg
                            obj
                            detail)))

;;! (function pattern-var?
;;    (form (pattern-var? pattern-element) -> bool)
;;    (contract any -> bool)
;;    (example (pattern-var? 'x) -> t)
;;    )
;; This function takes an object and determines if it
;; qualifies as a pattern variable.
(define pattern-var?
  (lambda (x)
    (and (symbol? x)
         (not (dot-dot-k? x))
         (not (memq x
                    '(
                      _
                      quasiquote
                      quote
                      unquote
                      unquote-splicing
;                      hash-table
;                      list-no-order
;                       list-rest
;                       list
;                       app
;                       struct
;                       var
;                       vector
;                       box
;                       ?
;                       and
;                       or
;                       not
;                       set!
;                       get!
                      ))))))

;;!(function dot-dot-k?
;;          (form (dot-dot-k? s) -> bool)
;;          (contract any -> bool)
;;          (example (stx-dot-dot-k? '..3) -> #t))
;; This function is a predicate that returns true if the argument
;; is a symbol '... or '___ where the last dot or
;; underscore can be an integer
(define dot-dot-k? (lambda (s)
                     (and (symbol? s)
                          (if (memq s '(... ___))
                              0
                              (let* ((s (symbol->string s))
                                     (n (string-length s)))
                                (and (<= 3 n)
                                     (memq (string-ref s 0)
                                           '(#\. #\_))
                                     (memq (string-ref s 1)
                                           '(#\. #\_))
                                     (andmap
                                      char-numeric?
                                      (string->list
                                       (substring s 2 n)))
                                     (string->number
                                      (substring s 2 n))))))))

;;!(function gen-match
;;          (form (gen-match exp tsf patlist stx [success-func])
;;                ->
;;                compiled-pattern)
;;          (contract (syntax-object list list syntax-object
;;                                   (list list -> syntax-object))
;;                    ->
;;                    syntax-object))
;; <p>gen-match is the gateway through which match, match-lambda,
;; match-lambda*,
;; match-let, match-let*, match-letrec, match-define access the match
;; expression compiler.
;;
;; <p>exp - the expression that is to be tested against the pattern.
;; This should normally be a piece of syntax that indirectly
;; represents the expression.  Because if it is the syntax of the
;; expression itself it will be duplicated many times throughout
;; the generated match test.
;;
;; <p>tsf - is a list of tests-seen-so-far and is used to
;; prevent generating tests for the same condition twice
;;
;; <p>patlist - is a list of the pattern clauses of the match expr
;; these can be of either form (pat body ...) or
;; (pat (=> fail) body ...)x
;;
;; <p>stx is the original syntax of the match expression.
;; This is only used for error reporting.
;;
;; <p>success-func - an optional argument which allows one to
;; specify how a successful match is treated.  This made
;; the creation of match-letrec and match-define macros simple.
;; The reason for this function is that most of the information
;; about a match (namely the bound match variables) is at the bottom
;; of the recursion tree. The success function must take two arguments
;; and it should return a syntax object.
(define gen-match
  (opt-lambda (exp tsf patlist stx [success-func #f])
    (include "test-structure.scm")
    ;(include "coupling-and-binding-new.scm")
    (include "coupling-and-binding.scm")
    (include "render-test-list.scm")
    (include "reorder-tests.scm")
    (include "update-counts.scm")
    (include "update-binding-counts.scm")
    (include "match-util.scm")
    (include "tag-negate-tests.scm")
    ;;!(function unreachable
    ;;          (form (unreachable plist match-expr) -> void)
    ;;          (contract (list syntax-object) -> void)
    ;;          (contract plist -> (is a list of unreached pattern clauses))
    ;;          (contract match-expr -> (is the origional match expr
    ;;                                   the clauses came from)))
    ;; This function takes a list of unreached clauses and the original
    ;; match expression and prints a warning for each of the unreached
    ;; match clauses to the current error port
    (define unreachable
      (lambda (plist match-expr)
        (map
         (lambda (x)
           (if (not (cdr x))
               (fprintf
                (current-error-port)
                "Warning: unreachable match clause ~e in ~e~n"
                (syntax-object->datum (car x))
                (syntax-object->datum match-expr))))
         plist)))

    ;;!(function gen-match-opt
    ;;          (form (gen-match exp tsf patlist stx [success-func])
    ;;                ->
    ;;                compiled-pattern)
    ;;          (contract (syntax-object list list syntax-object
    ;;                                   (list list -> syntax-object))
    ;;                    ->
    ;;                    syntax-object))
    ;; This function is left over from an experiment that explored the
    ;; idea that certain "shape" tests can be ommited if the input for
    ;; a match expression is known.
    ;; For example if one knows that the the match expression is only
    ;; ever going to be applied to a list of four items.  Then it
    ;; would behoove us to eliminate the extraneous tests that verify
    ;; this.
    (define gen-match-opt
      (opt-lambda (exp tsf patlist stx [success-func #f])
        (gen-help exp tsf patlist stx #t success-func)))

    ;;!(function gen-help
    ;;          (form (gen-help exp tsf patlist stx [success-func]) ->
    ;;                          syntax-object)
    ;;          (contract (syntax-object list list syntax-object
    ;;                                   (list list -> syntax-object))
    ;;                    ->
    ;;                    syntax-object))
    ;; This function does some basic house keeping before forwarding
    ;; the compilation to the gen function.  It sets up the list of
    ;; clauses so that one can mark that they have been "reached".  It
    ;; also wraps the final compilation in syntax which binds the
    ;; match-failure function.
    (define gen-help
      (opt-lambda (exp tsf patlist stx opt [success-func #f])
        (when
            (stx-null? patlist)
          (match:syntax-err stx "null clause list"))
        (let* ((marked-clauses (mark-patlist patlist))
               (compiled-match
                (quasisyntax/loc stx
                                 (let ((match-failure
                                        (lambda ()
                                          (match:error #,exp (quote #,stx)))))
                                   #,(gen exp tsf marked-clauses
                                          stx
                                          (syntax (match-failure))
                                          opt
                                          success-func)))))
          (unreachable marked-clauses stx)
          compiled-match)))

    ;;!(function mark-patlist
    ;;          (form (mark-patlist clauses) -> marked-clause-list)
    ;;          (contract list -> list))
    ;; This function takes each clause from the match expression and
    ;; pairs it with the dummy value #f.  This value will be set! when
    ;; the pattern matcher compiles a possible successful match for
    ;; the clause.  If it is not set to #t then the clause is
    ;; unreachable which is an indication of programmer error.
    (define mark-patlist
      (lambda (clauses)
        (map (lambda (x) (cons x #f)) (syntax->list clauses))))

    ;;!(function test-list-with-success-func
    ;;          (form (test-list-with-success-func exp car-patlist
    ;;                         stx success-func)
    ;;                ->
    ;;                (test-list success-func))
    ;;          (contract (syntax-object pair syntax-object
    ;;                                   (list list -> syntax-object))
    ;;                    ->
    ;;                    (list ((list list -> syntax) list ->
    ;;                           (list list -> syntax)))))
    ;; This function takes an exp which is to be matched, a marked
    ;; clause, and a syntax-object that is fro reporting errors. It
    ;; returns a pair the car of which is a list of test structs which
    ;; are in essense partially evaluated tests.  The cdr of the
    ;; result is a function which takes a failure function and a list
    ;; of let-bound expressions and returns a success-function.
    (define test-list-with-success-func
      (opt-lambda (exp car-patlist stx [success-func #f])
        (let ((clause1 (car car-patlist)))
          (let-values (((pat body fail-sym)
                        (syntax-case clause1 (=>)
                          ((pat (=> fail-sym) body1 bodys ...)
                           (values (syntax pat)
                                   (syntax (body1 bodys ...))
                                   (syntax fail-sym)))
                          ((pat body1 bodys ...)
                           (values (syntax pat)
                                   (syntax (body1 bodys ...)) #f))
                          ((pat) (match:syntax-err
                                  (syntax pat)
                                  "missing action for pattern"))
                          (pat (match:syntax-err
                                (syntax pat)
                                "syntax error in clause")))))
            (let* (
                   (success
                    (lambda (fail let-bound)
                      (if (not success-func)
                          (lambda (sf bv)
                            ;; mark this pattern as reached
                            (set-cdr! car-patlist #t)
                            (if fail-sym
                                (quasisyntax/loc
                                 stx
                                 (call/ec
                                  (lambda (fail-cont)
                                    (let
                                        ((failure
                                          (lambda ()
                                            (fail-cont
                                        ; it seems like fail is called
                                        ; twice in this situation
                                             #,( fail sf bv)))))
                                      ((lambda (#,fail-sym
                                                #,@(map car bv))
                                         #,@body)
                                       failure
                                       #,@(map (lambda (b)
                                                 (subst-bindings
                                                  (cdr b)
                                                  let-bound))
                                               bv))))))
                                (quasisyntax/loc
                                 stx
                                 ((lambda #,(map car bv)
                                    #,@body)
                                  #,@(map
                                      (lambda (b) (subst-bindings
                                                   (cdr b)
                                                   let-bound))
                                      bv)))))
                          (lambda (sf bv)
                            ;; mark this pattern as reached
                            (set-cdr! car-patlist #t)
                            (let ((bv (map
                                       (lambda (bind)
                                         (cons (car bind)
                                               (subst-bindings
                                                (cdr bind)
                                                let-bound)))
                                       bv)))
                              (success-func sf bv))))))
                   (test-list (render-test-list pat exp stx)))
              (cons test-list success))))))

    ;;!(function gen
    ;;          (form (gen exp tsf patlist stx failure-func opt success-func)
    ;;                ->
    ;;                syntax)
    ;;          (contract (syntax list list syntax
    ;;                     (() -> void) bool (list list -> syntax))
    ;;                    ->
    ;;                    syntax))
    ;; This function is primarily called by gen-help and takes the the
    ;; newly marked clauses and the failure-func which is really a
    ;; variable-name which will bound to the failure in the runtime
    ;; code.  This function then makes successive calls to
    ;; test-list-with-success-func which gives us a list of partially
    ;; compiled tests for each clause.  I say partially compiled
    ;; because the test structures containa a function that needs to
    ;; be coupled with the other functions of the other test
    ;; structures before actual compilation results.  This function
    ;; then takes these lists of partially compiled tests and reorders
    ;; them in an attempt to reduce the size of the final compiled
    ;; match expression.  Binding counts are also updated to help
    ;; determind which supexpressions of the expression to be matched
    ;; need to be bound by let expressions.  After all of this the
    ;; tests are "coupled" together for final compilation.
    (define gen
      (opt-lambda (exp tsf patlist stx failure-func opt [success-func #f])
        ;; iterate through list and render each pattern to a list of tests
        ;; and success functions
        (let ((rendered-list
               (let loop ((clause-list patlist))
                 (if (null? clause-list)
                     '()
                     (cons (test-list-with-success-func exp
                                                        (car clause-list)
                                                        stx
                                                        success-func)
                           (loop (cdr clause-list)))))))
          (update-counts rendered-list)
          (tag-negate-tests rendered-list)
          (update-binding-counts rendered-list)
          (let* ((rendered-list (reorder-all-lists rendered-list))
                 (output
                  (begin 
                    ;(pretty-print rendered-list)(newline)      
                    ((meta-couple rendered-list
                                  (lambda (sf bv) failure-func)
                                  '()
                                  '())
                     '() '()))))
            output))))
    (gen-help exp tsf patlist stx #f success-func)))

