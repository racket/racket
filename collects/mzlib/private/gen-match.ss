(module gen-match mzscheme
  
  (provide gen-match)
  
  (require (lib "etc.ss")
           (lib "stx.ss" "syntax")
           "match-helper.ss"
           "match-error.ss"           
           "coupling-and-binding.scm"		      
           "update-counts.scm"
           "update-binding-counts.scm"
           "render-test-list.scm"
           "reorder-tests.scm"
           "tag-negate-tests.scm"
           "convert-pat.ss")
  
  (require-for-template mzscheme
			(lib "etc.ss")
			"match-error.ss")
  
  ;;!(function mark-patlist
  ;;          (form (mark-patlist clauses) -> marked-clause-list)
  ;;          (contract list -> list))
  ;; This function takes each clause from the match expression and
  ;; pairs it with the dummy value #f.  This value will be set! when
  ;; the pattern matcher compiles a possible successful match for
  ;; the clause.  If it is not set to #t then the clause is
  ;; unreachable which is an indication of programmer error.
  (define (mark-patlist clauses)
    (syntax-map (lambda (x) (cons x #f)) clauses))
  
  ;; parse-clause : syntax -> syntax syntax maybe[syntax]
  ;; takes in a pattern
  ;; returns three values representing the pattern, the body and the failure symbol
  
  (define (parse-clause clause)
    (syntax-case clause (=>)
      [(pat) (match:syntax-err clause
                               "missing action for pattern")]
      [(pat (=> fail-sym)) 
       (match:syntax-err clause
                         "missing action for pattern")]
      [(pat (=> fail-sym) body ...)
       (values #'pat
               #'(body ...)
               #'fail-sym)]
      [(pat body ...)
       (values #'pat
               #'(body ...) 
               #f)]      
      [pat (match:syntax-err #'pat
                             "syntax error in clause")]))
  
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
  (define (test-list-with-success-func exp car-patlist stx success-func)
    (define-values (pat body fail-sym) (parse-clause (car car-patlist)))
    (define (success fail let-bound)
      (if (not success-func)
          (lambda (sf bv)
            ;; mark this pattern as reached
            (set-cdr! car-patlist #t)
            (if fail-sym
                #`(let/ec fail-cont
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
                               bv))))
                #`((lambda #,(map car bv)
                     #,@body)
                   #,@(map
                       (lambda (b) (subst-bindings
                                    (cdr b)
                                    let-bound))
                       bv))))
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
              (success-func sf bv)))))
    (define test-list (render-test-list pat exp (lambda (x) x) stx))
    (cons test-list success))
  
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
      (define (gen-help opt)
        (when (stx-null? patlist)
          (match:syntax-err stx "null clause list"))
        (let* ([marked-clauses (mark-patlist patlist)]
               [compiled-match
                #`(let ([match-failure (lambda () (match:error #,exp '#,stx))])
                    #,(gen exp tsf marked-clauses
                           stx
                           #'(match-failure)
                           opt
                           success-func))])
          (unreachable marked-clauses stx)
          compiled-match))
      
      
      
      
      
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
      (define (gen exp tsf patlist stx failure-func opt success-func)
          ;; iterate through list and render each pattern to a list of tests
          ;; and success functions
          (define rendered-list
            (map (lambda (clause) (test-list-with-success-func 
                                   exp clause stx success-func)) 
                 patlist))
          (update-counts rendered-list)
          (tag-negate-tests rendered-list)
          (update-binding-counts rendered-list)
          ((meta-couple (reorder-all-lists rendered-list)
                        (lambda (sf bv) failure-func)
                        '()
                        '())
           '() '()))
      (gen-help #f)))
  )