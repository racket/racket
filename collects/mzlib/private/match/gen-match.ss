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
           "render-helpers.ss"
           "reorder-tests.scm"
           "tag-negate-tests.scm"
	   "simplify-patterns.ss"
           "convert-pat.ss")
  
  (require-for-template mzscheme
			(lib "etc.ss")
			"match-error.ss")
  
  ;; mark-patlist : listof[x] -> listof[(cons x #f)]
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
  
  ;; test-list-with-success-func : syntax (cons syntax boolean) syntax success-func -> (cons test-list success-func)
  ;; This function takes an exp which is to be matched, a marked
  ;; clause, and a syntax-object that is for reporting errors. It
  ;; returns a pair the car of which is a list of test structs which
  ;; are in essense partially evaluated tests.  The cdr of the
  ;; result is a function which takes a failure function and a list
  ;; of let-bound expressions and returns a success-function.
  (define (test-list-with-success-func exp pat/mark stx success-func)
    (define-values (pat body fail-sym) (parse-clause (car pat/mark)))
    (define (success fail let-bound)
      (if (not success-func)
          (lambda (sf bv)
            ;; mark this pattern as reached
            (set-cdr! pat/mark #t)
            (with-syntax ([fail-var fail-sym]
                          [(bound-vars ...) (map car bv)]
                          [(args ...) (map (lambda (b) (subst-bindings (cdr b) let-bound)) bv)]
                          [body body])
              (if fail-sym
                  #`(let/ec fail-cont
                      (let ([fail-var (lambda () (fail-cont #,(fail sf bv)))]
                            [bound-vars args] ...)
                        . body))
                  #'(let ([bound-vars args] ...) . body))))
          (lambda (sf bv)
            ;; mark this pattern as reached
            (set-cdr! pat/mark #t)
            (let ((bv (map
                       (lambda (bind)
                         (cons (car bind)
                               (subst-bindings
                                (cdr bind)
                                let-bound)))
                       bv)))
              (success-func sf bv)))))
    (define test-list 
      (let* ([cert (lambda (x) x)]
	     [simplified-pat (simplify pat cert)])
	(render-test-list simplified-pat exp cert stx)))
    (cons test-list success))
  
  ;; gen-match : syntax list list syntax success-func -> syntax
  
  ;; <p>gen-match is the gateway through which match accesses the match
  ;; pattern compiler.
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
  ;; (pat (=> fail) body ...)
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
  (define/opt (gen-match exp patlist stx [success-func #f])
    (begin-with-definitions
      (when (stx-null? patlist)
        (match:syntax-err stx "null clause list"))
      ;; We set up the list of
      ;; clauses so that one can mark that they have been "reached".
      (define marked-clauses (mark-patlist patlist))
      (define failure-func #'(match-failure))
      ;; iterate through list and render each pattern to a list of partially compiled tests
      ;; and success functions.
      ;; These are partially compiled
      ;; because the test structures containa a function that needs to
      ;; be coupled with the other functions of the other test
      ;; structures before actual compilation results. 
      (define rendered-list (map (lambda (clause) (test-list-with-success-func 
                                                   exp clause stx success-func)) 
                                 marked-clauses))
      (update-counts rendered-list)
      (tag-negate-tests rendered-list)
      (update-binding-counts rendered-list)
      ;; couple the partially compiled tests together into the final result.
      (define compiled-exp 
        ((meta-couple (reorder-all-lists rendered-list)
                      (lambda (sf bv) failure-func)
                      '()
                      '())
         '() '()))
      ;; Also wrap the final compilation in syntax which binds the
      ;; match-failure function.
      (define compiled-match
        #`(let ([match-failure (lambda () #,(quasisyntax/loc stx (match:error #,exp)))])
            #,compiled-exp))
      (unreachable marked-clauses stx)
      compiled-match))
  )