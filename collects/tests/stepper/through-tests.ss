#lang scheme/base

  (require (for-syntax scheme/base)
           (for-syntax scheme/mpair)
           scheme/match
           stepper/private/shared
           stepper/private/model
           stepper/private/model-settings
           tests/utils/sexp-diff
           lang/run-teaching-program
           (only-in srfi/13 string-contains)
           ;; for xml testing:
           ;; mzlib/class
           ;; (all-except xml/xml-snipclass snip-class)
           ;; (all-except xml/scheme-snipclass snip-class)
           ;; mred
           #;(file "/Users/clements/clements/scheme-scraps/eli-debug.ss")
           )
  
  (provide (all-defined-out))

  (define test-directory (find-system-path 'temp-dir))
  
  (define display-only-errors (make-parameter #f))
  
  (define error-has-occurred-box (make-parameter #f))
  
  (define show-all-steps (make-parameter #f))

  (define disable-stepper-error-handling (make-parameter #f))
  
  (define (stream-ify stx-thunk iter)
    (lambda ()
      (let* ([next (stx-thunk)]
             [followup-thunk (if (eof-object? next) void (stream-ify stx-thunk iter))])
        (iter (expand next) followup-thunk))))

  (define (warn who fmt . args)
    (set-box! (error-has-occurred-box) #t)
    (fprintf (current-error-port) "~a: ~a\n" who (apply format fmt args)))

  (define (test-sequence-core namespace-spec teachpack-specs render-settings
                              show-lambdas-as-lambdas? enable-testing? in-port expected-steps)
    (let* ([current-error-display-handler (error-display-handler)]
           [all-steps
            (append expected-steps '((finished-stepping)))]
           [receive-result
            (lambda (result)
              (if (null? all-steps)
                (warn 'test-sequence
                      "ran out of expected steps. Given result: ~v" result)
                (begin
                  (if (compare-steps result (car all-steps))
                      (when (and (show-all-steps) (not (display-only-errors)))
                        (printf "test-sequence: steps match for expected result: ~v\n"
                                (car all-steps)))
                      (warn 'test-sequence
                          "steps do not match\n   given: ~v\nexpected: ~v"
                          (show-result result) (car all-steps)))
                  (set! all-steps (cdr all-steps)))))]
           [program-expander
            (let ([module-id (gensym "stepper-module-name-")])
            (lambda (init iter)
              (init)
              ((stream-ify (expand-teaching-program in-port read-syntax namespace-spec teachpack-specs #f module-id enable-testing?) iter))))])
      (let/ec escape
        (parameterize ([error-escape-handler (lambda () (escape (void)))])
          (go program-expander receive-result render-settings
              show-lambdas-as-lambdas?
              ;; language level:
              'testing
              ;; run-in-drscheme thunk:
              (lambda (thunk) (thunk))
              (disable-stepper-error-handling))))
      (error-display-handler current-error-display-handler)))

  (define (test-sequence namespace-spec teachpack-specs render-settings
                         show-lambdas-as-lambdas? enable-testing? exp-str expected-steps)
    (let ([filename (build-path test-directory "stepper-test")])
      (call-with-output-file filename
        (lambda (port) (fprintf port "~a" exp-str))
        #:exists
        'truncate)
      (unless (display-only-errors)
        (printf "testing string: ~v\n" exp-str))
      (letrec ([port (open-input-file filename)])
        (test-sequence-core namespace-spec teachpack-specs render-settings
                            show-lambdas-as-lambdas? enable-testing? port expected-steps))))

  (define (lang-level-test-sequence namespace-spec rs show-lambdas-as-lambdas? enable-testing?)
    (lambda args
      (apply test-sequence namespace-spec `() rs show-lambdas-as-lambdas? enable-testing? args)))

  (define (make-multi-level-test-sequence level-fns)
    (lambda args
      (for-each (lambda (fn) (apply fn args)) level-fns)))

  (define test-mz-sequence
    (lang-level-test-sequence 'mzscheme fake-mz-render-settings #t #f))
  (define test-beginner-sequence
    (lang-level-test-sequence `(lib "htdp-beginner.ss" "lang")
                              fake-beginner-render-settings #f #t))
  (define test-beginner-wla-sequence
    (lang-level-test-sequence `(lib "htdp-beginner-abbr.ss" "lang")
                              fake-beginner-wla-render-settings #f #t))
  (define test-intermediate-sequence
    (lang-level-test-sequence `(lib "htdp-intermediate.ss" "lang")
                              fake-intermediate-render-settings #f #t))
  (define test-intermediate/lambda-sequence
    (lang-level-test-sequence `(lib "htdp-intermediate-lambda.ss" "lang")
                              fake-intermediate/lambda-render-settings #t #t))
  (define test-advanced-sequence
    (lang-level-test-sequence `(lib "htdp-advanced.ss" "lang")
                              fake-advanced-render-settings #t #t))

  (define test-upto-int/lam
    (make-multi-level-test-sequence
     (list test-beginner-sequence
           test-beginner-wla-sequence
           test-intermediate-sequence
           test-intermediate/lambda-sequence)))

  (define test-upto-int
    (make-multi-level-test-sequence
     (list test-beginner-sequence
           test-beginner-wla-sequence
           test-intermediate-sequence)))

  (define test-bwla-to-int/lam
    (make-multi-level-test-sequence
     (list test-beginner-wla-sequence
           test-intermediate-sequence
           test-intermediate/lambda-sequence)))

  (define test-both-ints
    (make-multi-level-test-sequence
     (list test-intermediate-sequence
           test-intermediate/lambda-sequence)))

  (define test-lazy-sequence
    (lang-level-test-sequence `(lib "lazy.ss" "lazy")
                              fake-mz-render-settings #f #f))

  ;; mutate these to values you want to examine in the repl:
  (define bell-jar-specimen-1 #f)
  (define bell-jar-specimen-2 #f)

  ;; so->d/finished : call (syntax-object->hilite-datum stx #t).  For finished
  ;; steps, we want to ignore the highlight but not the xml boxes (and other
  ;; future stuff?)
  (define (so->d/finished stx)
    (syntax-object->hilite-datum stx #t))

  ;; (-> step-result? sexp? boolean?)
  (define (compare-steps actual expected)
    (match expected
      [`(before-after ,before ,after)
       (and (before-after-result? actual)
            (andmap (lambda (fn expected name)
                      (unless (list? (fn actual))
                        (warn 'compare-steps "not a list: ~v"
                              (syntax-object->hilite-datum (fn actual))))
                      (noisy-equal? (map syntax-object->hilite-datum
                                         (fn actual))
                                    expected
                                    name))
                    (list before-after-result-pre-exps
                          before-after-result-post-exps)
                    (list before after)
                    (list 'before 'after)))]
      [`(error ,err-msg)
       (and (error-result? actual)
            (string-contains (error-result-err-msg actual) err-msg))]
      [`(before-error ,before ,err-msg)
       (and (before-error-result? actual)
            (and (noisy-equal? (map syntax-object->hilite-datum
                                    (before-error-result-pre-exps actual))
                               before
                               'before)
                 (equal? err-msg (before-error-result-err-msg actual))))]
      [`(finished-stepping) (finished-stepping? actual)]
      [`(ignore) (warn 'compare-steps "ignoring one step") #t]
      [else (begin (warn 'compare-steps
                         "unexpected expected step type: ~v" expected)
                   #f)]))
  
  ;; used to display results in an error message
  (define (show-result r)
    (if (before-after-result? r)
        (list 'before-after-result
              (map (lambda (fn)
                     (unless (list? (fn r))
                       (warn 'show-result "not a list: ~v"
                             (syntax-object->hilite-datum (fn r))))
                     (map syntax-object->hilite-datum
                          (fn r)))
             (list before-after-result-pre-exps
                   before-after-result-post-exps)))
        r))

  ;; noisy-equal? : (any any . -> . boolean)
  ;; like equal?, but prints a noisy error message
  (define (noisy-equal? actual expected name)
    (if (equal? actual expected)
      #t
      (begin (warn 'not-equal?
                   "~e:\nactual:   ~e =/= \nexpected: ~e\n  here's the diff: ~e" name actual expected (sexp-diff actual expected))
             #f)))

  ;; (-> (listof sexp) (listof sexp) boolean?)
  (define (compare-finished finished-exps expected-exps)
    (and (>= (length finished-exps) (length expected-exps))
         (andmap (lambda (x y)
                   (if (equal? x y)
                     #t
                     (begin
                       (warn 'not-equal?
                                "~e =/= ~e\n  here's the diff: ~e"
                                x y (sexp-diff x y))
                       #f)))
                 (list-tail finished-exps
                            (- (length finished-exps) (length expected-exps)))
                 expected-exps)))

  ;; (-> (listof sexp) string?)
  (define (exprs->string exprs)
    (apply string-append
           (cdr (apply append (map (lambda (x) (list " " (format "~s" x)))
                                   exprs)))))

  (define list-of-tests null)

  (define (add-test name thunk)
    (when (assq name list-of-tests)
      (error 'add-test "name ~v is already in the list of tests" name))
    (set! list-of-tests (append list-of-tests (list (list name thunk)))))

  (define-syntax (t1 stx)
    (syntax-case stx ()
      [(_ name test)
       (syntax/loc stx (add-test `name (lambda () test)))]))
  
  ;; Eli can't help adding his own convenient but complex syntax here (JBC, 2006-11-14):

  (define-syntax (t stx)
    (define (maybe-mlist->list r)
      (if (mpair? r) 
          (mlist->list r)
          r))
    (define (split l)
      (let loop ([l l] [r '()])
        (cond [(null? l) (reverse (map maybe-mlist->list r))]
              [(symbol? (car l)) (loop (cdr l) (cons (car l) r))]
              [(or (null? r) (not (mpair? (car r))))
               (loop (cdr l) (cons (mlist (car l)) r))]
              [else (mappend! (car r) (mlist (car l))) 
                    (loop (cdr l) r)])))
    (define (process-hilites s)
      (syntax-case s ()
        [(x) (eq? #\{ (syntax-property s 'paren-shape))
         (with-syntax ([x (process-hilites #'x)]) #'(hilite x))]
        [(x . y) (let* ([x0 #'x]
                        [y0 #'y]
                        [x1 (process-hilites #'x)]
                        [y1 (process-hilites #'y)])
                   (if (and (eq? x0 x1) (eq? y0 y1))
                     s
                     (with-syntax ([x x1] [y y1]) #'(x . y))))]
        [_else s]))
    (define (process stx)
      (split (map (lambda (s)
                    (if (and (identifier? s)
                             (memq (syntax-e s) '(:: -> error:)))
                      (syntax-e s)
                      (process-hilites s)))
                  (syntax->list stx))))
    (define (parse l)
      (syntax-case l (::)
        [(fst :: rest ...)
         (cons #'fst
               (let loop ([rest #'(rest ...)])
                 (syntax-case rest (:: -> error:)
                   [(error: (err)) (list #'(error err))]
                   [() (list #'(finished-stepping))]
                   [(x -> y) (list #'(before-after x y) #'(finished-stepping))]
                   [(x -> error: (err)) (list #'(before-error x err))]
                   [(x -> y :: . rest)
                    (cons #'(before-after x y) (loop #'rest))]
                   [(x -> y -> . rest)
                    (cons #'(before-after x y) (loop #'(y -> . rest)))])))]))
    (syntax-case stx (::)
      [(_ name tester . rest)
       (with-syntax ([(exprs arg ...) (parse (process #'rest))])
         (quasisyntax/loc stx
           (add-test `name
             (lambda ()
               (tester
                ;printf "exprs = ~s\n args = ~s\n"
                (exprs->string `exprs) `(arg ...))))))]))
  
  ;; run a test : (list symbol test-thunk) -> boolean
  ;; run the named test, return #t if a failure occurred during the test
  (define (run-one-test test-pair)
    (unless (display-only-errors)
      (printf "running test: ~v\n" (car test-pair)))
    (parameterize ([error-has-occurred-box (box #f)])
      ((cadr test-pair))
      (if (unbox (error-has-occurred-box))
          (begin (fprintf (current-error-port) "...Error has occurred during test: ~v\n" (car test-pair))
                 #t)
          #f)))

  (define (run-all-tests)
    (ormap/no-shortcut 
     run-one-test
     list-of-tests))
  
  (define (run-all-tests-except nix-list)
    (ormap/no-shortcut 
     run-one-test
     (filter (lambda (pr) (not (member (car pr) nix-list)))
             list-of-tests)))

  (define (run-test name)
    (let ([maybe-test (assq name list-of-tests)])
      (if maybe-test
          (run-one-test maybe-test)
          (error 'run-test "test not found: ~e" name))))

  (define (run-tests names)
    (ormap/no-shortcut run-test names))
  
  
  ;; like an ormap, but without short-cutting
  (define (ormap/no-shortcut f args)
    (foldl (lambda (a b) (or a b)) #f (map f args)))

  (t mz1 test-mz-sequence
     (for-each (lambda (x) x) '(1 2 3))
     :: {(for-each (lambda (x) x) `(1 2 3))} -> (... {1} ...)
     :: ... -> (... {2} ...)
     :: ... -> (... {3} ...)
     :: ... -> {(void)})

  ;; new test case language:
  ;; an expected is (listof step)
  ;; a step is one of
  ;; (before-after exps exps)
  ;; (before-error exps str)
  ;; (error str)
  ;; (finished)
  ;; an exps is a list of s-expressions with certain non-hygienic extensions:
  ;;  - (hilite X) denotes the s-expression X, only highlighted
  ;;  - any        denotes any s-expression (matches everything)
  ;;  ... in principle, these could collide with programs that use the
  ;;      identifiers 'hilite' and 'any', but since I'm writing the test cases,
  ;;      I can alpha-rename manually to avoid collisions.

  ;; on top of this, the `t' macro makes things easier to write, informally:
  ;; (t name ; symbolic name for the test
  ;;    tester ; tester function that gets used
  ;;    expr1 ... :: expr2 ... -> expr3 ...)
  ;; means that `expr1 ...' is the original, the first step is
  ;;   (before-after (expr2 ...) (expr3 ...))
  ;; Cute stuff:
  ;; * use `::' to mark a new step that doesn't continue the previous one
  ;;     e1 :: e2 -> e3 -> e4
  ;;   is the same as
  ;;     e1 :: e2 -> e3 :: e3 -> e4
  ;; * use `-> error: "..."' for a `before-error' step
  ;; * use `:: error: "..."' for an `error' step
  ;; * a `finished-stepping' is added if no error was specified
  ;; * a `{...}' is replaced with `(hilite ...)'

  (t mz-app test-mz-sequence
     (+ 3 4)
     :: {(+ 3 4)} -> {7})

  (t mz-app2 test-mz-sequence
     ((lambda (x) (+ x 3)) 4)
     :: {((lambda (x) (+ x 3)) 4)} -> {(+ 4 3)} -> {7})

  (t mz-if test-mz-sequence
     (if 3 4 5)
     :: {(if 3 4 5)} -> {4})

  (t simple-if test-upto-int/lam
     (if true false true)
     :: {(if true false true)} -> {false})

  (t if-bool test-upto-int/lam
     (if (if true false true) false true)
     :: (if {(if true false true)} false true) -> (if {false} false true)
     :: {(if false false true)} -> {true})

  (t direct-app test-mz-sequence
     ((lambda (x) x) 3)
     :: {((lambda (x) x) 3)} -> {3})

;   (test-mz-sequence "((lambda (x) x) (begin (+ 3 4) (+ 4 5)))"
;		      `((before-after ((begin (hilite (+ 3 4)) (+ 4 5)))
;				      ((begin (hilite 7) (+ 4 5))))
;			(before-after ((hilite (begin 7 (+ 4 5)))) ((hilite (+ 4 5))))
;                        (before-after ((hilite (+ 4 5))) ((hilite 9)))
;			(finished-stepping)))

  (t curried test-mz-sequence
     ((lambda (a) (lambda (b) (+ a b))) 14)
     :: {((lambda (a) (lambda (b) (+ a b))) 14)}
     -> {(lambda (b) (+ 14 b))})

  (t case-lambda test-mz-sequence
     ((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)
     :: {((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)}
     -> {(+ 5 6)}
     -> {11})

  ;; not really a part of base mzscheme anymore
  #;(t 2armed-if test-mz-sequence
     (if 3 4)
     :: {(if 3 4)} -> {4})

  ;(test-mz-sequence "((call-with-current-continuation call-with-current-continuation) (call-with-current-continuation call-with-current-continuation))"
  ;                  `((before-after (((hilite ,h-p) (call-with-current-continuation call-with-current-continuation))) ((call-with-current-continuation call-with-current-continuation))
  ;                    (((hilite ,h-p) (call-with-current-continuation call-with-current-continuation))) ((lambda args ...)))
  ;                    (before-after (((lambda args ...) (hilite ,h-p))) ((call-with-current-continuation call-with-current-continuation))
  ;                    (((lambda args ...) (hilite ,h-p))) ((lambda args ...)))))

  ;(test-mz-sequence '(begin (define g 3) g)
  ;                  `((before-after ((hilite ,h-p)) (g)
  ;                    ((hilite ,h-p)) 3)))

  ;(syntax-object->datum (cadr (annotate-expr test2 'mzscheme 0 (lambda (x) x))))

  (t top-def test-upto-int/lam
     (define a (+ 3 4))
     :: (define a {(+ 3 4)})
     -> (define a {7}))

  (t top-def-ref test-upto-int/lam
     (define a 6) a
     :: (define a 6) {a} -> (define a 6) {6})

  (t app test-upto-int/lam
     (+ 4 129)
     :: {(+ 4 129)} -> {133})

  (t if test-upto-int/lam (if true 3 4)
     :: {(if true 3 4)} -> {3})

  (let ([def `(define (a3 x) (if true x x))])
    (t top-app test-upto-int
       ,def (a3 false)
       :: ,def {(a3 false)}
       -> ,def {(if true false false)}
       -> ,def {false})
    ;;
    (t top-app/lam test-intermediate/lambda-sequence
       ,def (a3 false)
       :: ,def ({a3} false)
       -> ,def ({(lambda (x) (if true x x))} false)
       :: ,def {((lambda (x) (if true x x)) false)}
       -> ,def {(if true false false)}
       -> ,def {false}))

  (let ([defs `((define (a12 x) (+ x 9)) (define b12 a12))])
    (t top-interref test-intermediate-sequence
       ,@defs (b12 12)
       :: ,@defs ({b12} 12)
       -> ,@defs ({a12} 12)
       :: ,@defs {(a12 12)}
       -> ,@defs {(+ 12 9)}
       -> ,@defs {21}))

  ;;;;;;;;;;;;
  ;;
  ;;  OR / AND
  ;;
  ;;;;;;;;;;;;;.

  (t or1 test-upto-int/lam
     (or false true false)
     :: {(or false true false)} -> {true})

  (t and1 test-upto-int/lam
     (and true false true)
     :: {(and true false true)} -> {false})

  (t and2 test-upto-int/lam
     (and true (if true true false))
     :: (and true {(if true true false)}) -> (and true {true})
     :: {(and true true)} -> {true})

  (let ([def `(define (b2 x) (and true x))])
    (t and3 test-upto-int
       ,def (b2 false)
       :: ,def {(b2 false)}
       -> ,def {(and true false)}
       -> ,def {false})
    ;;
    (t and3/lam test-intermediate/lambda-sequence
       (define (b2 x) (and true x)) (b2 false)
       :: ,def ({b2} false)
       -> ,def ({(lambda (x) (and true x))} false)
       :: ,def {((lambda (x) (and true x)) false)}
       -> ,def {(and true false)}
       -> ,def {false}))

  (let ([defs `((define a1 true)
                (define (b1 x) (and a1 true x)))])
    (t and4 test-upto-int
       ,@defs (b1 false)
       :: ,@defs {(b1 false)}
       -> ,@defs {(and a1 true false)}
       :: ,@defs (and {a1} true false)
       -> ,@defs (and {true} true false)
       :: ,@defs {(and true true false)}
       -> ,@defs {false})
    ;;
    (t and4/lam test-intermediate/lambda-sequence
       ,@defs (b1 false)
       :: ,@defs ({b1} false)
       -> ,@defs ({(lambda (x) (and a1 true x))} false)
       :: ,@defs {((lambda (x) (and a1 true x)) false)}
       -> ,@defs {(and a1 true false)}
       :: ,@defs (and {a1} true false)
       -> ,@defs (and {true} true false)
       :: ,@defs {(and true true false)}
       -> ,@defs {false}))

  (t bad-and test-upto-int/lam
     (and true 1)
     :: {(and true 1)}
     -> error: "and: question result is not true or false: 1")

  ;;;;;;;;;;;;;
  ;;
  ;;  COND
  ;;
  ;;;;;;;;;;;;;

  (t cond1 test-upto-int/lam
     (cond [false 4] [false 5] [true 3])
     :: {(cond (false 4) (false 5) (true 3))}
     -> {(cond (false 5) (true 3))}
     -> {(cond (true 3))}
     -> {3})

  (t cond-else test-upto-int/lam
     (cond [false 4] [else 9])
     :: {(cond [false 4] [else 9])}
     -> {(cond [else 9])}
     -> {9})

  (t cond-andelse test-upto-int/lam
     (cond [true 3] [else (and true true)])
     :: {(cond (true 3) (else (and true true)))} -> {3})

  (t bad-cond test-upto-int/lam
     (cond)
     :: error: "cond: expected a question--answer clause after `cond', but nothing's there")

  (t just-else test-upto-int/lam
     (cond [else 3])
     :: {(cond (else 3))} -> {3})

  (t nested-cond test-upto-int/lam
     (cond [else (cond [else 3])])
     :: {(cond (else (cond (else 3))))}
     -> {(cond (else 3))}
     -> {3})

  ;;  reconstruct can't handle 'begin'
  #;
  (test-mz-sequence "(cond [#f 3 4] [#t (+ 3 4) (+ 4 9)])"
    `((before-after ((hilite (cond (#f 3 4) (#t (+ 3 4) (+ 4 9)))))
                    ((hilite (cond (#t (+ 3 4) (+ 4 9))))))
      (before-after ((hilite (cond (#t (+ 3 4) (+ 4 9)))))
                    ((hilite (begin (+ 3 4) (+ 4 9)))))
      (before-after ((begin (hilite (+ 3 4)) (+ 4 9)))
                    ((begin (hilite 7) (+ 4 9))))
      (before-after ((hilite (begin 7 (+ 4 9))))
                    ((hilite (+ 4 9))))
      (before-after ((hilite (+ 4 9)))
                    ((hilite 13)))
      (finished-stepping)))

  (t nested-cond2 test-upto-int/lam
     (cond [false 3] [else (cond [true 4])])
     :: {(cond (false 3) (else (cond (true 4))))}
     -> {(cond (else (cond (true 4))))}
     -> {(cond (true 4))}
     -> {4})

  (t top-ref test-intermediate-sequence
     (define a4 +) a4
     :: (define a4 +) {a4}
     -> (define a4 +) {+})

  (t top-ref2 test-intermediate-sequence
     (define (f123 x) (+ x 13)) f123
     ::)

  (t top-ref3 test-intermediate/lambda-sequence
     (define (f123 x) (+ x 13)) f123
     :: (define (f123 x) (+ x 13)) {f123}
     -> (define (f123 x) (+ x 13)) {(lambda (x) (+ x 13))})

  (let* ([defs1 `((define (a x) (+ x 5)) (define b a))]
         [defs2 (append defs1 `((define c a)))])
    (t top-ref4 test-intermediate-sequence
       ,@defs1 (define c b) (c 3)
       :: ,@defs1 (define c {b})
       -> ,@defs1 (define c {a})
       :: ,@defs2 ({c} 3)
       -> ,@defs2 ({a} 3)
       :: ,@defs2 {(a 3)}
       -> ,@defs2 {(+ 3 5)}
       -> ,@defs2 {8}))

  (t define-struct test-upto-int/lam
     (define-struct mamba (rhythm tempo)) (mamba-rhythm (make-mamba 24 2))
     :: (define-struct mamba (rhythm tempo)) {(mamba-rhythm (make-mamba 24 2))}
     -> (define-struct mamba (rhythm tempo)) {24})

  (let ([def `(define a5 (lambda (a5) (+ a5 13)))])
    (t lam-def test-upto-int
       ,def (a5 23)
       :: ,def {(a5 23)}
       -> ,def {(+ 23 13)}
       -> ,def {36}))

  (let ([def `(define a5 (lambda (a5) (+ a5 13)))])
    (t lam-def/lam test-intermediate/lambda-sequence
       ,def (a5 23)
       :: ,def ({a5} 23)
       -> ,def ({(lambda (a5) (+ a5 13))} 23)
       :: ,def {((lambda (a5) (+ a5 13)) 23)}
       -> ,def {(+ 23 13)}
       -> ,def {36}))

  (let ([def `(define a_0 (lambda (x) (+ x 5)))])
    (t lam-let test-intermediate-sequence
       (let ([a (lambda (x) (+ x 5))]) (a 6))
       :: {(let ([a (lambda (x) (+ x 5))]) (a 6))}
       -> {(define a_0 (lambda (x) (+ x 5)))} {(a_0 6)}
       :: ,def {(a_0 6)}
       -> ,def {(+ 6 5)}
       -> ,def {11}))

  (let ([defs `((define c1 false)
                (define (d2 x) (or c1 false x)))])
    (t whocares test-upto-int
       ,@defs (d2 false)
       :: ,@defs {(d2 false)}
       -> ,@defs {(or c1 false false)}
       :: ,@defs (or {c1} false false)
       -> ,@defs (or {false} false false)
       :: ,@defs {(or false false false)}
       -> ,@defs {false}))

  (let ([defs `((define c1 false)
                (define (d2 x) (or c1 false x)))])
    (t whocares/lam test-intermediate/lambda-sequence
       ,@defs (d2 false)
       :: ,@defs ({d2} false)
       -> ,@defs ({(lambda (x) (or c1 false x))} false)
       :: ,@defs {((lambda (x) (or c1 false x)) false)}
       -> ,@defs {(or c1 false false)}
       :: ,@defs (or {c1} false false)
       -> ,@defs (or {false} false false)
       :: ,@defs {(or false false false)}
       -> ,@defs {false}))

  (let ([defs `((define (f x) (+ (g x) 10)) (define (g x) (- x 22)))])
    (t forward-ref test-upto-int
       ,@defs (f 13)
       :: ,@defs {(f 13)}
       -> ,@defs {(+ (g 13) 10)}
       :: ,@defs (+ {(g 13)} 10)
       -> ,@defs (+ {(- 13 22)} 10)
       -> ,@defs (+ {-9} 10)
       :: ,@defs {(+ -9 10)}
       -> ,@defs {1}))

  (let ([defs `((define (f x) (+ (g x) 10)) (define (g x) (- x 22)))])
    (t forward-ref/lam test-intermediate/lambda-sequence
       ,@defs (f 13)
       :: ,@defs ({f} 13)
       -> ,@defs ({(lambda (x) (+ (g x) 10))} 13)
       :: ,@defs {((lambda (x) (+ (g x) 10)) 13)}
       -> ,@defs {(+ (g 13) 10)}
       :: ,@defs (+ ({g} 13) 10)
       -> ,@defs (+ ({(lambda (x) (- x 22))} 13) 10)
       :: ,@defs (+ {((lambda (x) (- x 22)) 13)} 10)
       -> ,@defs (+ {(- 13 22)} 10)
       -> ,@defs (+ {-9} 10)
       :: ,@defs {(+ -9 10)}
       -> ,@defs {1}))
  
  
  ;; loops; I should add a mechanism to stop testing after n steps...
  #;(let ([defs '((define (f x) (cond (else (f x))))
                (define (g x) x))])
    (t pnkfelix test-intermediate/lambda-sequence
      ,@defs (f (g empty))
      :: ,@defs ({f} (g empty))
      -> ,@defs ({(lambda (x) (cond (else (f x))))} (g empty))
      :: ,@defs ((lambda (x) (cond (else (f x)))) ({g} empty))
      -> ,@defs ((lambda (x) (cond (else (f x)))) ({(lambda (x) x)} empty))))

  (t bad-cons test-upto-int/lam
     (cons 1 2)
     :: {(cons 1 2)}
     -> error: "cons: second argument must be of type <list>, given 1 and 2")

  (t1 prims
  (test-beginner-sequence "(cons 3 (cons 1 empty)) (list 1 2 3) (define-struct aa (b)) (make-aa 3)"
                          (let ([defs `((cons 3 (cons 1 empty)))])
                          `((before-after (,@defs (hilite (list 1 2 3)))
                                          (,@defs (hilite (cons 1 (cons 2 (cons 3 empty))))))
                            (finished-stepping)))))

  (t1 prims/non-beginner
  (test-bwla-to-int/lam "(cons 3 (cons 1 empty)) (list 1 2 3) (define-struct aa (b)) (make-aa 3)"
                        `((before-after ((cons 3 (hilite (cons 1 empty)))) ((cons 3 (hilite (list 1)))))
                          (before-after ((hilite (cons 3 (list 1)))) ((hilite (list 3 1))))
                          (finished-stepping))))


  (t1 map
  (test-mz-sequence "(map (lambda (x) x) (list 3 4 5))"
                    `((before-after ((map (lambda (x) x) (hilite (list 3 4 5))))
                                    ((map (lambda (x) x) (hilite `( 3 4 5)))))
                      (before-after ((hilite (map (lambda (x) x) `(3 4 5))))
                                    ((... (hilite 3) ...)))
                      (before-after (...)
                                    ((... (hilite 4) ...)))
                      (before-after (...)
                                    ((... (hilite 5) ...)))
                      (before-after (...) ((hilite `(3 4 5))))
                      (finished-stepping))))

  (t1 quoted-list
  (test-beginner-wla-sequence "'(3 4 5)"
                              `((finished-stepping))))

  (t1 quoted-list-display
     (test-bwla-to-int/lam "(define (f x) '((a))) (+ 3 4)"
                           `((before-after ((define (f x) (list (list 'a))) (hilite (+ 3 4)))
                                           ((define (f x) (list (list 'a))) (hilite 7)))
                             (finished-stepping))))


  ;;;;;;;;;;;;;
  ;;
  ;;  QUASIQUOTE
  ;;
  ;;;;;;;;;;;;;.

  ; note: we currently punt on trying to unwind quasiquote.

  (t1 qq1
  (test-beginner-wla-sequence "`(3 4 ,(+ 4 5))"
                              `((before-after ((cons 3 (cons 4 (cons (hilite (+ 4 5)) empty))))
                                              ((cons 3 (cons 4 (cons (hilite 9) empty)))))
                                (before-after ((cons 3 (cons 4 (hilite (cons 9 empty)))))
                                              ((cons 3 (cons 4 (hilite (list 9))))))
                                (before-after ((cons 3 (hilite (cons 4 (list 9)))))
                                              ((cons 3 (hilite (list 4 9)))))
                                (before-after ((hilite (cons 3 (list 4 9)))) ((hilite (list 3 4 9))))
                                (finished-stepping))))

  (t1 qq-splice
  (test-beginner-wla-sequence "`(3 ,@(list (+ 3 4) 5) 6)"
                              `((before-after ((cons 3 (append (list (hilite (+ 3 4)) 5) (cons 6 empty)))) ((cons 3 (append (list (hilite 7) 5) (cons 6 empty)))))
                                (before-after ((cons 3 (append (list 7 5) (hilite (cons 6 empty))))) ((cons 3 (append (list 7 5) (list 6)))))
                                (before-after ((cons 3 (hilite (append (list 7 5) (list 6))))) ((cons 3 (hilite (list 7 5 6)))))
                                (before-after ((hilite (cons 3 (list 7 5 6)))) ((hilite (list 3 7 5 6))))
                                (finished-stepping))))

  ;;;;;;;;;;;;;
  ;;
  ;;  LET
  ;;
  ;;;;;;;;;;;;;

  (t1 let1 (test-both-ints "(let ([a 3]) 4)"
                                       `((before-after ((hilite (let ([a 3]) 4))) ((hilite (define a_0 3)) (hilite 4)))
                                         (finished-stepping))))

  (t1 let2
     (test-both-ints "(let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))"
                     `((before-after ((hilite (let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))))
                                     ((hilite (define a_0 (+ 4 5))) (hilite (define b_0 (+ 9 20))) (hilite (+ a_0 b_0))))
                       (before-after ((define a_0 (hilite (+ 4 5))) (define b_0 (+ 9 20)) (+ a_0 b_0))
                                     ((define a_0 (hilite 9)) (define b_0 (+ 9 20)) (+ a_0 b_0)))
                       (before-after ((define a_0 9) (define b_0 (hilite (+ 9 20))) (+ a_0 b_0))
                                     ((define a_0 9) (define b_0 (hilite 29)) (+ a_0 b_0)))
                       (before-after ((define a_0 9) (define b_0 29) (+ (hilite a_0) b_0))
                                     ((define a_0 9) (define b_0 29) (+ (hilite 9) b_0)))
                       (before-after ((define a_0 9) (define b_0 29) (+ 9 (hilite b_0)))
                                     ((define a_0 9) (define b_0 29) (+ 9 (hilite 29))))
                       (before-after ((define a_0 9) (define b_0 29) (hilite (+ 9 29)))
                                     ((define a_0 9) (define b_0 29) (hilite 38)))
                       (finished-stepping))))

  (t1 let-scoping1
     (test-intermediate-sequence "(let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4)))"
                                 (let ([d1 `(define a_0 3)]
                                       [d2 `(define a_1 (lambda (x) (+ a_0 x)))])
                                   `((before-after ((hilite (let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4)))))
                                                   ((hilite (define a_0 3)) (hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4)))))
                                     (before-after (,d1 (hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4))))
                                                   (,d1 (hilite (define a_1 (lambda (x) (+ a_0 x)))) (hilite (a_1 4))))
                                     (before-after (,d1 ,d2 (hilite (a_1 4)))
                                                   (,d1 ,d2 (hilite (+ a_0 4))))
                                     (before-after (,d1 ,d2 (+ (hilite a_0) 4))
                                                   (,d1 ,d2 (+ (hilite 3) 4)))
                                     (before-after (,d1 ,d2 (hilite (+ 3 4)))
                                                   (,d1 ,d2 (hilite 7)))
                                     (finished-stepping)))))

  (t1 let-scoping2
     (test-intermediate/lambda-sequence "(let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4)))"
                                        (let* ([d1 `(define a_0 3)]
                                               [defs `(,d1 (define a_1 (lambda (x) (+ a_0 x))))])
                                        `((before-after ((hilite (let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4)))))
							((hilite (define a_0 3)) (hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4)))))
                                          (before-after (,d1 (hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4))))
                                                        (,d1 (hilite (define a_1 (lambda (x) (+ a_0 x)))) (hilite (a_1 4))))
                                          (before-after (,@defs ((hilite a_1) 4))
                                                        (,@defs ((hilite (lambda (x) (+ a_0 x))) 4)))
                                          (before-after (,@defs (hilite ((lambda (x) (+ a_0 x)) 4))) (,@defs (hilite (+ a_0 4))))
                                          (before-after (,@defs (+ (hilite a_0) 4)) (,@defs (+ (hilite 3) 4)))
                                          (before-after (,@defs (hilite (+ 3 4))) (,@defs (hilite 7)))
                                          (finished-stepping)))))

  (t1 let-scoping3
     (test-intermediate-sequence "(define a12 3) (define c12 19) (let ([a12 13] [b12 a12]) (+ b12 a12 c12))"
                                 (let* ([defs1 `((define a12 3) (define c12 19))]
                                        [defs2 `(,@defs1 (define a12_0 13))]
                                        [defs3 `(,@defs2 (define b12_0 3))])
                              `((before-after (,@defs1 (hilite (let ([a12 13] [b12 a12]) (+ b12 a12 c12))))
                                              (,@defs1 (hilite (define a12_0 13)) (hilite (define b12_0 a12)) (hilite (+ b12_0 a12_0 c12))))
                                (before-after (,@defs2 (define b12_0 (hilite a12)) (+ b12_0 a12_0 c12))
                                              (,@defs2 (define b12_0 (hilite 3)) (+ b12_0 a12_0 c12)))
                                (before-after (,@defs3 (+ (hilite b12_0) a12_0 c12))
                                              (,@defs3 (+ (hilite 3) a12_0 c12)))
                                (before-after (,@defs3 (+ 3 (hilite a12_0) c12))
                                              (,@defs3 (+ 3 (hilite 13) c12)))
                                (before-after (,@defs3 (+ 3 13 (hilite c12)))
                                              (,@defs3 (+ 3 13 (hilite 19))))
                                (before-after (,@defs3 (hilite (+ 3 13 19)))
                                              (,@defs3 (hilite 35)))
                                (finished-stepping)))))

  (t1 let-lifting1
     (test-intermediate-sequence "(let ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
                                 `((before-after ((hilite (let ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)))
                                                 ((hilite (define a_0 (lambda (x) (+ x 14)))) (hilite (define b_0 (+ 3 4))) (hilite 9)))
                                   (before-after ((define a_0 (lambda (x) (+ x 14))) (define b_0 (hilite (+ 3 4))) 9)
                                                 ((define a_0 (lambda (x) (+ x 14))) (define b_0 (hilite 7)) 9))
                                   (finished-stepping))))

    (t1 let-deriv
       (test-intermediate-sequence "(define (f g) (let ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)) (define gprime (f cos))"
                                   (let ([defs `((define (f g) (let ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)))])
                                     `((before-after (,@defs (define gprime (hilite (f cos))))
                                                     (,@defs (define gprime (hilite (let ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp)))))
                                     (before-after (,@defs (define gprime (hilite (let ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp))))
                                                   (,@defs (hilite (define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001)))) (define gprime (hilite gp_0))))
                                     (finished-stepping)))))

  (t1 let-assigned
     (test-intermediate-sequence "(define a (let ([f (lambda (x) (+ x 13))]) f))"
                                 `((before-after ((define a (hilite (let ([f (lambda (x) (+ x 13))]) f))))
                                                 ((hilite (define f_0 (lambda (x) (+ x 13)))) (define a (hilite f_0))))
                                   (finished-stepping))))

  (t1 let-assigned/lam
     (test-intermediate/lambda-sequence "(define a (let ([f (lambda (x) (+ x 13))]) f))"
                                        `((before-after ((define a (hilite (let ([f (lambda (x) (+ x 13))]) f))))
                                                        ((hilite (define f_0 (lambda (x) (+ x 13)))) (define a (hilite f_0))))
                                          (before-after ((define f_0 (lambda (x) (+ x 13))) (define a (hilite f_0)))
                                                        ((define f_0 (lambda (x) (+ x 13))) (define a (hilite (lambda (x) (+ x 13))))))
                                          (finished-stepping))))
  
    ;;;;;;;;;;;;;
    ;;
    ;;  LET*
    ;;
    ;;;;;;;;;;;;;

    (t1 let*-scoping1
       (test-both-ints "(define a 3) (define c 19) (let* ([a 13] [b a]) (+ b a c))"
                       (let* ([defs1 `((define a 3) (define c 19))]
                              [defs2 (append defs1 `((define a_0 13)))]
                              [defs3 (append defs2 `((define b_1 13)))])
                                `((before-after (,@defs1 (hilite (let* ([a 13] [b a]) (+ b a c))))
                                                (,@defs1 (hilite (define a_0 13)) (hilite (let* ([b a_0]) (+ b a_0 c)))))
                                  (before-after (,@defs2 (hilite (let* ([b a_0]) (+ b a_0 c))))
                                                (,@defs2 (hilite (define b_1 a_0)) (hilite (+ b_1 a_0 c))))
                                  (before-after (,@defs2 (define b_1 (hilite a_0)) (+ b_1 a_0 c))
                                                (,@defs2 (define b_1 (hilite 13)) (+ b_1 a_0 c)))
                                  (before-after (,@defs3 (+ (hilite b_1) a_0 c))
                                                (,@defs3 (+ (hilite 13) a_0 c)))
                                  (before-after (,@defs3 (+ 13 (hilite a_0) c))
                                                (,@defs3 (+ 13 (hilite 13) c)))
                                  (before-after (,@defs3 (+ 13 13 (hilite c)))
                                                (,@defs3 (+ 13 13 (hilite 19))))
                                  (before-after (,@defs3 (hilite (+ 13 13 19)))
                                                (,@defs3 (hilite 45)))
                                  (finished-stepping)))))

    (t1 let*-lifting1
       (test-intermediate-sequence "(let* ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
                                   (let ([defs `((define a_0 (lambda (x) (+ x 14))))])
                                     `((before-after ((hilite (let* ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)))
                                                     ((hilite (define a_0 (lambda (x) (+ x 14)))) (hilite (let* ([b (+ 3 4)]) 9))))
                                       (before-after (,@defs (hilite (let* ([b (+ 3 4)]) 9)))
                                                     (,@defs (hilite (define b_1 (+ 3 4))) (hilite 9)))
                                  (before-after (,@defs (define b_1 (hilite (+ 3 4))) 9)
                                                (,@defs (define b_1 (hilite 7)) 9))
                                  (finished-stepping)))))

  (t1 let*-deriv
     (test-intermediate-sequence "(define (f g) (let* ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)) (define gprime (f cos))"
                                 (let ([defs `((define (f g) (let* ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)))])
                                   `((before-after (,@defs (define gprime (hilite (f cos))))
                                                   (,@defs (define gprime (hilite (let* ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp)))))
                                     (before-after (,@defs (define gprime (hilite (let* ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp))))
                                                   (,@defs (hilite (define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001)))) (define gprime (hilite gp_0))))
                                (finished-stepping)))))

  (t1 let/let*
     (test-both-ints "(let* ([a 9]) (let ([b 6]) a))"
                     `((before-after ((hilite (let* ([a 9]) (let ([b 6]) a)))) ((hilite (define a_0 9)) (hilite (let ([b 6]) a_0))))
                       (before-after ((define a_0 9) (hilite (let ([b 6]) a_0)))
                                     ((define a_0 9) (hilite (define b_1 6)) (hilite a_0)))
                       (before-after ((define a_0 9) (define b_1 6) (hilite a_0))
                                     ((define a_0 9) (define b_1 6) (hilite 9)))
                       (finished-stepping))))

    ;;;;;;;;;;;;;
    ;;
    ;;  LETREC
    ;;
    ;;;;;;;;;;;;;

  (t1 letrec1
     (test-intermediate-sequence "(define a 3) (define c 19) (letrec ([a 13] [b a]) (+ b a c))"
                                 (let* ([defs1 `((define a 3) (define c 19))]
                                        [defs2 (append defs1 `((define a_0 13)))]
                                        [defs3 (append defs2 `((define b_0 13)))])
                                 `((before-after (,@defs1 (hilite (letrec ([a 13] [b a]) (+ b a c))))
                                                 (,@defs1 (hilite (define a_0 13)) (hilite (define b_0 a_0)) (hilite (+ b_0 a_0 c))))
                                   (before-after (,@defs2 (define b_0 (hilite a_0)) (+ b_0 a_0 c))
                                                 (,@defs2 (define b_0 (hilite 13)) (+ b_0 a_0 c)))
                                   (before-after (,@defs3 (+ (hilite b_0) a_0 c))
                                                 (,@defs3 (+ (hilite 13) a_0 c)))
                                   (before-after (,@defs3 (+ 13 (hilite a_0) c))
                                                 (,@defs3 (+ 13 (hilite 13) c)))
                                   (before-after (,@defs3 (+ 13 13 (hilite c)))
                                                 (,@defs3 (+ 13 13 (hilite 19))))
                                   (before-after (,@defs3 (hilite (+ 13 13 19)))
                                                 (,@defs3 (hilite 45)))
                                   (finished-stepping)))))

  (t1 letrec2
      (test-intermediate-sequence "(letrec ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
                                  `((before-after ((hilite (letrec ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)))
                                                  ((hilite (define a_0 (lambda (x) (+ x 14)))) (hilite (define b_0 (+ 3 4))) (hilite 9)))
                                    (before-after ((define a_0 (lambda (x) (+ x 14))) (define b_0 (hilite (+ 3 4))) 9)
                                                  ((define a_0 (lambda (x) (+ x 14))) (define b_0 (hilite 7)) 9))
                                    (finished-stepping))))

  (t1 letrec3
     (test-intermediate-sequence "(define (f g) (letrec ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)) (define gprime (f cos))"
                                 (let ([defs `((define (f g) (letrec ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)))])
                                   `((before-after (,@defs (define gprime (hilite (f cos))))
                                                   (,@defs (define gprime (hilite (letrec ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp)))))
                                     (before-after (,@defs (define gprime (hilite (letrec ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp))))
                                                   (,@defs (hilite (define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001)))) (define gprime (hilite gp_0))))
                                   (finished-stepping)))))
      ;;;;;;;;;;;;;
      ;;
      ;;  RECUR
      ;;
      ;;;;;;;;;;;;;

  ;; N.B. : we cheat here.  In particular, the rhs of the double-break expression should highlight the whole application, and not
  ;; just the applied loop identifier.  This is hard to fix because we have an application which is initially hidden, but then later
  ;; not hidden.  Fixing this involves parameterizing the unwind by what kind of break it was.  Yuck!  So we just fudge the test case.

  (t1 recur
      (test-advanced-sequence "(define (countdown n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1))))) (countdown 2)"
                              (let* ([defs1 `((define (countdown n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1))))))]
                                     [defs2 (append defs1 `((define (loop_0 n) (if (= n 0) 13 (loop_0 (- n 1))))))])
                                `((before-after (,@defs1 ((hilite countdown) 2))
                                                (,@defs1 ((hilite (lambda (n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1)))))) 2)))
                                  (before-after (,@defs1 (hilite ((lambda (n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1))))) 2)))
                                                (,@defs1 (hilite (recur loop ([n 2]) (if (= n 0) 13 (loop (- n 1)))))))
                                  (before-after (,@defs1 (hilite (recur loop ([n 2]) (if (= n 0) 13 (loop (- n 1))))))
                                                (,@defs1 (hilite (define (loop_0 n) (if (= n 0) 13 (loop_0 (- n 1))))) ((hilite loop_0) 2)))
                                  (before-after (,@defs2 ((hilite loop_0) 2))
                                                (,@defs2 ((hilite (lambda (n) (if (= n 0) 13 (loop_0 (- n 1))))) 2)))
                                  (before-after (,@defs2 (hilite ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) 2)))
                                                (,@defs2 (hilite (if (= 2 0) 13 (loop_0 (- 2 1))))))
                                  (before-after (,@defs2 (if (hilite (= 2 0)) 13 (loop_0 (- 2 1))))
                                                (,@defs2 (if (hilite false) 13 (loop_0 (- 2 1)))))
                                  (before-after (,@defs2 (hilite (if false 13 (loop_0 (- 2 1)))))
                                                (,@defs2 (hilite (loop_0 (- 2 1)))))
                                  (before-after (,@defs2 ((hilite loop_0) (- 2 1)))
                                                (,@defs2 ((hilite (lambda (n) (if (= n 0) 13 (loop_0 (- n 1))))) (- 2 1))))
                                  (before-after (,@defs2 ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) (hilite (- 2 1))))
                                                (,@defs2 ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) (hilite 1))))
                                  (before-after (,@defs2 (hilite ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) 1)))
                                                (,@defs2 (hilite (if (= 1 0) 13 (loop_0 (- 1 1))))))
                                  (before-after (,@defs2 (if (hilite (= 1 0)) 13 (loop_0 (- 1 1))))
                                                (,@defs2 (if (hilite false) 13 (loop_0 (- 1 1)))))
                                  (before-after (,@defs2 (hilite (if false 13 (loop_0 (- 1 1)))))
                                                (,@defs2 (hilite (loop_0 (- 1 1)))))
                                  (before-after (,@defs2 ((hilite loop_0) (- 1 1)))
                                                (,@defs2 ((hilite (lambda (n) (if (= n 0) 13 (loop_0 (- n 1))))) (- 1 1))))
                                  (before-after (,@defs2 ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) (hilite (- 1 1))))
                                                (,@defs2 ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) (hilite 0))))
                                  (before-after (,@defs2 (hilite ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) 0)))
                                                (,@defs2 (hilite (if (= 0 0) 13 (loop_0 (- 0 1))))))
                                  (before-after (,@defs2 (if (hilite (= 0 0)) 13 (loop_0 (- 0 1))))
                                                (,@defs2 (if (hilite true) 13 (loop_0 (- 0 1)))))
                                  (before-after (,@defs2 (hilite (if true 13 (loop_0 (- 0 1)))))
                                                (,@defs2 (hilite 13)))
  				  (finished-stepping)))))

    ;;;;;;;;;;;;;
    ;;
    ;;  LOCAL
    ;;
    ;;;;;;;;;;;;;


  (t1 empty-local
     (test-both-ints "(local () (+ 3 4))"
                     `((before-after ((hilite (local () (+ 3 4)))) ((hilite (+ 3 4))))
                       (before-after ((hilite (+ 3 4))) ((hilite 7)))
                       (finished-stepping))))

  (t1 local1
     (test-both-ints "(local ((define a 3) (define b 8)) 4)"
                     `((before-after ((hilite (local ((define a 3) (define b 8)) 4)))
                                     ((hilite (define a_0 3)) (hilite (define b_0 8)) (hilite 4)))
                       (finished-stepping))))

  (t1 local2
     (test-intermediate-sequence "(local ((define (a x) (+ x 9))) (a 6))"
                                 (let ([defs `((define (a_0 x) (+ x 9)))])
                                 `((before-after ((hilite (local ((define (a x) (+ x 9))) (a 6))))
						 ((hilite (define (a_0 x) (+ x 9))) (hilite (a_0 6))))
                                   (before-after (,@defs (hilite (a_0 6)))
                                                 (,@defs (hilite (+ 6 9))))
                                   (before-after (,@defs (hilite (+ 6 9)))
                                                 (,@defs (hilite 15)))
                                   (finished-stepping)))))

  (t1 local3
     (test-intermediate/lambda-sequence "(local ((define (a x) (+ x 9))) (a 6))"
                                        (let ([defs `((define (a_0 x) (+ x 9)))])
                                          `((before-after ((hilite (local ((define (a x) (+ x 9))) (a 6))))
                                                          ((hilite (define (a_0 x) (+ x 9))) (hilite (a_0 6))))
                                            (before-after (,@defs ((hilite a_0) 6))
                                                          (,@defs ((hilite (lambda (x) (+ x 9))) 6)))
                                            (before-after (,@defs (hilite ((lambda (x) (+ x 9)) 6)))
                                                          (,@defs (hilite (+ 6 9))))
                                            (before-after (,@defs (hilite (+ 6 9)))
                                                          (,@defs (hilite 15)))
                                            (finished-stepping)))))

  (t1 local4
     (test-intermediate-sequence "(local ((define (a x) (+ x 13))) a)"
                                 `((before-after ((hilite (local ((define (a x) (+ x 13))) a))) ((hilite (define (a_0 x) (+ x 13))) (hilite a_0)))
                                   (finished-stepping))))

  (t1 local5
     (test-intermediate/lambda-sequence "(local ((define (a x) (+ x 13))) a)"
                                        `((before-after ((hilite (local ((define (a x) (+ x 13))) a)))
                                                        ((hilite (define (a_0 x) (+ x 13))) (hilite a_0)))
                                          (before-after ((define (a_0 x) (+ x 13)) (hilite a_0))
                                                        ((define (a_0 x) (+ x 13)) (hilite (lambda (x) (+ x 13)))))
                                          (finished-stepping))))

  (t1 local-interref1
     (test-intermediate-sequence "(local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))"
                                 (let* ([defs1 `((define (a_0 x) (+ x 9)) (define b_0 a_0))]
                                        [defs2 (append defs1 `((define p_0 7)))])
                                 `((before-after ((hilite (local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))))
                                                 ((hilite (define (a_0 x) (+ x 9))) (hilite (define b_0 a_0)) (hilite (define p_0 (+ 3 4))) (hilite (b_0 1))))
                                   (before-after (,@defs1 (define p_0 (hilite (+ 3 4))) (b_0 1))
                                                 (,@defs1 (define p_0 (hilite 7)) (b_0 1)))
                                   (before-after (,@defs2 ((hilite b_0) 1))
                                                 (,@defs2 ((hilite a_0) 1)))
                                   (before-after (,@defs2 (hilite (a_0 1)))
                                                 (,@defs2 (hilite (+ 1 9))))
                                   (before-after (,@defs2 (hilite (+ 1 9)))
                                                 (,@defs2 (hilite 10)))
                                   (finished-stepping)))))

  (t1 local-interref2
     (test-intermediate/lambda-sequence "(local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))"
                                        (let* ([defs1 `((define (a_0 x) (+ x 9)))]
                                               [defs2 (append defs1 `((define b_0 (lambda (x) (+ x 9)))))]
                                               [defs3 (append defs2 `((define p_0 7)))])
                                        `((before-after ((hilite (local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))))
                                                        ((hilite (define (a_0 x) (+ x 9))) (hilite (define b_0 a_0)) (hilite (define p_0 (+ 3 4))) (hilite (b_0 1))))
                                          (before-after (,@defs1 (define b_0 (hilite a_0)) (define p_0 (+ 3 4)) (b_0 1))
                                                        (,@defs1 (define b_0 (hilite (lambda (x) (+ x 9)))) (define p_0 (+ 3 4)) (b_0 1)))
                                          (before-after (,@defs2 (define p_0 (hilite (+ 3 4))) (b_0 1))
                                                        (,@defs2 (define p_0 (hilite 7)) (b_0 1)))
                                          (before-after (,@defs3 ((hilite b_0) 1))
                                                        (,@defs3 ((hilite (lambda (x) (+ x 9))) 1)))
                                          (before-after (,@defs3 (hilite ((lambda (x) (+ x 9)) 1)))
                                                        (,@defs3 (hilite (+ 1 9))))
                                          (before-after (,@defs3 (hilite (+ 1 9)))
                                                        (,@defs3 (hilite 10)))
                                          (finished-stepping)))))

  (t1 local-gprime
     (test-intermediate-sequence "(define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)) (define gprime (f12 cos))"
                                 (let ([defs `((define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)))])
                                   `((before-after (,@defs (define gprime (hilite (f12 cos))))
                                                   (,@defs (define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp)))))
                                     (before-after (,@defs (define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp))))
                                                   (,@defs (hilite (define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))) (define gprime (hilite gp_0))))
                                     (finished-stepping)))))

  (t1 local-gprime/lambda
     (test-intermediate/lambda-sequence "(define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)) (define gprime (f12 cos))"
                                        (let ([defs `((define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)))])
                                          `((before-after (,@defs (define gprime ((hilite f12) cos)))
                                                          (,@defs (define gprime ((hilite (lambda (g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp))) cos))))
                                            (before-after (,@defs (define gprime (hilite ((lambda (g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)) cos))))
                                                          (,@defs (define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp)))))
                                            (before-after (,@defs (define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp))))
                                                          (,@defs (hilite (define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))) (define gprime (hilite gp_0))))
                                            (before-after (,@defs (define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)) (define gprime (hilite gp_0)))
                                                          (,@defs
                                                             (define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))
                                                             (define gprime (hilite (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))))))
                                          (finished-stepping)))))

  ; test generativity... that is, multiple evaluations of a local should get different lifted names:

  (t1 local-generative
     (test-intermediate-sequence "(define (a13 b13 c13) (b13 c13)) (define (f9 x) (local ((define (maker dc) x)) maker)) (define m1 (f9 3)) (a13 (f9 4) 1)"
                                 (let* ([defs1 `((define (a13 b13 c13) (b13 c13))
                                                 (define (f9 x) (local ((define (maker dc) x)) maker)))]
                                        [defs2 (append defs1 `((define (maker_0 dc) 3) (define m1 maker_0)))]
                                        [defs3 (append defs2 `((define (maker_1 dc) 4)))])
                                 `((before-after (,@defs1 (define m1 (hilite (f9 3))))
                                                 (,@defs1 (define m1 (hilite (local ((define (maker dc) 3)) maker)))))
                                   (before-after (,@defs1 (define m1 (hilite (local ((define (maker dc) 3)) maker))))
                                                 (,@defs1 (hilite (define (maker_0 dc) 3)) (define m1 (hilite maker_0))))
                                   (before-after (,@defs2 (a13 (hilite (f9 4)) 1))
                                                 (,@defs2 (a13 (hilite (local ((define (maker dc) 4)) maker)) 1)))
                                   (before-after (,@defs2 (a13 (hilite (local ((define (maker dc) 4)) maker)) 1))
                                                 (,@defs2 (hilite (define (maker_1 dc) 4)) (a13 (hilite maker_1) 1)))
                                   (before-after (,@defs3 (hilite (a13 maker_1 1)))
                                                 (,@defs3 (hilite (maker_1 1))))
                                   (before-after (,@defs3 (hilite (maker_1 1)))
                                                 (,@defs3 (hilite 4)))
                                   (finished-stepping)))))

  (t1 local-generative/lambda
     (test-intermediate/lambda-sequence "(define (a13 b13 c13) (b13 c13)) (define (f9 x) (local ((define (maker dc) x)) maker)) (define m1 (f9 3)) (a13 (f9 4) 1)"
                                        (let* ([defs1 `((define (a13 b13 c13) (b13 c13))
                                                        (define (f9 x) (local ((define (maker dc) x)) maker)))]
                                               [defs2 (append defs1 `((define (maker_0 dc) 3)))]
                                               [defs3 (append defs2 `((define m1 (lambda (dc) 3))))]
                                               [defs4 (append defs3 `((define (maker_1 dc) 4)))])
                                        `((before-after (,@defs1 (define m1 ((hilite f9) 3)))
                                                        (,@defs1 (define m1 ((hilite (lambda (x) (local ((define (maker dc) x)) maker))) 3))))
                                          (before-after (,@defs1 (define m1 (hilite ((lambda (x) (local ((define (maker dc) x)) maker)) 3))))
                                                        (,@defs1 (define m1 (hilite (local ((define (maker dc) 3)) maker)))))
                                          (before-after (,@defs1 (define m1 (hilite (local ((define (maker dc) 3)) maker))))
                                                        (,@defs1 (hilite (define (maker_0 dc) 3)) (define m1 (hilite maker_0))))
                                          (before-after (,@defs2 (define m1 (hilite maker_0)))
                                                        (,@defs2 (define m1 (hilite (lambda (dc) 3)))))
                                          (before-after (,@defs3 ((hilite a13) (f9 4) 1))
                                                        (,@defs3 ((hilite (lambda (b13 c13) (b13 c13))) (f9 4) 1)))
                                          (before-after (,@defs3 ((lambda (b13 c13) (b13 c13)) ((hilite f9) 4) 1))
                                                        (,@defs3 ((lambda (b13 c13) (b13 c13)) ((hilite (lambda (x) (local ((define (maker dc) x)) maker))) 4) 1)))
                                          (before-after (,@defs3 ((lambda (b13 c13) (b13 c13)) (hilite ((lambda (x) (local ((define (maker dc) x)) maker)) 4)) 1))
                                                        (,@defs3 ((lambda (b13 c13) (b13 c13)) (hilite (local ((define (maker dc) 4)) maker)) 1)))
                                          (before-after (,@defs3 ((lambda (b13 c13) (b13 c13)) (hilite (local ((define (maker dc) 4)) maker)) 1))
                                                        (,@defs3 (hilite (define (maker_1 dc) 4)) ((lambda (b13 c13) (b13 c13)) (hilite maker_1) 1)))
                                          (before-after (,@defs4 ((lambda (b13 c13) (b13 c13)) (hilite maker_1) 1))
                                                        (,@defs4 ((lambda (b13 c13) (b13 c13)) (hilite (lambda (dc) 4)) 1)))
                                          (before-after (,@defs4 (hilite ((lambda (b13 c13) (b13 c13)) (lambda (dc) 4) 1)))
                                                        (,@defs4 (hilite ((lambda (dc) 4) 1))))
                                          (before-after (,@defs4 (hilite ((lambda (dc) 4) 1)))
                                                        (,@defs4 (hilite 4)))
                                          (finished-stepping)))))

  ;;;;;;;;;;;;;
  ;;
  ;;  Reduction of Lambda in int/lambda
  ;;
  ;;;;;;;;;;;;;

  (t1 int/lam1
     (test-intermediate/lambda-sequence "(define f ((lambda (x) x) (lambda (x) x))) (f f)"
                                        (let ([defs `((define f (lambda (x) x)))])
                                        `((before-after ((define f (hilite ((lambda (x) x) (lambda (x) x)))))
                                                        ((define f (hilite (lambda (x) x)))))
                                          (before-after (,@defs ((hilite f) f))
                                                        (,@defs ((hilite (lambda (x) x)) f)))
                                          (before-after (,@defs ((lambda (x) x) (hilite f)))
                                                        (,@defs ((lambda (x) x) (hilite (lambda (x) x)))))
                                          (before-after (,@defs (hilite ((lambda (x) x) (lambda (x) x))))
                                                        (,@defs (hilite (lambda (x) x))))
                                          (finished-stepping)))))


  (t1 int/lam2
     (test-intermediate/lambda-sequence "(define f (if false (lambda (x) x) (lambda (x) x))) (f f)"
                                        (let ([defs `((define f (lambda (x) x)))])
                                        `((before-after ((define f (hilite (if false (lambda (x) x) (lambda (x) x)))))
                                                        ((define f (hilite (lambda (x) x)))))
                                          (before-after (,@defs ((hilite f) f))
                                                        (,@defs ((hilite (lambda (x) x)) f)))
                                          (before-after (,@defs ((lambda (x) x) (hilite f)))
                                                        (,@defs ((lambda (x) x) (hilite (lambda (x) x)))))
                                          (before-after (,@defs (hilite ((lambda (x) x) (lambda (x) x))))
                                                        (,@defs (hilite (lambda (x) x))))
                                          (finished-stepping)))))

  ;
  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  TIME
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;

  (t1 time
     (test-intermediate-sequence "(time (+ 3 4))"
                                 `((before-after ((hilite (+ 3 4)))
                                                 ((hilite 7)))
                                   (finished-stepping))))


  ;;;;;;;;;;;;;;;;
  ;;
  ;;  XML (uses MrEd)
  ;;
  ;;;;;;;;;;;;;;;;

  ;; NOT UPDATED FOR NEW TEST CASE FORMAT

  #;
  (t1 ddj-screenshot
     (test-mz-sequence (define-syntax (xml stx)
                                (letrec ([process-xexpr
                                          (lambda (xexpr)
                                            (syntax-case xexpr (lmx lmx-splice)
                                              [(lmx-splice unquoted) #`(unquote-splicing unquoted)]
                                              [(lmx unquoted) #`(unquote unquoted)]
                                              [(tag ([attr val] ...) . sub-xexprs)
                                               (identifier? #`tag)
                                               #`(tag ([attr val] ...) #,@(map process-xexpr (syntax->list #`sub-xexprs)))]
                                              [(tag . sub-xexprs)
                                               (identifier? #`tag)
                                               #`(tag () #,@(map process-xexpr (syntax->list #`sub-xexprs)))]
                                              [str
                                               (string? (syntax-e #`str))
                                               xexpr]))])
                                  (syntax-case stx ()
                                    [(_ xexpr) #`(quasiquote #,(process-xexpr #`xexpr))])))
     (xml (article (header (author "John Clements")
                           (title (if (< 3 4)
                                      (xml "No Title Available")
                                      (get-title))))
                   (text "More Sample Text")))
     '((before-after-finished ((define-syntax (xml stx)
                                 (letrec ([process-xexpr
                                           (lambda (xexpr)
                                             (syntax-case xexpr (lmx lmx-splice)
                                               [(lmx-splice unquoted) #`(unquote-splicing unquoted)]
                                               [(lmx unquoted) #`(unquote unquoted)]
                                               [(tag ([attr val] ...) . sub-xexprs)
                                                (identifier? #`tag)
                                                #`(tag ([attr val] ...) #,@(map process-xexpr (syntax->list #`sub-xexprs)))]
                                               [(tag . sub-xexprs)
                                                (identifier? #`tag)
                                                #`(tag () #,@(map process-xexpr (syntax->list #`sub-xexprs)))]
                                               [str
                                                (string? (syntax-e #`str))
                                                xexpr]))])
                                   (syntax-case stx ()
                                     [(_ xexpr) #`(quasiquote #,(process-xexpr #`xexpr))]))))
                              ((xml ))
                              ((xml (a ([a "x"]) "ab" "hdo" "hon")))))))

  #;
  (define (test-xml-sequence namespace-spec render-settings track-inferred-names? spec expected-steps)
    (letrec ([port (open-input-text-editor (construct-text spec))])
      (test-sequence-core namespace-spec render-settings track-inferred-names? port expected-steps)))

  #;
  (define (construct-text spec)
    (let ([new-text (instantiate text% ())])
      (for-each
       (match-lambda
         [`(xml-box ,@(xmlspec ...)) (send new-text insert (construct-xml-box xmlspec))]
         [(? string? text) (send new-text insert text)])
       spec)
      new-text))

  #;
  (define (test-xml-beginner-sequence spec expected)
    (test-xml-sequence `(lib "htdp-beginner.ss" "lang")
                        fake-beginner-render-settings
                        #t
                        spec
                        expected))

  #;
  (t1 xml-box1
     (test-xml-beginner-sequence `((xml-box "<abba>3</abba>"))
                                 `((finished-stepping))))

  #;
  (t1 xml-box2
     (text-xml-beginnner-sequence `("(cdr (cdr " (xml-box "<foozle>a b</foozle>") "))")
                                  `((before-after ((cdr (cdr (xml-box "<foozle>a b</foozle>"))))))))

  ;(t1 filled-rect-image
  ;   (test-upto-int-lam "(image-width (filled-rect 10 10 'blue))"
  ;                      `((before-after ((image-width (hilite (filled-rect 10 10 'blue)))) ((image-width (hilite )))))))
  ; add image test: (image-width (filled-rect 10 10 'blue))


  
  (t check-expect test-upto-int/lam
     (check-expect (+ 3 4) (+ 8 9)) (check-expect (+ 1 1) 2) (check-expect (+ 2 2) 4) (+ 4 5)
     :: {(+ 4 5)} -> {9}
     :: 9 (check-expect (+ 3 4) {(+ 8 9)}) -> 9 (check-expect (+ 3 4) {17})
     :: 9 (check-expect {(+ 3 4)} 17) -> 9 (check-expect {7} 17)
     :: 9 false (check-expect {(+ 1 1)} 2) -> 9 false (check-expect {2} 2)
     :: 9 false true (check-expect {(+ 2 2)} 4) -> 9 false true (check-expect {4} 4))
  
  (t1 check-within
      (test-upto-int/lam
       "(check-within (+ 3 4) (+ 8 10) (+ 10 90)) (check-expect (+ 1 1) 2)(+ 4 5)"
       `((before-after ((hilite (+ 4 5)))
                       ((hilite 9)))
         (before-after (9 (check-within (+ 3 4) (hilite (+ 8 10)) (+ 10 90)))
                       (9 (check-within (+ 3 4) (hilite 18) (+ 10 90))))
         (before-after (9 (check-within (+ 3 4) 18 (hilite (+ 10 90))))
                       (9 (check-within (+ 3 4) 18 (hilite 100))))
         (before-after (9 (check-within (hilite (+ 3 4)) 18 100))
                       (9 (check-within (hilite 7) 18 100)))
         (before-after (9 true (check-expect (hilite (+ 1 1)) 2))
                       (9 true (check-expect (hilite 2) 2))))))
  
  
  (t1 check-within-bad
      (test-upto-int/lam
       "(check-within (+ 3 4) (+ 8 10) 0.01) (+ 4 5) (check-expect (+ 1 1) 2)"
       `((before-after ((hilite (+ 4 5)))
                       ((hilite 9)))
         (before-after (9 (check-within (+ 3 4) (hilite (+ 8 10)) 0.01))
                       (9 (check-within (+ 3 4) (hilite 18) 0.01)))
         (before-after (9 (check-within (hilite (+ 3 4)) 18 0.01))
                       (9 (check-within (hilite 7) 18 0.01)))
         (before-after (9 false (check-expect (hilite (+ 1 1)) 2))
                       (9 false (check-expect (hilite 2) 2))))))

  (let ([errmsg "rest: expected argument of type <non-empty list>; given ()"])
  (t1 check-error
      (test-upto-int/lam
       "(check-error (+ (+ 3 4) (rest empty)) (string-append \"rest: \" \"expected argument of type <non-empty list>; given ()\")) (check-expect (+ 3 1) 4) (+ 4 5)"
       `((before-after ((hilite (+ 4 5)))
                       ((hilite 9)))
         (before-after (9 (check-error (+ (+ 3 4) (rest empty)) (hilite (string-append "rest: " "expected argument of type <non-empty list>; given ()"))))
                       (9 (check-error (+ (+ 3 4) (rest empty)) (hilite ,errmsg))))
         (before-after (9 (check-error (+ (hilite (+ 3 4)) (rest empty)) ,errmsg))
                       (9 (check-error (+ (hilite 7) (rest empty)) ,errmsg)))
         (before-after (9 true (check-expect (hilite (+ 3 1)) 4))
                       (9 true (check-expect (hilite 4) 4)))))))
  
  (t1 check-error-bad
      (test-upto-int/lam
       "(check-error (+ (+ 3 4) (rest empty)) (string-append \"b\" \"ogus\")) (check-expect (+ 3 1) 4) (+ 4 5)"
       `((before-after ((hilite (+ 4 5)))
                       ((hilite 9)))
         (before-after (9 (check-error (+ (+ 3 4) (rest empty)) (hilite (string-append "b" "ogus"))))
                       (9 (check-error (+ (+ 3 4) (rest empty)) (hilite "bogus"))))
         (before-after (9 (check-error (+ (hilite (+ 3 4)) (rest empty)) "bogus"))
                       (9 (check-error (+ (hilite 7) (rest empty)) "bogus")))
         (before-after (9 false (check-expect (hilite (+ 3 1)) 4))
                       (9 false (check-expect (hilite 4) 4))))))

  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  TEACHPACK TESTS
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;

  ; as you can see, many teachpack tests work only in mred:
  ;; (require mred)


  (define test-teachpack-sequence (lambda (teachpack-specs expr-string expected-results)
                                    ;(let ([new-custodian (make-custodian)])
                                     ; (parameterize ([current-custodian new-custodian])
                                      ;  (parameterize ([current-eventspace (make-eventspace)])
                                          (test-sequence `(lib "htdp-beginner.ss" "lang") teachpack-specs fake-beginner-render-settings #f #f expr-string expected-results)
                                    ;))
                                     ; (custodian-shutdown-all new-custodian))
      ))
  

  ; uses set-render-settings!
  ;(reconstruct:set-render-settings! fake-beginner-render-settings)
  ;(test-sequence "(define (check-guess guess target) 'TooSmall) (guess-with-gui check-guess)"
  ;               `((before-after ((hilite ,h-p)) ((guess-with-gui check-guess)))
  ;                 (((hilite ,h-p)) (true)))
  ;               `((define (check-guess guess target) 'toosmall) true)
  ;               tp-namespace)

  #;
  (t1 teachpack-drawing
  (test-teachpack-sequence
   `((lib "draw.ss" "htdp"))
   "(define (draw-limb i) (cond
 [(= i 1) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
 [(= i 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))
 (and (start 100 100)
 (draw-limb 0))"
   `((before-after-finished ((define (draw-limb i) (cond [(= i 1) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
                                                         [(= i 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))
                            ((and (hilite (start 100 100)) (draw-limb 0)))
                            ((and (hilite true) (draw-limb 0))))
     (before-after ((and true (hilite (draw-limb 0))))
                   ((and true (hilite (cond [(= 0 1) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
                                            [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))))
     (before-after ((and true (cond [(hilite (= 0 1)) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
                                    [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))
                   ((and true (cond [(hilite false) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
                                    [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))))
     (before-after ((and true (hilite (cond [false (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
                                            [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))))
                   ((and true (hilite (cond [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))))
     (before-after ((and true (cond [(hilite (= 0 0)) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))
                   ((and true (cond [(hilite true) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))))
     (before-after ((and true (hilite (cond [true (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))))
                   ((and true (hilite (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)))))
     (before-after ((and true (draw-solid-line (make-posn (hilite (+ 1 10)) 10) (make-posn 10 100) 'red)))
                   ((and true (draw-solid-line (make-posn (hilite 11) 10) (make-posn 10 100) 'red))))
     (before-after ((and true (hilite (draw-solid-line (make-posn 11 10) (make-posn 10 100) 'red))))
                   ((and true (hilite true))))
     (before-after ((hilite (and true true)))
                   ((hilite true)))
     (finished-stepping))))
  
  #;(t1 teachpack-universe
      (test-teachpack-sequence
       `((lib "universe.ss" "2htdp"))
       "(define (z world)
  (empty-scene 100 100))

(big-bang 3
          (on-tick add1)
          (on-draw z))"
       `((finished-stepping))))

  #;
  (t1 teachpack-name-rendering
     (test-teachpack-sequence
      `((file "/Users/clements/plt/teachpack/htdp/draw.ss"))
      "(start 300 300) (if true (get-key-event) 3)"
      `((before-after ((hilite (start 300 300)))
                     ((hilite true)))
        (before-after-finished (true)
                               ((hilite (if true (get-key-event) 3)))
                               ((hilite (get-key-event))))
        (before-after ((hilite (get-key-event)))
                      ((hilite false)))
        (finished-stepping))))

  #;
  (t1 teachpack-hop-names
     (test-teachpack-sequence
      `((file "/Users/clements/plt/teachpack/htdp/draw.ss"))
      "(start 300 300) (define (a x y) (+ 3 4)) (if true (on-key-event a) 3)"
      `((before-after ((hilite (start 300 300)))
                      ((hilite true)))
        (before-after-finished (true (define (a x y) (+ 3 4)))
                               ((hilite (if true (on-key-event a) 3)))
                               ((hilite (on-key-event a))))
        (before-after ((hilite (on-key-event a)))
                      ((hilite true)))
        (finished-stepping))))

  #;
  (t1 teachpack-web-interaction
  (test-teachpack-sequence
   `(htdp/servlet2)
   "(define (adder go) (inform (number->string (+ (single-query (make-number \"enter 10\")) (single-query (make-number \"enter 20\")))))) (adder true)"
   `((before-after-finished ((define (adder go) (inform (number->string (+ (single-query (make-number "enter 10")) (single-query (make-number "enter 20")))))))
                            ((hilite (adder true)))
                            ((hilite (inform (number->string (+ (single-query (make-number "enter 10")) (single-query (make-number "enter 20"))))))))
     (before-after ((inform (number->string (+ (single-query (hilite (make-number "enter 10"))) (single-query (make-number "enter 20")))))) ; this step looks wrong wrong wrong.
                   ((inform (number->string (+ (single-query (hilite (make-numeric "enter 10"))) (single-query (make-number "enter 20")))))))
     (before-after ((inform (number->string (+ (hilite (single-query (make-numeric "enter 10"))) (single-query (make-number "enter 20"))))))
                   ((inform (number->string (+ (hilite 10) (single-query (make-number "enter 20")))))))
     (before-after ((inform (number->string (+ 10 (single-query (hilite (make-number "enter 20")))))))
                   ((inform (number->string (+ 10 (single-query (hilite (make-numeric "enter 20"))))))))
     (before-after ((inform (number->string (+ 10 (hilite (single-query (make-numeric "enter 20")))))))
                   ((inform (nut
                             mber->string (+ 10 (hilite 20))))))
     (before-after ((inform (number->string (hilite (+ 10 20)))))
                   ((inform (number->string (hilite 30)))))
     (before-after ((inform (hilite (number->string 30))))
                   ((inform (hilite "30"))))
     (before-after ((hilite (inform "30")))
                   ((hilite true)))
     (finished-stepping))))


  ;;;;;;;;;;;;;
  ;;
  ;;  Set!
  ;;
  ;;;;;;;;;;;;;

  (t1 top-ref-to-lifted
     (test-advanced-sequence "(define a (local ((define i1 0) (define (i2 x) i1)) i2)) (+ 3 4)"
                             (let ([defs `((define i1_0 0) (define (i2_0 x) i1_0))])
                               `((before-after ((define a (hilite (local ((define i1 0) (define (i2 x) i1)) i2))))
                                               ((hilite (define i1_0 0)) (hilite (define (i2_0 x) i1_0)) (define a (hilite i2_0))))
                                 (before-after (,@defs (define a (hilite i2_0)))
                                               (,@defs (define a (hilite (lambda (x) i1_0)))))
                                 (before-after (,@defs (define a (lambda (x) i1_0)) (hilite (+ 3 4)))
                                               (,@defs (define a (lambda (x) i1_0)) (hilite 7)))))))

  (t1 set!
     (test-advanced-sequence "(define a 3) (set! a (+ 4 5)) a"
                             `((before-after ((define a 3) (set! a (hilite (+ 4 5))))
                                             ((define a 3) (set! a (hilite 9))))
                               (before-after ((hilite (define a 3)) (hilite (set! a 9)))
                                             ((hilite (define a 9)) (hilite (void))))
                               (before-after ((define a 9) (void) (hilite a))
                                             ((define a 9) (void) (hilite 9)))
                               (finished-stepping))))

  (t1 local-set!
     (test-advanced-sequence
      "(define a (local ((define in 14) (define (getter dc) in) (define (modder n) (set! in n))) modder)) (a 15)"
      (let ([d1 `(define in_0 14)]
            [d2 `(define (getter_0 dc) in_0)]
            [d3 `(define (modder_0 n) (set! in_0 n))]
            [d4 `(define a (lambda (n) (set! in_0 n)))])
        `((before-after ((define a (hilite (local ((define in 14) (define (getter dc) in) (define (modder n) (set! in n))) modder))))
                        ((hilite ,d1) (hilite ,d2) (hilite ,d3) (define a (hilite modder_0))))
          (before-after (,d1 ,d2 ,d3 (define a (hilite modder_0)))
                        (,d1 ,d2 ,d3 (define a (hilite (lambda (n) (set! in_0 n))))))
          (before-after (,d1 ,d2 ,d3 ,d4 ((hilite a) 15))
                        (,d1 ,d2 ,d3 ,d4 ((hilite (lambda (n) (set! in_0 n))) 15)))
          (before-after (,d1 ,d2 ,d3 ,d4 (hilite ((lambda (n) (set! in_0 n)) 15)))
                        (,d1 ,d2 ,d3 ,d4 (hilite (set! in_0 15))))
          (before-after ((hilite ,d1) ,d2 ,d3 , d4 (hilite (set! in_0 15)))
                        ((hilite (define in_0 15)) ,d2 ,d3 ,d4 (void)))
          (finished-stepping)))))

  ;;;;;;;;;;;
  ;;
  ;;  BEGIN
  ;;
  ;;;;;;;;;;;

  (t1 simple-begin
     (test-advanced-sequence "(+ 3 (begin 4 5))"
       `((before-after ((+ 3 (hilite (begin 4 5))))
                       ((+ 3 (hilite 5))))
         (before-after ((hilite (+ 3 5)))
                       ((hilite 8)))
         (finished-stepping))))

  (t1 begin-onlyvalues
     (test-advanced-sequence "(+ 3 (begin 4 5 6))"
       `((before-after ((+ 3 (hilite (begin 4 5 6))))
                       ((+ 3 (hilite (begin 5 6)))))
         (before-after ((+ 3 (hilite (begin 5 6))))
                       ((+ 3 (hilite 6))))
         (before-after ((hilite (+ 3 6)))
                       ((hilite 9))))))

  (t1 begin
     (test-advanced-sequence "(begin (+ 3 4) (+ 4 5) (+ 9 8))"
      `((before-after ((begin (hilite (+ 3 4)) (+ 4 5) (+ 9 8)))
                      ((begin (hilite 7) (+ 4 5) (+ 9 8))))
        (before-after ((hilite (begin 7 (+ 4 5) (+ 9 8))))
                      ((hilite (begin (+ 4 5) (+ 9 8)))))
        (before-after ((begin (hilite (+ 4 5)) (+ 9 8)))
                      ((begin (hilite 9) (+ 9 8))))
        (before-after ((hilite (begin 9 (+ 9 8))))
                      ((hilite (+ 9 8))))
        (before-after ((hilite (+ 9 8)))
                      ((hilite 17)))
        (finished-stepping))))
  
  (t begin-let-bug test-advanced-sequence
     (let ([x 3]) (begin 3 4))
     :: {(let ([x 3]) (begin 3 4))}
     -> {(define x_0 3)} {(begin 3 4)}
     :: (define x_0 3) {(begin 3 4)}
     -> (define x_0 3) 4)

  (t1 empty-begin
     (test-advanced-sequence "(begin)"
      `((error "begin: expected a sequence of expressions after `begin', but nothing's there"))))

  ;;;;;;;;;;;;
  ;;
  ;; BEGIN0
  ;;
  ;;;;;;;;;;;;
  
  (t1 empty-begin0
      (test-advanced-sequence "(begin0)"
       `((error "begin0: expected a sequence of expressions after `begin0', but nothing's there"))))
  
  (t1 trivial-begin0
     (test-advanced-sequence "(begin0 3)"
      `((before-after ((hilite (begin0 3)))
                           ((hilite 3)))
        (finished-stepping))))
  
  ;; urg... the first element of a begin0 is in tail position if there's only one.
  (t1 one-item-begin0
     (test-advanced-sequence "(begin0 (+ 3 4))"
       `((before-after ((hilite (begin0 (+ 3 4))))
                            ((hilite (+ 3 4))))
         (before-after ((hilite (+ 3 4)))
                            ((hilite 7)))
         (finished-stepping))))
  
  (t begin0-onlyvalues test-advanced-sequence
     (begin0 3 4 5)
     :: {(begin0 3 4 5)}
     -> {(begin0 3 5)}
     -> {3})
  
  (t begin0 test-advanced-sequence
     (begin0 (+ 3 4) (+ 4 5) (+ 6 7))
     :: (begin0 {(+ 3 4)} (+ 4 5) (+ 6 7))
     -> (begin0 {7} (+ 4 5) (+ 6 7))
     :: (begin0 7 {(+ 4 5)} (+ 6 7))
     -> (begin0 7 {9} (+ 6 7))
     :: {(begin0 7 9 (+ 6 7))}
     -> {(begin0 7 (+ 6 7))}
     :: (begin0 7 {(+ 6 7)})
     -> (begin0 7 {13})
     :: {(begin0 7 13)}
     -> {7})
  

  ;; LAZY.SS:

  (t lazy1 test-lazy-sequence
     (! (+ 3 4))
     :: 3 -> 3
     :: 3 -> 3
     :: 3 -> 3
     :: {(! (+ 3 4))} -> {7})

  (t lazy2 test-lazy-sequence
     (+ (+ 3 4) 5)
     :: (+ {(+ 3 4)} 5)
     -> (+ {7} 5)
     :: {(+ 7 5)}
     -> {12})

  (t lazy3 test-lazy-sequence
     ((lambda (x y) (* x x)) (+ 1 2) (+ 3 4))
     :: {((lambda (x y) (* x x)) (+ 1 2) (+ 3 4))}
     -> {(* (+ 1 2) (+ 1 2))}
     :: (* {(+ 1 2)} {(+ 1 2)})
     -> (* {3} {3})
     :: {(* 3 3)}
     -> {9})

  #;
  (t1 teachpack-callbacks
     (test-teachpack-sequence
      "(define (f2c x) x) (convert-gui f2c)" `() ; placeholder
      ))

  
  ;; run whatever tests are enabled (intended for interactive use):
  (define (ggg)
    (parameterize (#;[disable-stepper-error-handling #t]
                   #;[display-only-errors #f]
                   #;[store-steps #f]
                   #;[show-all-steps #t])
      #;(run-tests '(check-expect check-within check-within-bad check-error) #;'(#;check-expect #;check-expect-2 check-within check-within-bad check-error))
      #;(run-tests '(teachpack-universe))
      (run-all-tests)))
  


