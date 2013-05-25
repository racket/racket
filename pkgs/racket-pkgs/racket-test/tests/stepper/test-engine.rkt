#lang scheme

(require stepper/private/shared
         stepper/private/model
         tests/utils/sexp-diff
         lang/run-teaching-program
         (only-in srfi/13 string-contains)
         scheme/contract
         "language-level-model.rkt")


;; A SIMPLE EXAMPLE OF USING THIS FRAMEWORK:

;; note that this example uses the abbreviation from test-abbrev; don't uncomment it!

#;
(let* ([defs1 `((define (a x) (+ x 5)) (define b a))]
       [defs2 (append defs1 `((define c a)))])
  (apply         ;; you can abstract over this application with a define-syntax
   run-one-test  
   (tt 'top-ref4                   ;; - the name of the test
       m:intermediate              ;; - the language level (or levels) to run in
       ,@defs1 (define c b) (c 3)  ;; - the expressions to test (everything up to the first ::)
       :: ,@defs1 (define c {b})   ;; - the steps; the '::' divides steps, repeated '->'s indicate
       -> ,@defs1 (define c {a})   ;;    that the 'before' of the second step is the 'after' of
       :: ,@defs2 ({c} 3)          ;;    the first one.  the curly braces indicate the hilighted sexp.
       -> ,@defs2 ({a} 3)
       :: ,@defs2 {(a 3)}
       -> ,@defs2 {(+ 3 5)}
       -> ,@defs2 {8})))




;; PARAMETERS THAT CONTROL TESTING

(provide test-directory
         display-only-errors
         show-all-steps
         disable-stepper-error-handling)

(define test-directory (find-system-path 'temp-dir))

;; use this parameter to suppress output except in error cases:
(define display-only-errors (make-parameter #f))

;; use this parameter to show successful steps as well as unsuccessful ones:
(define show-all-steps (make-parameter #f))

;; use this parameter to prevent the stepper from capturing errors
;; (so that you can take advantage of DrRacket's error reporting)
(define disable-stepper-error-handling (make-parameter #f))


;; DATA DEFINITIONS:

;; a step is one of 
;; - `(before-after ,before ,after) where before and after are sexp-with-hilite's
;; - `(error ,err-msg) where err-msg is a string
;; - `(before-error ,before ,err-msg) where before is an sexp-with-hilite and err-msg is a string
;; - `(finished-stepping)
;; or
;; - `(ignore)
(define (step? sexp)
  (match sexp
    [(list 'before-after before after) #t]
    [(list 'error (? string? msg)) #t]
    [(list 'before-error before (? string? msg)) #t]
    [(list 'finished-stepping) #t]
    [(list 'ignore) #t]
    [else #f]))

;; a model-or-models is one of
;; - an ll-model, or
;; - (listof ll-model?)
(define model-or-models/c (or/c ll-model? (listof ll-model?)))

;; THE METHOD THAT RUNS A TEST:

(provide/contract [run-one-test (symbol? model-or-models/c string? (listof step?) (listof (list/c string? string?)) . -> . boolean?)])
;; run-one-test : symbol? model-or-models? string? steps? extra-files -> boolean?

;; the ll-model determines the behavior of the stepper w.r.t. "language-level"-y things:
;; how should values be rendered, should steps be displayed (i.e, will the input & output
;; steps look just the same), etc. If 

;; the string contains a program to be evaluated. The string is an ironclad if blunt way
;; of ensuring that the program has no syntax information associated with it.

;; the steps lists the desired steps.  The easiest way to understand these is probably just to 
;; read the code for the comparison given in "compare-steps", below.

;; the extra-files contain a list of other files that must occur in the same directory. 

;; run the named test, return #t if a failure occurred during the test.

(define (run-one-test name models exp-str expected-steps extra-files)
  (unless (display-only-errors)
    (printf "running test: ~v\n" name))
  (let ([error-has-occurred-box (box #f)])
    (test-sequence/many models exp-str expected-steps extra-files error-has-occurred-box)
    (if (unbox error-has-occurred-box)
        (begin (eprintf "...Error has occurred during test: ~v\n" name)
               #f)
        #t)))

(provide/contract [string->expanded-syntax-list (-> ll-model? string? (listof syntax?))])
(define (string->expanded-syntax-list ll-model exp-str)
  (define expander-thunk ((string->expander-thunk ll-model exp-str)))
  (let loop ()
    (define next (expander-thunk))
    (cond [(eof-object? next) '()]
          [else (cons next (loop))])))





;; test-sequence/many : model-or-models/c string? steps? -> (void)
;; run a given test through a bunch of language models (or just one).
(define (test-sequence/many models exp-str expected-steps extra-files error-box)
  (cond [(list? models)(for-each (lambda (model) (test-sequence model exp-str expected-steps extra-files error-box))
                                 models)]
        [else (test-sequence models exp-str expected-steps extra-files error-box)]))

(define port null)

;; test-sequence : ll-model? string? steps? extra-files? -> (void)
;; given a language model and an expression and a sequence of steps,
;; check to see whether the stepper produces the desired steps
(define (test-sequence the-ll-model exp-str expected-steps extra-files error-box)
  (parameterize ([current-directory test-directory])
    (for ([f (in-list extra-files)])
      (display-to-file (second f) (first f) #:exists 'truncate))
    (define expander-thunk (string->expander-thunk the-ll-model exp-str))
    (match the-ll-model
      [(struct ll-model (namespace-spec render-settings enable-testing?))
       (unless (display-only-errors)
         (printf "testing string: ~v\n" exp-str))
       (test-sequence/core render-settings expander-thunk expected-steps error-box)
       (close-input-port port)
       (delete-file "stepper-test")
       (for ([f (in-list extra-files)])
         (delete-file (first f)))])))


;; given a language level model and a string, produce a thunk that returns
;; syntax objects expanded in the given language:
;; EFFECT: creates a file called "stepper-test" in test-directory
(define (string->expander-thunk the-ll-model exp-str)
  (parameterize ([current-directory test-directory])
    (match the-ll-model
      [(struct ll-model (namespace-spec render-settings enable-testing?))
       (let ([filename "stepper-test"])
         (display-to-file exp-str filename #:exists 'truncate)
         (set! port (open-input-file "stepper-test"))
         (let* (#;[port (open-input-file filename)]
                [module-id (gensym "stepper-module-name-")])
           ;; thunk this so that syntax errors happen within the error handlers:
           (lambda () (expand-teaching-program port read-syntax namespace-spec '() module-id enable-testing?))))])))

;; test-sequence/core : render-settings? boolean? syntax? steps?
;; this is a front end for calling the stepper's "go"; the main 
;; responsibility here is to fake the behavior of DrRacket and collect the
;; resulting steps.
(define (test-sequence/core render-settings expanded-thunk expected-steps error-box)
  (let* ([current-error-display-handler (error-display-handler)]
         [all-steps
          (append expected-steps '((finished-stepping)))]
         ;; the values of certain parameters aren't surviving; create 
         ;; lexical bindings for them:
         [current-show-all-steps (show-all-steps)]
         [current-display-only-errors (display-only-errors)]
         [receive-result
          (lambda (result)
            (if (null? all-steps)
                (warn error-box
                      'test-sequence
                      "ran out of expected steps. Given result: ~v" result)
                (begin
                  (if (compare-steps result (car all-steps) error-box)
                      (when (and current-show-all-steps (not current-display-only-errors))
                        (printf "test-sequence: steps match for expected result: ~v\n"
                                (car all-steps)))
                      (warn error-box
                            'test-sequence
                            "steps do not match\n   given: ~v\nexpected: ~v"
                            (show-result result error-box)
                            (car all-steps)))
                  (set! all-steps (cdr all-steps)))))]
         [iter-caller
          (lambda (init iter)
            (init)
            (call-iter-on-each (expanded-thunk) iter))])
    (let/ec escape
      (parameterize ([error-escape-handler (lambda () (escape (void)))])
        (go iter-caller receive-result render-settings
            #:disable-error-handling? (disable-stepper-error-handling))))
    (error-display-handler current-error-display-handler)))

(define-namespace-anchor n-anchor)

;; it seems to be okay to use the same namespace for all of the tests... 
(define test-namespace (make-base-namespace))
(namespace-attach-module (namespace-anchor->empty-namespace n-anchor)
                         'mzlib/pconvert-prop
                         test-namespace)
(namespace-attach-module (namespace-anchor->empty-namespace n-anchor)
                         'racket/private/promise
                         test-namespace)
(parameterize ([current-namespace test-namespace])
    (namespace-require 'test-engine/racket-tests)
    ;; make the test engine happy by adding a binding for test~object:
    (namespace-set-variable-value! 'test~object #f))

;; call-iter-on-each : (-> syntax?) (syntax? (-> 'a) -> 'a) -> void/c
;; call the given iter on each syntax in turn (iter bounces control
;; back to us by calling the followup-thunk).
(define (call-iter-on-each stx-thunk iter)
  (parameterize ([current-namespace test-namespace])
    (let iter-loop ()
      (let* ([next (stx-thunk)]
             [followup-thunk (if (eof-object? next) 
                                 void
                                 iter-loop)]
             [expanded (expand next)])
        ;;(printf "~v\n" expanded)
        (iter expanded followup-thunk)))))


(define (warn error-box who fmt . args)
  (set-box! error-box #t)
  (eprintf "~a: ~a\n" who (apply format fmt args)))


;; (-> step-result? sexp? boolean?)
(define (compare-steps actual expected error-box)
  (match expected
    [`(before-after ,before ,after)
     (and (before-after-result? actual)
          (andmap (lambda (fn expected name)
                    (unless (list? (fn actual))
                      (warn error-box
                            'compare-steps "not a list: ~v"
                            (syntax->hilite-datum (fn actual))))
                    (noisy-equal? (map syntax->hilite-datum
                                       (fn actual))
                                  expected
                                  name
                                  error-box))
                  (list before-after-result-pre-exps
                        before-after-result-post-exps)
                  (list before after)
                  (list 'before 'after)))]
    [`(error ,err-msg)
     (and (error-result? actual)
          (string-contains (error-result-err-msg actual) err-msg))]
    [`(before-error ,before ,err-msg)
     (and (before-error-result? actual)
          (and (noisy-equal? (map syntax->hilite-datum
                                  (before-error-result-pre-exps actual))
                             before
                             'before
                             error-box)
               (equal? err-msg (before-error-result-err-msg actual))))]
    [`(finished-stepping) (finished-stepping? actual)]
    [`(ignore) (warn error-box 
                     'compare-steps "ignoring one step") #t]
    [else (begin (warn error-box 
                       'compare-steps
                       "unexpected expected step type: ~v" expected)
                 #f)]))



;; used to display results in an error message
(define (show-result r error-box)
  (if (before-after-result? r)
      (list 'before-after-result
            (map (lambda (fn)
                   (unless (list? (fn r))
                     (warn error-box 
                           'show-result "not a list: ~v"
                           (syntax->hilite-datum (fn r))))
                   (map syntax->hilite-datum
                        (fn r)))
                 (list before-after-result-pre-exps
                       before-after-result-post-exps)))
      r))

;; noisy-equal? : (any any . -> . boolean)
;; like equal?, but prints a noisy error message
(define (noisy-equal? actual expected name error-box)
  (if (equal? actual expected)
      #t
      (begin (warn error-box 'not-equal?
                   "~.s:\nactual:   ~e =/= \nexpected: ~e\n  here's the diff: ~e" name actual expected (sexp-diff actual expected))
             #f)))




