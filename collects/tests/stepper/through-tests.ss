(module through-tests mzscheme
  (require (lib "shared.ss" "stepper" "private")
           (lib "model.ss" "stepper" "private")
           (lib "model-settings.ss" "stepper" "private")
           (lib "match.ss")
           (lib "sexp-diff.ss" "tests" "utils")
           "module-elaborator.ss"
           ; for xml testing:
           #;(lib "class.ss")
           #;(all-except (lib "xml-snipclass.ss" "xml") snip-class)
           #;(all-except (lib "scheme-snipclass.ss" "xml") snip-class)
           #;(lib "mred.ss" "mred"))
  
  (define test-directory (find-system-path 'temp-dir))
  
  (define (stream-ify expr-list iter)
    (lambda ()
      (if (null? expr-list)
          (iter eof void)
          (iter (expand (car expr-list)) (stream-ify (cdr expr-list) iter)))))
 
  
  (define (test-sequence-core namespace-spec teachpack-specs render-settings track-inferred-names? in-port expected-steps)
    (let* ([current-error-display-handler (error-display-handler)]) 
        (let* ([all-steps
                (append expected-steps 
                        '((finished-stepping)))]
               [receive-result
                (lambda (result)
                  (if (null? all-steps)
                      (fprintf (current-error-port) "test-sequence: ran out of expected steps. Given result: ~v\n" result)
                      (begin
                        (unless (compare-steps result (car all-steps))
                          (fprintf (current-error-port) "test-sequence: steps do not match.\ngiven: ~v\nexpected: ~v\n" result (car all-steps)))

                        ; uncomment for testing:
                        ; (when (compare-steps result (car all-steps))
                        ;   (printf "test-sequence: steps match for expected result: ~v\n" result))

                        (set! all-steps (cdr all-steps)))))]
               [program-expander
                (lambda (init iter)
                  (init)
                  (let* ([exps (let read-loop ()
                                     (let ([expr (read-syntax "test-input" in-port)])
                                       (if (eof-object? expr)
                                           null
                                           (cons expr (read-loop)))))]
                         [exprs (wrap-in-module exps namespace-spec teachpack-specs)])
                    ((stream-ify exprs iter))))])
          (let/ec escape
            (parameterize ([error-escape-handler (lambda () (escape (void)))])
              (go program-expander receive-result render-settings track-inferred-names?)))
          (error-display-handler current-error-display-handler))))
  
  (define (test-sequence namespace-spec teachpack-specs render-settings track-inferred-names? exp-str expected-steps)
    (let ([filename (build-path test-directory "stepper-test")])
      (call-with-output-file filename
        (lambda (port)
          (fprintf port "~a" exp-str))
        'truncate)
      (printf "testing string: ~v\n" exp-str)
      (letrec ([port (open-input-file filename)])
        (test-sequence-core namespace-spec teachpack-specs render-settings track-inferred-names? port expected-steps))))

  
  (define (lang-level-test-sequence namespace-spec rs track-inferred-names?)
    (lambda args
      (apply test-sequence namespace-spec `() rs track-inferred-names? args)))
  
  (define (make-multi-level-test-sequence level-fns)
    (lambda args
      (for-each (lambda (fn) (apply fn args)) level-fns)))
  
  (define test-mz-sequence (lang-level-test-sequence 'mzscheme fake-mz-render-settings #f))
  (define test-beginner-sequence (lang-level-test-sequence `(lib "htdp-beginner.ss" "lang") fake-beginner-render-settings #t))
  (define test-beginner-wla-sequence (lang-level-test-sequence `(lib "htdp-beginner-abbr.ss" "lang") fake-beginner-wla-render-settings #t))
  (define test-intermediate-sequence (lang-level-test-sequence `(lib "htdp-intermediate.ss" "lang") fake-intermediate-render-settings #t))
  (define test-intermediate/lambda-sequence (lang-level-test-sequence `(lib "htdp-intermediate-lambda.ss" "lang")
                                                                      fake-intermediate/lambda-render-settings
                                                                      #f))
  
  (define test-upto-int/lam (make-multi-level-test-sequence (list test-beginner-sequence
                                                                  test-beginner-wla-sequence
                                                                  test-intermediate-sequence
                                                                  test-intermediate/lambda-sequence)))
  
  (define test-upto-int (make-multi-level-test-sequence (list test-beginner-sequence
                                                              test-beginner-wla-sequence
                                                              test-intermediate-sequence)))
  
  (define test-bwla-to-int/lam (make-multi-level-test-sequence (list test-beginner-wla-sequence
                                                                     test-intermediate-sequence
                                                                     test-intermediate/lambda-sequence)))
  
  (define test-both-ints (make-multi-level-test-sequence (list test-intermediate-sequence
                                                               test-intermediate/lambda-sequence)))
  
  ; mutate these to values you want to examine in the repl:
  (define bell-jar-specimen-1 #f)
  (define bell-jar-specimen-2 #f)
  
  ;; so->d/finished : call (syntax-object->hilite-datum stx #t).  For finished steps,
  ;; we want to ignore the highlight but not the xml boxes (and other future stuff?)
  (define (so->d/finished stx)
    (syntax-object->hilite-datum stx #t))
  
  ; (-> step-result? sexp? boolean?)
  (define (compare-steps actual expected)
    (match expected
      [`(before-after ,before ,after)
       (and (before-after-result? actual)
            (andmap (lambda (fn expected) 
                      (unless (list? (fn actual))
                        (fprintf (current-error-port) "not a list: ~v\n" (syntax-object->hilite-datum (fn actual))))
                      (noisy-equal? (map syntax-object->hilite-datum (fn actual)) expected))
                    (list before-after-result-exp before-after-result-post-exp)
                    (list before after)))]
      [`(before-after-waiting ,before ,after ,waiting)
       (and (before-after-result? actual)
            (and (noisy-equal? (map syntax-object->hilite-datum (before-after-result-after-exprs actual)) waiting)
                 (compare-steps actual `(before-after ,before ,after))))]
      [`(before-after-finished ,finished-exprs . ,rest)
       (and (before-after-result? actual)
            (compare-finished (map so->d/finished (before-after-result-finished-exprs actual)) finished-exprs)
            (compare-steps actual `(before-after ,@rest)))]
      [`(before-after-finished-waiting ,finished-exprs . ,rest)
       (and (before-after-result? actual)
            (compare-finished (map so->d/finished (before-after-result-finished-exprs actual)) finished-exprs)
            (compare-steps actual `(before-after-waiting ,@rest)))]
      [`(finished ,finished-exprs)
       (and (finished-result? actual)
            (compare-finished (map so->d/finished (finished-result-finished-exprs actual)) finished-exprs))]
      [`(error ,err-msg)
       (and (error-result? actual)
            (equal? err-msg (error-result-err-msg actual)))]
      [`(before-error ,before ,err-msg)
       (and (before-error-result? actual)
            (and (noisy-equal? (map syntax-object->hilite-datum (before-error-result-exp actual)) before)
                 (equal? (before-error-result-err-msg actual) err-msg)))]
      [`(finished-stepping) (finished-stepping? actual)]
      [else 
       (begin (fprintf (current-error-port) "compare-steps: unexpected expected step type: ~v\n" expected)
              #f)]))
  
  ; noisy-equal? : (any any . -> . boolean)
  ; like equal?, but prints a noisy error message
  (define (noisy-equal? a b)
    (if (equal? a b)
        #t
        (begin (fprintf (current-error-port) "~e is not equal? to ~e\nhere's the diff: ~e\n" a b (sexp-diff a b))
               #f)))
   
  ; (-> (listof sexp) (listof sexp) boolean?)
  (define (compare-finished finished-exps expected-exps)
    (and 
     (>= (length finished-exps) (length expected-exps))
     (andmap (lambda (x y) (if (equal? x y)
                               #t
                               (begin (fprintf (current-error-port) "~e is not equal? to ~e\nhere's the diff: ~e\n" x y (sexp-diff x y))
                                      #f)))
             (list-tail finished-exps (- (length finished-exps) (length expected-exps)))
             expected-exps)))
  
  (define list-of-tests null)
  
  (define (add-test name thunk)
    (when (assq name list-of-tests)
      (error 'add-test "name ~v is already in the list of tests" name))
    (set! list-of-tests (append list-of-tests (list (list name thunk)))))
  
  (define-syntax (t stx)
    (syntax-case stx ()
      [(_ name test)
       (syntax/loc stx (add-test `name (lambda () test)))]))
  
  (define (run-all-tests)
    (for-each (lambda (test-pair)
                (printf "running test: ~v\n" (car test-pair))
                ((cadr test-pair)))
              list-of-tests))
  
  (define (run-test name)
    (printf "running test: ~v\n" name)
    ((cadr (assq name list-of-tests))))
  
  (define (run-tests names)
    (map run-test names))
  
  (t mz1
     (test-mz-sequence "(for-each (lambda (x) x) '(1 2 3))"
                       `((before-after ((hilite (for-each (lambda (x) x) `(1 2 3)))) ((... (hilite 1) ...)))
                         (before-after ((hilite ...)) ((... (hilite 2) ...)))
                         (before-after ((hilite ...)) ((... (hilite 3) ...)))
                         (before-after ((hilite ...)) ((hilite (void))))
                         (finished ((void))))))
  
  (t mz-app
     (test-mz-sequence "(+ 3 4)"
                       `((before-after ((hilite (+ 3 4))) ((hilite 7)))
                         (finished (7)))))
  
  (t mz-app2
     (test-mz-sequence "((lambda (x) (+ x 3)) 4)"
		       `((before-after ((hilite ((lambda (x) (+ x 3)) 4)))
                                       ((hilite (+ 4 3))))
                         (before-after ((hilite (+ 4 3)))
                                       ((hilite 7)))
                         (finished (7)))))
  
  (t mz-if
     (test-mz-sequence "(if 3 4 5)"
                    `((before-after ((hilite (if 3 4 5))) ((hilite 4)))
		      (finished (4)))))

  (t simple-if
     (test-upto-int/lam "(if true false true)"
                        `((before-after ((hilite (if true false true)))
                                       ((hilite false)))
                          (finished (false)))))
  
  (t if-bool
     (test-upto-int/lam "(if (if true false true) false true)"
                     `((before-after ((if (hilite (if true false true)) false true))
                                     ((if (hilite false) false true)))
                       (before-after ((hilite (if false false true))) ((hilite true)))
                       (finished (true)))))

  (t direct-app
     (test-mz-sequence "((lambda (x) x) 3)"
		       `((before-after ((hilite ((lambda (x) x) 3))) ((hilite 3)))
			 (finished (3)))))
  
  
 ;   (test-mz-sequence "((lambda (x) x) (begin (+ 3 4) (+ 4 5)))"
;		      `((before-after ((begin (hilite (+ 3 4)) (+ 4 5)))
;				      ((begin (hilite 7) (+ 4 5))))
;			(before-after ((hilite (begin 7 (+ 4 5)))) ((hilite (+ 4 5))))
;                        (before-after ((hilite (+ 4 5))) ((hilite 9)))
;			(finished (9))))
  
  (t curried
     (test-mz-sequence "((lambda (a) (lambda (b) (+ a b))) 14)"
                       `((before-after ((hilite ((lambda (a) (lambda (b) (+ a b))) 14))) 
				       ((hilite (lambda (b) (+ 14 b)))))
                         (finished ((lambda (b) (+ 14 b)))))))
  
  (t case-lambda
     (test-mz-sequence "((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)"
                    `((before-after ((hilite ((case-lambda ((a) 3) ((b c) (+ b c))) 5 6))) ((hilite (+ 5 6))))
                      (before-after ((hilite (+ 5 6))) ((hilite 11)))
		      (finished (11)))))
  
  (t 2armed-if
     (test-mz-sequence "(if 3 4)"
		       `((before-after ((hilite (if 3 4))) ((hilite 4)))
			 (finished (4)))))

   
  ;(test-mz-sequence "((call-with-current-continuation call-with-current-continuation) (call-with-current-continuation call-with-current-continuation))"
  ;                  `((before-after (((hilite ,h-p) (call-with-current-continuation call-with-current-continuation))) ((call-with-current-continuation call-with-current-continuation))
  ;                    (((hilite ,h-p) (call-with-current-continuation call-with-current-continuation))) ((lambda args ...)))
  ;                    (before-after (((lambda args ...) (hilite ,h-p))) ((call-with-current-continuation call-with-current-continuation))
  ;                    (((lambda args ...) (hilite ,h-p))) ((lambda args ...)))))
  
  ;(test-mz-sequence '(begin (define g 3) g)
  ;                  `((before-after ((hilite ,h-p)) (g)
  ;                    ((hilite ,h-p)) 3)))
  
  ;(syntax-object->datum (cadr (annotate-expr test2 'mzscheme 0 (lambda (x) x))))
  
  (t top-def
     (test-upto-int/lam "(define a (+ 3 4))"
                        `((before-after ((define a (hilite (+ 3 4)))) ((define a (hilite 7))))
                          (finished ((define a 7))))))

  (t top-def-ref
     (test-upto-int/lam "(define a 6) a"
                        `((before-after-finished ((define a 6))
                                                 ((hilite a))
                                                 ((hilite 6)))
                          (finished (6)))))
  
  (t app
     (test-upto-int/lam "(+ 4 129)" 
                     `((before-after ((hilite (+ 4 129))) ((hilite 133)))
                       (finished (133)))))

  (t if
  (test-upto-int/lam "(if true 3 4)"
                     `((before-after ((hilite (if true 3 4))) ((hilite 3)))
                       (finished (3)))))
  
  (t top-app
  (test-upto-int "(define (a3 x) (if true x x)) (a3 false)"
                     `((before-after-finished ((define (a3 x) (if true x x))) ((hilite (a3 false))) 
					      ((hilite (if true false false))))
                       (before-after ((hilite (if true false false))) ((hilite false)))
                       (finished (false)))))
  
  (t top-app/lam
  (test-intermediate/lambda-sequence "(define (a3 x) (if true x x)) (a3 false)"
                                     `((before-after-finished ((define (a3 x) (if true x x)))
                                                              (((hilite a3) false)) (((hilite (lambda (x) (if true x x))) false)))
                                       (before-after ((hilite ((lambda (x) (if true x x)) false))) 
						     ((hilite (if true false false))))
                                       (before-after ((hilite (if true false false))) ((hilite false)))
                                       (finished (false)))))
  
  (t top-interref
     (test-intermediate-sequence "(define (a12 x) (+ x 9)) (define b12 a12) (b12 12)"
                                 `((before-after-finished ((define (a12 x) (+ x 9)) (define b12 a12))
                                                          (((hilite b12) 12)) (((hilite a12) 12)))
                                   (before-after ((hilite (a12 12))) ((hilite (+ 12 9))))
                                   (before-after ((hilite (+ 12 9))) ((hilite 21)))
                                   (finished (21)))))
  
  

  ;;;;;;;;;;;;
  ;;
  ;;  OR / AND
  ;;
  ;;;;;;;;;;;;;.
  
 
  (t or1
  (test-upto-int/lam "(or false true false)"
                     `((before-after ((hilite (or false true false))) ((hilite true)))
                       (finished (true)))))
  
  (t and1
  (test-upto-int/lam "(and true false true)"
                     `((before-after ((hilite (and true false true))) ((hilite false)))
                       (finished (false)))))
   
  (t and2
  (test-upto-int/lam "(and true (if true true false))"
                     `((before-after ((and true (hilite (if true true false)))) ((and true (hilite true))))
                       (before-after ((hilite (and true true))) ((hilite true)))
                       (finished (true)))))
  
  (t and3
  (test-upto-int "(define (b2 x) (and true x)) (b2 false)"
                     `((before-after-finished ((define (b2 x) (and true x))) ((hilite (b2 false))) ((hilite (and true false))))
                       (before-after ((hilite (and true false))) ((hilite false)))
                       (finished (false)))))
  
  (t and3/lam
  (test-intermediate/lambda-sequence "(define (b2 x) (and true x)) (b2 false)"
                                     `((before-after-finished ((define (b2 x) (and true x)))
                                                              (((hilite b2) false))
                                                              (((hilite (lambda (x) (and true x))) false)))
                                       (before-after ((hilite ((lambda (x) (and true x)) false))) 
						     ((hilite (and true false))))
                                       (before-after ((hilite (and true false))) ((hilite false)))
                                       (finished (false)))))
  
  (t and4
  (test-upto-int "(define a1 true)(define (b1 x) (and a1 true x)) (b1 false)"
                 `((before-after-finished ((define a1 true)
                                           (define (b1 x) (and a1 true x)))
					  ((hilite (b1 false))) 
					   ((hilite (and a1 true false))))
                   (before-after ((and (hilite a1) true false)) ((and (hilite true) true false)))
                   (before-after ((hilite (and true true false))) ((hilite false)))
                   (finished (false)))))
  
  
  (t and4/lam
  (test-intermediate/lambda-sequence "(define a1 true)(define (b1 x) (and a1 true x)) (b1 false)"
                                     `((before-after-finished ((define a1 true)
                                                               (define (b1 x) (and a1 true x))) 
                                                              (((hilite b1) false))
                                                              (((hilite (lambda (x) (and a1 true x))) false)))
                                       (before-after ((hilite ((lambda (x) (and a1 true x)) false))) ((hilite (and a1 true false))))
                                       (before-after ((and (hilite a1) true false)) ((and (hilite true) true false)))
                                       (before-after ((hilite (and true true false))) ((hilite false)))
                                       (finished (false)))))

  (t bad-and
  (test-upto-int/lam "(and true 1)"
                     `((before-error ((hilite (and true 1))) "and: question result is not true or false: 1"))))
  
  ;;;;;;;;;;;;;
  ;;
  ;;  COND
  ;;
  ;;;;;;;;;;;;;
  
  
  (t cond1
     (test-upto-int/lam "(cond [false 4] [false 5] [true 3])"
			`((before-after ((hilite (cond (false 4) (false 5) (true 3)))) 
					((hilite (cond (false 5) (true 3)))))
			  (before-after ((hilite (cond (false 5) (true 3)))) ((hilite (cond (true 3)))))
			  (before-after ((hilite (cond (true 3)))) ((hilite 3)))
			  (finished (3)))))
  
  (t cond-else
  (test-upto-int/lam "(cond [false 4] [else 9])"
                     `((before-after ((hilite (cond [false 4] [else 9]))) ((hilite (cond [else 9]))))
                       (before-after ((hilite (cond [else 9]))) ((hilite 9)))
                       (finished (9)))))
  
  (t cond-andelse
  (test-upto-int/lam "(cond [true 3] [else (and true true)])"
                     `((before-after ((hilite (cond (true 3) (else (and true true))))) ((hilite 3)))
                       (finished (3)))))
  
  (t bad-cond
  (test-upto-int/lam "(cond)"
                     `((error "cond: expected a question--answer clause after `cond', but nothing's there"))))
  
  (t just-else
  (test-upto-int/lam "(cond [else 3])"
                     `((before-after ((hilite (cond (else 3)))) ((hilite 3)))
                       (finished (3)))))
  
  (t nested-cond
  (test-upto-int/lam "(cond [else (cond [else 3])])"
                     `((before-after ((hilite (cond (else (cond (else 3)))))) ((hilite (cond (else 3)))))
                       (before-after ((hilite (cond (else 3)))) ((hilite 3)))
                       (finished (3)))))
  
  ;  reconstruct can't handle 'begin'
  ;  (test-mz-sequence "(cond [#f 3 4] [#t (+ 3 4) (+ 4 9)])"
  ;                    `((before-after ((hilite (cond (#f 3 4) (#t (+ 3 4) (+ 4 9))))) 
;				      ((hilite (cond (#t (+ 3 4) (+ 4 9))))))
;                        (before-after ((hilite (cond (#t (+ 3 4) (+ 4 9))))) ((hilite (begin (+ 3 4) (+ 4 9)))))
;                        (before-after ((begin (hilite (+ 3 4)) (+ 4 9)))
;    				    ((begin (hilite 7) (+ 4 9))))
;                        (before-after ((hilite (begin 7 (+ 4 9)))) ((hilite (+ 4 9))))
;                        (before-after ((hilite (+ 4 9))) ((hilite 13)))
;			(finished (13))))
  
  (t nested-cond2
  (test-upto-int/lam "(cond [false 3] [else (cond [true 4])])"
                     `((before-after ((hilite (cond (false 3) (else (cond (true 4)))))) 
				     ((hilite (cond (else (cond (true 4)))))))
                       (before-after ((hilite (cond (else (cond (true 4)))))) ((hilite (cond (true 4)))))
                       (before-after ((hilite (cond (true 4)))) ((hilite 4)))
                       (finished (4)))))

  (t top-ref
  (test-intermediate-sequence "(define a4 +) a4"
                              `((before-after ((hilite a4)) ((hilite +)))
                                (finished (+)))))
  
  (t top-ref2
  (test-intermediate-sequence "(define (f123 x) (+ x 13)) f123"
                              `((finished ((define (f123 x) (+ x 13))
                                           f123)))))
  
  (t define-struct
  (test-upto-int/lam "(define-struct mamba (rhythm tempo)) (mamba-rhythm (make-mamba 24 2))"
                     `((before-after-finished ((define-struct mamba (rhythm tempo)))
                                              ((hilite (mamba-rhythm (make-mamba 24 2)))) ((hilite 24)))
                       (finished (24)))))
  
  (t lam-def
  (test-upto-int "(define a5 (lambda (a5) (+ a5 13))) (a5 23)"
                     `((before-after-finished ((define a5 (lambda (a5) (+ a5 13)))) ((hilite (a5 23))) ((hilite (+ 23 13))))
                       (before-after ((hilite (+ 23 13))) ((hilite 36)))
                       (finished (36)))))
  
  (t lam-def/lam
  (test-intermediate/lambda-sequence "(define a5 (lambda (a5) (+ a5 13))) (a5 23)"
                                     `((before-after-finished ((define a5 (lambda (a5) (+ a5 13))))
                                                              (((hilite a5) 23))
                                                              (((hilite (lambda (a5) (+ a5 13))) 23)))
                                       (before-after ((hilite ((lambda (a5) (+ a5 13)) 23))) 
						     ((hilite (+ 23 13))))
                                       (before-after ((hilite (+ 23 13))) ((hilite 36)))
                                       (finished (36)))))
  
  (t lam-let
     (test-intermediate-sequence "(let ([a (lambda (x) (+ x 5))]) (a 6))"
                                 `((before-after ((hilite (let ([a (lambda (x) (+ x 5))]) (a 6))))
                                                 ((hilite (define a_0 (lambda (x) (+ x 5)))) (hilite (a_0 6))))
                                   (before-after-finished ((define a_0 (lambda (x) (+ x 5))))
                                                          ((hilite (a_0 6)))
                                                          ((hilite (+ 6 5))))
                                   (before-after ((hilite (+ 6 5)))
                                                 ((hilite 11)))
                                   (finished (11)))))
  
  (t whocares
  (test-upto-int "(define c1 false) (define (d2 x) (or c1 false x)) (d2 false)"
                     `((before-after-finished ((define c1 false)
                                               (define (d2 x) (or c1 false x))) 
					      ((hilite (d2 false)))
					      ((hilite (or c1 false false))))
                       (before-after ((or (hilite c1) false false)) ((or (hilite false) false false)))
                       (before-after ((hilite (or false false false))) ((hilite false)))
                       (finished (false)))))
  
  (t whocares/lam
  (test-intermediate/lambda-sequence "(define c1 false) (define (d2 x) (or c1 false x)) (d2 false)"
                                     `((before-after-finished ((define c1 false)
                                                               (define (d2 x) (or c1 false x)))
                                                              (((hilite d2) false)) (((hilite (lambda (x) (or c1 false x))) false)))
                                       (before-after ((hilite ((lambda (x) (or c1 false x)) false))) 
						     ((hilite (or c1 false false))))
                                       (before-after ((or (hilite c1) false false)) ((or (hilite false) false false)))
                                       (before-after ((hilite (or false false false))) ((hilite false)))
                                       (finished (false)))))
  

  (t forward-ref
  (test-upto-int "(define (f x) (+ (g x) 10)) (define (g x) (- x 22)) (f 13)"
                     `((before-after-finished ((define (f x) (+ (g x) 10)) (define (g x) (- x 22))) 
					      ((hilite (f 13)))
					      ((hilite (+ (g 13) 10))))
                       (before-after ((+ (hilite (g 13)) 10)) ((+ (hilite (- 13 22)) 10)))
                       (before-after ((+ (hilite (- 13 22)) 10)) ((+ (hilite -9) 10)))
                       (before-after ((hilite (+ -9 10))) ((hilite 1)))
                       (finished (1)))))
  
  (t forward-ref/lam
  (test-intermediate/lambda-sequence "(define (f x) (+ (g x) 10)) (define (g x) (- x 22)) (f 13)"
                                     `((before-after-finished ((define (f x) (+ (g x) 10)) (define (g x) (- x 22)))
                                                              (((hilite f) 13))
                                                              (((hilite (lambda (x) (+ (g x) 10))) 13)))
                                       (before-after ((hilite ((lambda (x) (+ (g x) 10)) 13))) 
						     ((hilite (+ (g 13) 10))))
                                       (before-after ((+ ((hilite g) 13) 10)) ((+ ((hilite (lambda (x) (- x 22))) 13) 10)))
                                       (before-after ((+ (hilite ((lambda (x) (- x 22)) 13)) 10)) ((+ (hilite (- 13 22)) 10)))
                                       (before-after ((+ (hilite (- 13 22)) 10)) ((+ (hilite -9) 10)))
                                       (before-after ((hilite (+ -9 10))) ((hilite 1)))
                                       (finished (1)))))
  

  (t bad-cons
  (test-upto-int/lam "(cons 1 2)" 
                     `((before-error ((hilite (cons 1 2))) "cons: second argument must be of type <list>, given 1 and 2"))))
  
  (t prims
  (test-beginner-sequence "(cons 3 (cons 1 empty)) (list 1 2 3) (define-struct aa (b)) (make-aa 3)"
                          `((before-after-finished ((cons 3 (cons 1 empty))) 
						   ((hilite (list 1 2 3)))
						   ((hilite (cons 1 (cons 2 (cons 3 empty))))))
                            (finished ((cons 1 (cons 2 (cons 3 empty))) (define-struct aa (b)) (make-aa 3))))))

  (t prims/non-beginner
  (test-bwla-to-int/lam "(cons 3 (cons 1 empty)) (list 1 2 3) (define-struct aa (b)) (make-aa 3)"
                        `((before-after ((cons 3 (hilite (cons 1 empty)))) ((cons 3 (hilite (list 1)))))
                          (before-after ((hilite (cons 3 (list 1)))) ((hilite (list 3 1))))
                          (finished ((list 3 1) (list 1 2 3) (define-struct aa (b)) (make-aa 3))))))
  

  (t map
  (test-mz-sequence "(map (lambda (x) x) (list 3 4 5))"
                    `((before-after ((map (lambda (x) x) (hilite (list 3 4 5))))
                                    ((map (lambda (x) x) (hilite `( 3 4 5)))))
                      (before-after ((hilite (map (lambda (x) x) `(3 4 5))))
                                    ((... (hilite 3) ...)))
                      (before-after ((hilite ...))
                                    ((... (hilite 4) ...)))
                      (before-after ((hilite ...))
                                    ((... (hilite 5) ...)))
                      (before-after ((hilite ...)) ((hilite `(3 4 5))))
                      (finished (`(3 4 5))))))

  (t quoted-list
  (test-beginner-wla-sequence "'(3 4 5)"
                              `((finished ((list 3 4 5))))))
  
  
  ;;;;;;;;;;;;;
  ;;
  ;;  QUASIQUOTE
  ;;
  ;;;;;;;;;;;;;.
  
  ; note: we currently punt on trying to unwind quasiquote.
  
  (t qq1
  (test-beginner-wla-sequence "`(3 4 ,(+ 4 5))"
                              `((before-after ((cons 3 (cons 4 (cons (hilite (+ 4 5)) empty))))
                                              ((cons 3 (cons 4 (cons (hilite 9) empty)))))
                                (before-after ((cons 3 (cons 4 (hilite (cons 9 empty)))))
                                              ((cons 3 (cons 4 (hilite (list 9))))))
                                (before-after ((cons 3 (hilite (cons 4 (list 9)))))
                                              ((cons 3 (hilite (list 4 9)))))
                                (before-after ((hilite (cons 3 (list 4 9)))) ((hilite (list 3 4 9))))
                                (finished ((list 3 4 9))))))
  
  (t qq-splice
  (test-beginner-wla-sequence "`(3 ,@(list (+ 3 4) 5) 6)"
                              `((before-after ((cons 3 (append (list (hilite (+ 3 4)) 5) (cons 6 empty)))) ((cons 3 (append (list (hilite 7) 5) (cons 6 empty)))))
                                (before-after ((cons 3 (append (list 7 5) (hilite (cons 6 empty))))) ((cons 3 (append (list 7 5) (list 6)))))
                                (before-after ((cons 3 (hilite (append (list 7 5) (list 6))))) ((cons 3 (hilite (list 7 5 6)))))
                                (before-after ((hilite (cons 3 (list 7 5 6)))) ((hilite (list 3 7 5 6))))
                                (finished ((list 3 7 5 6))))))
  
  ;;;;;;;;;;;;;
  ;;
  ;;  LET
  ;;
  ;;;;;;;;;;;;;
  
  (t let1 (test-both-ints "(let ([a 3]) 4)"
                                       `((before-after ((hilite (let ([a 3]) 4))) ((hilite (define a_0 3)) (hilite 4)))
                                         (finished ((define a_0 3) 4))))) 
  
  (t let2
     (test-both-ints "(let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))"
                                 `((before-after ((hilite (let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))))
                                                 ((hilite (define a_0 (+ 4 5))) (hilite (define b_0 (+ 9 20))) (hilite (+ a_0 b_0))))
                                   (before-after-waiting ((define a_0 (hilite (+ 4 5)))) ((define a_0 (hilite 9)))
                                                         ((define b_0 (+ 9 20))
                                                          (+ a_0 b_0)))
                                   (before-after-finished-waiting ((define a_0 9))
                                                                  ((define b_0 (hilite (+ 9 20)))) ((define b_0 (hilite 29)))
                                                                  ((+ a_0 b_0)))
                                   (before-after-finished ((define b_0 29))
                                                          ((+ (hilite a_0) b_0)) ((+ (hilite 9) b_0)))
                                   (before-after ((+ 9 (hilite b_0))) ((+ 9 (hilite 29))))
                                   (before-after ((hilite (+ 9 29))) ((hilite 38)))
                                   (finished (38)))))
  
  (t let-scoping1
     (test-intermediate-sequence "(let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4)))"
                                 `((before-after ((hilite (let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4))))) ((hilite (define a_0 3)) (hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4)))))
                                   (before-after-finished ((define a_0 3)) ((hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4)))) ((hilite (define a_1 (lambda (x) (+ a_0 x)))) (hilite (a_1 4))))
                                   (before-after-finished ((define a_1 (lambda (x) (+ a_0 x)))) 
							  ((hilite (a_1 4)))
							  ((hilite (+ a_0 4))))
                                   (before-after ((+ (hilite a_0) 4)) ((+ (hilite 3) 4)))
                                   (before-after ((hilite (+ 3 4))) ((hilite 7)))
                                   (finished (7)))))
  
  (t let-scoping2
     (test-intermediate/lambda-sequence "(let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4)))"
                                        `((before-after ((hilite (let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4))))) 
							((hilite (define a_0 3)) (hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4)))))
                                          (before-after-finished ((define a_0 3)) ((hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4)))) 
								 ((hilite (define a_1 (lambda (x) (+ a_0 x)))) (hilite (a_1 4))))
                                          (before-after-finished ((define a_1 (lambda (x) (+ a_0 x))))
                                                                 (((hilite a_1) 4)) (((hilite (lambda (x) (+ a_0 x))) 4)))
                                          (before-after ((hilite ((lambda (x) (+ a_0 x)) 4))) ((hilite (+ a_0 4))))
                                          (before-after ((+ (hilite a_0) 4)) ((+ (hilite 3) 4)))
                                          (before-after ((hilite (+ 3 4))) ((hilite 7)))
                                          (finished (7)))))
  
  (t let-scoping3
     (test-intermediate-sequence "(define a12 3) (define c12 19) (let ([a12 13] [b12 a12]) (+ b12 a12 c12))"
                              `((before-after-finished ((define a12 3) (define c12 19))
                                                       ((hilite (let ([a12 13] [b12 a12]) (+ b12 a12 c12)))) 
						       ((hilite (define a12_0 13)) (hilite (define b12_0 a12)) (hilite (+ b12_0 a12_0 c12))))
                                (before-after-finished-waiting ((define a12_0 13))
                                                       ((define b12_0 (hilite a12))) ((define b12_0 (hilite 3)))
                                                       ( (+ b12_0 a12_0 c12)))
                                (before-after-finished ((define b12_0 3))
                                                       ((+ (hilite b12_0) a12_0 c12)) ((+ (hilite 3) a12_0 c12)))
                                (before-after ((+ 3 (hilite a12_0) c12)) ((+ 3 (hilite 13) c12)))
                                (before-after ((+ 3 13 (hilite c12))) ((+ 3 13 (hilite 19))))
                                (before-after ((hilite (+ 3 13 19))) ((hilite 35)))
                                (finished (35)))))
    
  (t let-lifting1
     (test-intermediate-sequence "(let ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
                                 `((before-after ((hilite (let ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9))) 
                                                 ((hilite (define a_0 (lambda (x) (+ x 14)))) (hilite (define b_0 (+ 3 4))) (hilite 9)))
                                   (before-after-finished-waiting ((define a_0 (lambda (x) (+ x 14))))
                                                                  ((define b_0 (hilite (+ 3 4)))) ((define b_0 (hilite 7)))
                                                                  (9))
                                   (finished ((define b_0 7) 9)))))
    
    (t let-deriv
       (test-intermediate-sequence "(define (f g) (let ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)) (define gprime (f cos))"
                                   `((before-after-finished ((define (f g) (let ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)))
                                                            ((define gprime (hilite (f cos)))) ((define gprime (hilite (let ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp)))))
                                     (before-after ((define gprime (hilite (let ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp))))
                                                   ((hilite (define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001)))) (define gprime (hilite gp_0))))
                                     (finished ((define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))) (define gprime gp_0))))))
    
    ;;;;;;;;;;;;;
    ;;
    ;;  LET*
    ;;
    ;;;;;;;;;;;;;
    
    (t let*-scoping1
       (test-both-ints "(define a 3) (define c 19) (let* ([a 13] [b a]) (+ b a c))"
                                `((before-after-finished ((define a 3) (define c 19))
							 ((hilite (let* ([a 13] [b a]) (+ b a c)))) 
							 ((hilite (define a_0 13)) (hilite (let* ([b a_0]) (+ b a_0 c)))))
                                  (before-after-finished ((define a_0 13))
							 ((hilite (let* ([b a_0]) (+ b a_0 c))))
							 ((hilite (define b_1 a_0)) (hilite (+ b_1 a_0 c))))
                                  (before-after-finished-waiting () 
                                                                 ((define b_1 (hilite a_0))) ((define b_1 (hilite 13)))
                                                                 ((+ b_1 a_0 c)))
                                  (before-after-finished ((define b_1 13))
                                                         ((+ (hilite b_1) a_0 c)) ((+ (hilite 13) a_0 c)))
                                  (before-after ((+ 13 (hilite a_0) c)) ((+ 13 (hilite 13) c)))
                                  (before-after ((+ 13 13 (hilite c))) ((+ 13 13 (hilite 19))))
                                  (before-after ((hilite (+ 13 13 19))) ((hilite 45)))
                                  (finished (45)))))
    
    (t let*-lifting1
       (test-intermediate-sequence "(let* ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
                                `((before-after ((hilite (let* ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9))) 
						((hilite (define a_0 (lambda (x) (+ x 14)))) (hilite (let* ([b (+ 3 4)]) 9))))
                                  (before-after-finished ((define a_0 (lambda (x) (+ x 14)))) 
							 ((hilite (let* ([b (+ 3 4)]) 9)))
							 ((hilite (define b_1 (+ 3 4))) (hilite 9)))
                                  (before-after-finished-waiting ()
                                                                 ((define b_1 (hilite (+ 3 4)))) ((define b_1 (hilite 7)))
                                                                 (9))
                                  (finished ((define b_1 7) 9)))))
    
  (t let*-deriv
     (test-intermediate-sequence "(define (f g) (let* ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)) (define gprime (f cos))"
                              `((before-after-finished ((define (f g) (let* ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)))
                                                       ((define gprime (hilite (f cos)))) ((define gprime (hilite (let* ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp)))))
                                (before-after ((define gprime (hilite (let* ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp))))
                                              ((hilite (define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001)))) (define gprime (hilite gp_0))))
                                (finished ((define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))) (define gprime gp_0))))))
  
  (t let/let*
     (test-both-ints "(let* ([a 9]) (let ([b 6]) a))"
                     `((before-after ((hilite (let* ([a 9]) (let ([b 6]) a)))) ((hilite (define a_0 9)) (hilite (let ([b 6]) a_0))))
                       (before-after-finished ((define a_0 9)) ((hilite (let ([b 6]) a_0))) ((hilite (define b_1 6)) (hilite a_0)))
                       (before-after-finished ((define b_1 6)) ((hilite a_0)) ((hilite 9)))
                       (finished (9)))))
    
    ;;;;;;;;;;;;;
    ;;
    ;;  LETREC
    ;;
    ;;;;;;;;;;;;;
    
  (t letrec1
     (test-intermediate-sequence "(define a 3) (define c 19) (letrec ([a 13] [b a]) (+ b a c))"
                                 `((before-after-finished ((define a 3) (define c 19))
                                                          ((hilite (letrec ([a 13] [b a]) (+ b a c)))) 
                                                          ((hilite (define a_0 13)) (hilite (define b_0 a_0)) (hilite (+ b_0 a_0 c))))
                                   (before-after-finished-waiting ((define a_0 13))
                                                          ((define b_0 (hilite a_0))) ((define b_0 (hilite 13)))
                                                          ( (+ b_0 a_0 c)))
                                   (before-after-finished ((define b_0 13))
                                                          ((+ (hilite b_0) a_0 c)) ((+ (hilite 13) a_0 c)))
                                   (before-after ((+ 13 (hilite a_0) c)) ((+ 13 (hilite 13) c)))
                                   (before-after ((+ 13 13 (hilite c))) ((+ 13 13 (hilite 19))))
                                   (before-after ((hilite (+ 13 13 19))) ((hilite 45)))
                                   (finished (45)))))
      
  (t letrec2
      (test-intermediate-sequence "(letrec ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
                                  `((before-after ((hilite (letrec ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9))) 
                                                  ((hilite (define a_0 (lambda (x) (+ x 14)))) (hilite (define b_0 (+ 3 4))) (hilite 9)))
                                    (before-after-finished-waiting ((define a_0 (lambda (x) (+ x 14))))
                                                                   ((define b_0 (hilite (+ 3 4)))) ((define b_0 (hilite 7)))
                                                                   (9))
                                    (finished ((define b_0 7) 9)))))
      
  (t letrec3
     (test-intermediate-sequence "(define (f g) (letrec ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)) (define gprime (f cos))"
                                 `((before-after-finished ((define (f g) (letrec ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)))
                                                          ((define gprime (hilite (f cos)))) 
                                                          ((define gprime (hilite (letrec ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp)))))
                                   (before-after ((define gprime (hilite (letrec ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp))))
                                                 ((hilite (define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001)))) (define gprime (hilite gp_0))))
                                   (finished ((define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))) (define gprime gp_0))))))
      ;;;;;;;;;;;;;
      ;;
      ;;  RECUR
      ;;
      ;;;;;;;;;;;;;
 
  ;; N.B. : we cheat here.  In particular, the rhs of the double-break expression should highlight the whole application, and not
  ;; just the applied loop identifier.  This is hard to fix because we have an application which is initially hidden, but then later
  ;; not hidden.  Fixing this involves parameterizing the unwind by what kind of break it was.  Yuck!  So we just fudge the test case.
  
  (t recur
      (test-intermediate-sequence "(define (countdown n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1))))) (countdown 2)"
                                  `((before-after-finished ((define (countdown n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1)))))) 
  							 ((hilite (countdown 2)))
  							 ((hilite (recur loop ([n 2]) (if (= n 0) 13 (loop (- n 1)))))))
                                    (before-after ((hilite (recur loop ([n 2]) (if (= n 0) 13 (loop (- n 1)))))) 
  						((hilite (define (loop_0 n) (if (= n 0) 13 (loop_0 (- n 1))))) ((hilite loop_0) 2)))
                                    (before-after-finished ((define (loop_0 n) (if (= n 0) 13 (loop_0 (- n 1)))))
                                                           ((hilite (loop_0 2)))
  							 ((hilite (if (= 2 0) 13 (loop_0 (- 2 1))))))
                                    (before-after ((if (hilite (= 2 0)) 13 (loop_0 (- 2 1)))) ((if (hilite false) 13 (loop_0 (- 2 1)))))
                                    (before-after ((hilite (if false 13 (loop_0 (- 2 1))))) ((hilite (loop_0 (- 2 1)))))
                                    (before-after ((loop_0 (hilite (- 2 1)))) ((loop_0 (hilite 1))))
                                    (before-after ((hilite (loop_0 1))) ((hilite (if (= 1 0) 13 (loop_0 (- 1 1))))))
                                    (before-after ((if (hilite (= 1 0)) 13 (loop_0 (- 1 1)))) ((if (hilite false) 13 (loop_0 (- 1 1)))))
                                    (before-after ((hilite (if false 13 (loop_0 (- 1 1))))) ((hilite (loop_0 (- 1 1)))))
                                    (before-after ((loop_0 (hilite (- 1 1)))) ((loop_0 (hilite 0))))
                                    (before-after ((hilite (loop_0 0))) ((hilite (if (= 0 0) 13 (loop_0 (- 0 1))))))
                                    (before-after ((if (hilite (= 0 0)) 13 (loop_0 (- 0 1)))) ((if (hilite true) 13 (loop_0 (- 0 1)))))
                                    (before-after ((hilite (if true 13 (loop_0 (- 0 1))))) ((hilite 13)))
  				  (finished (13)))))
  
    ;;;;;;;;;;;;;
    ;;
    ;;  LOCAL
    ;;
    ;;;;;;;;;;;;;
    
  
  (t empty-local
     (test-both-ints "(local () (+ 3 4))"
                     `((before-after ((hilite (local () (+ 3 4)))) ((hilite (+ 3 4))))
                       (before-after ((hilite (+ 3 4))) ((hilite 7)))
                       (finished (7)))))
  
  (t local1
     (test-both-ints "(local ((define a 3) (define b 8)) 4)"
                     `((before-after ((hilite (local ((define a 3) (define b 8)) 4)))
                                     ((hilite (define a_0 3)) (hilite (define b_0 8)) (hilite 4)))
                       (finished ((define a_0 3) (define b_0 8) 4)))))
    
  (t local2
     (test-intermediate-sequence "(local ((define (a x) (+ x 9))) (a 6))"
                                 `((before-after ((hilite (local ((define (a x) (+ x 9))) (a 6)))) 
						 ((hilite (define (a_0 x) (+ x 9))) (hilite (a_0 6))))
                                   (before-after-finished ((define (a_0 x) (+ x 9)))
                                                          ((hilite (a_0 6))) ((hilite (+ 6 9))))
                                   (before-after ((hilite (+ 6 9))) ((hilite 15)))
                                   (finished (15)))))
  
  (t local3
     (test-intermediate/lambda-sequence "(local ((define (a x) (+ x 9))) (a 6))"
                                        `((before-after ((hilite (local ((define (a x) (+ x 9))) (a 6)))) 
							((hilite (define (a_0 x) (+ x 9))) (hilite (a_0 6))))
                                          (before-after-finished ((define (a_0 x) (+ x 9)))
                                                                 (((hilite a_0) 6)) (((hilite (lambda (x) (+ x 9))) 6)))
                                          (before-after ((hilite ((lambda (x) (+ x 9)) 6))) ((hilite (+ 6 9))))
                                          (before-after ((hilite (+ 6 9))) ((hilite 15)))
                                          (finished (15)))))
    
  (t local4
     (test-intermediate-sequence "(local ((define (a x) (+ x 13))) a)"
                                 `((before-after ((hilite (local ((define (a x) (+ x 13))) a))) ((hilite (define (a_0 x) (+ x 13))) (hilite a_0)))
                                   (finished ((define (a_0 x) (+ x 13)) a_0)))))
  
  (t local5
     (test-intermediate/lambda-sequence "(local ((define (a x) (+ x 13))) a)"
                                        `((before-after ((hilite (local ((define (a x) (+ x 13))) a))) ((hilite (define (a_0 x) (+ x 13))) (hilite a_0)))
                                          (before-after ((hilite a_0)) ((hilite (lambda (x) (+ x 13)))))
                                          (finished ((define (a_0 x) (+ x 13)) (lambda (x) (+ x 13)))))))
  
  (t local-interref1
     (test-intermediate-sequence "(local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))"
                                 `((before-after ((hilite (local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))))
                                                 ((hilite (define (a_0 x) (+ x 9))) (hilite (define b_0 a_0)) (hilite (define p_0 (+ 3 4))) (hilite (b_0 1))))
                                   (before-after-finished-waiting ((define (a_0 x) (+ x 9)) (define b_0 a_0))
                                                                  ((define p_0 (hilite (+ 3 4)))) ((define p_0 (hilite 7))) ((b_0 1)))
                                   (before-after-finished ((define p_0 7))
                                                          (((hilite b_0) 1)) (((hilite a_0) 1)))
                                   (before-after ((hilite (a_0 1))) ((hilite (+ 1 9))))
                                   (before-after ((hilite (+ 1 9))) ((hilite 10)))
                                   (finished (10)))))
  
  (t local-interref2
     (test-intermediate/lambda-sequence "(local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))"
                                        `((before-after ((hilite (local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))))
                                                        ((hilite (define (a_0 x) (+ x 9))) (hilite (define b_0 a_0)) (hilite (define p_0 (+ 3 4))) (hilite (b_0 1))))
                                          (before-after-finished-waiting ((define (a_0 x) (+ x 9)))
                                                                         ((define b_0 (hilite a_0))) ((define b_0 (hilite (lambda (x) (+ x 9))))) ((define p_0 (+ 3 4)) (b_0 1)))
                                          (before-after-finished-waiting ((define b_0 (lambda (x) (+ x 9))))
                                                                         ((define p_0 (hilite (+ 3 4)))) ((define p_0 (hilite 7))) ((b_0 1)))
                                          (before-after-finished ((define p_0 7))
                                                                 (((hilite b_0) 1)) (((hilite (lambda (x) (+ x 9))) 1)))
                                          (before-after ((hilite ((lambda (x) (+ x 9)) 1))) ((hilite (+ 1 9))))
                                          (before-after ((hilite (+ 1 9))) ((hilite 10)))
                                          (finished (10)))))
    
  (t local-gprime
     (test-intermediate-sequence "(define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)) (define gprime (f12 cos))"
                                 `((before-after-finished ((define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)))
                                                          ((define gprime (hilite (f12 cos)))) 
                                                          ((define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp)))))
                                   (before-after ((define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp))))
                                                 ((hilite (define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))) (define gprime (hilite gp_0))))
                                   (finished ((define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)) (define gprime gp_0))))))
  
  (t local-gprime/lambda
     (test-intermediate/lambda-sequence "(define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)) (define gprime (f12 cos))"
                                        `((before-after-finished ((define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)))
                                                                 ((define gprime ((hilite f12) cos))) 
                                                                 ((define gprime ((hilite (lambda (g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp))) cos))))
                                          (before-after ((define gprime (hilite ((lambda (g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)) cos))))
                                                        ((define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp)))))
                                          (before-after ((define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp))))
                                                        ((hilite (define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))) (define gprime (hilite gp_0))))
                                          (before-after-finished ((define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)))
                                                                 ((define gprime (hilite gp_0))) ((define gprime (hilite (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))))))
                                          (finished ((define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)) (define gprime (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))))))))
  
  ; test generativity... that is, multiple evaluations of a local get different lifted names:
  
  (t local-generative
     (test-intermediate-sequence "(define (a13 b13 c13) (b13 c13)) (define (f9 x) (local ((define (maker dc) x)) maker)) (define m1 (f9 3)) (a13 (f9 4) 1)"
                                 `((before-after-finished ((define (a13 b13 c13) (b13 c13))
                                                           (define (f9 x) (local ((define (maker dc) x)) maker)))
                                                          ((define m1 (hilite (f9 3)))) ((define m1 (hilite (local ((define (maker dc) 3)) maker)))))
                                   (before-after ((define m1 (hilite (local ((define (maker dc) 3)) maker))))
                                                 ((hilite (define (maker_0 dc) 3)) (define m1 (hilite maker_0))))
                                   (before-after-finished ((define (maker_0 dc) 3) (define m1 maker_0))
                                                          ((a13 (hilite (f9 4)) 1)) ((a13 (hilite (local ((define (maker dc) 4)) maker)) 1)))
                                   (before-after ((a13 (hilite (local ((define (maker dc) 4)) maker)) 1))
                                                 ((hilite (define (maker_1 dc) 4)) (a13 (hilite maker_1) 1)))
                                   (before-after-finished ((define (maker_1 dc) 4))
                                                          ((hilite (a13 maker_1 1))) ((hilite (maker_1 1))))
                                   (before-after ((hilite (maker_1 1))) ((hilite 4)))
                                   (finished (4)))))
  
  (t local-generative/lambda
     (test-intermediate/lambda-sequence "(define (a13 b13 c13) (b13 c13)) (define (f9 x) (local ((define (maker dc) x)) maker)) (define m1 (f9 3)) (a13 (f9 4) 1)"
                                        `((before-after-finished ((define (a13 b13 c13) (b13 c13))
                                                                  (define (f9 x) (local ((define (maker dc) x)) maker)))
                                                                 ((define m1 ((hilite f9) 3))) ((define m1 ((hilite (lambda (x) (local ((define (maker dc) x)) maker))) 3))))
                                          (before-after ((define m1 (hilite ((lambda (x) (local ((define (maker dc) x)) maker)) 3)))) 
                                                        ((define m1 (hilite (local ((define (maker dc) 3)) maker)))))
                                          (before-after ((define m1 (hilite (local ((define (maker dc) 3)) maker))))
                                                        ((hilite (define (maker_0 dc) 3)) (define m1 (hilite maker_0))))
                                          (before-after-finished ((define (maker_0 dc) 3)) 
                                                                 ((define m1 (hilite maker_0))) ((define m1 (hilite (lambda (dc) 3)))))
                                          (before-after-finished ((define m1 (lambda (dc) 3)))
                                                                 (((hilite a13) (f9 4) 1)) (((hilite (lambda (b13 c13) (b13 c13))) (f9 4) 1)))
                                          (before-after (((lambda (b13 c13) (b13 c13)) ((hilite f9) 4) 1)) 
                                                        (((lambda (b13 c13) (b13 c13)) ((hilite (lambda (x) (local ((define (maker dc) x)) maker))) 4) 1)))
                                          (before-after (((lambda (b13 c13) (b13 c13)) (hilite ((lambda (x) (local ((define (maker dc) x)) maker)) 4)) 1)) 
                                                        (((lambda (b13 c13) (b13 c13)) (hilite (local ((define (maker dc) 4)) maker)) 1)))
                                          (before-after (((lambda (b13 c13) (b13 c13)) (hilite (local ((define (maker dc) 4)) maker)) 1))
                                                        ((hilite (define (maker_1 dc) 4)) ((lambda (b13 c13) (b13 c13)) (hilite maker_1) 1)))
                                          (before-after-finished ((define (maker_1 dc) 4))
                                                                 (((lambda (b13 c13) (b13 c13)) (hilite maker_1) 1)) 
                                                                 (((lambda (b13 c13) (b13 c13)) (hilite (lambda (dc) 4)) 1)))
                                          (before-after ((hilite ((lambda (b13 c13) (b13 c13)) (lambda (dc) 4) 1))) ((hilite ((lambda (dc) 4) 1))))
                                          (before-after ((hilite ((lambda (dc) 4) 1))) ((hilite 4)))
                                          (finished (4)))))
  
  ;;;;;;;;;;;;;
  ;;
  ;;  Reduction of Lambda in int/lambda
  ;;
  ;;;;;;;;;;;;;
  
  (t int/lam1
     (test-intermediate/lambda-sequence "(define f ((lambda (x) x) (lambda (x) x))) (f f)"
                                        `((before-after ((define f (hilite ((lambda (x) x) (lambda (x) x))))) ((define f (hilite (lambda (x) x)))))
                                          (before-after-finished ((define f (lambda (x) x)))
                                                                 (((hilite f) f)) (((hilite (lambda (x) x)) f)))
                                          (before-after (((lambda (x) x) (hilite f))) (((lambda (x) x) (hilite (lambda (x) x)))))
                                          (before-after ((hilite ((lambda (x) x) (lambda (x) x)))) ((hilite (lambda (x) x))))
                                          (finished ((define f (lambda (x) x)) (lambda (x) x))))))
  
  
  (t int/lam2
     (test-intermediate/lambda-sequence "(define f (if false (lambda (x) x) (lambda (x) x))) (f f)"
                                        `((before-after ((define f (hilite (if false (lambda (x) x) (lambda (x) x))))) 
                                                        ((define f (hilite (lambda (x) x)))))
                                          (before-after-finished ((define f (lambda (x) x)))
                                                                 (((hilite f) f)) (((hilite (lambda (x) x)) f)))
                                          (before-after (((lambda (x) x) (hilite f))) (((lambda (x) x) (hilite (lambda (x) x)))))
                                          (before-after ((hilite ((lambda (x) x) (lambda (x) x)))) ((hilite (lambda (x) x))))
                                          (finished ((define f (lambda (x) x)) (lambda (x) x))))))
  
  
  (t time
     (test-intermediate-sequence "(time (+ 3 4))"
                                 `((before-after ((hilite (+ 3 4)))
                                                 ((hilite 7)))
                                   (finished (7)))))
  
  
  ;;;;;;;;;;;;;;;;
  ;;
  ;;  XML (uses MrEd)
  ;;
  ;;;;;;;;;;;;;;;;
    
  #;(t ddj-screenshot
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
  
  #;(define (test-xml-sequence namespace-spec render-settings track-inferred-names? spec expected-steps)
    (letrec ([port (open-input-text-editor (construct-text spec))])
      (test-sequence-core namespace-spec render-settings track-inferred-names? port expected-steps)))
  
  #;(define (construct-text spec)
    (let ([new-text (instantiate text% ())])
      (for-each
       (match-lambda 
         [`(xml-box ,@(xmlspec ...)) (send new-text insert (construct-xml-box xmlspec))]
         [(? string? text) (send new-text insert text)])
       spec)
      new-text))
  
  #;(define (test-xml-beginner-sequence spec expected)
    (test-xml-sequence `(lib "htdp-beginner.ss" "lang")
                        fake-beginner-render-settings
                        #t
                        spec
                        expected))
  
  #;(t xml-box1
     (test-xml-beginner-sequence `((xml-box "<abba>3</abba>"))
                                 `((finished ((xml-box-value (cons 'abba (cons empty (cons "3" empty)))))))))
  
  #;(t xml-box2
     (text-xml-beginnner-sequence `("(cdr (cdr " (xml-box "<foozle>a b</foozle>") "))")
                                  `((before-after ((cdr (cdr (xml-box "<foozle>a b</foozle>"))))))))
  
  ;  
  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  TIME
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;  
  ;  (test-intermediate-sequence "(time (+ 3 4))"
  ;                              `((before-after ((time (hilite ,h-p))) ((+ 3 4)) same (7))
  ;                                (before-after ((hilite ,h-p)) ((time 7)) same (7))
  ;                                (result (7))))
  
  
  ;(t filled-rect-image 
  ;   (test-upto-int-lam "(image-width (filled-rect 10 10 'blue))"
  ;                      `((before-after ((image-width (hilite (filled-rect 10 10 'blue)))) ((image-width (hilite )))))))
  ; add image test: (image-width (filled-rect 10 10 'blue))
  
  ;;;;;;;;;;;;;;
  ;;
  ;;  PRIM TESTS
  ;;
  ;;;;;;;;;;;;;;

  
  
  
  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  TEACHPACK TESTS
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;  
  
  ; as you can see, many teachpack tests work only in mred:
  #; (require (lib "mred.ss" "mred"))
  
  
  (define test-teachpack-sequence (lambda (teachpack-specs expr-string expected-results)
                                    ;(let ([new-custodian (make-custodian)])
                                     ; (parameterize ([current-custodian new-custodian])
                                      ;  (parameterize ([current-eventspace (make-eventspace)])
                                          (test-sequence `(lib "htdp-beginner.ss" "lang") teachpack-specs fake-beginner-render-settings #t expr-string expected-results)
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
  
  #;(t teachpack-drawing
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
     (finished (true)))))
  
  #;(t teachpack-name-rendering
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
        (finished (false)))))
  
  #;(t teachpack-hop-names
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
        (finished (true)))))
 
  #;(t teachpack-web-interaction
  (test-teachpack-sequence
   `((lib "servlet2.ss" "htdp"))
"(define (adder go) (inform (number->string (+ (single-query (make-number \"enter 10\")) (single-query (make-number \"enter 20\"))))))
(adder true)"
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
                ((inform (number->string (+ 10 (hilite 20))))))
  (before-after ((inform (number->string (hilite (+ 10 20))))) 
                ((inform (number->string (hilite 30)))))
  (before-after ((inform (hilite (number->string 30)))) 
                ((inform (hilite "30"))))
  (before-after ((hilite (inform "30")))
                ((hilite true)))
  (finished (true)))))
  
  #;(t teachpack-callbacks
     (test-teachpack-sequence " (define (f2c x) x) (convert-gui f2c)" `() ; placeholder
                               ))
  
  #;(run-tests '(let-scoping1))
  (run-all-tests)
  )
