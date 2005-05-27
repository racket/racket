(require (prefix annotate: (lib "annotate.ss" "stepper" "private")))
(require (prefix kernel: (lib "kerncase.ss" "syntax")))
(require (lib "syncheck-debug.ss" "drscheme" "private"))
(require (lib "marks.ss" "stepper" "private"))
(require "tests-common.ss")

(load "/Users/clements/plt/tests/mzscheme/testing.ss")

(reset-namespaces) 

(SECTION 'stepper-annotater)


; check-mark : test the validity of a mark.  the symbols in 'binding-ids' must be included. the symbols
; in excluded-ids must not appear in the list.  If binding-ids is 'all, then no symbols other than those
; in the binding-ids may appear.
; 
; note: cheap hack: if a string is supplied rather than a symbol in binding-ids, it passes if the string
; is a prefix of (symbol->string) of one of the present symbols.
; (syntax-object (listof (union symbol string)) (union (listof symbol) 'all) -> void)

(define (binding-match? input actual)
  (if (string? input)
      (and (>= (string-length (symbol->string actual)) (string-length input))
           (string=? (substring (symbol->string actual) 0 (string-length input)) input))
      (eq? input actual)))

(define (check-mark mark-stx binding-ids excluded-ids) 
  (let* ([bindings (syntax-case mark-stx (lambda )
                     [(lambda ()
                        (mark-maker
                         source
                         label
                         . bindings))
                      (let loop ([binding-list (syntax->list (syntax bindings))])
                        (cond [(null? binding-list) null]
                              [(null? (cdr binding-list)) 
                               (error 'check-mark "odd number of elements in binding list: ~a" (syntax-object->datum (syntax bindings)))]
                              [else
                               (let* ([quoted-stx (stx-protector-stx (syntax-e (cadr binding-list)))])
                                 (when (not (module-identifier=?
                                             (car binding-list)
                                             quoted-stx))
                                   (error 'check-mark "binding pair does not match: ~a, ~a" (syntax-object->datum (car binding-list))
                                          (syntax-object->datum (cadr binding-list))))
                                 (when (not (identifier? (car binding-list)))
                                   (error 'check-mark "syntax object is not an identifier: ~a" (syntax-object->datum (car bindings-list))))
                                 (cons (syntax-e (car binding-list))
                                     (loop (cddr binding-list))))]))])])
    (let loop ([remaining bindings])
      (unless (null? remaining)
        (when (memq (car remaining) (cdr remaining))
          (error 'check-mark "binding ~a appears twice in binding-list: ~a" (car remaining) bindings))
        (loop (cdr remaining))))
    (for-each (lambda (desired)
                (unless (ormap (lambda (binding)
                                 (binding-match? desired binding))
                               bindings)
                  (error 'check-mark "binding ~a not contained in binding-list: ~a" desired bindings)))
              binding-ids)
    (if (eq? excluded-ids 'all)
        (for-each (lambda (binding)
                    (unless (ormap (lambda (binding-id)
                                     (binding-match? binding-id binding))
                                   binding-ids)
                      (error 'check-mark "binding ~a does not appear in desired list: ~a" binding binding-ids)))
                  bindings)
        (for-each (lambda (not-desired)
                    (when (memq not-desired bindings)
                      (error 'check-mark "excluded binding ~a contained in binding-list: ~a" not-desired bindings)))
                  excluded-ids))))

; test cases

(syntax-case (expand #'(let ([a 1] [b 2]) (begin a b))) (let-values begin)
  [(let-values bindings a b)
   (begin
     (err/rt-test (check-mark (let* ([a-var #'a]) (make-full-mark #'3 'a (list a-var a-var))) '() '()) exn:user?) ; binding appears twice
     (test (void) check-mark (make-full-mark #'3 'a (list #'a #'b)) '(a b) 'all)
     (err/rt-test (check-mark (make-full-mark #'3 'a (list #'a #'b)) '(a b c) 'all) exn:user?) ; c isn't there
     (err/rt-test (check-mark (make-full-mark #'3 'a (list #'a #'b)) '(a) 'all) exn:user?) ; bad 'all
     (test (void) check-mark (make-full-mark #'3 'a (list #'a #'b)) '(a) '(c))
     (err/rt-test (check-mark (make-full-mark #'3 'a (list #'a #'b)) '(a) '(b c)) exn:user?) ; b is there
     (test (void) check-mark (make-full-mark #'3 'a (list #'a #'b)) '(a) '())
     (test (void) check-mark (make-full-mark #'3 'a (list #'arg0-436 #'c)) '("arg0" c) '())
     (err/rt-test (check-mark (make-full-mark #'3 'a (list #'arg0-436 #'c)) '("djs") '()) exn:user?))])

; syntax-symbol=? : compares a list of symbols to a given symbol (or to each other if sym = #f)
; (union symbol #f) (listof syntax-object) -> boolean

(define (syntax-symbol=? sym lostx)
  (if sym
      (andmap (lambda (stx) (eq? sym (syntax-e stx))) lostx)
      (if (pair? lostx)
          (andmap (lambda (stx) (eq? (syntax-e stx) (syntax-e (car lostx)))) lostx)
          #t)))

(define (wrap-expand-unwrap stx language-level-spec)
  (let* ([wrapped (datum->syntax-object #f `(module test-module ,language-level-spec
                                              ,@stx))]
         [expanded (expand wrapped)])
    (with-syntax ([(module name lang (_ . exprs)) expanded])
      (syntax->list (syntax exprs)))))

(test '((if (#%app verify-boolean (#%datum . 3) 'if) (#%datum . 4) (#%datum . 5)))
      map syntax-object->datum (wrap-expand-unwrap (list #'(if 3 4 5)) '(lib "htdp-beginner.ss" "lang")))
         
(define (annotate-exprs-no-break stx-list)
  (annotate-exprs (map expand stx-list) void))

(define (namespace-annotate-expr stx namespace)
  ;(when (syntax? stx)
  ;  (error 'namespace-rewrite-expr "namespace-rewrite-expr accepts s-exps, not syntax-objects"))
  (parameterize ([current-namespace namespace])
    (let*-values ([(annotated new-env)
                   (annotate:annotate (expand stx) annotate:initial-env-package break 'foot-wrap)])
      annotated)))

; the following procedure is used to test just the top-level-rewrite part of the annotater:

(define (namespace-rewrite-expr stx namespace)
  ;(when (syntax? stx)
  ;  (error 'namespace-rewrite-expr "namespace-rewrite-expr accepts s-exps, not syntax-objects"))
  (parameterize ([current-namespace namespace])
    (annotate:top-level-rewrite (expand stx))))

; strip-outer-lambda takes off a lambda wrapped around a test expression. For testing purposes,
; we often want to establish lexical bindings, then strip them off to check the test expr

(define (strip-outer-lambda stx)
  (syntax-case stx (lambda begin with-continuation-mark)
    [(with-continuation-mark
      debug-key-1
      debug-mark-1
      (closure-storing-proc
       (lambda args
         content)
       debug-mark-2))
     (syntax content)]))


; test case:
(test #t 
      (lambda ()
        (syntax-case  (strip-outer-lambda (car (annotate-exprs-no-break (list #'(lambda (a b c) 3)))))
          (with-continuation-mark lambda quote-syntax #%datum)
          [(with-continuation-mark 
            key
            bogus-key
            (begin
              break-proc-1
              (#%datum . 3)))
           #t])))

(define (strip-return-value-wrap stx)
  (syntax-case stx (let*)
    [(let* ([result-var-0 expr])
       break-0
       result-var-1)
     (begin
       (test 'result syntax-e (syntax result-var-0))
       (test 'result syntax-e (syntax result-var-1))
       (syntax expr))]))

(test 'a syntax-e
      (strip-return-value-wrap 
       (syntax-case (car (annotate-exprs-no-break (list #'a))) (with-continuation-mark begin)
         [(with-continuation-mark
           key-0
           mark-0
           (begin
             break-0
             expr))
          (syntax expr)])))
     
; test notes to myself:
; the never-undefined property can only be tested in the non-foot-wrap modes
; hard to test 3D values like closure-capturing-proc
; hard to test whether the source pointer is right.

(define test-cases
  ; begin
  (list (list (list #'(lambda (a b c) (begin a b (begin a c))))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark begin)
                  [(with-continuation-mark
                    debug-key-1
                    debug-mark-1
                    (begin
                      pre-break-1
                      (begin
                        (with-continuation-mark
                         debug-key-2
                         debug-mark-2
                         a-var-0)
                        (with-continuation-mark
                         debug-key-3
                         debug-mark-3
                         b-var-0)
                        (with-continuation-mark
                         debug-key-4
                         debug-mark-4
                         begin-body-4))))
                   (begin
                     (test 'a syntax-e (syntax a-var-0))
                     (test 'b syntax-e (syntax b-var-0))
                     (test (void) check-mark (syntax debug-mark-1) '(a b c) 'all)
                     (test (void) check-mark (syntax debug-mark-2) '() 'all)
                     (test (void) check-mark (syntax debug-mark-3) '() 'all)
                     (test (void) check-mark (syntax debug-mark-4) '(a c) 'all))])))
         
        ; lambda : general test
        (list (list #'(lambda (a b) (lambda (b c) (+ b c) (+ a b 4))))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark begin lambda)
                  [(with-continuation-mark
                    debug-key
                    debug-mark-1
                    (begin
                      pre-break-1
                      (closure-capturing-proc
                       (lambda (b c) 
                         (with-continuation-mark
                          debug-key-2
                          debug-mark-lambda-body
                          (begin
                            pre-break-2
                            (begin
                              . begin-bodies))))
                       debug-mark-3)))
                    (begin (test (void) check-mark (syntax debug-mark-1) '(a) 'all)
                           (test (void) check-mark (syntax debug-mark-3) '(a) 'all))])))
        
        ; improper arg-list:
        (list (list #'(lambda (a b . c) (begin b c)))
              (lambda (stx)
                (syntax-case stx (with-continuation-mark begin lambda)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (closure-capturing-proc
                     (lambda (a b . c)
                       (with-continuation-mark
                        key-1
                        mark-1
                        body))
                     mark-2))
                   (begin (test (void) check-mark (syntax mark-0) '() 'all)
                          (test (void) check-mark (syntax mark-1) '(b c) 'all)
                          (test (void) check-mark (syntax mark-2) '() 'all))])))
                    
        ; test of lambda's one-label inferred-names :
        (list (list #'(define (a x) (+ x 1)))
              (lambda (stx)
                (kernel:kernel-syntax-case stx #f
                   [(define-values (a)
                      (with-continuation-mark
                       debug-key
                       debug-mark
                       (closure-capturing-proc
                        lambda-exp
                        closure-info)))
                    (test 'a syntax-property (syntax lambda-exp) 'inferred-name)])))
        
        ; test of lambda's cons-pair inferred-names (that is, with lifting):
        (list (list #'(let ([a (lambda (x) (+ x 1))]) 3))
              (lambda (stx)
                (syntax-case stx (let*-values with-continuation-mark begin set!-values set!)
                   [(let*-values _c
                       (with-continuation-mark debug-key_1 debug_mark_1
                                               ;(begin
                                               ;  (break-proc_1)
                                                 (begin
                                                   (set!-values a-label
                                                                (with-continuation-mark
                                                                 debug-key
                                                                 debug-mark
                                                                 (closure-capturing-proc
                                                                  lambda-exp
                                                                  closure-info
                                                                  lifter-val)))
                                                   (set! counter 1)
                                                   body)
                                               ;  )
                                               ))
                    (test 'a syntax-property (syntax lambda-exp) 'inferred-name)])))
        
        ; case-lambda
        (list (list #'(lambda (d e) (case-lambda ((b) b d) ((c) c e))))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark lambda quote-syntax)
                  [(with-continuation-mark
                    debug-key
                    debug-mark-1
                    (begin
                      pre-break-1
                      (closure-storing-proc
                       (case-lambda ((b) . bodies_1)
                                    ((c) . bodies_2))
                       closure-info)))
                   (test (void) check-mark (syntax debug-mark-1) '(d e) '(b c))])))
        
        ; if
        (list (list #'(lambda (a b c d) (if (a b) (a c) (a d))))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark if let-values begin let)
                  [(let ([test-0 *unevaluated*-0])
                     (with-continuation-mark
                      debug-key_1
                      debug-mark-1
                      (begin
                        pre-break-1
                        (begin
                          (set! test-1 (let-values temp-bindings
                                         (with-continuation-mark
                                          debug-key_2
                                          debug-mark-test
                                          . test-clauses)))
                          (break-0)
                          (if test-2
                              (let-values temp-bindings_2
                                (with-continuation-mark
                                 debug-key_3
                                 debug-mark-then
                                 . then-clauses))
                              (let-values temp-bindings-3
                                (with-continuation-mark
                                 debug-key-4
                                 debug-mark-else
                                 . else-clauses)))))))
                   (begin
                     (test 'if-temp syntax-e (syntax test-0))
                     (test 'if-temp syntax-e (syntax test-1))
                     (test 'if-temp syntax-e (syntax test-2))
                     (test (void) check-mark (syntax debug-mark-1) '(a b c d if-temp) '())
                     (test (void) check-mark (syntax debug-mark-test) '() '(a b c d if-temp))
                     (test (void) check-mark (syntax debug-mark-then) '(a c) '(b d if-temp))
                     (test (void) check-mark (syntax debug-mark-else) '(a d) '(b c if-temp)))])))
        
        ; one-armed if
        (list (list #'(lambda (a b c) (if (begin a b) (begin a c))))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark if let-values begin)
                  [(let ([test-0 *unevaluated*-0])
                     (with-continuation-mark
                      debug-key-1
                      debug-mark-1
                      (begin
                        pre-break-1
                        (begin
                          (set! test-1 (with-continuation-mark
                                        debug-key_2
                                        debug-mark-test
                                        . test-clauses))
                          (break-0)
                          (if test-2
                              (with-continuation-mark
                               debug-key_3
                               debug-mark-then
                               . then-clauses))))))
                   (begin
                     (test 'if-temp syntax-e (syntax test-0))
                     (test 'if-temp syntax-e (syntax test-1))
                     (test 'if-temp syntax-e (syntax test-2))
                     (test (void) check-mark (syntax debug-mark-1) '(a b c if-temp) 'all)
                     (test (void) check-mark (syntax debug-mark-test) '() 'all)
                     (test (void) check-mark (syntax debug-mark-then) '(a c ) 'all))])))
        
        ; top-level begin
        (list (list #'(begin (define a 3) (begin (begin a))))
              (lambda (stx)
                (syntax-case stx (begin with-continuation-mark define-values)
                  [(begin
                     (define-values . rest)
                     (begin
                       (begin
                         (with-continuation-mark key-3 mark-3 (begin var-break-1 a-exp-3)))))
                   (test 'a syntax-e (strip-return-value-wrap (syntax a-exp-3)))])))
        
        ; begin0
        (list (list #'(lambda (a b) (begin0 a b)))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (begin begin0 with-continuation-mark)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (begin
                      pre-break-0
                      (begin0
                        (with-continuation-mark
                         key-result
                         mark-result
                         result-expr)
                        (with-continuation-mark
                         key-other
                         mark-other
                         other-expr))))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a b) 'all)
                     (test (void) check-mark (syntax mark-result) '() 'all)
                     (test (void) check-mark (syntax mark-other) '() 'all))])))
        
        ; begin0 : inferred-name transfer
        (list (list #'(define-values (a) (begin0 (lambda () 3) 4)))
              (lambda (stx)
                (syntax-case stx (begin0 define-values with-continuation-mark)
                  [(define-values names
                     (with-continuation-mark
                      key-0
                      mark-0
                      (begin0
                        (with-continuation-mark
                         key-1
                         mark-1
                         (procedure-capturing-proc
                          procedure
                          mark-2))
                        . rest)))
                   (test 'a syntax-property (syntax procedure) 'inferred-name)])))
        ; let 
        (list (list #'(lambda (a b c) (let* ([d b] [e (begin a d)]) (begin a b c d))))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (begin with-continuation-mark let*-values set!-values set!)
                  [(let*-values bindings
                     (with-continuation-mark
                      key-0
                      mark-0
                      (begin
                        pre-break-0
                        ;(begin
                        ;  break-1
                          (begin
                            (set!-values vars-0 (with-continuation-mark key-1 mark-1 body-1))
                            (set! let-counter-0 1)
                            (set!-values vars-1 (with-continuation-mark key-2 mark-2 body-2))
                            (set! let-counter-1 2)
                            ;(begin
                            ;  break-2
                              body-3
                            ;  )
                            )
                        ;  )
                        )))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a b c d e lifter-d-1 lifter-e-2 let-counter) 'all)
                     (test '(d) syntax-object->datum (syntax vars-0))
                     (test '(e) syntax-object->datum (syntax vars-1))
                     (test (void) check-mark (syntax mark-1) '() 'all)
                     (test (void) check-mark (syntax mark-2) '() 'all))])))
        
        ; letrec --- the only thing that needs to be tested with letrec is that the undefined value is properly used.
        
        ; set!
        (list (list #'(lambda (a b c) (set! a (begin b c))))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (begin with-continuation-mark set!)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (begin
                      pre-break-0
                      (set! var (with-continuation-mark key-1 mark-1 body))))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a b c) 'all)
                     (test 'a syntax-object->datum (syntax var))
                     (test (void) check-mark (syntax mark-1) '() 'all))])))
        
        ; set! with top-level-var
        (list (list #'(set! a 9))
              (lambda (stx)
                (syntax-case stx (set! with-continuation-mark)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (set! var val))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a) 'all)
                     (test 'a syntax-e (syntax var)))])))
        
        ; quote
        (list (list #'(quote a))
              (lambda (stx)
                (syntax-case stx (quote with-continuation-mark)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (quote sym))
                   (begin
                     (test (void) check-mark (syntax mark-0) '() 'all)
                     (test 'a syntax-e (syntax sym)))])))
                     
        ; quote-syntax
        (list (list #'(quote-syntax a))
              (lambda (stx)
                (syntax-case stx (quote-syntax with-continuation-mark)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (quote-syntax sym))
                   (begin
                     (test (void) check-mark (syntax mark-0) '() 'all)
                     (test 'a syntax-e (syntax sym)))])))
        
        ; wcm is explicitly not working (as opposed to _lots_ of other forms, which simply won't work in 
        ; a stepper sense.  Begin0, for example.  I think.  And set!.  etc., etc.
        
        ; application
        
        (list (list #'(lambda (a b c) (a b)))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (let-values with-continuation-mark begin set!)
                  [(let-values arg-temps
                     (with-continuation-mark
                      key-0
                      mark-0
                      (begin
                        pre-break-0
                        (begin
                          (set! var-0 (with-continuation-mark key-1 mark-1 sym-1))
                          (set! var-1 (with-continuation-mark key-2 mark-2 sym-2))
                          (begin 
                            break-0
                            (with-continuation-mark key-3 mark-3 (sym-3 sym-4)))))))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a b "arg0" "arg1") 'all)
                     (test "arg0" substring (symbol->string (syntax-e (syntax var-0))) 0 4)
                     (test (void) check-mark (syntax mark-1) '() 'all)
                     (test 'a syntax-e (syntax sym-1))
                     (test "arg1" substring (symbol->string (syntax-e (syntax var-1))) 0 4)
                     (test (void) check-mark (syntax mark-2) '() 'all)
                     (test 'b syntax-e (syntax sym-2))
                     (test (void) check-mark (syntax mark-3) '("arg0" "arg1") 'all)
                     (test "arg0" substring (symbol->string (syntax-e (syntax sym-3))) 0 4)
                     (test "arg1" substring (symbol->string (syntax-e (syntax sym-4))) 0 4))])))
        
        ; application with return-wrap
        (list (list #'(+ 3 4))
              (lambda (stx)
                (syntax-case stx (let-values with-continuation-mark begin set!)
                  [(let-values arg-temps
                     (with-continuation-mark
                      key-0
                      mark-0
                      (begin
                        (set! var-1 rhs-1)
                        (set! var-2 rhs-2)
                        (set! var-3 rhs-3)
                        (begin
                          break-0
                          (with-continuation-mark 
                           key-3
                           mark-3
                           result-expr)))))
                   (syntax-case (strip-return-value-wrap (syntax result-expr)) ()
                     [(var-4 var-5 var-6)
                      (begin
                        (test "arg0" substring (symbol->string (syntax-e (syntax var-4))) 0 4)
                        (test "arg1" substring (symbol->string (syntax-e (syntax var-5))) 0 4)
                        (test "arg2" substring (symbol->string (syntax-e (syntax var-6))) 0 4))])])))
        
         ; application with non-var in fun pos
        (list (list #'(4 3 4))
              (lambda (stx)
                (syntax-case stx (let-values with-continuation-mark begin set!)
                  [(let-values arg-temps
                     (with-continuation-mark
                      key-0
                      mark-0
                      (begin
                        (set! var-1 rhs-1)
                        (set! var-2 rhs-2)
                        (set! var-3 rhs-3)
                        (begin
                          break-0
                          (with-continuation-mark key-3 mark-3 (var-4 var-5 var-6))))))
                   (begin 
                     (test "arg0" substring (symbol->string (syntax-e (syntax var-4))) 0 4)
                     (test "arg1" substring (symbol->string (syntax-e (syntax var-5))) 0 4)
                     (test "arg2" substring (symbol->string (syntax-e (syntax var-6))) 0 4))])))
                    
        ; datum
        (list (list #'3)
              (lambda (stx)
                (syntax-case stx (with-continuation-mark #%datum)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (#%datum . 3))
                   #t])))
        
        ; define-values
        (list (list #'(define-values (a b) b))
              (lambda (stx)
                (syntax-case stx (with-continuation-mark define-values)
                  [(define-values (sym-0 sym-1)
                     (with-continuation-mark
                      key-0
                      mark-0
                      body))
                   (begin
                     (test 'a syntax-e (syntax sym-0))
                     (test 'b syntax-e (syntax sym-1))
                     (test (void) check-mark (syntax mark-0) '() 'all))])))
        
        ; top-level vars
        (list (list #'a)
              (lambda (stx)
                (syntax-case stx (begin with-continuation-mark)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (begin 
                      break-0
                      body))
                   (begin
                     (test (void) check-mark (syntax mark-0) '() 'all)
                     (test 'a syntax-e (strip-return-value-wrap (syntax body))))])))
        
        ; lexical vars
        (list (list #'(lambda (a b) a))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (begin with-continuation-mark)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (begin
                      pre-break-0
                      sym-0))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a) 'all)
                     (test 'a syntax-e (syntax sym-0)))])))
        
        ; correct labelling of variables:
        (list (list #'(lambda (b) (let ([a 13]) (begin a b))))
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (begin with-continuation-mark let*-values set!-values)
                  [(let*-values bindings
                     (with-continuation-mark
                      key-0
                      mark-0
                      (begin
                        pre-break-0
                        ;(begin
                        ;  break-0
                          (begin
                            (set!-values (a-var-0) rest0)
                            (set! counter-0 1)
                            ;(begin 
                            ;  break-1
                              (with-continuation-mark
                               key-1
                               mark-1
                               (begin
                                 (with-continuation-mark key-2 mark-2 (begin break-2 a-var-1))
                                 (with-continuation-mark key-3 mark-3 (begin pre-break-1 b-var-0))))
                            ;  )
                            )
                        ;  )
                        )))
                   (begin
                     (test 'a syntax-e (syntax a-var-0))
                     (test 'a syntax-e (strip-return-value-wrap (syntax a-var-1)))
                     (test 'b syntax-e (syntax b-var-0))
                     (test 'let-bound syntax-property (syntax a-var-0) 'stepper-binding-type)
                     (test 'let-bound syntax-property (strip-return-value-wrap (syntax a-var-1)) 'stepper-binding-type)
                     (test 'lambda-bound syntax-property (syntax b-var-0) 'stepper-binding-type)
                     )])))
                        
        
        ))

(for-each (lambda (test-case)
            ((cadr test-case) (car (annotate-exprs-no-break (car test-case)))))
          test-cases)

;(namespace-annotate-expr '(or 3 4 5) beginner-namespace)
  
(syntax-case (namespace-rewrite-expr '(lambda (a) a) mz-namespace) (lambda)
  [(lambda (a-var-0) a-var-1)
   (begin
     (test 'lambda-bound syntax-property (syntax a-var-0) 'stepper-binding-type)
     (test 'lexical identifier-binding (syntax a-var-0)) 
     (test 'lambda-bound syntax-property (syntax a-var-1) 'stepper-binding-type)
     (test 'lexical identifier-binding (syntax a-var-1)))])

(syntax-case (namespace-rewrite-expr (datum->syntax-object #'here '(case-lambda ((a) a) ((a b) b))) mz-namespace) (case-lambda)
  [(case-lambda ((a-0) a-1) ((a-2 b-0) b-1))
   (begin
     (test 'lexical identifier-binding (syntax b-0))
     (test 'lexical identifier-binding (syntax b-1)))])

;(syntax-case (car (namespace-rewrite-expr "(or true false true)" beginner-namespace)) (let-values if)
;  [(let-values ([(or-part-0) true-0]) (if or-part-1 or-part-2 rest))
;   (begin
;     (test 'or-part syntax-e (syntax or-part-0))
;     (test 'let-bound syntax-property (syntax or-part-0) 'stepper-binding-type)
;     (test 'or-part syntax-e (syntax or-part-1))
;     (test 'let-bound syntax-property (syntax or-part-1) 'stepper-binding-type))])

(parameterize ([current-namespace beginner-namespace])
  (err/rt-test (eval (car (annotate-exprs-no-break (string->stx-list "(define first 3)")))) exn:user?))

;(test 7 eval (car (annotate-exprs-no-break (list #'(begin (+ 3 4) (+ 4 5))))))
;(test 9 eval (car (annotate-exprs-no-break (list #'(begin (+ 3 4) (+ 4 5))))))

(report-errs)
