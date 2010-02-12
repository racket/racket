#lang scheme

(require "../private/matcher.ss"
         errortrace/errortrace-lib
         errortrace/errortrace-key)
(provide test test-syn-err tests reset-count
         syn-err-test-namespace
         print-tests-passed
         equal/bindings?
         runtime-error-source)

(define syn-err-test-namespace (make-base-namespace))
(parameterize ([current-namespace syn-err-test-namespace])
  (eval '(require redex/reduction-semantics)))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ expected got)
     (with-syntax ([line (syntax-line (syntax got))]
                   [fn (if (path? (syntax-source (syntax got)))
                           (path->string (syntax-source (syntax got)))
                           "<unknown file>")])
       (syntax/loc stx (test/proc (λ () expected) got line fn)))]))

(define (runtime-error-source sexp src)
  (let/ec return
    (cadar
     (continuation-mark-set->list
      (exn-continuation-marks
       (with-handlers ((exn:fail? values))
         (parameterize ([current-compile (make-errortrace-compile-handler)])
           (eval (read-syntax src (open-input-string (format "~s" sexp)))))
         (return 'no-source)))
      errortrace-key))))

(define-syntax (test-syn-err stx)
  (syntax-case stx ()
    [(_ exp msg-re num-locs)
     (with-syntax ([expected-locs (syntax/loc stx (build-list num-locs (λ (_) src)))])
       (syntax
        (let* ([src (gensym)]
               [p (read-syntax src (open-input-string (format "~s" 'exp)))])
          (let-values ([(locs msg)
                        (with-handlers ([exn:fail:syntax?
                                         (λ (exn)
                                           (values 
                                            (if (exn:srclocs? exn)
                                                (map srcloc-source 
                                                     ((exn:srclocs-accessor exn) exn))
                                                null)
                                            (exn-message exn)))])
                          (parameterize ([current-namespace syn-err-test-namespace])
                            (expand p))
                          (values (void) null))])
            (test msg msg-re)
            (test locs expected-locs)))))]
    [(tse exp msg-re)
     (syntax/loc stx (tse exp msg-re 1))]))

(define tests 0)
(define failures 0)
(define (reset-count) 
  (set! tests 0)
  (set! failures 0))

(define (print-tests-passed filename)
  (cond
    [(= 0 failures)
     (printf "~a: all ~a tests passed.\n" filename tests)]
    [else
     (printf "~a: ~a test~a failed.\n" filename failures (if (= 1 failures) "" "s"))]))

(define (test/proc run expected line filename)
  ;(printf "testing line ~s:~s\n" filename line)
  (let ([got (with-handlers ((exn:fail? values)) (run))])
    (set! tests (+ tests 1))
    (unless (and (not (exn? got))
                 (matches? got expected))
      (set! failures (+ 1 failures))
      (fprintf (current-error-port)
               "test/proc: file ~a line ~a:\n     got ~s\nexpected ~s\n\n" 
               filename 
               line
               got
               expected))))

(define (matches? got expected)
  (cond
    [(regexp? expected)
     (and (string? got) (regexp-match expected got) #t)]
    [else
     (equal/bindings? got expected)]))

;; equal/bindings? : any any -> boolean
;; compares two sexps (with embedded bindings) for equality.
;; uses an order-insensitive comparison for the bindings
(define (equal/bindings? fst snd)
  (let loop ([fst fst]
             [snd snd])
    (cond
      [(pair? fst)
       (and (pair? snd) 
            (loop (car fst) (car snd))
            (loop (cdr fst) (cdr snd)))]
      [(mtch? fst)
       (and (mtch? snd)
            (loop (mtch-bindings fst)
                  (mtch-bindings snd))
            (let ([g1 (gensym 'run-match-test-sym)])
              (equal/bindings? (mtch-context fst)
                               (mtch-context snd)))
            (equal/bindings? (mtch-hole fst)
                             (mtch-hole snd)))]
      [(bindings? fst)
       (and (bindings? snd)
            (let ([fst-table (bindings-table fst)]
                  [snd-table (bindings-table snd)])
              (and (= (length fst-table)
                      (length snd-table))
                   (andmap
                    loop
                    (sort fst-table rib-lt)
                    (sort snd-table rib-lt)))))]
      [(and (bind? fst)
            (bind? snd)
            (context? (bind-exp fst))
            (context? (bind-exp snd)))
       (and (equal? (bind-name fst) (bind-name snd))
            (let ([g (gensym 'run-match-test-sym2)])
              (equal/bindings? (bind-exp fst)
                               (bind-exp snd))))]
      [(and (hole? fst)
            (hole? snd))
       #t]
      [else (equal? fst snd)])))

;; rib-lt : rib rib -> boolean
(define (rib-lt r1 r2) (string<=? (format "~s" (bind-name r1))
                                  (format "~s" (bind-name r2))))
