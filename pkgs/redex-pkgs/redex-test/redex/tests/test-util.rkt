#lang scheme

(require redex/private/matcher
         redex/private/lang-struct
         (for-syntax syntax/parse setup/path-to-relative)
         setup/path-to-relative
         racket/runtime-path)
(provide test tests reset-count
         print-tests-passed
         equal/bindings?
         test-contract-violation
         test/proc)

(define (read-syntax-test path)
  (call-with-input-file path
    (λ (port)
      (port-count-lines! port)
      (read-syntax path port))))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ expected got)
     (with-syntax ([line (syntax-line stx)]
                   [fn (if (path? (syntax-source (syntax got)))
                           (path->relative-string/library (syntax-source (syntax got)))
                           "<unknown file>")])
       (syntax/loc stx (test/proc (λ () expected) got line fn)))]))

(define tests 0)
(define failures 0)
(define (reset-count) 
  (set! tests 0)
  (set! failures 0))

(define (print-tests-passed filename)
  (cond
    [(= 0 tests)
     (printf "~a: no tests were run.\n" filename)]
    [(= 0 failures)
     (printf "~a: ~a passed.\n" 
             filename 
             (case tests
               [(1) "1 test"]
               [(2) "both tests"]
               [else (format "~a tests" tests)]))]
    [else
     (printf "~a: ~a test~a failed.\n" filename failures (if (= 1 failures) "" "s"))]))

(define (test/proc run expected line filename)
  ;(printf "testing line ~s:~s\n" filename line)
  (let ([got (with-handlers ((exn:fail? values)) (run))])
    (set! tests (+ tests 1))
    (unless (and (not (exn? got))
                 (matches? got expected))
      (set! failures (+ 1 failures))
      (eprintf "test: file ~a line ~a:\n     got ~s\nexpected ~s\n"
               filename
               line
               got
               expected)
      (when (exn:fail? got)
        ((error-display-handler) (exn-message got) got))
      (eprintf "\n"))))

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

(define-syntax (test-contract-violation stx)
  (syntax-parse stx
    [(_ expr
        (~or (~once (~seq #:blaming blaming:expr))
             (~optional (~seq #:message message:expr)
                        #:defaults ([message #'""]))
             (~optional (~seq #:extract extract:expr)
                        #:defaults ([extract #'values])))
        ...)
     #`(test (with-handlers ([(λ (exn)
                                (let ([exn (extract exn)])
                                  (and (exn:fail:contract:blame? exn)
                                       (regexp-match? 
                                        blaming
                                        (format "~a" (blame-positive (exn:fail:contract:blame-object exn)))))))
                              exn-message])
               expr
               (gensym 'no-violation))
             #,(syntax/loc stx (regexp message)))]))
