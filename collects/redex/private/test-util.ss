#lang scheme

(require "matcher.ss")
(provide test test-syn-err tests reset-count
         syn-err-test-namespace
         print-tests-passed
         equal/bindings?)

(define syn-err-test-namespace (make-base-namespace))
(parameterize ([current-namespace syn-err-test-namespace])
  (eval '(require "../reduction-semantics.ss")))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ expected got)
     (with-syntax ([line (syntax-line (syntax got))]
                   [fn (if (path? (syntax-source (syntax got)))
                           (path->string (syntax-source (syntax got)))
                           "<unknown file>")])
       (syntax/loc stx (test/proc (λ () expected) got line fn)))]))

(define-syntax (test-syn-err stx)
  (syntax-case stx ()
    [(_ exp regexp)
     (syntax/loc stx
       (test
        (parameterize ([current-namespace syn-err-test-namespace])
          (with-handlers ((exn:fail:syntax? exn-message))
            (expand 'exp)
            'no-error-raised))
        regexp))]))

(define tests 0)
(define failures 0)
(define (reset-count) (set! tests 0))

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
