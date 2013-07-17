#lang racket/base
(require racket/list
         racket/contract)

(struct monitor (label) #:transparent)
(struct monitor:proj monitor (proj-label v) #:transparent)
(struct monitor:call monitor (proj-label f app-label kws kw-args args) #:transparent)
(struct monitor:return monitor (proj-label f app-label kws kw-args args rets) #:transparent)

(define (monitor/c monitor-allows? label c)
  (define ctc (coerce-contract 'monitored c))
  (make-contract
   #:name (build-compound-type-name 'monitored label c)
   #:projection
   (λ (b)
     (define proj ((contract-projection ctc) b))
     (define bs (blame-swap b))
     (λ (x)
       (define proj-label (gensym label))
       (define proj-x (proj x))
       ; XXX Find a way to get a meaningful reason why the monitor failed
       (if (monitor-allows? (monitor:proj label proj-label proj-x))
           (if (procedure? proj-x)
               (make-keyword-procedure
                ; XXX Could I specialize for a few arguments/returns/no kws?
                (λ (kws kw-args . args)
                  (define app-label (gensym label))
                  (if (monitor-allows? (monitor:call label proj-label proj-x app-label kws kw-args args))
                      (call-with-values
                       (λ () (keyword-apply proj-x kws kw-args args))
                       (λ rets
                         (if (monitor-allows? (monitor:return label proj-label proj-x app-label kws kw-args args rets))
                             (apply values rets)
                             (raise-blame-error b x "temporal monitor disallowed return of ~e" rets))))
                      (cond
                        [(and (empty? kws) (empty? kw-args))
                         (raise-blame-error bs x "temporal monitor disallowed call with\n\targuments ~e" args)]
                        [else
                         (raise-blame-error bs x "temporal monitor disallowed call with\n\tkeywords ~e\n\tkeyword arguments ~e\n\tnormal arguments ~e" kws kw-args args)]))))
               proj-x)
           (raise-blame-error b x "temporal monitor disallowed projection of ~e" x))))))

(provide (struct-out monitor)
         (struct-out monitor:proj)
         (struct-out monitor:call)
         (struct-out monitor:return)
         monitor/c)
