#lang scheme

(provide/contract (find-referenced-locals ((listof identifier?) syntax? . -> . (listof identifier?))))
(define (find-referenced-locals env-ids stx)
  (local ([define id-hash (make-custom-hash free-identifier=?
                                            (λ (v) (equal-hash-code (syntax->datum v)))
                                            (λ (v) (equal-secondary-hash-code (syntax->datum v))))]
          [define (find stx)
            (syntax-case stx ()
              [(head . tail)
               (begin
                 (find #'head)
                 (find #'tail))]
              [id (identifier? stx)
                  (begin
                    (unless (dict-ref id-hash stx false)
                    (dict-set! id-hash stx true)))]
              [_ (void)])])
    (find stx)
    (filter (λ (env-id) (dict-ref id-hash env-id false)) env-ids)))
