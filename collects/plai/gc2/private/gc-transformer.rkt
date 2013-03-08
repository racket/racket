#lang racket/base
(require racket/contract
         racket/dict)
(provide
 (contract-out
  [find-referenced-locals (-> (listof identifier?) syntax? (listof identifier?))]
  [switch-over (-> (listof identifier?) (listof identifier?) (listof identifier?) (listof identifier?))]))

(define (find-referenced-locals env-ids stx)
  (define id-hash
    (make-custom-hash free-identifier=?
                      (λ (v) (equal-hash-code (syntax->datum v)))
                      (λ (v) (equal-secondary-hash-code (syntax->datum v)))))
  (let find ([stx stx])
    (syntax-case stx ()
      [(head . tail)
       (begin
         (find #'head)
         (find #'tail))]
      [id (identifier? stx)
          (unless (dict-ref id-hash stx #false)
            (dict-set! id-hash stx #true))]
      [_ (void)]))
  (filter (λ (env-id) (dict-ref id-hash env-id #false)) env-ids))

(define (switch-over src-ids dest-ids to-switch)
  (for/list ([id (in-list to-switch)])
    (or (for/or ([src-id (in-list src-ids)]
                 [dest-id (in-list dest-ids)])
          (and (free-identifier=? src-id id)
               dest-id))
        (error 'switch-over "didn't find src-id for ~s" id))))

