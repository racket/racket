#lang racket/base
(require "../common/check.rkt")

(provide (struct-out logger)
         logger-name
         create-logger
         logger-receivers)

(struct logger (topic     ; symbol or #f
                parent    ; logger or #f
                propagate-filters
                [receiver-box+backrefs #:mutable] ; list of (cons weak-box any)
                [prune-counter #:mutable] ; number of adds before checking empied boxes
                [permanent-receivers #:mutable] ; receivers to retain strongly
                [max-receiver-level #:mutable] ; up-to-date if `local-level-timestamp` = `(unbox root-level-timestamp-box)`
                topic-level-cache ; topic -> level cache
                [local-level-timestamp #:mutable] ; integer
                root-level-timestamp-box ; box of integer
                level-sema-box)) ; box for sema to report when a receiver is added

(define/who (logger-name logger)
  (check who logger? logger)
  (logger-topic logger))

(define (create-logger #:topic topic #:parent parent #:propagate-filters propagate-filters)
  (logger topic
          parent
          propagate-filters
          null ; receiver-box+backrefs
          8 ; prune-counter
          null ; permanent-receivers
          'none ; max-receiver-level
          (make-vector 16) ; topic-level-cache
          -1 ; local-level-timestamp
          (if parent
              (logger-root-level-timestamp-box parent)
              (box 0))
          (if parent
              (logger-level-sema-box parent)
              (box #f))))

;; Get log receivers, dropping any boxes made empty due to a weak
;; reference:
(define (logger-receivers logger)
  (for*/list ([b+r (in-list (logger-receiver-box+backrefs logger))]
              [lr (in-value (weak-box-value (car b+r)))]
              #:when lr)
    lr))
