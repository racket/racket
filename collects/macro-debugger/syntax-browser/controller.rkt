#lang racket/base
(require racket/class
         unstable/class-iop
         "interfaces.rkt"
         "partition.rkt"
         unstable/gui/notify)
(provide controller%)

;; displays-manager-mixin
(define displays-manager-mixin
  (mixin () (displays-manager<%>)
    ;; displays : (list-of display<%>)
    (field [displays null])

    ;; add-syntax-display : display<%> -> void
    (define/public (add-syntax-display c)
      (set! displays (cons c displays)))

    ;; remove-all-syntax-displays : -> void
    (define/public (remove-all-syntax-displays)
      (set! displays null))

    (super-new)))

;; selection-manager-mixin
(define selection-manager-mixin
  (mixin (displays-manager<%>) (selection-manager<%>)
    (inherit-field displays)
    (define-notify selected-syntax (new notify-box% (value #f)))

    (super-new)
    (listen-selected-syntax
     (lambda (new-value)
       (for-each (lambda (display) (send/i display display<%> refresh))
                 displays)))))

;; mark-manager-mixin
(define mark-manager-mixin
  (mixin () (mark-manager<%>)
    (init-field/i [primary-partition partition<%> (new-bound-partition)])
    (super-new)

    ;; get-primary-partition : -> partition
    (define/public-final (get-primary-partition)
      primary-partition)

    ;; reset-primary-partition : -> void
    (define/public-final (reset-primary-partition)
      (set! primary-partition (new-bound-partition)))))

;; secondary-partition-mixin
(define secondary-partition-mixin
  (mixin (displays-manager<%>) (secondary-partition<%>)
    (inherit-field displays)
    (define-notify identifier=? (new notify-box% (value #f)))
    (define-notify secondary-partition (new notify-box% (value #f)))

    (listen-identifier=?
     (lambda (name+proc)
       (set-secondary-partition
        (and name+proc
             (new partition% (relation (cdr name+proc)))))))
    (listen-secondary-partition
     (lambda (p)
       (for ([d displays])
         (send/i d display<%> refresh))))
    (super-new)))

(define controller%
  (class* (secondary-partition-mixin
           (selection-manager-mixin
            (mark-manager-mixin
             (displays-manager-mixin
              object%))))
    (controller<%>)
    (super-new)))
