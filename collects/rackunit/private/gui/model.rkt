#lang racket/base
(require racket/class
         unstable/class-iop
         data/gvector
         "../base.rkt"
         "interfaces.rkt"
         "cache-box.rkt")
(provide case-result%
         suite-result%)

(define result%
  (class* object% ()
    (super-new)

    (init-field parent
                controller
                name
                test)

    (when parent
      (send/i parent suite<%> add-child this))

    (define/public (get-parent) parent)
    (define/public (get-name) name)
    (define/public (get-controller) controller)
    (define/public (get-test) test)
    ))

;; case-result%
(define case-result%
  (class* result% (case<%>)
    (super-new)

    (inherit get-name
             get-parent
             get-controller)

    ;; *result : #f or test-result
    ;; #f means not finished executing
    (define *result #f)

    (define result #f)
    (define properties #f)
    (define timing #f)
    (define output null)
    (define trash null)

    (define/public (update *result* result* properties* timing* output* trash*)
      (set! *result *result*)
      (set! result result*)
      (set! properties properties*)
      (set! timing timing*)
      (set! output output*)
      (set! trash trash*)
      (send/i (get-controller) controller<%> on-model-status-change this))

    (define/public (finished?) (and *result #t))
    (define/public (success?) (test-success? *result))
    (define/public (failure?) (test-failure? *result))
    (define/public (error?) (test-error? *result))

    (define/public (get-total-cases) 1)
    (define/public (get-total-successes)
      (if (success?) 1 0))
    (define/public (get-total-failures)
      (if (or (failure?) (error?)) 1 0))

    (define/public (get-result) result)
    (define/public (get-timing) timing)
    (define/public (get-trash) trash)
    (define/public (has-trash?) (pair? trash))
    (define/public (get-property p)
      (let [(v (assq p properties))]
        (and v (cdr v))))
    (define/public (get-property-set p)
      (map cdr (filter (lambda (kv) (eq? (car kv) p)) properties)))
    (define/public (get-all-properties)
      properties)

    (define/public (get-output) (reverse output))
    (define/public (has-output?) (pair? output))))


;; suite-result%
(define suite-result%
  (class* result% (suite<%>)
    (super-new)
    (inherit get-name
             get-parent
             get-controller)

    (define done? #f)
    (define children (make-gvector))

    ;; get-children : -> (listof result<%>)
    (define/public (get-children)
      (for/list ([x (in-gvector children)]) x))

    (define/public (add-child c)
      (gvector-add! children c))

    (define/public (finish!)
      (set! done? #t)
      (send/i (get-controller) controller<%> on-model-status-change this))

    (define children-cache
      (cache (for/fold ([cs 0] [ss 0] [fs 0] [out? #f] [trash? #f])
                 ([c (in-gvector children)])
               (values (+ cs (send/i c result<%> get-total-cases))
                       (+ ss (send/i c result<%> get-total-successes))
                       (+ fs (send/i c result<%> get-total-failures))
                       (or out? (send/i c result<%> has-output?))
                       (or trash? (send/i c result<%> has-trash?))))))

    (define/public (finished?)
      done?)
    (define/public (get-total-cases)
      (define-values (c _s _f _o _t) (cache-ref children-cache))
      c)
    (define/public (get-total-successes)
      (define-values (_c s _f _o _t) (cache-ref children-cache))
      s)
    (define/public (get-total-failures)
      (define-values (_c _s f _o _t) (cache-ref children-cache))
      f)
    (define/public (has-output?)
      (define-values (_c _s _f o _t) (cache-ref children-cache))
      o)
    (define/public (has-trash?)
      (define-values (_c _s _f _o t) (cache-ref children-cache))
      t)

    (define/public (success?)
      (and (finished?) (zero? (get-total-failures))))
    (define/public (failure?)
      (positive? (get-total-failures)))
    (define/public (error?) #f)

    ;; on-child-status-change : model<%> -> void
    (define/public (on-child-status-change child)
      (let ([result
             (call-with-values (lambda () (cache-ref children-cache)) list)])
        (cache-invalidate! children-cache)
        (let ([new-result
               (call-with-values (lambda () (cache-ref children-cache)) list)])
          (unless (equal? new-result result)
            (send/i (get-controller) controller<%> on-model-status-change this)))))))
