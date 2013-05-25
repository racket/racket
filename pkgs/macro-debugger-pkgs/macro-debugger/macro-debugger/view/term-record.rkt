#lang racket/base
(require racket/class
         racket/match
         syntax/stx
         unstable/find
         unstable/class-iop
         "interfaces.rkt"
         "step-display.rkt"
         macro-debugger/model/deriv
         macro-debugger/model/deriv-util
         macro-debugger/model/deriv-parser
         macro-debugger/model/trace
         macro-debugger/model/reductions-config
         macro-debugger/model/reductions
         macro-debugger/model/steps
         "cursor.rkt")

(provide term-record%)

;; TermRecords

(define term-record%
  (class* object% (term-record<%>)
    (init-field/i (stepper widget<%>))

    (define/i config config<%>
      (send/i stepper widget<%> get-config))
    (define/i displayer step-display<%>
      (send/i stepper widget<%> get-step-displayer))

    ;; Data

    (init-field [events #f])

    (init-field [raw-deriv #f])
    (define raw-deriv-oops #f)

    (define deriv #f)
    (define deriv-hidden? #f)
    (define shift-table #f)

    (define raw-steps #f)
    (define raw-steps-estx #f) ;; #f if raw-steps-exn is exn
    (define raw-steps-exn #f) ;; #f if raw-steps-estx is syntax
    (define raw-steps-binders #f)
    (define raw-steps-definites #f)
    (define raw-steps-oops #f)

    (define steps #f)

    ;; --

    (define steps-position #f)

    (define/private (status msg)
      (send stepper change-status msg))
    (define-syntax-rule (with-status msg . body)
      (begin (send stepper change-status msg)
             (begin0 (let () . body))))

    (super-new)

    (define-syntax define-guarded-getters
      (syntax-rules ()
        [(define-guarded-getters guard (method expr) ...)
         (begin (define/public (method) guard expr) ...)]))

    (define/public (get-events) events)
    (define/public (get-raw-deriv) raw-deriv)

    (define-guarded-getters (recache-deriv!)
      [get-deriv deriv]
      [get-deriv-hidden? deriv-hidden?]
      [get-shift-table shift-table])
    (define-guarded-getters (recache-raw-steps!)
      [get-raw-steps-binders raw-steps-binders]
      [get-raw-steps-definites raw-steps-definites]
      [get-raw-steps-exn raw-steps-exn]
      [get-raw-steps-oops raw-steps-oops])
    (define-guarded-getters (recache-steps!)
      [get-steps steps])

    ;; invalidate-steps! : -> void
    ;; Invalidates cached parts that depend on reductions config
    (define/public (invalidate-steps!)
      (set! steps #f))

    ;; invalidate-raw-steps! : -> void
    (define/public (invalidate-raw-steps!)
      (invalidate-steps!)
      (set! raw-steps #f)
      (set! raw-steps-estx #f)
      (set! raw-steps-exn #f)
      (set! raw-steps-binders #f)
      (set! raw-steps-definites #f)
      (set! raw-steps-oops #f))

    ;; invalidate-synth! : -> void
    ;; Invalidates cached parts that depend on macro-hiding policy
    (define/public (invalidate-synth!)
      (invalidate-raw-steps!))

    ;; invalidate-deriv! : -> void
    (define/public (invalidate-deriv!)
      (invalidate-synth!)
      (set! deriv #f)
      (set! deriv-hidden? #f)
      (set! shift-table #f))

    ;; recache! : -> void
    (define/public (recache!)
      (recache-steps!))

    ;; recache-raw-deriv! : -> void
    (define/private (recache-raw-deriv!)
      (unless (or raw-deriv raw-deriv-oops)
        (with-handlers ([(lambda (e) #t)
                         (lambda (e)
                           (set! raw-deriv-oops e))])
          (with-status "Parsing expansion derivation"
            (set! raw-deriv
                  (parse-derivation
                   (events->token-generator events)))))))

    ;; recache-deriv! : -> void
    (define/private (recache-deriv!)
      (unless (or deriv deriv-hidden?)
        (recache-raw-deriv!)
        (when raw-deriv
          (with-status "Processing expansion derivation"
            (let ([process (send/i stepper widget<%> get-preprocess-deriv)])
              (let ([d (process raw-deriv)])
                (when (not d)
                  (set! deriv-hidden? #t))
                (when d
                  (set! deriv d)
                  (set! shift-table (compute-shift-table d)))))))))

    ;; recache-synth! : -> void
    (define/private (recache-synth!)
      (recache-deriv!))

    ;; recache-raw-steps! : -> void
    (define/private (recache-raw-steps!)
      (unless (or raw-steps raw-steps-oops)
        (recache-synth!)
        (when deriv
          (with-status "Computing reduction steps"
            (let ([show-macro? (or (send/i stepper widget<%> get-show-macro?)
                                   (lambda (id) #t))])
              (with-handlers ([(lambda (e) #t)
                               (lambda (e)
                                 (set! raw-steps-oops e))])
                (let-values ([(raw-steps* binders* definites* estx* error*)
                              (parameterize ((macro-policy show-macro?))
                                (reductions+ deriv))])
                  (set! raw-steps raw-steps*)
                  (set! raw-steps-estx estx*)
                  (set! raw-steps-exn error*)
                  (set! raw-steps-binders binders*)
                  (set! raw-steps-definites definites*))))))))

    ;; recache-steps! : -> void
    (define/private (recache-steps!)
      (unless (or steps)
        (recache-raw-steps!)
        (when raw-steps
          (with-status "Processing reduction steps"
            (set! steps
                  (and raw-steps
                       (let* ([filtered-steps 
                               (if (send/i config config<%> get-show-rename-steps?)
                                   raw-steps
                                   (filter (lambda (x) (not (rename-step? x)))
                                           raw-steps))]
                              [processed-steps
                               (if (send/i config config<%> get-one-by-one?)
                                   (reduce:one-by-one filtered-steps)
                                   filtered-steps)])
                         (cursor:new processed-steps))))
            (restore-position)))))

    ;; reduce:one-by-one : (list-of step) -> (list-of step)
    (define/private (reduce:one-by-one rs)
      (let loop ([rs rs])
        (match rs
          [(cons (struct step (type s1 s2)) rs)
           (list* (make prestep type s1)
                  (make poststep type s2)
                  (loop rs))]
          [(cons (struct misstep (type s1 exn)) rs)
           (list* (make misstep type s1 exn)
                  (loop rs))]
          [(cons (and r (remarkstep type s1 contents)) rs)
           (list* r (loop rs))]
          ['()
           null])))

    ;; Navigation

    (define/public-final (has-prev?)
      (and (get-steps) (not (cursor:at-start? (get-steps)))))
    (define/public-final (has-next?)
      (and (get-steps) (not (cursor:at-end? (get-steps)))))

    (define/public-final (get-step-index)
      (let ([steps (get-steps)])
        (and steps (cursor-position steps))))
    (define/public-final (get-step-count)
      (let ([steps (get-steps)])
        (and steps (cursor-count steps))))

    (define/public-final (navigate-to-start)
      (cursor:move-to-start (get-steps))
      (save-position))
    (define/public-final (navigate-to-end)
      (cursor:move-to-end (get-steps))
      (save-position))
    (define/public-final (navigate-previous)
      (cursor:move-prev (get-steps))
      (save-position))
    (define/public-final (navigate-next)
      (cursor:move-next (get-steps))
      (save-position))
    (define/public-final (navigate-to n)
      (cursor:skip-to (get-steps) n)
      (save-position))

    ;; save-position : -> void
    (define/private (save-position)
      (when (cursor? steps)
        (let ([step (cursor:next steps)])
          (cond [(not step)
                 ;; At end; go to the end when restored
                 (set! steps-position +inf.0)]
                [(protostep? step)
                 (set! steps-position
                       (extract-protostep-seq step))]))))

    ;; restore-position : number -> void
    (define/private (restore-position)
      (define (seek)
        (let ([step (cursor:next steps)])
          (cond [(not step)
                 ;; At end; stop
                 (void)]
                [(protostep? step)
                 (let ([step-pos (extract-protostep-seq step)])
                   (cond [(not step-pos)
                          (cursor:move-next steps)
                          (seek)]
                         [(< step-pos steps-position)
                          (cursor:move-next steps)
                          (seek)]
                         [else (void)]))])))
      (when steps-position
        (seek)))

    ;; extract-protostep-seq : step -> number/#f
    (define/private (extract-protostep-seq step)
      ;; FIXME: add back step numbers
      (state-seq (protostep-s1 step)))

    ;; Warnings display

    ;; on-get-focus : -> void
    (define/public (on-get-focus)
      (recache-synth!))

    ;; on-lose-focus : -> void
    (define/public (on-lose-focus)
      (when steps (cursor:move-to-start steps))
      (set! steps-position #f))

    ;; Rendering

    ;; display-initial-term : -> void
    (define/public (display-initial-term)
      (with-status "Rendering term"
        (cond [raw-deriv-oops
               (send/i displayer step-display<%> add-internal-error
                       "derivation" raw-deriv-oops #f events)]
              [else
               (send/i displayer step-display<%> add-syntax (wderiv-e1 deriv))])))

    ;; display-final-term : -> void
    (define/public (display-final-term)
      (recache-steps!)
      (with-status "Rendering term"
        (cond [(syntax? raw-steps-estx)
               (send/i displayer step-display<%> add-syntax raw-steps-estx
                       #:binders raw-steps-binders
                       #:shift-table shift-table
                       #:definites raw-steps-definites)]
              [(exn? raw-steps-exn)
               (send/i displayer step-display<%> add-error raw-steps-exn)]
              [else (display-oops #f)])))

    ;; display-step : -> void
    (define/public (display-step)
      (recache-steps!)
      (with-status "Rendering step"
        (cond [steps
               (let ([step (cursor:next steps)])
                 (if step
                     (send/i displayer step-display<%> add-step step
                             #:shift-table shift-table)
                     (send/i displayer step-display<%> add-final raw-steps-estx raw-steps-exn
                             #:binders raw-steps-binders
                             #:shift-table shift-table
                             #:definites raw-steps-definites)))]
              [else (display-oops #t)])))

    ;; display-oops : boolean -> void
    (define/private (display-oops show-syntax?)
      (cond [raw-steps-oops
             (send/i displayer step-display<%> add-internal-error
                     "steps" raw-steps-oops
                     (and show-syntax? (wderiv-e1 deriv))
                     events)]
            [raw-deriv-oops
             (send/i displayer step-display<%> add-internal-error
                     "derivation" raw-deriv-oops #f events)]
            [else
             (error 'term-record::display-oops "internal error")]))
    ))


;; compute-shift-table : deriv -> hash[id => (listof id)]
(define (compute-shift-table d)
  (define ht (make-hasheq))
  (define module-forms
    (find p:module? d #:stop-on-found? #t))
  (define module-shift-renamers
    (for/list ([mf module-forms])
      (let ([shift (p:module-shift mf)]
            [body (p:module-body mf)])
        (and shift body
             (with-syntax ([(_module _name _lang shifted-body) shift])
               (add-rename-mapping ht (wderiv-e2 body) #'shifted-body))))))
  ht)

(define (add-rename-mapping ht from to)
  (define (loop from to)
    (cond [(and (stx-pair? from) (stx-pair? to))
           (loop (stx-car from) (stx-car to))
           (loop (stx-cdr from) (stx-cdr to))]
          [(and (identifier? from) (identifier? to))
           (hash-set! ht from (cons to (hash-ref ht from null)))]
          [else (void)]))
  (loop from to)
  (void))
