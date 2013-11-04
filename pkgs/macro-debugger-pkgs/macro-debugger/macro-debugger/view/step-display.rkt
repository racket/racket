#lang racket/base
(require racket/class
         racket/match
         racket/gui/base
         unstable/class-iop
         "interfaces.rkt"
         macro-debugger/model/steps
	 (prefix-in sb: macro-debugger/syntax-browser/interfaces)
         macro-debugger/view/debug-format)

#;
(provide step-display%
         step-display<%>)
(provide (all-defined-out))
;; Struct for one-by-one stepping

(define-struct (prestep protostep) ())
(define-struct (poststep protostep) ())

(define (prestep-term1 s) (state-term (protostep-s1 s)))
(define (poststep-term2 s) (state-term (protostep-s1 s)))

(define step-display%
  (class* object% (step-display<%>)

    (init-field/i (config config<%>))
    (init-field ((sbview syntax-widget)))
    (super-new)

    (define/public (add-internal-error part exn stx events)
      (send/i sbview sb:syntax-browser<%> add-text
              (string-append
               (if (exn:break? exn)
                   "Macro stepper was interrupted"
                   "Macro stepper error")
               (if part
                   (format " (~a)" part)
                   "")))
      (when (exn? exn)
        (send/i sbview sb:syntax-browser<%> add-text " ")
        (send/i sbview sb:syntax-browser<%> add-clickback "[details]"
                (lambda _ (show-internal-error-details exn events))))
      (send/i sbview sb:syntax-browser<%> add-text ".  ")
      (when stx (send/i sbview sb:syntax-browser<%> add-text "Original syntax:"))
      (send/i sbview sb:syntax-browser<%> add-text "\n")
      (when stx (send/i sbview sb:syntax-browser<%> add-syntax stx)))

    (define/private (show-internal-error-details exn events)
      (case (message-box/custom (if (exn:break? exn)
                                    "Macro stepper was interrupted"
                                    "Macro stepper internal error")
                                (format "Internal error:\n~a" (exn-message exn))
                                "Show error"
                                "Dump debugging file"
                                "Cancel")
        ((1) (queue-callback
              (lambda ()
                (raise exn))))
        ((2) (queue-callback
              (lambda ()
                (let ([file (put-file)])
                  (when file
                    (write-debug-file file exn events))))))
        ((3 #f) (void))))

    (define/public (add-error exn)
      (send*/i sbview sb:syntax-browser<%>
        (add-error-text (exn-message exn))
        (add-text "\n")))

    (define/public (add-step step
                             #:shift-table [shift-table #f])
      (cond [(step? step)
             (show-step step shift-table)]
            [(misstep? step)
             (show-misstep step shift-table)]
            [(remarkstep? step)
             (show-remarkstep step shift-table)]
            [(prestep? step)
             (show-prestep step shift-table)]
            [(poststep? step)
             (show-poststep step shift-table)]))

    (define/public (add-syntax stx
                               #:binders [binders '#hash()]
                               #:definites [definites #f]
                               #:shift-table [shift-table '#hash()])
      (send/i sbview sb:syntax-browser<%> add-syntax stx
              #:binders binders
              #:definites definites
              #:shift-table shift-table))

    (define/public (add-final stx error
                              #:binders binders
                              #:definites definites
                              #:shift-table [shift-table #f])
      (when stx
        (send*/i sbview sb:syntax-browser<%>
          (add-text "Expansion finished\n")
          (add-syntax stx
                      #:binders binders
                      #:definites definites
                      #:shift-table shift-table)))
      (when error
        (add-error error)))

    ;; show-lctx : Step -> void
    (define/private (show-lctx step shift-table)
      (define state (protostep-s1 step))
      (define lctx (state-lctx state))
      (for ([bf lctx])
        (send/i sbview sb:syntax-browser<%> add-text 
                "\nwhile executing macro transformer in:\n")
        (insert-syntax/redex (bigframe-term bf)
                             (bigframe-foci bf)
                             (state-binders state)
                             shift-table
                             (state-uses state)
                             (state-frontier state))))

    ;; separator : Step [...] -> void
    (define/private (separator step #:compact? [compact? #f])
      (insert-step-separator (step-type->string (protostep-type step))
                             #:compact? compact?))

    ;; show-step : Step -> void
    (define/private (show-step step shift-table)
      (let-values ([(common-context state1 state2)
                    (factor-common-context (protostep-s1 step)
                                           (step-s2 step))])
        (show-state/redex state1 shift-table)
        (separator step)
        (show-state/contractum state2 shift-table)
        (show-common-context common-context state1 shift-table)
        (show-lctx step shift-table)))

    (define/private (factor-common-context state1 state2)
      (if (send/i config config<%> get-split-context?)
          (factor-common-context* state1 state2)
          (values null state1 state2)))

    (define/private (factor-common-context* state1 state2)
      (match-define
       (struct state (e1 foci1 ctx1 lctx1 binders1 uses1 frontier1 seq1)) state1)
      (match-define
       (struct state (e2 foci2 ctx2 lctx2 binders2 uses2 frontier2 seq2)) state2)
      (define (common xs ys acc)
        (if (and (pair? xs) (pair? ys) (eq? (car xs) (car ys)))
            (common (cdr xs) (cdr ys) (cons (car xs) acc))
            (values (reverse xs) (reverse ys) acc)))
      (define-values (ctx1z ctx2z common-ctx)
        (common (reverse ctx1) (reverse ctx2) null))
      (define state1z
        (make-state e1 foci1 ctx1z lctx1 binders1 uses1 frontier1 seq1))
      (define state2z
        (make-state e2 foci2 ctx2z lctx2 binders2 uses2 frontier2 seq2))
      (values common-ctx state1z state2z))

    (define/private (show-common-context ctx state1 shift-table)
      (match-define
       (struct state (_ _ _ _ _ uses1 frontier1 _)) state1)
      (when (pair? ctx)
        (let* ([hole-stx #'~~HOLE~~]
               [the-syntax (context-fill ctx hole-stx)])
          (send*/i sbview sb:syntax-browser<%>
            (add-text "\nin context:\n")
            (add-syntax the-syntax
                        #:definites uses1
                        #:binders (state-binders state1)
                        #:shift-table shift-table
                        #:substitutions (list (cons hole-stx "[ HOLE ]")))))))

    (define/private (show-state/redex state shift-table)
      (insert-syntax/redex (state-term state)
                           (state-foci state)
                           (state-binders state)
                           shift-table
                           (state-uses state)
                           (state-frontier state)))

    (define/private (show-state/contractum state shift-table)
      (insert-syntax/contractum (state-term state)
                                (state-foci state)
                                (state-binders state)
                                shift-table
                                (state-uses state)
                                (state-frontier state)))

    ;; show-prestep : Step -> void
    (define/private (show-prestep step shift-table)
      (separator step #:compact? #t)
      (show-state/redex (protostep-s1 step) shift-table)
      (show-lctx step shift-table))

    ;; show-poststep : Step -> void
    (define/private (show-poststep step shift-table)
      (separator step #:compact? #t)
      (show-state/contractum (protostep-s1 step) shift-table)
      (show-lctx step shift-table))

    ;; show-misstep : Step -> void
    (define/private (show-misstep step shift-table)
      (define state (protostep-s1 step))
      (separator step #:compact? #t)
      (send*/i sbview sb:syntax-browser<%>
        (add-error-text (exn-message (misstep-exn step)))
        (add-text "\n"))
      (when (exn:fail:syntax? (misstep-exn step))
        (for ([e (exn:fail:syntax-exprs (misstep-exn step))])
          (send/i sbview sb:syntax-browser<%> add-syntax e
                  #:binders (state-binders state)
                  #:definites (state-uses state)
                  #:shift-table shift-table)))
      (show-lctx step shift-table))

    (define/private (show-remarkstep step shift-table)
      (define state (protostep-s1 step))
      (for ([content (in-list (remarkstep-contents step))])
        (cond [(string? content)
               (send*/i sbview sb:syntax-browser<%>
                       (add-text content)
                       (add-text "\n"))]
              [(syntax? content)
               (send*/i sbview sb:syntax-browser<%>
                 (add-syntax content
                             #:binders (state-binders state)
                             #:definites (state-uses state)
                             #:shift-table shift-table)
                 (add-text "\n"))]))
      (show-lctx step shift-table))

    ;; insert-syntax/color
    (define/private (insert-syntax/color stx foci binders shift-table
                                         definites frontier hi-color)
      (define highlight-foci? (send/i config config<%> get-highlight-foci?))
      (define highlight-frontier? (send/i config config<%> get-highlight-frontier?))
      (send/i sbview sb:syntax-browser<%> add-syntax stx
              #:definites definites
              #:binders binders
              #:shift-table shift-table
              #:hi-colors (list hi-color
                                "WhiteSmoke")
              #:hi-stxss (list (if highlight-foci? foci null)
                               (if highlight-frontier? frontier null))))

    ;; insert-syntax/redex
    (define/private (insert-syntax/redex stx foci binders shift-table
                                         definites frontier)
      (insert-syntax/color stx foci binders shift-table
                           definites frontier "MistyRose"))

    ;; insert-syntax/contractum 
    (define/private (insert-syntax/contractum stx foci binders shift-table
                                              definites frontier)
      (insert-syntax/color stx foci binders shift-table
                           definites frontier "LightCyan"))

    ;; insert-step-separator : string -> void
    (define/private (insert-step-separator text #:compact? compact?)
      (send*/i sbview sb:syntax-browser<%>
        (add-text (if compact? "" "\n"))
        (add-text
         (make-object image-snip%
                      (collection-file-path "red-arrow.bmp" "icons")))
        (add-text "  [")
        (add-text text)
        (add-text "]\n\n")))
    ))
