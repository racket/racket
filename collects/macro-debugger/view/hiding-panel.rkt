#lang racket/base
(require racket/class
         racket/gui/base
         racket/match
         unstable/class-iop
         "interfaces.rkt"
         "../model/hiding-policies.rkt"
         "../util/mpi.rkt"
         unstable/gui/notify)
(provide macro-hiding-prefs-widget%)

(define mode:disable "Disable")
(define mode:standard "Standard")
(define mode:custom "Custom ...")

#|

TODO

 - allow entry of more policies
 - visual feedback on rules applying to selected identifier
   (need to switch from list to editor)

|#

;; macro-hiding-prefs-widget%
(define macro-hiding-prefs-widget%
  (class* object% (hiding-prefs<%>)
    (init parent)
    (init-field/i (stepper widget<%>))
    (init-field config)

    (define/public (get-policy)
      (let ([mode (get-mode)])
        (cond [(not (macro-hiding-enabled?)) #f]
              [(equal? mode mode:standard) standard-policy]
              [(equal? mode mode:custom) (get-custom-policy)])))

    (define/private (get-custom-policy)
      (let ([hide-racket? (send box:hide-racket get-value)]
            [hide-libs? (send box:hide-libs get-value)]
            [hide-contracts? (send box:hide-contracts get-value)]
            [hide-transformers? (send box:hide-phase1 get-value)]
            [specialized-policies (get-specialized-policies)])
        (policy->predicate
         `(custom ,hide-racket?
                  ,hide-libs?
                  ,hide-contracts?
                  ,hide-transformers?
                  ,specialized-policies))))

    (define super-panel
      (new vertical-panel%
           (parent parent)
           (stretchable-height #f)))
    (define top-line-panel
      (new horizontal-panel%
           (parent super-panel)
           (alignment '(left center))
           (stretchable-height #f)))
    (define customize-panel
      (new horizontal-panel%
           (parent super-panel)
           (stretchable-height #f)
           (alignment '(left top))
           (style '(deleted))))
    (define left-pane
      (new vertical-pane%
           (parent customize-panel)
           (stretchable-width #f)
           (alignment '(left top))))
    (define right-pane
      (new vertical-pane%
           (parent customize-panel)))

    (define mode-selector
      (choice/notify-box
       top-line-panel
       "Macro hiding: "
       (list mode:disable mode:standard mode:custom)
       (get-field macro-hiding-mode config)))
    (define top-line-inner-panel
      (new horizontal-panel%
           (parent top-line-panel)
           (alignment '(right center))
           (style '(deleted))))

    (define/private (get-mode)
      (send/i config config<%> get-macro-hiding-mode))

    (define/private (macro-hiding-enabled?)
      (let ([mode (get-mode)])
        (or (equal? mode mode:standard)
            (and (equal? mode mode:custom)
                 (send box:hiding get-value)))))

    (define/private (ensure-custom-mode)
      (unless (equal? (get-mode) mode:custom)
        (send/i config config<%> set-macro-hiding-mode mode:custom)))

    (define/private (update-visibility)
      (let ([customizing (equal? (get-mode) mode:custom)])
        (send top-line-panel change-children
              (lambda (children)
                (append (remq top-line-inner-panel children)
                        (if customizing (list top-line-inner-panel) null))))
        (send super-panel change-children
              (lambda (children)
                (append (remq customize-panel children)
                        (if (and customizing (send box:edit get-value))
                            (list customize-panel)
                            null))))))

    (send/i config config<%> listen-macro-hiding-mode
            (lambda (value)
              (update-visibility)
              (force-refresh)))

    (define box:hiding
      (new check-box%
           (label "Enable macro hiding")
           (value #t)
           (parent top-line-inner-panel)
           (callback (lambda (c e) (force-refresh)))))
    (define box:edit
      (new check-box%
           (label "Show policy editor")
           (parent top-line-inner-panel)
           (value #t)
           (callback (lambda (c e) (update-visibility)))))

    (define box:hide-racket
      (new check-box%
           (label "Hide racket syntax")
           (parent left-pane)
           (value #t)
           (callback (lambda (c e) (refresh)))))
    (define box:hide-libs
      (new check-box%
           (label "Hide library syntax")
           (parent left-pane)
           (value #t)
           (callback (lambda (c e) (refresh)))))
    (define box:hide-contracts
      (new check-box%
           (label "Hide contracts (heuristic)")
           (parent left-pane)
           (value #t)
           (callback (lambda (c e) (refresh)))))
    (define box:hide-phase1
      (new check-box%
           (label "Hide phase>0")
           (parent left-pane)
           (value #t)
           (callback (lambda (c e) (refresh)))))

    (define look-ctl
      (new list-box% (parent right-pane) (label "")
           (choices null) (style '(extended))
           (callback
            (lambda (c e)
              (send delete-ctl enable (pair? (send c get-selections)))))))

    (define look-button-pane
      (new horizontal-pane% (parent right-pane) (stretchable-width #f)))

    (define delete-ctl
      (new button% (parent look-button-pane) (label "Delete rule") (enabled #f)
           (callback (lambda _ (delete-selected) (refresh)))))
    (define add-hide-id-button
      (new button% (parent look-button-pane) (label "Hide macro") (enabled #f)
           (callback (lambda _ (add-hide-identifier) (refresh)))))
    (define add-show-id-button
      (new button% (parent look-button-pane) (label "Show macro") (enabled #f)
           (callback (lambda _ (add-show-identifier) (refresh)))))
    ;;(new grow-box-spacer-pane% (parent right-pane))

    ;; Methods

    (define stx #f)

    ;; refresh : -> void
    (define/public (refresh)
      (when (macro-hiding-enabled?)
        (send/i stepper widget<%> refresh/resynth)))

    ;; force-refresh : -> void
    (define/private (force-refresh)
      (send/i stepper widget<%> refresh/resynth))

    ;; set-syntax : syntax/#f -> void
    (define/public (set-syntax lstx)
      (set! stx (and (identifier? lstx) lstx))
      (send add-show-id-button enable (identifier? lstx))
      (send add-hide-id-button enable (identifier? lstx)))

    ;; A PolicyLine is an Entry
    ;; Entry is defined in ../model/hiding-policies

    ;; identifier-policies : (listof Entry)
    (define identifier-policies null)

    ;; get-specialized-policies : -> (listof Entry)
    (define/private (get-specialized-policies)
      identifier-policies)

    (define/public (add-hide-identifier)
      (when (identifier? stx)
        (add-policy-line 'hide-if `(free=? ,stx))))

    (define/public (add-show-identifier)
      (when (identifier? stx)
        (add-policy-line 'show-if `(free=? ,stx))))

    ;; add-policy-line : 'show-if/'hide-if Condition -> void
    (define/private (add-policy-line action condition)
      (set! identifier-policies
            (cons `(,action ,condition)
                  (remove-policy/condition condition identifier-policies)))
      (update-list-view)
      (ensure-custom-mode))

    ;; update-list-view : -> void
    (define/private (update-list-view)
      (send look-ctl set null)
      (for ([policy identifier-policies])
        (send look-ctl append (policy->string policy) policy)))

    ;; delete-selected : -> void
    (define/private (delete-selected)
      (define to-delete (sort (send look-ctl get-selections) <))
      (set! identifier-policies
            (let loop ([i 0] [policies identifier-policies] [to-delete to-delete])
              (cond [(null? to-delete) policies]
                    [(= i (car to-delete))
                     (loop (add1 i) (cdr policies) (cdr to-delete))]
                    [else
                     (cons (car policies)
                           (loop (add1 i) (cdr policies) to-delete))])))
      (update-list-view))

    (super-new)
    (update-visibility)))


(define (remove-policy/condition condition policies)
  (filter (lambda (p) (not (same-condition? (cadr p) condition)))
          policies))


;; ----

(define (policy->string policy)
  (string-limit 200
                (string-append 
                 (case (car policy)
                   ((show-if) "show ")
                   ((hide-if) "hide "))
                 (condition->string (cadr policy)))))

(define (string-limit size s)
  (cond [(> (string-length s) size)
         (string-append (substring s 0 (- size 3)) "...")]
        [else s]))

(define (condition->string condition)
  (match condition
    [`(free=? ,id)
     (let ([b (identifier-binding id)])
       (or #| (identifier->string id) |#
           (cond [(list? b)
                  (let ([mod (caddr b)]
                        [name (cadddr b)])
                    (if (self-mpi? mod)
                        (format "'~a' defined in this module" name)
                        (format "'~s' imported from ~a" name (mpi->string mod))))]
                 [else
                  (symbol->string (syntax-e id))])))]
    [_
     "<condition>"]))

#|
(require scribble/xref
         scribble/manual-struct
         setup/xref)

(define xref-p (delay (load-collections-xref)))

(define (identifier->string id)
  (define binding-info (identifier-binding id))
  (define xref (force xref-p))
  (define definition-tag
    (and xref 
         (xref-binding->definition-tag xref binding-info #f)))
  (and definition-tag
       (let-values ([(path tag) (xref-tag->path+anchor xref definition-tag)])
         (define index-entry
           (and path (xref-tag->index-entry xref definition-tag)))
         (define desc
           (and index-entry (entry-desc index-entry)))
         (and desc
              (let ([name (exported-index-desc-name desc)]
                    [libs (exported-index-desc-from-libs desc)])
                (format "'~a' from ~a"
                        name
                        (mpi->string (car libs))))))))
|#



#|
(define (get-id-key id)
  id
  #; ;; FIXME
  (let ([binding (identifier-binding id)])
    (get-id-key/binding id binding)))

(define (get-id-key/binding id binding)
  (cond [(pair? binding)
         (list (car binding) (cadr binding))]
        [else id]))

(define (key=? key1 key2)
  (cond [(and (identifier? key1) (identifier? key2))
         (free-identifier=? key1 key2)]
        [(and (pair? key1) (pair? key2))
         (and (equal? (car key1) (car key2))
              (equal? (cadr key1) (cadr key2)))]
        [else #f]))

(define (key->text key)
  (cond [(pair? key)
         (let ([name (cadddr key)]
               [mod (caddr key)])
           (format "'~s' from ~a"
                   name
                   (mpi->string mod)))]
        [else (symbol->string (syntax-e key))]))
|#
