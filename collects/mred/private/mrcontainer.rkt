#lang racket/base

(require racket/class
         racket/list
         (prefix-in wx: "kernel.rkt")
         "lock.rkt"
         "helper.rkt"
         "const.rkt"
         "wx.rkt"
         "check.rkt"
         "wxcontainer.rkt"
         "mrwindow.rkt")

(provide area-container<%>
         (protect-out internal-container<%>
                      check-container-parent
                      make-container%
                      make-subwindow%)
         area-container-window<%>
         (protect-out make-area-container-window%))

(define area-container<%>
  (interface (area<%>)
    reflow-container container-flow-modified begin-container-sequence end-container-sequence
    container-size
    get-children change-children place-children
    after-new-child
    add-child delete-child
    border spacing
    set-alignment get-alignment))

(define internal-container<%> (interface ()))

(define (check-container-parent who p)
  (unless (is-a? p internal-container<%>)
    (unless (is-a? p area-container<%>)
      (raise-argument-error (who->name who) "(is-a?/c area-container<%>)" p))
    (raise-arguments-error (who->name who) "invalid container;\n given container is not an instance of a built-in container class"
                           "given container" p)))

(define-local-member-name
  has-wx-child?
  adopt-wx-child)

(define (make-container% %) ; % implements area<%>
  (class* % (area-container<%> internal-container<%>)
    (init mk-wx get-wx-pan get-wx-outer-pan mismatches parent
          ;; for keyword use
          [border no-val]
          [spacing no-val]
          [alignment no-val])
    (let ([cwho '(iconstructor area-container)])
      (unless (eq? border no-val) (check-margin-integer cwho border))
      (unless (eq? spacing no-val) (check-margin-integer cwho spacing))
      (unless (eq? alignment no-val)
        (unless (and (list? alignment)
                     (= 2 (length alignment))
                     (memq (car alignment) '(left center right))
                     (memq (cadr alignment) '(top center bottom)))
          (raise-argument-error (who->name cwho) "(list/c (or/c 'left center right) (or/c 'top 'center 'bottom))" alignment))))
    (define get-wx-panel get-wx-pan)
    
    (define bdr (param get-wx-panel border))
    (define spc (param get-wx-panel spacing))
    (public [bdr border] [spc spacing])
    (public*
     [after-new-child (lambda (c)
                        (check-instance '(method area-container<%> after-new-child) subarea<%> 'subarea<%> #f c)
                        (void))]
     [reflow-container (entry-point (lambda () (send (send (get-wx-panel) get-top-level) force-redraw)))]
     [container-flow-modified (entry-point (lambda ()
                                             (let ([p (get-wx-panel)])
                                               (send p need-move-children)
                                               (send p force-redraw))))]
     [begin-container-sequence (entry-point (lambda () (send (send (get-wx-panel) get-top-level) begin-container-sequence)))]
     [end-container-sequence (entry-point (lambda () (send (send (get-wx-panel) get-top-level) end-container-sequence)))]
     [get-children (entry-point (lambda () (map wx->proxy
                                                (let ([l (send (get-wx-panel) get-children)]
                                                      [h (send (get-wx-panel) get-hidden-child)])
                                                  (if h (remq h l) l)))))]
     [set-alignment (entry-point (lambda (h v) (send (get-wx-panel) alignment h v)))]
     [get-alignment (entry-point (lambda () (send (get-wx-panel) get-alignment)))]
     [change-children (entry-point
                       (lambda (f)
                         (unless (and (procedure? f)
                                      (procedure-arity-includes? f 1))
                           (raise-argument-error (who->name '(method container<%> change-children))
                                                 "(procedure-arity-includes/c 1)"
                                                 f))
                         (send (get-wx-panel) change-children
                               (lambda (kids)
                                 (let* ([hidden (send (get-wx-panel) get-hidden-child)]
                                        [mred-kids (map wx->proxy (remq hidden kids))]
                                        [l (as-exit (lambda () (f mred-kids)))])
                                   (unless (and (list? l)
                                                (andmap (lambda (x) (is-a? x internal-subarea<%>)) l))
                                     (raise-arguments-error 'change-children
                                                            "result of given procedure was not a list of subareas"
                                                            "procedure" f
                                                            "result" l))
                                   (append
                                    (if hidden (list hidden) null)
                                    (map mred->wx l)))))))]
     [container-size (entry-point
                      (lambda (l)
                                        ; Check l, even though we don't use it
                        (unless (and (list? l)
                                     (andmap
                                      (lambda (l)
                                        (and (list? l) (= (length l) 4)
                                             (integer? (car l)) (exact? (car l)) (<= 0 (car l) 10000)
                                             (integer? (cadr l)) (exact? (cadr l)) (<= 0 (cadr l) 10000)))
                                      l))
                          (raise-argument-error (who->name '(method area-container<%> container-size))
                                                "(listof (list/c (integer-in 0 10000) (integer-in 0 10000) any/c any/c))"
                                                l))
                        (let ([l (send (get-wx-panel) do-get-graphical-min-size)])
                          (apply values l))))]
     [place-children (entry-point (lambda (l w h) (send (get-wx-panel) do-place-children l w h)))]
     [add-child (entry-point
                 (lambda (c)
                   (check-instance '(method area-container<%> add-child) subwindow<%> 'subwindow<%> #f c)
                   (send (get-wx-panel) add-child (mred->wx c))))]
     [delete-child (entry-point
                    (lambda (c)
                      (check-instance '(method area-container<%> delete-child) subwindow<%> 'subwindow<%> #f c)
                      (send (get-wx-panel) delete-child (mred->wx c))))]
     [has-wx-child? (lambda (child-wx) ; called in atomic mode
                      (memq child-wx (send (get-wx-panel) get-children)))]
     [adopt-wx-child (lambda (child-wx) ; called in atomic mode
                       (let ([wxp (get-wx-panel)])
                         (send child-wx set-area-parent wxp)
                         (send wxp adopt-child child-wx)))])
    (super-make-object mk-wx get-wx-panel get-wx-outer-pan mismatches parent)
    (unless (eq? border no-val) (bdr border))
    (unless (eq? spacing no-val) (spc spacing))
    (unless (eq? alignment no-val) (set-alignment . alignment))))

(define area-container-window<%>
  (interface (window<%> area-container<%>)))

(define (make-area-container-window% %) ; % implements window<%> (and area-container<%>)
  (class* % (area-container-window<%>)
    (init mk-wx get-wx-pan get-wx-outer-pan mismatches label parent cursor)
    (super-make-object mk-wx get-wx-pan get-wx-outer-pan mismatches label parent cursor)))


(define (make-subwindow% %)
  (class %
    (super-new)
    (inherit set-parent
             get-parent
             is-shown?
             show)
    (define/public (reparent new-parent)
      (check-container-parent '(subwindow<%> reparent) new-parent)
      (unless (as-entry
               (lambda ()
                 (let ([p1 (send (mred->wx this) get-top-level)]
                       [p2 (send (mred->wx new-parent) get-top-level)])
                   (eq? (send p1 get-eventspace) (send p1 get-eventspace)))))
        (raise-arguments-error
         (who->name '(subwindow<%> reparent))
         "current parent's eventspace is not the same as the eventspace of the new parent"
         "subwindow" this
         "new parent" new-parent))
      (let loop ([p new-parent])
        (when p
          (when (eq? p this)
            (raise-arguments-error
             (who->name '(subwindow<%> reparent))
             (if (eq? new-parent this)
                 "cannot set parent to self"
                 "cannot set parent to a descedant")
             "subwindow" this))
          (loop (send p get-parent))))
      (let* ([added? (memq this (send (get-parent) get-children))]
             [shown? (and added? (is-shown?))])
        (when added?
          (send (get-parent) delete-child this))
        (as-entry
         (lambda ()
           (let ([wx (mred->wx this)])
             ;; double-check that delete succeeded:
             (unless (send (get-parent) has-wx-child? wx)
               ;; double-check that we're not creating a loop at the wx level:
               (unless (let loop ([p (mred->wx new-parent)])
                         (and p
                              (or (eq? p wx)
                                  (loop (send p get-parent)))))
                 ;; Ok --- really reparent:
                 (send new-parent adopt-wx-child wx)
                 (set-parent new-parent))))))
        (when added?
          (send new-parent add-child this))
        (when shown?
          (show #t))))))
