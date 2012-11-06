#lang racket/base

(require racket/gui racket/class "marks.rkt")

(provide display-break-stuff)

(define f
  (new frame%
       [label (format "Breakpoints Inspector")]
       [width 400] [height 500]))
(define sel (new choice% [label "Breakpoint#"] [choices '()] [parent f]
                 [callback (lambda (c e) (show-sel))] [stretchable-width #t]))
(define ec  (new editor-canvas% [parent f]))
(define t   (new text%))
(send ec set-editor t)

(define selections '())
(define (add-sel num mset bkind retvals)
  (set! selections (cons (list num mset bkind retvals) selections))
  (let ([num (number->string num)])
    (send sel append num)
    (send sel set-string-selection num)
    (show-sel)))

(define (show-sel)
  (let* ([num (string->number (send sel get-string-selection))]
         [bpt (assq num selections)])
    (send* t (lock #f) (erase))
    (if (not bpt)
        (send* t (insert (format "Breakpoint #~a not found!\n" num)))
        (let-values ([(mset bkind retvals) (apply values (cdr bpt))])
          (send* t
            (insert (format "Breakpoint #~a:\n" num))
            (insert (format "  break-kind: ~v\n" bkind))
            (insert "marks:\n"))
          (if mset
              (for-each
               (lambda (mark)
                 (let* ([em (expose-mark mark)]
                        [source (car em)]
                        [label (cadr em)]
                        [binding-set (caddr em)])
                   (send* t
                     (insert (format "  label: ~v\n" label))
                     ;; we really want one of those nice collapsible
                     ;; syntax-viewer thingies here:
                     (insert (format "  source : ~v\n"
                                     (syntax->datum source)))
                     ;; here too, though this isn't a syntax object.
                     (insert (format "  bindings: ~v\n" binding-set)))))
               (extract-mark-list mset))
              (send t insert "  nothing!\n"))
          (send t insert "returned-value-list:\n")
          (if retvals
              (for-each (lambda (v) (send t insert (format "  ~v\n" v)))
                        retvals)
              (send t insert "  nothing!\n"))))
    (send* t (lock #t))))

;; display-break-stuff : show the information associated with a breakpoint.
;; Useful for people building steppers for new languages
(define (display-break-stuff break-number mark-set break-kind
                             returned-value-list)
  (add-sel break-number mark-set break-kind returned-value-list)
  (send f show #t))
