
(module typesetter mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           "params.ss"
           "pretty-range.ss"
           "pretty-printer.ss"
           "color.ss"
           "interfaces.ss"
           "util.ss")

  (provide typesetter-for-text%
           code-style)

  ;; typesetter-for-text%
  (define typesetter-for-text%
    (class* object% (typesetter<%>)
      ;; controller : syntax-pp-controller<%>
      (init-field controller)
      (init-field ((stx syntax)))
      (init-field text)

      ;; typesetter<%> methods
      (define/public (get-output-port) output-port)
      (define/public (get-current-position)
        (send text get-snip-position end-anchor))
      (define/public (get-colorer) colorer)

      ;; Internals

      (define start-anchor (new anchor-snip%))
      (define end-anchor (new anchor-snip%))
      (send text insert start-anchor)
      (send text insert end-anchor)

      (define output-port (make-text-port text end-anchor))

      (define/private (get-start-position)
        (send text get-snip-position start-anchor))

      (define/private (get-end-position)
        (send text get-snip-position end-anchor))

      (define syntax-pp 
        (new syntax-pp%
             (main-stx stx)
             (typesetter this)
             (primary-partition 
              (send controller get-primary-partition))))
      (define colorer
        (new syntax-text-colorer% 
             (text text)
             (start-anchor start-anchor)
             (end-anchor end-anchor)
             (syntax-pp syntax-pp)
             (controller controller)))

      ;; Initialize
      (with-unlock text
        (send syntax-pp pretty-print-syntax)
        ;; Pretty printer always inserts final newline.
        ;; We remove it here.
        ;; FIXME
        (let ([end (get-end-position)])
          (send text delete (sub1 end) end))
        (send text change-style
              (code-style text)
              (get-start-position)
              (get-end-position))
        (send colorer apply-styles)
        (for-each
         (lambda (range)
           (let* ([stx (range-obj range)]
                  [start (range-start range)]
                  [end (range-end range)])
             (when (syntax? stx)
               (send text set-clickback start end 
                     (lambda (_1 _2 _3)
                       (send controller on-select-syntax stx))))))
         (send (send syntax-pp get-range) all-ranges))
        (send controller add-view-colorer colorer))

      (super-new)))

  ;; make-text-port : text -> port
  ;; builds a port from a text object.  
  (define (make-text-port text end-anchor)
    (make-output-port #f
                      always-evt
                      (lambda (s start end flush? enable-break?)
                        (send text insert
                              (bytes->string/utf-8 s #f start end)
                              (send text get-snip-position end-anchor))
                        (- end start))
                      void
                      (lambda (special buffer? enable-break?)
                        (send text insert special
                              (send text get-snip-position end-anchor))
                        #t)))

  ;; code-style : text<%> -> style<%>
  (define (code-style text)
    (let* ([style-list (send text get-style-list)]
           [style (send style-list find-named-style "Standard")]
           [font-size (current-syntax-font-size)])
      (if font-size
          (send style-list find-or-create-style
                style
                (make-object style-delta% 'change-size font-size))
          style)))

  (define anchor-snip%
    (class snip%
      (define/override (copy)
        (make-object string-snip% ""))
      (super-instantiate ())))
  )
