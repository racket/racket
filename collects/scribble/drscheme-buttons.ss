#lang scheme/base

(require scheme/runtime-path
         scheme/gui/base
         scheme/class
         mrlib/bitmap-label
         scheme/system
         setup/xref)

(provide drscheme-buttons)

(define-runtime-path pdf-png-path "pdf.png")
(define-runtime-path html-png-path "html.png")
(define pdf.png (make-object bitmap% pdf-png-path 'png/mask))
(define html.png (make-object bitmap% html-png-path 'png/mask))

(define-namespace-anchor anchor)

(define (make-render-button label bmp mode suffix extra-cmdline)
  (list 
   label
   bmp
   (λ (drs-frame)
     (let* ([t (send drs-frame get-definitions-text)]
            [fn (send t get-filename)])
       (if (and fn (not (send t is-modified?)))
           (let ([p (open-output-string)])
             (parameterize ([current-namespace (make-base-namespace)]
                            [current-output-port p]
                            [current-error-port p]
                            [current-command-line-arguments
                             (list->vector 
                              (append
                               extra-cmdline
                               (list mode (if (path? fn) (path->string fn) fn))))])
               (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'setup/xref)
               (dynamic-require 'scribble/run #f)
               (let-values ([(base name dir?) (split-path fn)])
                 (system (format "open ~a" (path-replace-suffix name suffix)))))
             (message-box "Scribble" (get-output-string p) drs-frame))
           (message-box "Not Named" "Cannot render unsaved file"))))))

(define drscheme-buttons
  (case (system-type)
    [(macosx)
     ;; really this is only to guard the "open" system call above.
     (list (make-render-button "PDF" pdf.png "--pdf" #".pdf" null)
           (make-render-button "HTML" html.png "--html" #".html" '("++xref-in" "setup/xref" "load-collections-xref")))]
    [else
     '()]))
