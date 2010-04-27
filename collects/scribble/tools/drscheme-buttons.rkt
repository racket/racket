#lang scheme/base

(require scheme/runtime-path
         scheme/gui/base
         scheme/class
         mrlib/bitmap-label
         scheme/system
         setup/xref
         net/sendurl)

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
           (let-values ([(p) (open-output-string)]
                        [(base name dir?) (split-path fn)])
             (parameterize ([current-namespace (make-base-namespace)]
                            [current-output-port p]
                            [current-error-port p]
                            [current-command-line-arguments
                             (list->vector 
                              (append
                               extra-cmdline
                               (list "--dest" (path->string base))
                               (list mode (if (path? fn) (path->string fn) fn))))])
               (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'setup/xref)
               (dynamic-require 'scribble/run #f)
               (cond
                 [(equal? label "HTML")
                  (send-url/file (path-replace-suffix fn suffix))]
                 [else (system (format "open ~a" (path-replace-suffix name suffix)))]))
             (message-box "Scribble" (get-output-string p) drs-frame))
           (message-box "Not Named" "Cannot render unsaved file"))))))

(define drscheme-buttons
  (let ([html-button
         (make-render-button "Scribble HTML" html.png "--html" #".html" 
                             '("++xref-in" "setup/xref" "load-collections-xref"))]
        [pdf-button
         ;; only available on OSX currently
         ;; when we have a general way of opening pdfs, can use that
         (make-render-button "Scribble PDF" pdf.png "--pdf" #".pdf" null)])
    (case (system-type)
      [(macosx) (list html-button pdf-button)]
      [else (list html-button)])))
