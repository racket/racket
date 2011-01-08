#lang racket/base

(require racket/runtime-path
         racket/gui/base
         racket/class
         mrlib/bitmap-label
         racket/system
         setup/xref
         net/sendurl
         racket/sandbox)

(provide drracket-buttons)

(define-runtime-path pdf-png-path "pdf.png")
(define-runtime-path html-png-path "html.png")
(define pdf.png (make-object bitmap% pdf-png-path 'png/mask))
(define html.png (make-object bitmap% html-png-path 'png/mask))

(define-namespace-anchor anchor)

(define original-error-display-handler (error-display-handler))

(define (make-render-button label bmp mode suffix extra-cmdline)
  (list 
   label
   bmp
   (λ (drs-frame)
     (let* ([t (send drs-frame get-definitions-text)]
            [fn (send t get-filename)])
       (if fn
           (let ()
             (send t save-file fn)
             (define p (open-output-string))
             (define-values (base name dir?) (split-path fn))
             (define sb 
               (parameterize ([sandbox-security-guard (current-security-guard)])
                 (make-evaluator 'racket/base)))
             (define result
               (call-in-sandbox-context
                sb
                (λ ()
                  (with-handlers (((λ (x) #t) (λ (e) (list 'exn e))))
                    (parameterize ([current-output-port p]
                                   [current-error-port p]
                                   [current-directory base]
                                   [error-display-handler original-error-display-handler]
                                   [current-command-line-arguments
                                    (list->vector 
                                     (append
                                      extra-cmdline
                                      (list "--quiet")
                                      (list mode (if (path? fn) (path->string fn) fn))))])
                      (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'setup/xref)
                      (dynamic-require 'scribble/run #f)
                      (list 'normal))))))
             (cond
               [(eq? (list-ref result 0) 'exn)
                (define exn (list-ref result 1))
                (define sp (open-output-string))
                (cond
                  [(exn? exn)
                   (fprintf sp "~a\n" (exn-message exn))
                   (for ([x (in-list (continuation-mark-set->context (exn-continuation-marks exn)))])
                     (fprintf sp "  ~s\n" x))]
                  [else
                   (fprintf sp "uncaught exn: ~s\n" exn)])
                (message-box "Scribble HTML - DrRacket"
                             (get-output-string sp))]
               [(equal? suffix #".html")
                (send-url/file (path-replace-suffix fn suffix))]
               [else
                (system (format "open ~s" (path->string (path-replace-suffix fn suffix))))])
             (let ([s (get-output-string p)])
               (unless (equal? s "")
                 (message-box "Scribble" s drs-frame))))
           (message-box "Scribble" "Cannot render buffer without filename"))))))

(define drracket-buttons
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
