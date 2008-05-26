#lang scheme/base

(require "../config.ss"
         scribble/manual
         scribble/struct
         scribble/decode
         setup/dirs)

(provide main-page)

(define page-info
  (let ([links (filter pair? links)])
    (lambda (id)
      (cond [(assq id links) => cdr]
            [else (error 'main-page "page id not found: ~e" id)]))))

;; the second argument specifies installation/user specific, and if
;; it's missing, then it's a page with a single version
(define (main-page id [installation-specific? '?])
  (define info (page-info id))
  (make-splice (list (title #:style '(no-toc)
                            (car info)
                            (case installation-specific?
                              [(?) ""]
                              [(#t) " (installation)"]
                              [(#f) ""])) ; can be " (user)"
                     (front-toc id))))

;; FIXME: Use this to avoid hard-wiring manual titles and paths in config.ss
(define (resolve s [f s])
  (resolved-module-path-name
   (module-path-index-resolve
    (module-path-index-join `(lib ,(format "scribblings/~a/~a.scrbl" s f))
                            #f))))

(define (front-toc-items up)
  (map (lambda (item)
         (if (eq? item '---)
           (list '--- (make-toc-element #f null '(nbsp)))
           (let* ([id    (car item)]
                  [info  (page-info id)]
                  [label (car info)]
                  [root  (cadr info)]
                  [path  (caddr info)]
                  [text  (make-element "tocsubseclink" (list label))]
                  [link  (link (case root
                                 [(plt)  (build-path (find-doc-dir) path)]
                                 [(user) (string-append up path)]
                                 [(#f)   path]
                                 [else (error "internal error (main-page)")])
                               #:underline? #f text)])
             (list id
                   (make-toc-element #f null (list link))
                   (make-toc-element #f null (list text))))))
       links))

(define (front-toc here)
  ;; massages the current path to an up string
  (let* ([up (regexp-replace #rx"[^/]+$" (caddr (page-info here)) "")]
         [up (regexp-replace* #rx"[^/]*/" up "../")])
    (make-splice (map (lambda (item)
                        (let ([id (if (pair? item) (car item) item)])
                          ((if (eq? here id) caddr cadr) item)))
                      (front-toc-items up)))))
