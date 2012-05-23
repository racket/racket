#lang racket/base

;; This file provides the utilities that racket's `help' form uses.
;; It is required dynamically when used from racket, to avoid the
;; loading overhead, and to have racket independent of having the
;; documentation system.

(require setup/xref
         scribble/xref
         scribble/manual-struct
         net/uri-codec
         net/sendurl
         racket/string
         racket/path
         "search.rkt")

(provide search-for find-help find-help/lib go-to-main-page)

(define (search-for strs)
  (perform-search (string-join strs)))

(define-namespace-anchor anchor)

(define (find-help/lib sym lib)
  (let ([id (parameterize ([current-namespace
                            (namespace-anchor->empty-namespace anchor)])
              (namespace-require `(for-label ,lib))
              (namespace-syntax-introduce (datum->syntax #f sym)))])
    (if (identifier-label-binding id)
      (find-help id)
      (error 'help "no binding for identifier: ~a from module: ~a" sym lib))))

(define (find-help id)
  (let* ([lb (identifier-label-binding id)]
         [b (and (not lb) (identifier-binding id))]
         [any-b (or lb b)]
         [b (and (not (eq? 'lexical any-b)) any-b)]
         [xref (load-collections-xref
                (lambda ()
                  (printf "Loading help index...\n")))])
    (if b
      (let ([tag (xref-binding->definition-tag xref b (if lb #f 0))])
        (if tag
          (go-to-tag xref tag)
          (error 'help
                 "no documentation found for: ~.s provided by: ~a"
                 (syntax-e id)
                 (module-path-index-resolve (caddr b)))))
      (search-for-exports xref (syntax-e id) any-b))))

(define (search-for-exports xref sym binding)
  (let ([idx (xref-index xref)]
        [libs null])
    (for ([entry (in-list idx)])
      (when (and (exported-index-desc? (entry-desc entry))
                 (eq? sym (exported-index-desc-name (entry-desc entry))))
        (set! libs (append libs (exported-index-desc-from-libs
                                 (entry-desc entry))))))
    (if (null? libs)
      (printf "Not found in any library's documentation: ~a\n" sym)
      (begin
        (printf "~a, but provided by:\n"
                (if binding
                    "No documentation available for binding"
                    "No current binding"))
        (let loop ([libs libs])
          (unless (null? libs)
            (unless (member (car libs) (cdr libs))
              (printf "  ~a\n" (car libs)))
            (loop (cdr libs))))))))

(define (report-sending-browser file)
  (printf "Sending to web browser...\n  file: ~a\n" file))

(define (go-to-main-page)
  (send-main-page #:notify report-sending-browser))

(define (go-to-tag xref t)
  (let-values ([(file anchor) (xref-tag->path+anchor xref t)])
    (report-sending-browser file)
    (when anchor (printf "  anchor: ~a\n" anchor))
    (unless (send-url/file file #:fragment (and anchor (uri-encode anchor)))
      (error 'help "browser launch failed"))))
