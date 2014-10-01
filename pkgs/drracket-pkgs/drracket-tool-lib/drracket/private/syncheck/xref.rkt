#lang racket/base
(require setup/xref 
         scribble/xref)
(provide get-index-entry-info)

(define xref (load-collections-xref))

(define req-chan (make-channel))

(define thd
  (thread
   (λ ()
     (let loop ()
       (define-values (binding-info cd resp-chan nack-evt) (apply values (channel-get req-chan)))
       (define resp
         (parameterize ([current-directory cd])
           (and xref
                (let ([definition-tag (xref-binding->definition-tag xref binding-info #f)])
                  (and definition-tag
                       (let-values ([(path tag) (xref-tag->path+anchor xref definition-tag)])
                         (and path
                              (let ([index-entry (xref-tag->index-entry xref definition-tag)])
                                (and index-entry
                                     (list (entry-desc index-entry)
                                           path
                                           definition-tag
                                           tag))))))))))
       (thread
        (λ ()
          (sync (channel-put-evt resp-chan resp)
                nack-evt)))
       (loop)))))

;; this function is called from a thread that might be killed
;; (but the body of this module is run in a context where it is
;; guaranteed that that custodian doesn't get shut down)
(define (get-index-entry-info binding-info)
  (and (not (thread-dead? thd))
       (sync
        (nack-guard-evt
         (λ (nack-evt)
           (define resp-chan (make-channel))
           (channel-put req-chan (list binding-info (current-directory) resp-chan nack-evt))
           resp-chan)))))
