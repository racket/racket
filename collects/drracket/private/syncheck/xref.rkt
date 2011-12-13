#lang racket/base
(require setup/xref 
         racket/promise
         scribble/xref
         scribble/manual-struct)
(provide get-index-entry-info)

(define delayed-xref
  (if (getenv "PLTDRXREFDELAY")
      (begin
        (printf "PLTDRXREFDELAY: using plain delay\n")
        (delay (begin
                 (printf "PLTDRXREFDELAY: loading xref\n")
                 (begin0
                   (load-collections-xref)
                   (printf "PLTDRXREFDELAY: loaded xref\n")))))
      (delay/idle (load-collections-xref))))

(define req-chan (make-channel))

(define thd
  (thread
   (λ ()
     (let loop ()
       (define-values (binding-info resp-chan nack-evt) (apply values (channel-get req-chan)))
       (define xref (force delayed-xref))
       (define resp
         (and xref
              (let ([definition-tag (xref-binding->definition-tag xref binding-info #f)])
                (and definition-tag
                     (let-values ([(path tag) (xref-tag->path+anchor xref definition-tag)])
                       (and path
                            (let ([index-entry (xref-tag->index-entry xref definition-tag)])
                              (and index-entry
                                   (list (entry-desc index-entry)
                                         path
                                         tag)))))))))
       (sync (channel-put-evt resp-chan resp)
             nack-evt)
       (loop)))))

;; this function is called from a thread that might be killed
;; (but the body of this module is run in a context where it is
;; guaranteed that that custodian doesn't get shut down)
(define (get-index-entry-info binding-info)
  (sync
   (nack-guard-evt
    (λ (nack-evt)
      (define resp-chan (make-channel))
      (channel-put req-chan (list binding-info resp-chan nack-evt))
      resp-chan))))
