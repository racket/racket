#lang racket/base
(require racket/class
         racket/contract/base)

(provide gl-context%
         gl-context<%>
         
         do-call-as-current
         do-swap-buffers)

(define-local-member-name
  do-call-as-current
  do-swap-buffers)

(define lock-ch (make-channel))
(define lock-holder-ch (make-channel))
(define (lock-manager)
  (let loop ()
    (sync (handle-evt
           lock-ch
           (lambda (p)
             (let ([t (car p)]
                   [ch (cdr p)])
               (let waiting-loop ()
                 (sync (handle-evt
                        (thread-dead-evt t)
                        (lambda (v) (loop)))
                       (handle-evt
                        ch
                        (lambda (v) (loop)))
                       (handle-evt
                        (channel-put-evt lock-holder-ch t)
                        (lambda (v) (waiting-loop))))))))
          (handle-evt
           (channel-put-evt lock-holder-ch #f)
           (lambda (v) (loop))))))
(define manager-t (thread/suspend-to-kill lock-manager))

;; Implemented by subclasses:
(define gl-context%
  (class object%
    (define/private (with-gl-lock t)
      (thread-resume manager-t (current-thread))
      (if (eq? (current-thread) (channel-get lock-holder-ch))
          (t)
          (let ([ch (make-channel)])
            (dynamic-wind
                (lambda ()
                  (channel-put lock-ch (cons (current-thread) ch)))
                t
                (lambda ()
                  (channel-put ch #t))))))

    (define/public (call-as-current t)
      (with-gl-lock
       (lambda ()
         (do-call-as-current t))))

    (define/public (swap-buffers)
      (with-gl-lock
       (lambda ()
         (do-swap-buffers))))

    (define/public (ok?) #t)

    (define/public (do-call-as-current t) (t))
    (define/public (do-swap-buffers t) (void))

    (super-new)))

(define gl-context<%>
  (interface ()
    [call-as-current (->*m [(-> any)] [evt? any/c] any)]
    [ok? (->m boolean?)]
    [swap-buffers (->m any)]))
