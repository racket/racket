#lang racket/base
(require racket/class
         racket/contract/base)

(provide gl-context%
         gl-context<%>
         
         do-call-as-current
         do-swap-buffers
         
         get-current-gl-context)

(define-local-member-name
  do-call-as-current
  do-swap-buffers)

(define lock-ch (make-channel))
(define lock-holder-ch (make-channel))
(define (lock-manager)
  (define none '#(#f #f #f))
  (let loop ()
    (sync (handle-evt
           lock-ch
           (lambda (p)
             (let ([t (vector-ref p 0)]
                   [ch (vector-ref p 2)])
               (let waiting-loop ()
                 (sync (handle-evt
                        (thread-dead-evt t)
                        (lambda (v) (loop)))
                       (handle-evt
                        ch
                        (lambda (v) (loop)))
                       (handle-evt
                        (channel-put-evt lock-holder-ch p)
                        (lambda (v) (waiting-loop))))))))
          (handle-evt
           (channel-put-evt lock-holder-ch none)
           (lambda (v) (loop))))))
(define manager-t (thread/suspend-to-kill lock-manager))

(define gl-context<%>
  (interface ()
    [call-as-current (->*m [(-> any)] [evt? any/c] any)]
    [ok? (->m boolean?)]
    [swap-buffers (->m any)]
    [get-handle (->m any)]))

(define current-gl-context (make-thread-cell #f))
(define (get-current-gl-context) (thread-cell-ref current-gl-context))

;; Implemented by subclasses:
(define gl-context%
  (class* object% (gl-context<%>)
    (define/private (with-gl-lock t alternate-evt enable-break?)
      (thread-resume manager-t (current-thread))
      (define current (channel-get lock-holder-ch))
      (if (and (eq? (vector-ref current 0) (current-thread))
               (eq? (vector-ref current 1) this))
          (t)
          ((if enable-break? sync/enable-break sync)
           (let ([ch (make-channel)])
             (handle-evt (channel-put-evt lock-ch (vector (current-thread) this ch))
                         (lambda (val)
                           (dynamic-wind
                               (lambda ()
                                 (thread-cell-set! current-gl-context this))
                               t
                               (lambda ()
                                 (thread-cell-set! current-gl-context #f)
                                 (channel-put ch #t))))))
           alternate-evt)))

    (define/public (get-handle)
      #f)
    
    (define/public (call-as-current t [alternate-evt never-evt] [enable-breaks? #f])
      (with-gl-lock
       (lambda ()
         (do-call-as-current t))
       alternate-evt
       enable-breaks?))

    (define/public (swap-buffers)
      (with-gl-lock
       (lambda ()
         (do-swap-buffers))
       never-evt
       #f))

    (define/public (ok?) #t)

    (define/public (do-call-as-current t) (t))
    (define/public (do-swap-buffers t) (void))

    (super-new)))
