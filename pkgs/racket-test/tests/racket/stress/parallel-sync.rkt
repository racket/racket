#lang racket/base
(require racket/list
         ffi/unsafe/os-thread
         ffi/unsafe/os-async-channel)

;; make sure that polling an evt that is made ready via a
;; external signal --- in this case via an os-async-channel
;; posted by an OS thread --- doesn't stop polling by somehow
;; fumbling the external signal

(when (os-thread-enabled?)

  ;; make-worker/os-thread : (-> X) -> X
  (define (make-worker/os-thread)
    ;; req-os-chan : (OS-Async-Channelof (-> Void))
    ;; Worker just receives procedures to execute.
    (define req-chan (make-os-async-channel))
    (call-in-os-thread
     (lambda ()
       (let handle-requests ()
         ((os-async-channel-get req-chan))
         (handle-requests))))
    (define (send-request proc)
      (define resp-chan (make-os-async-channel))
      (os-async-channel-put req-chan
                            (lambda ()
                              (os-async-channel-put resp-chan (proc))))
      (cond
        [(sync/timeout 10 resp-chan)
         => values]
        [else
         (error "seems stuck")]))
    send-request)

  (define worker (make-worker/os-thread))

  (thread-wait
   (thread #:pool 'own
           (lambda ()
             (for ([i (in-range #e1e3)])
               (define xs (build-list (random #e2e3) (lambda (j) (random))))
               (define (work) (sort xs <))
               (when (= (modulo i 1e3) 0) (printf "~s\n" i))
               (apply + (worker work))))))

  (printf "ok\n"))
