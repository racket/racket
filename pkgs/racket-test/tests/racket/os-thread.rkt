#lang racket/base
(require ffi/unsafe
         ffi/unsafe/os-thread
         ffi/unsafe/os-async-channel)

(when (os-thread-enabled?)
  (define ch1 (make-os-async-channel))
  (define ch2 (make-os-async-channel))
  
  (define wb
    (make-weak-box
     (thread (lambda ()
               (call-in-os-thread
                (lambda ()
                  (os-async-channel-put ch1 'ready)
                  (os-async-channel-get ch2)))
               (sync never-evt)))))

  (sync (system-idle-evt))
  (os-async-channel-get ch1)

  (collect-garbage)

  (when (weak-box-value wb)
    (error "thread was retained"))

  (os-async-channel-put ch2 'done)

  'success)

