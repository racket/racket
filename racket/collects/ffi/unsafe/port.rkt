#lang racket/base
(require (only-in '#%unsafe
                  unsafe-file-descriptor->port
                  unsafe-port->file-descriptor
                  unsafe-file-descriptor->semaphore
                  unsafe-socket->port
                  unsafe-port->socket
                  unsafe-socket->semaphore))
(provide (all-from-out '#%unsafe)
         unsafe-fd->evt)

(module fd-evt racket/base
  (require (only-in '#%unsafe
                    unsafe-poller
                    unsafe-poll-fd
                    unsafe-poll-ctx-fd-wakeup))
  (provide (protect-out unsafe-fd->evt))

  (struct fd-evt (sfd mode)
    #:property prop:evt
    (unsafe-poller
     (lambda (self wakeups)
       (define sfd (fd-evt-sfd self))
       (define mode (fd-evt-mode self))
       (cond
         [(unsafe-poll-fd sfd mode)
          (values (list self) #f)]
         [else
          (when wakeups
            (unsafe-poll-ctx-fd-wakeup wakeups sfd mode))
          (values #f self)]))))

  ;; Differences between unsafe-fd->evt and unsafe-{file-descriptor,socket}->semaphore:
  ;; - treats fd as socket (cf unsafe-poll-fd, unsafe-poll-ctx-fd-wakeup)
  ;; - level-triggered, not edge-triggered
  ;; - no table, no 'remove, no trigger-on-remove
  ;; - no cooperation with ports created by unsafe-{file-descriptor,socket}->port

  ;; TODO: add table, 'remove (or just close operation?); avoids race condition ??

  (define (unsafe-fd->evt sfd mode)
    (unless (exact-integer? sfd)
      (raise-argument-error 'unsafe-fd->evt "handle-integer?" 0 sfd mode))
    (unless (memq mode '(read write))
      (raise-argument-error 'unsafe-fd->evt "(or/c 'read 'write)" 1 sfd mode))
    (fd-evt sfd mode)))

(require (submod "." fd-evt))
