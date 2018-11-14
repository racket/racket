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
                    unsafe-start-atomic
                    unsafe-end-atomic
                    unsafe-poller
                    unsafe-poll-fd
                    unsafe-poll-ctx-fd-wakeup))
  (provide (protect-out unsafe-fd->evt))

  (struct fd-evt (sfd mode [closed? #:mutable])
    #:property prop:evt
    (unsafe-poller
     (lambda (self wakeups)
       (define sfd (fd-evt-sfd self))
       (define mode (fd-evt-mode self))
       (cond
         [(fd-evt-closed? self)
          (values (list self) #f)]
         [(unsafe-poll-fd sfd mode)
          (values (list self) #f)]
         [else
          (when wakeups
            (unsafe-poll-ctx-fd-wakeup wakeups sfd mode))
          (values #f self)]))))

  ;; sfd=>{read,write}-evt : (Hasheqv Nat => fd-evt)
  (define sfd=>read-evt  (make-hasheqv))
  (define sfd=>write-evt (make-hasheqv))

  ;; Differences between unsafe-fd->evt and unsafe-{file-descriptor,socket}->semaphore:
  ;; - treats fd as socket (cf unsafe-poll-fd, unsafe-poll-ctx-fd-wakeup)
  ;; - level-triggered, not edge-triggered
  ;; - no cooperation with ports created by unsafe-{file-descriptor,socket}->port

  (define (unsafe-fd->evt sfd mode)
    (unless (exact-integer? sfd)
      (raise-argument-error 'unsafe-fd->evt "handle-integer?" 0 sfd mode))
    (unsafe-start-atomic)
    (begin0
        (case mode
          [(read)  (hash-ref! sfd=>read-evt  sfd (lambda () (fd-evt sfd mode #f)))]
          [(write) (hash-ref! sfd=>write-evt sfd (lambda () (fd-evt sfd mode #f)))]
          [(check-read)  (hash-ref sfd=>read-evt  sfd #f)]
          [(check-write) (hash-ref sfd=>write-evt sfd #f)]
          [(remove)
           (define (remove-and-close sfd=>evt)
             (define evt (hash-ref sfd=>evt sfd #f))
             (when evt
               (hash-remove! sfd=>evt sfd)
               (set-fd-evt-closed?! evt #t)))
           (remove-and-close sfd=>read-evt)
           (remove-and-close sfd=>write-evt)
           #f]
          [(internal-debug)
           `((read  ,(hash-keys sfd=>read-evt))
             (write ,(hash-keys sfd=>write-evt)))]
          [else
           (unsafe-end-atomic)
           (raise-argument-error 'unsafe-fd->evt
                                 "(or/c 'read 'write 'check-read 'check-write 'remove)"
                                 1 sfd mode)])
      (unsafe-end-atomic))))

(require (submod "." fd-evt))
