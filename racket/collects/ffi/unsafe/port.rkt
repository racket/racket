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

  (define socket-different?
    (case (system-type 'os)
      [(windows) #t]
      [else #f]))

  (struct fd-evt (sfd mode socket? [closed? #:mutable])
    #:property prop:evt
    (unsafe-poller
     (lambda (self wakeups)
       (define sfd (fd-evt-sfd self))
       (define mode (fd-evt-mode self))
       (define socket? (fd-evt-socket? self))
       (cond
         [(fd-evt-closed? self)
          (values (list self) #f)]
         [(unsafe-poll-fd sfd mode socket?)
          (values (list self) #f)]
         [else
          (when wakeups
            (unsafe-poll-ctx-fd-wakeup wakeups sfd mode #;socket?))
          (values #f self)]))))

  ;; {file-descriptor,socket}=>{read,write}-evt : (Hasheqv Nat => fd-evt)
  (define file-descriptor=>read-evt  (make-hasheqv))
  (define file-descriptor=>write-evt (make-hasheqv))
  (define socket=>read-evt  (if socket-different? (make-hasheqv) file-descriptor=>read-evt))
  (define socket=>write-evt (if socket-different? (make-hasheqv) file-descriptor=>write-evt))

  ;; Differences between unsafe-fd->evt and unsafe-{file-descriptor,socket}->semaphore:
  ;; - level-triggered, not edge-triggered
  ;; - no cooperation with ports created by unsafe-{file-descriptor,socket}->port

  (define (unsafe-fd->evt sfd mode [socket0? #t])
    (define socket? (and socket0? #t))
    (define sfd=>read-evt  (if socket? socket=>read-evt  file-descriptor=>read-evt))
    (define sfd=>write-evt (if socket? socket=>write-evt file-descriptor=>write-evt))
    (unless (exact-integer? sfd)
      (raise-argument-error 'unsafe-fd->evt "handle-integer?" 0 sfd mode socket0?))
    (unsafe-start-atomic)
    (begin0
        (case mode
          [(read)  (hash-ref! sfd=>read-evt  sfd (lambda () (fd-evt sfd mode socket? #f)))]
          [(write) (hash-ref! sfd=>write-evt sfd (lambda () (fd-evt sfd mode socket? #f)))]
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
                                 1 sfd mode socket0?)])
      (unsafe-end-atomic))))

(require (submod "." fd-evt))
