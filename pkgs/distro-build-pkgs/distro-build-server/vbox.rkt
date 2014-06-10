#lang racket/base
(require racket/system
         racket/string)

(provide start-vbox-vm
         stop-vbox-vm)

(define VBoxManage (find-executable-path "VBoxManage"))
(define use-headless? #t)

(define (system*/string . args)
  (define s (open-output-string))
  (parameterize ([current-output-port s])
    (apply system* args))
  (get-output-string s))

(define (vbox-state vbox)
  (define s (system*/string VBoxManage "showvminfo" vbox))
  (define m (regexp-match #rx"(?m:^State:[ ]*([a-z]+(?: [a-z]+)*))" s))
  (define state (and m (string->symbol (cadr m))))
  (case state
    [(|powered off| aborted) 'off]
    [(running saved paused) state]
    [(restoring) (vbox-state vbox)]
    [else 
     (eprintf "~a\n" s)
     (error 'vbox-state "could not get virtual machine status: ~s" vbox)]))

(define (vbox-control vbox what)
  (system* VBoxManage "controlvm" vbox what))

(define (vbox-start vbox)
  (apply system* VBoxManage "startvm" vbox 
         (if use-headless?
             '("--type" "headless")
             null))
  ;; wait for the machine to get going:
  (let loop ([n 0])
    (unless (eq? 'running (vbox-state vbox))
      (unless (= n 20)
        (sleep 0.5)
        (loop (add1 n))))))

(define call-with-vbox-lock
  (let ([s (make-semaphore 1)]
        [lock-cust (current-custodian)])
    (lambda (thunk)
      (define t (current-thread))
      (define ready (make-semaphore))
      (define done (make-semaphore))
      (parameterize ([current-custodian lock-cust])
        (thread (lambda () 
                  (semaphore-wait s)
                  (semaphore-post ready)
                  (sync t done)
                  (semaphore-post s))))
      (sync ready)
      (thunk)
      (semaphore-post done))))

(define (printf/flush fmt . args)
  (apply printf fmt args)
  (flush-output))

(define (start-vbox-vm vbox
                       #:max-vms [max-vm 1]
                       #:dry-run? [dry-run? #f]
                       #:log-status [log-status printf/flush])
  (define (check-count)
    (define s (system*/string VBoxManage "list" "runningvms"))
    (unless ((length (string-split s "\n")) . < . max-vm)
      (error 'start-vbox "too many virtual machines running (>= ~a) to start: ~s"
             max-vm
             vbox)))
  (log-status "Starting VirtualBox machine ~s\n" vbox)
  (unless dry-run?
    (case (vbox-state vbox)
      [(running) (void)]
      [(paused) (vbox-control vbox "resume")]
      [(off saved) (call-with-vbox-lock
                    (lambda ()
                      (check-count)
                      (vbox-start vbox)))])
    (unless (eq? (vbox-state vbox) 'running)
      (error 'start-vbox "could not get virtual machine started: ~s" vbox))
    ;; pause a little to let the VM get networking ready, etc.
    (sleep 3)))

(define (stop-vbox-vm vbox
                      #:dry-run? [dry-run? #f]
                      #:log-status [log-status printf/flush])
  (log-status "Stopping VirtualBox machine ~s\n" vbox)
  (unless dry-run?
    (vbox-control vbox "savestate")
    (unless (eq? (vbox-state vbox) 'saved)
      (error 'start-vbox "virtual machine isn't in the expected saved state: ~s" vbox))))
