#lang racket/base
(require "private/cm-minimal.rkt"
         (submod "private/cm-minimal.rkt" cm-internal)
         racket/contract/base
         racket/place
         racket/path
         racket/promise)

(provide (except-out (all-from-out "private/cm-minimal.rkt")
                     current-path->mode)

         file-stamp-in-collection
         file-stamp-in-paths

         make-compile-lock
         compile-lock->parallel-lock-client

         (contract-out
          [current-path->mode
           (parameter/c (or/c #f (-> path? (and/c path? relative-path?))))]))

(define (file-stamp-in-collection p)
  (file-stamp-in-paths p (current-library-collection-paths)))

(define (file-stamp-in-paths p paths)
  (let ([p-eles (explode-path (simple-form-path p))])
    (let c-loop ([paths paths])
      (cond
        [(null? paths) #f]
        [else
         (let i-loop ([collects-eles (explode-path (simple-form-path (car paths)))]
                      [p-eles p-eles])
           (cond
             [(null? collects-eles)
              ;; we're inside the collection hierarchy, so we just 
              ;; use the date of the original file (or the zo, whichever
              ;; is newer).
              (let-values ([(base name dir) (split-path p)])
                (let* ([p-date (try-file-time p)]
                       [alt-date (and (not p-date)
                                      (try-file-time (rkt->ss p)))]
                       [date (or p-date alt-date)]
                       [get-path (lambda ()
                                   (if p-date
                                       p
                                       (rkt->ss p)))]
                       [modes (use-compiled-file-paths)]
                       [roots (current-compiled-file-roots)]
                       [get-zo-date+mode (lambda (name)
                                           (ormap
                                            (lambda (root)
                                              (ormap
                                               (lambda (mode)
                                                 (let ([v (try-file-time
                                                           (build-path 
                                                            (reroot-path* base root)
                                                            mode
                                                            (path-add-extension name #".zo")))])
                                                   (and v (list* v mode root))))
                                               modes))
                                            roots))]
                       [main-zo-date+mode (and (or p-date (not alt-date))
                                               (get-zo-date+mode name))]
                       [alt-zo-date+mode (and (or alt-date
                                                  (and (not p-date) 
                                                       (not alt-date)
                                                       (not main-zo-date+mode)))
                                              (get-zo-date+mode (rkt->ss name)))]
                       [zo-date+mode (or main-zo-date+mode alt-zo-date+mode)]
                       [zo-date (and zo-date+mode (car zo-date+mode))]
                       [get-zo-path (lambda ()
                                      (let-values ([(name mode root)
                                                    (if main-zo-date+mode
                                                        (values (path-add-extension name #".zo")
                                                                (cadr main-zo-date+mode)
                                                                (cddr main-zo-date+mode))
                                                        (values (path-add-extension (rkt->ss name) #".zo")
                                                                (cadr alt-zo-date+mode)
                                                                (cddr alt-zo-date+mode)))])
                                        (build-path (reroot-path* base root) mode name)))])
                  (cond
                   [(and zo-date
                         (or (not date)
                             (zo-date . > . date)))
                    (cons zo-date
                          (delay (get-compiled-file-sha1 (get-zo-path))))]
                   [date
                    (cons date
                          (delay (get-source-sha1 (get-path))))]
                   [else #f])))]
             [(null? p-eles) 
              ;; this case shouldn't happen... I think.
              (c-loop (cdr paths))]
             [else
              (cond
                [(equal? (car p-eles) (car collects-eles))
                 (i-loop (cdr collects-eles) (cdr p-eles))]
                [else 
                 (c-loop (cdr paths))])]))]))))

(define (reroot-path* base root)
  (cond
   [(eq? root 'same) base]
   [(relative-path? root)
    (build-path base root)]
   [else
    (reroot-path base root)]))

;; ----------------------------------------

(define (make-compile-lock)
  (define-values (manager-side-chan build-side-chan) (place-channel))
  (struct pending (response-chan zo-path died-chan-manager-side) #:transparent)
  (struct running (zo-path died-chan-manager-side) #:transparent)
  
  (define currently-locked-files (make-hash))
  (define pending-requests '())
  (define running-compiles '())
  
  (thread
   (λ ()
     (let loop ()
       (apply
        sync
        (handle-evt
         manager-side-chan
         (λ (req)
           (define command (list-ref req 0))
           (define zo-path (list-ref req 1))
           (define response-manager-side (list-ref req 2))
           (define died-chan-manager-side (list-ref req 3))
           (define compilation-thread-id (list-ref req 4))
           (case command
             [(lock)
              (cond
                [(hash-ref currently-locked-files zo-path #f)
                 (log-info (format "compile-lock: ~s ~a already locked" zo-path compilation-thread-id))
                 (set! pending-requests (cons (pending response-manager-side zo-path died-chan-manager-side)
                                              pending-requests))
                 (loop)]
                [else
                 (log-info (format "compile-lock: ~s ~a obtained lock" zo-path compilation-thread-id))
                 (hash-set! currently-locked-files zo-path #t)
                 (place-channel-put response-manager-side #t)
                 (set! running-compiles (cons (running zo-path died-chan-manager-side) running-compiles))
                 (loop)])]
             [(unlock)
              (log-info (format "compile-lock: ~s ~a unlocked" zo-path compilation-thread-id))
              (define (same-pending-zo-path? pending) (equal? (pending-zo-path pending) zo-path))
              (define to-unlock (filter same-pending-zo-path? pending-requests))
              (set! pending-requests (filter (compose not same-pending-zo-path?) pending-requests))
              (for ([pending (in-list to-unlock)])
                (place-channel-put (pending-response-chan pending) #f))
              (hash-remove! currently-locked-files zo-path)
              (set! running-compiles (filter (λ (a-running) (not (equal? (running-zo-path a-running) zo-path)))
                                             running-compiles))
              (loop)])))
        (for/list ([running-compile (in-list running-compiles)])
          (handle-evt
           (running-died-chan-manager-side running-compile)
           (λ (compilation-thread-id)
             (define zo-path (running-zo-path running-compile))
             (set! running-compiles (remove running-compile running-compiles))
             (define same-zo-pending 
               (filter (λ (pending) (equal? zo-path (pending-zo-path pending)))
                       pending-requests))
             (cond
               [(null? same-zo-pending)
                (log-info (format "compile-lock: ~s ~a died; no else waiting" zo-path compilation-thread-id))
                (hash-remove! currently-locked-files zo-path)
                (loop)]
               [else
                (log-info (format "compile-lock: ~s ~a died; someone else waiting" zo-path compilation-thread-id))
                (define to-be-running (car same-zo-pending))
                (set! pending-requests (remq to-be-running pending-requests))
                (place-channel-put (pending-response-chan to-be-running) #t)
                (set! running-compiles 
                      (cons (running zo-path (pending-died-chan-manager-side to-be-running))
                            running-compiles))
                (loop)]))))))))
  
  build-side-chan)

(define (compile-lock->parallel-lock-client build-side-chan [custodian #f])
  (define monitor-threads (make-hash))
  (define add-monitor-chan (make-channel))
  (define kill-monitor-chan (make-channel))
  
  (when custodian
    (parameterize ([current-custodian custodian])
      (thread
       (λ () 
         (let loop ()
           (sync
            (handle-evt add-monitor-chan
                        (λ (arg)
                          (define-values (zo-path monitor-thread) (apply values arg))
                          (hash-set! monitor-threads zo-path monitor-thread)
                          (loop)))
            (handle-evt kill-monitor-chan
                        (λ (zo-path)
                          (define thd/f (hash-ref monitor-threads zo-path #f))
                          (when thd/f (kill-thread thd/f))
                          (hash-remove! monitor-threads zo-path)
                          (loop)))))))))
  
  (λ (command zo-path)
    (define compiling-thread (current-thread))
    (define-values (response-builder-side response-manager-side) (place-channel))
    (define-values (died-chan-compiling-side died-chan-manager-side) (place-channel))
    (place-channel-put build-side-chan (list command 
                                             zo-path
                                             response-manager-side
                                             died-chan-manager-side 
                                             (eq-hash-code compiling-thread)))
    (cond
      [(eq? command 'lock)
       (define monitor-thread
        (and custodian
             (parameterize ([current-custodian custodian])
               (thread
                (λ ()
                  (thread-wait compiling-thread)
                  ;; compiling thread died; alert the server
                  ;; & remove this thread from the table
                  (place-channel-put died-chan-compiling-side (eq-hash-code compiling-thread))
                  (channel-put kill-monitor-chan zo-path))))))
       (when monitor-thread (channel-put add-monitor-chan (list zo-path monitor-thread)))
       (define res (place-channel-get response-builder-side))
       (when monitor-thread
         (unless res ;; someone else finished compilation for us; kill the monitor
           (channel-put kill-monitor-chan zo-path)))
       res]
      [(eq? command 'unlock)
       (when custodian 
         ;; we finished the compilation; kill the monitor
         (channel-put kill-monitor-chan zo-path))])))
