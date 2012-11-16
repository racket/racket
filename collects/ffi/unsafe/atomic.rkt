#lang racket/base
(require ffi/unsafe
         (for-syntax racket/base))

(provide (protect-out in-atomic-mode?
                      start-atomic
                      end-atomic
                      start-breakable-atomic
                      end-breakable-atomic
                      call-as-atomic
                      call-as-nonatomic))

(define start-atomic
  (get-ffi-obj 'scheme_start_atomic_no_break #f (_fun -> _void)))

(define end-atomic
  (get-ffi-obj 'scheme_end_atomic_can_break #f (_fun -> _void)))

(define start-breakable-atomic
  (get-ffi-obj 'scheme_start_atomic #f (_fun -> _void)))

(define end-breakable-atomic
  (get-ffi-obj 'scheme_end_atomic #f (_fun -> _void)))

(define in-atomic-mode?
  (get-ffi-obj 'scheme_is_atomic #f (_fun -> (r : _int) -> (positive? r))))

;; ----------------------------------------

(define monitor-owner #f)

;; An exception may be constructed while we're entered:
(define entered-err-string-handler
  (lambda (s n)
    (call-as-nonatomic
     (lambda ()
       ((error-value->string-handler) s n)))))

(define old-paramz #f)
(define old-break-paramz #f)

(define extra-atomic-depth 0)

(define exited-key (gensym 'as-exit))
(define lock-tag (make-continuation-prompt-tag 'lock))

(define (call-as-atomic f)
  (unless (and (procedure? f)
               (procedure-arity-includes? f 0))
    (raise-type-error 'call-as-atomic "procedure (arity 0)" f))
  (cond
   [(eq? monitor-owner (current-thread))
    ;; Increment atomicity level for cooperation with anything
    ;; that is sensitive to the current depth of atomicity.
    (dynamic-wind (lambda () 
                    (start-breakable-atomic)
                    (set! extra-atomic-depth (add1 extra-atomic-depth)))
                  f
                  (lambda () 
                    (set! extra-atomic-depth (sub1 extra-atomic-depth))
                    (end-breakable-atomic)))]
   [else
    (with-continuation-mark 
        exited-key 
        #f
      (call-with-continuation-prompt
       (lambda ()
         (dynamic-wind
             (lambda () 
               (start-breakable-atomic)
               (set! monitor-owner (current-thread)))
             (lambda () 
               (set! old-paramz (current-parameterization))
               (set! old-break-paramz (current-break-parameterization))
               (parameterize ([error-value->string-handler entered-err-string-handler])
                 (parameterize-break 
                  #f
                  (call-with-exception-handler
                   (lambda (exn)
                     ;; Get out of atomic region before letting
                     ;;  an exception handler work
                     (if (continuation-mark-set-first #f exited-key)
                         exn ; defer to previous exn handler
                         (abort-current-continuation
                          lock-tag
                          (lambda () (raise exn)))))
                   f))))
             (lambda ()
               (set! monitor-owner #f)
               (set! old-paramz #f)
               (set! old-break-paramz #f)
               (end-breakable-atomic))))
       lock-tag
       (lambda (t) (t))))]))

(define (call-as-nonatomic f)
  (unless (and (procedure? f)
               (procedure-arity-includes? f 0))
    (raise-type-error 'call-as-nonatomic "procedure (arity 0)" f))
  (unless (eq? monitor-owner (current-thread)) 
    (error 'call-as-nonatomic "not in atomic area for ~e" f))
  (let ([paramz old-paramz]
        [break-paramz old-break-paramz]
        [extra-depth extra-atomic-depth])
    (with-continuation-mark 
        exited-key 
        #t ; disables special exception handling
      (call-with-parameterization
       paramz
       (lambda ()
         (call-with-break-parameterization
          break-paramz
          (lambda ()
            (dynamic-wind
                (lambda ()
                  (set! monitor-owner #f)
                  (set! extra-atomic-depth 0)
                  (end-breakable-atomic)
                  (let loop ([i extra-depth])
                    (unless (zero? i)
                      (end-breakable-atomic)
                      (loop (sub1 i)))))
                f
                (lambda ()
                  (start-breakable-atomic)
                  (set! old-paramz paramz)
                  (set! old-break-paramz break-paramz)
                  (let loop ([i extra-depth])
                    (unless (zero? i)
                      (start-breakable-atomic)
                      (loop (sub1 i))))
                  (set! extra-atomic-depth extra-depth)
                  (set! monitor-owner (current-thread)))))))))))
