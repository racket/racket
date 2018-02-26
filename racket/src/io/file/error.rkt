#lang racket/base
(require "../host/rktio.rkt"
         "../host/error.rkt")

(provide raise-filesystem-error
         copy-file-step-string

         maybe-raise-missing-module
         set-maybe-raise-missing-module!)

(define (raise-filesystem-error who orig-err base-msg)
  (define err (cond
                [(racket-error? orig-err RKTIO_ERROR_EXISTS)
                 orig-err]
                [else
                 (remap-rktio-error orig-err)]))
  (define msg (cond
                [(racket-error? err RKTIO_ERROR_EXISTS)
                 ;; don't add "system error", because it
                 ;; will be redundant
                 (if who
                     (string-append (symbol->string who) ": " base-msg)
                     base-msg)]
                [else
                 (format-rktio-message who err base-msg)]))
  (raise
   (cond
     [(racket-error? err RKTIO_ERROR_EXISTS)
      (exn:fail:filesystem:exists
       msg
       (current-continuation-marks))]
     [(not (eq? (rktio-errkind err) RKTIO_ERROR_KIND_RACKET))
      (exn:fail:filesystem:errno
       msg
       (current-continuation-marks)
       (cons (rktio-errno err)
             (let ([kind (rktio-errkind err)])
               (cond
                 [(eqv? kind RKTIO_ERROR_KIND_POSIX) 'posix]
                 [(eqv? kind RKTIO_ERROR_KIND_WINDOWS) 'windows]
                 [(eqv? kind RKTIO_ERROR_KIND_GAI) 'gai]
                 [else (error 'raise-filesystem-error "confused about rktio error")]))))]
     [else
      (exn:fail:filesystem
       msg
       (current-continuation-marks))])))

(define (copy-file-step-string err)
  (cond
    [(racket-error? err RKTIO_ERROR_EXISTS)
     "destination exists"]
    [else
     (define step (vector-ref err 2))
     (cond
       [(eqv? step RKTIO_COPY_STEP_OPEN_SRC)
        "cannot open source file"]
       [(eqv? step RKTIO_COPY_STEP_OPEN_DEST)
        "cannot open destination file"]
       [(eqv? step RKTIO_COPY_STEP_READ_SRC_DATA)
        "error reading source file"]
       [(eqv? step RKTIO_COPY_STEP_WRITE_DEST_DATA)
        "error writing destination file"]
       [(eqv? step RKTIO_COPY_STEP_READ_SRC_METADATA)
        "error reading source-file metadata"]
       [(eqv? step RKTIO_COPY_STEP_WRITE_DEST_METADATA)
        "error writing destination-file metadata"]
       [else "copy failed"])]))

(define maybe-raise-missing-module void)

(define (set-maybe-raise-missing-module! proc)
  (set! maybe-raise-missing-module proc))
