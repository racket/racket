#lang racket/base
(require racket/gui/base
         racket/class
         racket/file
         setup/main-collects)
(require (for-syntax racket/base
                     syntax/path-spec
                     compiler/cm-accomplice
                     setup/main-collects))

(provide include-bitmap
         include-bitmap/relative-to)

(define-syntax (-include-bitmap stx)
  (syntax-case stx ()
    [(_ orig-stx source path-spec type)
     (let* ([c-file (resolve-path-spec #'path-spec #'source #'orig-stx)]
            [content
             (with-handlers ([exn:fail?
                              (lambda (exn)
                                (error 'include-bitmap
                                       "could not load ~e: ~a"
                                       c-file
                                       (if (exn? exn)
                                           (exn-message exn)
                                           (format "~e" exn))))])
               (with-input-from-file c-file
                 (lambda ()
                   (read-bytes (file-size c-file)))))])
       (register-external-file c-file)
       (with-syntax ([content content]
                     [c-file (path->main-collects-relative c-file)])
         (syntax/loc stx
           (get-or-load-bitmap content 'c-file type))))]))

(define-syntax (include-bitmap/relative-to stx)
  (syntax-case stx ()
    [(_ source path-spec) #`(-include-bitmap #,stx source path-spec 'unknown/mask)]
    [(_ source path-spec type) #`(-include-bitmap #,stx source path-spec type)]))

(define-syntax (include-bitmap stx)
  (syntax-case stx ()
    [(_ path-spec) #`(-include-bitmap #,stx #,stx path-spec 'unknown/mask)]
    [(_ path-spec type) #`(-include-bitmap #,stx #,stx path-spec type)]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run-time support

(define cached (make-hash))

(define (get-or-load-bitmap content orig type)
  (hash-ref cached 
            (cons content type)
            (λ ()
              (define-values (in out) (make-pipe))
              (thread
               (λ () 
                 (display content out)
                 (close-output-port out)))
              
              (define bm (make-object bitmap% in type))
              (unless (send bm ok?)
                (error 'include-bitmap
                       "unable to parse image, originated from: ~a"
                       (path->string (main-collects-relative->path orig))))
              (hash-set! cached (cons content type) bm)
              bm)))
