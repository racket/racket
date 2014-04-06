#lang racket/base
(require "setup.rkt")

(provide run-single-installer install-planet-package clean-planet-package reindex-user-documentation)

;; run-single-installer : string (-> string) -> void
;; runs the installer on the given package
(define (run-single-installer file get-target-dir 
                              #:show-beginning-of-file? [show-beginning-of-file? #f])
  (run-single-installer/internal file get-target-dir #f #f #f show-beginning-of-file?))

;; install-planet-package : path path (list string string (listof string) nat nat) -> void
;; unpacks and installs the given planet package into the given path
(define (install-planet-package file directory spec)
  (run-single-installer/internal file (lambda () directory) (cons directory spec) #f #f #f))

;; clean-planet-package : path (list string string (listof string) nat nat) -> void
;; cleans the given planet package
(define (clean-planet-package directory spec)
  (run-single-installer/internal #f (lambda () directory) (cons directory spec) #f #t #f))

;; reindex-user-documentation
;; call after installing or uninstalling a set of Planet packages
(define (reindex-user-documentation)
  (run-single-installer/internal #f current-directory #f '(("scribblings/main/user")) #f #f))

;; run-single-installer : string (-> string) (list path string string nat nat) -> void
;; creates a separate thread, runs the installer in that thread,
;; returns when the thread completes
(define (run-single-installer/internal file get-target-dir planet-spec collections clean? 
                                       show-beginning-of-file?)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust]
                 [current-namespace (make-base-namespace)]
                 [exit-handler (lambda (v) (custodian-shutdown-all cust))])
    (define succeeded? #f)
    (define thd
      (thread
       (lambda ()
         (set! succeeded?
               (setup #:jobs 1
                      #:file file
                      #:get-target-dir get-target-dir
                      #:planet-specs (and planet-spec (list planet-spec))
                      #:collections collections)))))
    (dynamic-wind
     void
     (lambda ()
       (with-handlers ([exn:break? (lambda (exn)
                                     (break-thread thd)
                                     (sleep 0.1)
                                     (raise exn))])
         (thread-wait thd)
         (when show-beginning-of-file?
           (unless succeeded?
             (define (sep) (display "----------------------------------------\n"))
             (printf "The first 100 characters of the file:\n")
             (sep)
             (call-with-input-file file
               (λ (port)
                 (display (read-string 100 port))))
             (newline)
             (sep)))))
     (lambda () (custodian-shutdown-all cust)))))
