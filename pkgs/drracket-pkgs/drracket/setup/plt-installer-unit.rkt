#lang racket/base
(require racket/unit
         mred/mred-sig
         racket/class
         "plt-installer-sig.rkt"
         (prefix-in single: setup/plt-single-installer)
         mrlib/terminal
         string-constants)

(provide plt-installer@)
(define-unit plt-installer@
  (import mred^)
  (export setup:plt-installer^)
  
  (define on-installer-run (make-parameter void))
  
  ;; with-installer-window : ((union (instanceof dialog%) (instanceof frame%)) -> void) (-> void) -> void
  ;; creates a frame and sets up the current error and output ports
  ;; before calling `do-install'. 
  ;; runs the installer in a separate thread and returns immediately,
  ;; before the installation is complete. The cleanup thunk is called when installation completes
  (define (with-installer-window do-install cleanup-thunk)
    (define installer-run (on-installer-run))
    (parameterize ([on-terminal-run 
                    (λ ()
                      (printf "\nInstallation complete.\n")
                      (installer-run))])
      (in-terminal
       (λ (custodian frame) (do-install frame))
       #:title (string-constant plt-installer-progress-window-title)
       #:cleanup-thunk cleanup-thunk)))
  
  (define run-single-installer single:run-single-installer)
  
  (define (run-installer file [cleanup-thunk void])
    (with-installer-window 
     (lambda (frame)
       (run-single-installer 
        file
        (lambda ()
          (sleep 0.2) ; kludge to allow f to appear first
          (end-busy-cursor)
          ;; do these strings ever appear? (should move to string-constants, if so)
          (let ([d (get-directory 
                    "Select the destination for unpacking"
                    frame)])
            (unless d
              (printf ">>> Cancelled <<<\n"))
            (begin-busy-cursor)
            d))))
     cleanup-thunk)))
