#lang racket/unit
  (require string-constants
           "drsig.rkt"
           racket/gui/base
           framework)
  
  
  (import)
  (export drracket:init^)
  
  (define original-output-port (current-output-port))
  (define original-error-port (current-error-port))
  
  (define primitive-eval (current-eval))
  (define primitive-load (current-load))
  
  (define system-logger (current-logger))
  
  (define system-custodian (current-custodian))
  (define system-eventspace (current-eventspace))
  (define system-thread (current-thread))
  (define system-namespace (current-namespace))
  (define first-dir (current-directory))
  
  (define error-display-eventspace (make-eventspace))
  
  (define original-error-display-handler (error-display-handler))
  
  (define error-display-handler-message-box-title
    (make-parameter (string-constant drscheme-internal-error)))
  
  (define system-security-guard (current-security-guard))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  internal error display support
  ;;
  
  (define error-display-chan (make-channel))
  (thread
   (λ () 
     (define-struct recent (msg when))
     (define currently-visible-chan (make-channel))
     (let loop ([recently-seen-errors/unfiltered '()]
                [currently-visible #f])
       (sync
        (handle-evt
         error-display-chan
         (λ (msg+exn)
           (define recently-seen-errors 
             ;; recent errors are ones less than 5 minutes old
             (let ([now (current-seconds)])
               (filter (λ (x) (<= (+ (recent-when x) (* 60 5)) now))
                       recently-seen-errors/unfiltered)))
           (define-values (msg exn) (apply values msg+exn))
           (cond
             [currently-visible 
              ;; drop errors when we have one waiting to be clicked on
              (loop recently-seen-errors #t)]
             [(ormap (λ (x) (equal? msg (recent-msg x))) 
                     recently-seen-errors)
              ;; drop the error if we've seen it recently
              (loop recently-seen-errors #f)]
             [else
              ;; show the error
              (define title (error-display-handler-message-box-title))
              (define text (let ([p (open-output-string)])
                             (parameterize ([current-error-port p]
                                            [current-output-port p])
                               (original-error-display-handler msg exn))
                             (get-output-string p)))
              
              (parameterize ([current-eventspace error-display-eventspace]
                             [current-custodian system-custodian])
                (thread
                 (λ () 
                   (message-box title text #f '(stop ok) #:dialog-mixin frame:focus-table-mixin)
                   (channel-put currently-visible #f))))
              (loop (cons (make-recent msg (current-seconds)) recently-seen-errors)
                    #t)])))
        (handle-evt
         currently-visible-chan
         (λ (val) 
           (loop recently-seen-errors/unfiltered #f)))))))
  
  ;; override error-display-handler to duplicate the error
  ;; message in both the standard place (as defined by the
  ;; current error-display-handler) and in a message box
  ;; identifying the error as a drscheme internal error.
  (error-display-handler
   (λ (msg exn)
     ;; this  may raise an exception if the port is gone.
     (with-handlers ([exn:fail? (λ (x) (void))])
       (original-error-display-handler msg exn))
     (channel-put error-display-chan (list msg exn))))
