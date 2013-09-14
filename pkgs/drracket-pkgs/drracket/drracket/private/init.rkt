#lang racket/unit
  (require string-constants
           drracket/private/drsig
           racket/gui/base
           racket/list
           racket/class
           framework)
  
  (import [prefix drracket: drracket:interface^]
          [prefix drracket:rep: drracket:rep^])
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
  (define get-last-N-errors-chan (make-channel))
  (define number-of-errors-to-save 5)
  (thread
   (λ () 
     (define-struct recent (msg when))
     (define currently-visible-chan (make-channel))
     (let loop ([recently-seen-errors/unfiltered '()]
                [last-N-errors '()]
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
           (define new-last-N-errors (cons msg+exn 
                                           (take last-N-errors 
                                                 (min (- number-of-errors-to-save 1)
                                                      (length last-N-errors)))))
           (define-values (msg exn) (apply values msg+exn))
           (cond
             [currently-visible 
              ;; drop errors when we have one waiting to be clicked on
              (loop recently-seen-errors new-last-N-errors #t)]
             [(ormap (λ (x) (equal? msg (recent-msg x))) 
                     recently-seen-errors)
              ;; drop the error if we've seen it recently
              (loop recently-seen-errors new-last-N-errors #f)]
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
                   (channel-put currently-visible-chan #f))))
              (loop (cons (make-recent msg (current-seconds)) recently-seen-errors)
                    new-last-N-errors
                    #t)])))
        (handle-evt
         get-last-N-errors-chan
         (λ (c)
           (channel-put c last-N-errors)
           (loop
            (loop recently-seen-errors/unfiltered
                  last-N-errors
                  currently-visible))))
        (handle-evt
         currently-visible-chan
         (λ (val) 
           (loop recently-seen-errors/unfiltered last-N-errors #f)))))))
  
  (define (get-last-N-errors)
    (define c (make-channel))
    (channel-put get-last-N-errors-chan c)
    (channel-get c))
  
  ;; override error-display-handler to duplicate the error
  ;; message in both the standard place (as defined by the
  ;; current error-display-handler) and in a message box
  ;; identifying the error as a drracket internal error.
  (error-display-handler
   (λ (msg exn)
     ;; this  may raise an exception if the port is gone.
     (with-handlers ([exn:fail? (λ (x) (void))])
       (original-error-display-handler msg exn))
     (channel-put error-display-chan (list msg exn))
     
     ;; try to end any unclosed edit-sequences in any definitions
     ;; texts or interactions texts. That is, we try to recover a
     ;; little bit from errors that are raised in the dynamic 
     ;; extent of an edit-sequence.
     (when (eq? (current-thread) (eventspace-handler-thread system-eventspace))
       (for ([f (in-list (get-top-level-windows))])
         (when (is-a? f drracket:unit:frame<%>)
           (let loop ([o f])
             (cond
               [(is-a? o editor-canvas%)
                (define t (send o get-editor))
                (when (or (is-a? t drracket:unit:definitions-text<%>)
                          (is-a? t drracket:rep:text<%>))
                  (let loop ()
                    (when (send t in-edit-sequence?)
                      (send t end-edit-sequence)
                      (loop))))]
               [(is-a? o area-container<%>)
                (for ([c (in-list (send o get-children))])
                  (loop c))])))))))
