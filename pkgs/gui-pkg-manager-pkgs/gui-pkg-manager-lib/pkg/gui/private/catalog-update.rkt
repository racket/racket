#lang racket/base
(require racket/gui/base 
         racket/class
         racket/contract
         string-constants
         pkg/lib
         (prefix-in db: pkg/db))
(provide
 pkg-catalog-update-local/simple-status-dialog)

(define (pkg-catalog-update-local/simple-status-dialog #:parent [parent #f])
  (define error-message-shown? #f)
  (define d%
    (class dialog%
      (define/override (on-subwindow-event w e)
        (if (and error-message-shown? (send e button-down?))
            (if (is-a? w button%)
                #f
                (if (or (is-a? w message%)
                        (and
                         (is-a? w editor-canvas%)
                         (let-values ([(w h) (send w get-client-size)])
                           (< (send e get-x) w))))
                    (begin
                      (send w popup-menu
                            (let ([m (make-object popup-menu%)])
                              (make-object menu-item%
                                "Copy Message"
                                m
                                (lambda (i e)
                                  (send the-clipboard
                                        set-clipboard-string
                                        (send err-msg-txt get-text 0 (send err-msg-txt last-position))
                                        (send e get-time-stamp))))
                              m)
                            (send e get-x)
                            (send e get-y))
                      #t)
                    #f))
            #f))
      (super-new)))
  (define d (new d% 
                 [width 600]
                 [parent parent]
                 [label (string-constant update-catalog)]))
  (define m (new message% 
                 [parent d]
                 [label ""]
                 [stretchable-width #t]))
  
  (define err-msg-txt (new text%))
  (define (show-exn exn)
    (set! error-message-shown? #t)
    (send d begin-container-sequence)
    (parameterize ([current-output-port (open-output-text-editor err-msg-txt)])
      (display (exn-message exn))
      (define context (continuation-mark-set->context
                       (exn-continuation-marks exn)))
      (unless (null? context)
        (printf "\n")
        (for ([x (in-list context)])
          (printf "\n  ~s" x))))
    (define sd (make-object style-delta% 'change-family 'modern))
    (send sd set-delta-foreground "darkred")
    (send err-msg-txt change-style sd 0 (send err-msg-txt last-position))
    (send err-msg-txt lock #t)
    (send err-msg-txt hide-caret #t)
    (define ec (new editor-canvas%
                    [parent d]
                    [min-height 400]
                    [editor err-msg-txt]))
    (define bp (new horizontal-panel% 
                    [parent d]
                    [stretchable-height #f]
                    [alignment '(right center)]))
    (define b (new button% 
                   [label (string-constant ok)]
                   [parent bp]
                   [callback
                    (λ (_1 _2) (send d show #f))]))
    (send d end-container-sequence))
  
  (thread
   (λ ()
     (with-handlers ([exn:fail?
                      (λ (exn) 
                        (queue-callback
                         (λ ()
                           (show-exn exn))))])
       (for ([catalog (in-list (db:get-catalogs))])
         (define s (make-semaphore 0))
         (queue-callback
          (λ ()
            (send m set-label (format (string-constant updating-catalog-from) catalog))
            (semaphore-post s)))
         (semaphore-wait s)
         (pkg-catalog-update-local #:catalogs (list catalog)
                                   #:set-catalogs? #f
                                   #:quiet? #t))
       (queue-callback 
        (λ () (send d show #f))))))
  
  (send d show #t))
