#lang racket/base

(require racket/gui/base
         racket/class
         racket/set
         racket/contract
         racket/list
         syntax/moddep
         syntax/toplevel
         framework
         string-constants
         mrlib/graph
         drracket/private/drsig
         "eval-helpers-and-pref-init.rkt"
         racket/unit
         racket/async-channel
         racket/match
         setup/private/lib-roots
         racket/port
         drracket/private/rectangle-intersect
         drracket/private/standalone-module-browser)

(provide module-overview@)

(define adding-file (string-constant module-browser-adding-file))
(define unknown-module-name "? unknown module name")

;; probably, at some point, the module browser should get its
;; own output ports or something instead of wrapping these ones
(define original-output-port (current-output-port))
(define original-error-port (current-error-port))

(define filename-constant (string-constant module-browser-filename-format))
(define font-size-gauge-label (string-constant module-browser-font-size-gauge-label))
(define progress-label (string-constant module-browser-progress-label))
(define laying-out-graph-label (string-constant module-browser-laying-out-graph-label))
(define open-file-format (string-constant module-browser-open-file-format))
(define lib-paths-checkbox-constant (string-constant module-browser-show-lib-paths))

(define (set-box/f b v) (when (box? b) (set-box! b v)))

(define-unit module-overview@
  (import [prefix drracket:frame: drracket:frame^]
          [prefix drracket:eval: drracket:eval^]
          [prefix drracket:language-configuration: drracket:language-configuration/internal^]
          [prefix drracket:language: drracket:language^])
  (export (rename drracket:module-overview^ 
                  [_module-overview/file module-overview/file]
                  [_make-module-overview-pasteboard make-module-overview-pasteboard]))
  
  (define (module-overview parent)
    (let ([filename (get-file #f parent)])
      (when filename
        (module-overview/file filename parent fill-pasteboard
                              overview-frame% canvas:basic% pasteboard:basic%))))
  

  (define (_module-overview/file filename parent) 
    (module-overview/file filename parent fill-pasteboard
                          overview-frame% canvas:basic% pasteboard:basic%))
  (define (_make-module-overview-pasteboard vertical? mouse-currently-over)
    (make-module-overview-pasteboard vertical? mouse-currently-over
                                     pasteboard:basic%
                                     #:on-boxed-word-double-click
                                     (λ (fn)
                                       (when fn
                                         (handler:edit-file fn)))))
  
  ;                                                                
  ;                                                                
  ;                                                                
  ;    ;;;                                      ;;;;   ;     ;  ;  
  ;   ;                                        ;    ;  ;     ;  ;  
  ;   ;                                       ;        ;     ;  ;  
  ;  ;;;;  ; ;  ;;;    ; ;;  ;;     ;;;       ;        ;     ;  ;  
  ;   ;    ;;  ;   ;   ;;  ;;  ;   ;   ;      ;        ;     ;  ;  
  ;   ;    ;       ;   ;   ;   ;  ;    ;      ;        ;     ;  ;  
  ;   ;    ;    ;;;;   ;   ;   ;  ;;;;;;      ;     ;  ;     ;  ;  
  ;   ;    ;   ;   ;   ;   ;   ;  ;           ;     ;  ;     ;  ;  
  ;   ;    ;   ;   ;   ;   ;   ;   ;           ;    ;  ;     ;  ;  
  ;   ;    ;    ;;;;;  ;   ;   ;    ;;;;        ;;;;;   ;;;;;   ;  
  ;                                                                
  ;                                                                
  ;                                                                
  
  
  (define (fill-pasteboard pasteboard filename-or-text/pos show-status send-user-thread/eventspace)
    
    (define text/pos 
      (if (drracket:language:text/pos? filename-or-text/pos)
          filename-or-text/pos
          (let ([t (make-object text:basic%)])
            (send t load-file filename-or-text/pos)
            (drracket:language:text/pos
             t
             0
             (send t last-position)))))
    
    (define progress-channel (make-async-channel))
    (define connection-channel (make-async-channel))
    
    (define-values/invoke-unit process-program-unit
      (import process-program-import^)
      (export process-program-export^))
    
    ;; =user thread=
    (define (iter sexp continue)
      (cond
        [(eof-object? sexp) 
         (custodian-shutdown-all user-custodian)]
        [else
         (add-connections sexp)
         (continue)]))
    (define init-complete (make-semaphore 0))
    
    (define user-custodian #f)
    (define user-thread #f)
    (define error-str #f)
    
    (define init-dir
      (let* ([bx (box #f)]
             [filename (send (drracket:language:text/pos-text text/pos) get-filename bx)])
        (get-init-dir 
         (and (not (unbox bx)) filename))))
    
    (define (init)
      (set! user-custodian (current-custodian))
      (set! user-thread (current-thread))
      (moddep-current-open-input-file
       (λ (filename)
         (let* ([p (open-input-file filename)]
                [wxme? (regexp-match-peek #rx#"^WXME" p)])
           (if wxme?
               (let ([t (new text%)])
                 (close-input-port p)
                 (send t load-file filename)
                 (let ([prt (open-input-text-editor t)])
                   (port-count-lines! prt)
                   prt))
               p))))
      (current-output-port (swallow-specials original-output-port))
      (current-error-port (swallow-specials original-error-port))
      (current-load-relative-directory #f)
      (current-directory init-dir)
      (error-display-handler (λ (str exn) 
                               (set! error-str str)
                               (when (exn? exn)
                                 (set! error-str
                                       (apply
                                        string-append
                                        error-str
                                        (for/list ([x (in-list (continuation-mark-set->context 
                                                                (exn-continuation-marks exn)))])
                                          (format "\n  ~s" x)))))))
      
      ;; instead of escaping when there's an error on the user thread,
      ;; we just shut it all down. This kills the event handling loop
      ;; for the eventspace and wakes up the thread below
      ;; NOTE: we cannot set this directly in `init' since the call to `init'
      ;; is wrapped in a parameterize of the error-escape-handler
      (queue-callback
       (λ ()
         (error-escape-handler
          (λ () (custodian-shutdown-all user-custodian)))
         (semaphore-post init-complete))))
    
    (define (swallow-specials port)
      (define-values (in out) (make-pipe-with-specials))
      (thread
       (λ ()
         (let loop ()
           (define c (read-char-or-special in))
           (cond
             [(char? c)
              (display c out)
              (loop)]
             [(eof-object? c)
              (close-output-port out)
              (close-input-port in)]
             [else
              (loop)]))))
      out)
    
    (define (kill-termination) (void))
    (define complete-program? #t)

    ((drracket:eval:traverse-program/multiple
      (preferences:get (drracket:language-configuration:get-settings-preferences-symbol))
      init
      kill-termination)
     text/pos
     iter
     complete-program?)
    
    (semaphore-wait init-complete)
    (send-user-thread/eventspace user-thread user-custodian)
    
    ;; this thread puts a "cap" on the end of the connection-channel
    ;; so that we know when we've gotten to the end.
    ;; this ensures that we can completely flush out the
    ;; connection-channel.
    (thread
     (λ ()
       (sync (thread-dead-evt user-thread))
       (async-channel-put connection-channel 'done)))
    
    (send pasteboard begin-adding-connections)
    (let ([evt
           (choice-evt
            (handle-evt progress-channel (λ (x) (cons 'progress x)))
            (handle-evt connection-channel (λ (x) (cons 'connect x))))])
      (let loop ()
        (let* ([evt-value (yield evt)]
               [key (car evt-value)]
               [val (cdr evt-value)])
          (case key
            [(progress) 
             (show-status val)
             (loop)]
            [(connect)
             (unless (eq? val 'done)
               (let ([name-original (list-ref val 0)]
                     [name-require (list-ref val 1)]
                     [path-key (list-ref val 2)]
                     [require-depth (list-ref val 3)])
                 (send pasteboard add-connection name-original name-require path-key require-depth))
               (loop))]))))
    (send pasteboard end-adding-connections)
    
    (custodian-shutdown-all user-custodian)
    
    (cond
      [error-str
       (message-box 
        (string-constant module-browser)
        (format (string-constant module-browser-error-expanding)
                error-str))
       #f]
      [else
       #t]))
  
  (define overview-frame%
    (class (drracket:frame:basics-mixin
            frame:standard-menus%)
      (define/override (edit-menu:between-select-all-and-find menu) (void))
      (define/override (edit-menu:between-redo-and-cut menu) (void))
      (define/override (edit-menu:between-find-and-preferences menu) (void))
      
      (define/override (edit-menu:create-cut?) #f)
      (define/override (edit-menu:create-copy?) #f)
      (define/override (edit-menu:create-paste?) #f)
      (define/override (edit-menu:create-clear?) #f)
      (define/override (edit-menu:create-select-all?) #f)
      
      (define/override (on-size w h)
        (preferences:set 'drracket:module-overview:window-width w)
        (preferences:set 'drracket:module-overview:window-height h)
        (super on-size w h))
      (super-new))))