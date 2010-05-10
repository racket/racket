#lang racket/base

  (require mred
           scheme/unit
           racket/port
           racket/class
           syntax/toplevel
           framework
           "drsig.rkt")
  
  ;; to ensure this guy is loaded (and the snipclass installed) in the drscheme namespace & eventspace
  ;; these things are for effect only!
  (require mrlib/cache-image-snip
           (prefix-in image-core: mrlib/image-core))
  
  (define op (current-output-port))
  (define (oprintf . args) (apply fprintf op args))
  
  (provide eval@)
  (define-unit eval@
    (import [prefix drracket:language-configuration: drracket:language-configuration/internal^]
            [prefix drracket:rep: drracket:rep^]
            [prefix drracket:init: drracket:init^]
            [prefix drracket:language: drracket:language^]
            [prefix drracket:unit: drracket:unit^])
    (export drracket:eval^)
    
    (define (traverse-program/multiple language-settings
                                       init
                                       kill-termination)
      (let-values ([(eventspace custodian) 
                    (build-user-eventspace/custodian
                     language-settings
                     init
                     kill-termination)])
        (let ([language (drracket:language-configuration:language-settings-language
                         language-settings)]
              [settings (drracket:language-configuration:language-settings-settings
                         language-settings)])
          (λ (input iter complete-program?)
            (let-values ([(port src)
                          (cond
                            [(input-port? input) (values input #f)]
                            [else (values
                                   (let* ([text (drracket:language:text/pos-text input)]
                                          [start (drracket:language:text/pos-start input)]
                                          [end (drracket:language:text/pos-end input)]
                                          [text-port (open-input-text-editor text start end values
                                                                             (send text get-port-name))])
                                     (port-count-lines! text-port)
                                     (let* ([line (send text position-paragraph start)]
                                            [column (- start (send text paragraph-start-position line))]
                                            [relocated-port (relocate-input-port text-port 
                                                                                 (+ line 1)
                                                                                 column
                                                                                 (+ start 1))])
                                       (port-count-lines! relocated-port)
                                       relocated-port))
                                   (drracket:language:text/pos-text input))])])
              (parameterize ([current-eventspace eventspace])
                (queue-callback
                 (λ ()
                   (let ([read-thnk 
                          (if complete-program?
                              (send language front-end/complete-program port settings)
                              (send language front-end/interaction port settings))])
                     (let loop ()
                       (let ([in (read-thnk)])
                         (cond
                           [(eof-object? in)
                            (iter in (λ () (void)))]
                           [else
                            (iter in (λ () (loop)))]))))))))))))
    
    (define (expand-program/multiple language-settings
                                     eval-compile-time-part? 
                                     init
                                     kill-termination)
      (let ([res (traverse-program/multiple language-settings init kill-termination)])
        (λ (input iter complete-program?)
          (let ([expanding-iter
                 (λ (rd cont)
                   (cond
                     [(eof-object? rd) (iter rd cont)]
                     [eval-compile-time-part? 
                      (iter (expand-top-level-with-compile-time-evals rd) cont)]
                     [else (iter (expand rd) cont)]))])
            (res input 
                 expanding-iter
                 complete-program?)))))
    
    (define (expand-program input
                            language-settings
                            eval-compile-time-part? 
                            init
                            kill-termination
                            iter)
      ((expand-program/multiple 
        language-settings
        eval-compile-time-part? 
        init
        kill-termination)
       input
       iter
       #t))
    
    
    (define (build-user-eventspace/custodian language-settings init kill-termination)
      (let* ([user-custodian (make-custodian)]
             [eventspace (parameterize ([current-custodian user-custodian])
                           (make-eventspace))]
             [language (drracket:language-configuration:language-settings-language
                        language-settings)]
             [settings (drracket:language-configuration:language-settings-settings
                        language-settings)]
             [eventspace-main-thread #f]
             [run-in-eventspace
              (λ (thnk)
                (parameterize ([current-eventspace eventspace])
                  (let ([sema (make-semaphore 0)]
                        [ans #f])
                    (queue-callback
                     (λ ()
                       (let/ec k
                         (parameterize ([error-escape-handler
                                         (let ([drscheme-expand-program-error-escape-handler
                                                (λ () (k (void)))])
                                           drscheme-expand-program-error-escape-handler)])
                           (set! ans (thnk))))
                       (semaphore-post sema)))
                    (semaphore-wait sema)
                    ans)))]
             [drs-snip-classes (get-snip-classes)])
        (run-in-eventspace
         (λ ()
           (current-custodian user-custodian)
           (set-basic-parameters drs-snip-classes)
           (drracket:rep:current-language-settings language-settings)))
        (send language on-execute settings run-in-eventspace)
        (run-in-eventspace
         (λ ()
           (set! eventspace-main-thread (current-thread))
           (init)
           (break-enabled #t)))
        (thread
         (λ ()
           (thread-wait eventspace-main-thread)
           (kill-termination)))
        (values eventspace user-custodian)))
    
    ;; get-snip-classes : -> (listof snipclass)
    ;; returns a list of the snip classes in the current eventspace
    (define (get-snip-classes)
      (let loop ([n (send (get-the-snip-class-list) number)])
        (if (zero? n)
            null
            (cons (send (get-the-snip-class-list) nth (- n 1))
                  (loop (- n 1))))))
    
    ;; set-basic-parameters : (listof (is-a/c? snipclass%)) -> void
    ;; sets the parameters that are shared between the repl's initialization
    ;; and expand-program
    (define (set-basic-parameters snip-classes)
      (for-each (λ (snip-class) (send (get-the-snip-class-list) add snip-class))
                snip-classes)
      
      (current-thread-group (make-thread-group))
      (current-command-line-arguments #())
      (current-pseudo-random-generator (make-pseudo-random-generator))
      (current-evt-pseudo-random-generator (make-pseudo-random-generator))
      (read-curly-brace-as-paren #t)
      (read-square-bracket-as-paren #t)
      (error-print-width 250)
      (current-ps-setup (make-object ps-setup%))
      
      (current-namespace (make-empty-namespace))
      (for-each (λ (x) (namespace-attach-module drracket:init:system-namespace x))
                to-be-copied-module-names))
    
    ;; these module specs are copied over to each new user's namespace 
    (define to-be-copied-module-specs
      (list 'mzscheme
            '(lib "mzlib/foreign.rkt")
            '(lib "mred/mred.rkt")
            '(lib "mrlib/cache-image-snip.rkt")
            '(lib "mrlib/image-core.rkt")
	    '(lib "mrlib/matrix-snip.rkt")
            '(lib "mzlib/pconvert-prop.rkt")
            '(lib "planet/terse-info.rkt")))
    
    ;; ensure that they are all here.
    (for-each (λ (x) (dynamic-require x #f)) to-be-copied-module-specs)
    ;; get the names of those modules.
    (define to-be-copied-module-names
      (let ([get-name
             (λ (spec)
               (if (symbol? spec)
                   spec
                   ((current-module-name-resolver) spec #f #f)))])
        (map get-name to-be-copied-module-specs)))
    
    ;; build-input-port : string[file-exists?] -> (values input any)
    ;; constructs an input port for the load handler. Also
    ;; returns a value representing the source of code read from the file.
    (define (build-input-port filename)
      (let* ([p (open-input-file filename)]
             [chars (list (read-char p)
                          (read-char p)
                          (read-char p)
                          (read-char p))])
        (close-input-port p)
        (cond
          [(equal? chars (string->list "WXME"))
           (let ([text (make-object text%)])
             (send text load-file filename)
             (let ([port (open-input-text-editor text)])
               (port-count-lines! port)
               (values port text)))]
          [else
           (let ([port (open-input-file filename)])
             (port-count-lines! port)
             (values port filename))]))))
