;; load this file as a tool to run the test suites

(module run-tests mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework"))

  (provide ask-test-suite)
  
  (define test-thread
    (let ([kill-old void])
      (lambda (test thunk)
        (kill-old)
        (let ([thread-desc (thread
                            (lambda ()
                              (printf "t>> ~a started~n" test)
                              (thunk)
                              (printf "t>> ~a finished~n" test)))])
          (set! kill-old
                (lambda ()
                  (when (thread-running? thread-desc)
                    (kill-thread thread-desc)
                    (printf "t>> killed ~a~n" test))))))))
      
  (define all-tests (map symbol->string (load (build-path (collection-path "tests" "drscheme")
                                                          "README"))))
  
  (define (make-repl)
    (test-thread
     "REPL"
     (lambda ()
       (let ([startup "~/.mzschemerc"])
         (when (file-exists? startup)
           (load startup)))
       (case (system-type)
         [(windows macos)
          (graphical-read-eval-print-loop (current-eventspace))]
         [else
          (read-eval-print-loop)]))))
      
  (define (run-test-suite filename)
    (test-thread
     filename
     (lambda ()
       ((dynamic-require `(lib ,filename "tests" "drscheme") 'run-test)))))
      
  (define current-test-suite-frame #f)
      
  (define test-suite-frame%
    (class frame%
      (define/override (on-size w h)
        (preferences:set 'drscheme:test-suite:frame-width w)
        (preferences:set 'drscheme:test-suite:frame-height h))
      (define/augment (on-close)
        (inner (void) on-close)
        (set! current-test-suite-frame #f))
      (super-new)))

  (define (ask-test-suite parent)
    (if current-test-suite-frame
        (send current-test-suite-frame show #t)
        (let* ([drscheme-test-dir (collection-path "tests" "drscheme")]
               [frame (make-object test-suite-frame% 
                        "Test Suites"
                        parent
                        (preferences:get 'drscheme:test-suite:frame-width)
                        (preferences:get 'drscheme:test-suite:frame-height))]
               [panel (make-object vertical-panel% frame)]
               [top-panel (make-object vertical-panel% panel)]
               [bottom-panel (make-object horizontal-panel% panel)])
          (send top-panel stretchable-height #f)
          (make-object button%
            "REPL" 
            bottom-panel
            (lambda (_1 _2)
              (send frame show #f)
              (make-repl)))
          
          (when drscheme-test-dir
            (send top-panel stretchable-height #t)
            (send bottom-panel stretchable-height #f)
            (letrec ([lb (make-object list-box%
                           #f
                           all-tests
                           top-panel
                           (lambda (b e)
                             (when (eq? (send e get-event-type) 'list-box-dclick)
                               (run-test-suite-callback))))]
                     [run-test-suite-callback
                      (lambda ()
                        (let ([selection (send lb get-selection)])
                          (when selection
                            (send frame show #f)
                            (let ([test (list-ref all-tests selection)])
                              (preferences:set
                               'drscheme:test-suite:file-name
                               test)
                              (run-test-suite
                               test)))))])
              
	      ;; set values from preferences
              (let* ([test-suite (preferences:get 'drscheme:test-suite:file-name)]
                     [num (send lb find-string test-suite)])
                (when num
                  (send lb set-string-selection test-suite)
                  (send lb set-first-visible-item num)
                  (test:run-interval (preferences:get 'drscheme:test-suite:run-interval))))
              
              (send
               (make-object button%
                 "Run Test Suite"
                 bottom-panel
                 (lambda (_1 _2)
                   (run-test-suite-callback))
                 '(border))
               focus))
            
            (let* ([pre-times (list 0 10 50 100 500)]
                   [times (if (member (test:run-interval) pre-times)
                              pre-times
                              (append pre-times (list (test:run-interval))))]
                   [choice
                    (make-object choice%
                      "Run Interval"
                      (map number->string times)
                      top-panel
                      (lambda (choice event)
                        (let ([time (list-ref times (send choice get-selection))])
                          (preferences:set 'drscheme:test-suite:run-interval time)
                          (test:run-interval time))))])
              (send choice set-selection
                    (let loop ([l times]
                               [n 0])
                      (if (= (car l) (test:run-interval))
                          n
                          (loop (cdr l)
                                (+ n 1)))))))
          (make-object button%
            "Cancel" 
            bottom-panel
            (lambda (_1 _2)
              (send frame show #f)))
          (make-object grow-box-spacer-pane% bottom-panel)
          (send frame show #t)
          (set! current-test-suite-frame frame)))))
