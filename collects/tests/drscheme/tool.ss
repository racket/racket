;; load this file as a tool to run the test suites

(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "list.ss")
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework"))
  
  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
            
      (define (phase1) (void))
      (define (phase2) (void))
      
      (preferences:set-default 'drscheme:test-suite:file-name "repl-tests.ss" string?)
      (preferences:set-default 'drscheme:test-suite:run-interval 10 number?)
      
      (preferences:set-default 'drscheme:test-suite:frame-width #f (lambda (x) (or (not x) (number? x))))
      (preferences:set-default 'drscheme:test-suite:frame-height 300 (lambda (x) (or (not x) (number? x))))
      
      (define (tool-mixin super%)
        (class super%
          (inherit get-button-panel)
          (super-new)
          (let* ([bitmap (make-object bitmap% 
                           (if (<= (get-display-depth) 1)
                               (build-path (collection-path "icons") "bb-sm-bw.bmp")
                               (build-path (collection-path "icons") "bb-small.bmp"))
                           'bmp)]
                 [button (make-object button%
                           (if (send bitmap ok?) bitmap "Console")
                           (get-button-panel)
                           (lambda (button evt) 
                             (let ([ask-test-suite (dynamic-require '(lib "run-tests.ss" "tests" "drscheme")
                                                                    'ask-test-suite)])
                               (ask-test-suite this))))])
            (send (get-button-panel) change-children
                  (lambda (l)
                    (cons button (remq button l)))))))
      
      (when (getenv "PLTDRTESTS")
        (printf "PLTDRTESTS: installing unit frame mixin\n")
        (drscheme:get/extend:extend-unit-frame tool-mixin)))))
