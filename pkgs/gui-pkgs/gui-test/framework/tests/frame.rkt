#lang racket/base

  (require "test-suite-utils.rkt")

  (module test racket/base)
  
  (send-sexp-to-mred '(send (make-object frame:basic%
                              "dummy to keep from quitting")
                            show #t))
  
  (define (test-creation name class-expression . args)
    (test
     name
     (lambda (x) (eq? 'passed x))
     (lambda ()
       (let ([frame-label
              (queue-sexp-to-mred
               `(let ([f (instantiate ,class-expression () ,@args)])
                  (send f show #t)
                  (send f get-label)))])
         (wait-for-frame frame-label)
         (queue-sexp-to-mred
          '(send (get-top-level-focus-window) close))
         'passed))))
  
  (test-creation
   'basic%-creation
   'frame:basic%
   '(label "test"))
  (test-creation
   'basic-mixin-creation
   '(frame:basic-mixin frame%)
   '(label "test"))
  
  (test-creation
   'info-mixin-creation
   '(frame:info-mixin frame:basic%)
   '(label "test"))
  
  (test-creation
   'info%-creation
   'frame:info%
   '(label "test"))
  
  (test-creation
   'text-info-mixin-creation
   '(frame:text-info-mixin frame:info%)
   '(label "test"))
  (test-creation
   'text-info%-creation
   'frame:text-info%
   '(label "test"))
  
  (test-creation
   'pasteboard-info-mixin-creation
   '(frame:pasteboard-info-mixin frame:info%)
   '(label "test"))
  
  (test-creation
   'pasteboard-info%-creation
   'frame:pasteboard-info%
   '(label "test"))
  
  (test-creation
   'standard-menus%-creation
   'frame:standard-menus%
   '(label "test"))
  
  (test-creation
   'standard-menus-mixin
   '(frame:standard-menus-mixin frame:basic%)
   '(label "test"))
  
  (test-creation
   'text%-creation
   'frame:text%)
  (test-creation
   'text-mixin-creation
   '(frame:text-mixin frame:editor%))
  (test-creation
   'text-mixin-creation
   '(frame:text-mixin frame:editor%))
  
  (test-creation
   'searchable%-creation
   'frame:searchable%)
  (test-creation
   'searchable-mixin
   '(frame:searchable-mixin frame:text%))
  
  (test-creation
   'pasteboard-mixin-creation
   '(frame:pasteboard-mixin frame:editor%))
  (test-creation
   'pasteboard-mixin-creation
   '(frame:pasteboard-mixin (frame:editor-mixin frame:standard-menus%)))
  (test-creation
   'pasteboard%-creation
   'frame:pasteboard%)
  
  (define (test-open name class-expression)
    (let* ([test-file-contents "test"]
           [tmp-file-name "framework-tmp"]
           [tmp-file (collection-file-path tmp-file-name "framework" "tests")])
      (test
       name
       (lambda (x)
         (when (file-exists? tmp-file)
           (delete-file tmp-file))
         (equal? x test-file-contents))
       (lambda ()
         (let ([frame-name 
                (queue-sexp-to-mred
                 `(let ([frame (new ,class-expression)])
                    (preferences:set 'framework:file-dialogs 'common)
                    (send frame show #t)
                    (send frame get-label)))])
           (wait-for-frame frame-name)
           (send-sexp-to-mred
            `(test:menu-select "File" "Open..."))
           (wait-for-frame "Open File")
           (call-with-output-file tmp-file
             (lambda (port)
               (display test-file-contents port))
             #:exists 'truncate)
           (queue-sexp-to-mred
            `(send (find-labelled-window "Filename:") focus))
           (send-sexp-to-mred
            `(begin ,(case (system-type)
                       [(macos macosx) `(test:keystroke #\a '(meta))]
                       [(unix) `(test:keystroke #\a '(meta))]
                       [(windows) `(test:keystroke #\a '(control))]
                       [else (error 'file-open-dialog "unknown system type: ~a" (system-type))])
                    (for-each test:keystroke
                              (string->list ,(path->string tmp-file)))
                    (test:keystroke #\return)))
           (wait-for-frame tmp-file-name)
           (begin0
             (queue-sexp-to-mred
              `(let* ([w (get-top-level-focus-window)])
                 (send (send w get-editor) get-text)))
             (send-sexp-to-mred
              `(test:close-top-level-window (get-top-level-focus-window)))
             (wait-for-frame frame-name)
             (queue-sexp-to-mred
              `(send (get-top-level-focus-window) close))))))))
  
  (test-open "frame:searchable open" 'frame:searchable%)
  (test-open "frame:text open" 'frame:text%)

