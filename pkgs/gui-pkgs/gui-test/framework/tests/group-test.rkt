#lang racket/base
(require "test-suite-utils.rkt")

(define windows-menu-prefix 
  (let ([basics (list "Bring Frame to Front..." "Most Recent Window" 
                      #f)])
    (if (eq? (system-type) 'macosx)
        (list* "Minimize" "Zoom" basics)
        basics)))
(send-sexp-to-mred
 '(define-syntax car*
    (syntax-rules ()
      [(car* x) (if (pair? x)
                    (car x)
                    (error 'car* "got a non-pair for ~s" 'x))])))

;; this test uses a new eventspace so that the gracket function
;; current-eventspace-has-standard-menus? returns #f and thus
;; all of the platforms behave the same way.
(test
 'exit-on
 (lambda (x) (equal? x '("first")))
 (lambda ()
   (send-sexp-to-mred `(define new-eventspace (make-eventspace)))
   (send-sexp-to-mred
    '(begin (parameterize ([current-eventspace new-eventspace])
              (send (make-object frame:basic% "first") show #t))
            (preferences:set 'framework:verify-exit #t)))
   (wait-for-frame "first" 'new-eventspace)
   (send-sexp-to-mred
    `(queue-callback (lambda () 
                       (parameterize ([current-eventspace new-eventspace])
                         (send (get-top-level-focus-window) close)))))
   (wait-for-frame "Warning" 'new-eventspace)
   (send-sexp-to-mred
    `(parameterize ([current-eventspace new-eventspace])
       (test:button-push "Cancel")))
   (wait-for-frame "first" 'new-eventspace)
   (send-sexp-to-mred
    `(parameterize ([current-eventspace new-eventspace])
       (map (lambda (x) (send x get-label))
            (send (group:get-the-frame-group) get-frames))))))

;; after the first test, we should have one frame that will always
;; be in the group.

(test
 'one-frame-registered
 (lambda (x) (equal? x (list "test" "first")))
 (lambda ()
   (queue-sexp-to-mred
    `(send (make-object frame:basic% "test") show #t))
   (wait-for-frame "test")
   (queue-sexp-to-mred
    `(begin0 (map (lambda (x) (send x get-label))
                  (send (group:get-the-frame-group) get-frames))
             (send (get-top-level-focus-window) close)))))

(test
 'two-frames-registered
 (lambda (x) (equal? x (list "test2" "test1" "first")))
 (lambda ()
   (queue-sexp-to-mred
    '(send (make-object frame:basic% "test1") show #t))
   (wait-for-frame "test1")
   (queue-sexp-to-mred
    '(send (make-object frame:basic% "test2") show #t))
   (wait-for-frame "test2")
   (queue-sexp-to-mred
    `(begin0 (let ([frames (send (group:get-the-frame-group) get-frames)])
               (for-each (lambda (x)
                           (unless (equal? (send x get-label) "first")
                             (send x close)))
                         frames)
               (map (lambda (x) (send x get-label)) frames))))))

(test
 'one-frame-unregistered
 (lambda (x) (equal? x (list "test1" "first")))
 (lambda ()
   (queue-sexp-to-mred
    '(send (make-object frame:basic% "test1") show #t))
   (wait-for-frame "test1")
   (queue-sexp-to-mred
    '(send (make-object frame:basic% "test2") show #t))
   (wait-for-frame "test2")
   (queue-sexp-to-mred
    `(send (get-top-level-focus-window) close))
   (queue-sexp-to-mred
    `(let ([frames (send (group:get-the-frame-group) get-frames)])
       (for-each (lambda (x)
                   (unless (equal? (send x get-label) "first")
                     (send x close)))
                 frames)
       (map (lambda (x) (send x get-label)) frames)))))

(when (eq? (system-type) 'macosx)
  
  (test
   'windows-menu
   (lambda (x)
     (equal? x (append windows-menu-prefix (list "first" "test"))))
   (λ ()
     (queue-sexp-to-mred
      '(let ([frame (make-object frame:basic% "test")])
         (send frame show #t)))
     (wait-for-frame "test")
     (queue-sexp-to-mred
      '(let ([mb (send (get-top-level-focus-window) get-menu-bar)])
         (send mb on-demand)
         (define labels
           (for/list ([x (send (car* (send mb get-items)) get-items)])
             (and (is-a? x labelled-menu-item<%>) (send x get-label))))
         (send (get-top-level-focus-window) close)
         labels))))
  
  (test
   'windows-menu-unshown
   (lambda (x)
     (equal? x (append windows-menu-prefix (list "first" "test"))))
   (lambda ()
     (queue-sexp-to-mred
      '(let ([frame1 (make-object frame:basic% "test")]
             [frame2 (make-object frame:basic% "test-not-shown")])
         (send frame1 show #t)))
     (wait-for-frame "test")
     (queue-sexp-to-mred
      '(let ([mb (send (get-top-level-focus-window) get-menu-bar)])
         (send mb on-demand)
         (define items
           (for/list ([x (send (car* (send mb get-items)) get-items)])
             (and (is-a? x labelled-menu-item<%>) (send x get-label))))
         (send (get-top-level-focus-window) close)
         items))))
  
  (test
   'windows-menu-sorted1
   (lambda (x)
     (equal? x (append windows-menu-prefix (list "aaa" "bbb" "first"))))
   (lambda ()
     (queue-sexp-to-mred
      '(let ([frame (make-object frame:basic% "aaa")])
         (send frame show #t)))
     (wait-for-frame "aaa")
     (queue-sexp-to-mred
      '(let ([frame (make-object frame:basic% "bbb")])
         (send frame show #t)))
     (wait-for-frame "bbb")
     (queue-sexp-to-mred
      `(let ([frames (send (group:get-the-frame-group) get-frames)])
         (define mb (send (car* frames) get-menu-bar))
         (send mb on-demand)
         (begin0 (map (lambda (x)
                        (and (is-a? x labelled-menu-item<%>) (send x get-label)))
                      (send (car* (send mb get-items))
                            get-items))
                 (for-each (lambda (x)
                             (unless (equal? (send x get-label) "first")
                               (send x close)))
                           frames))))))
  
  (test
   'windows-menu-sorted2
   (lambda (x)
     (equal? x (append windows-menu-prefix (list "aaa" "bbb" "first"))))
   (lambda ()
     (queue-sexp-to-mred
      '(let ([frame (make-object frame:basic% "bbb")])
         (send frame show #t)))
     (wait-for-frame "bbb")
     (queue-sexp-to-mred
      '(let ([frame (make-object frame:basic% "aaa")])
         (send frame show #t)))
     (wait-for-frame "aaa")
     (queue-sexp-to-mred
      `(let ([frames (send (group:get-the-frame-group) get-frames)])
         (define mb (send (car* frames) get-menu-bar))
         (send mb on-demand)
         (begin0 (map (lambda (x)
                        (and (is-a? x labelled-menu-item<%>) (send x get-label)))
                      (send (car* (send mb get-items))
                            get-items))
                 (for-each (lambda (x)
                             (unless (equal? (send x get-label) "first")
                               (send x close)))
                           frames)))))))
