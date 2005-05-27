(module group-test mzscheme
  (require "test-suite-utils.ss")
  
  (test
   'exit-on
   (lambda (x) #t)
   (lambda ()
     (send-sexp-to-mred
      '(begin (send (make-object frame:basic% "first") show #t)
              (preferences:set 'framework:verify-exit #t)))
     (wait-for-frame "first")
     (send-sexp-to-mred
      `(queue-callback (lambda () (send (get-top-level-focus-window) close))))
     (wait-for-frame "Warning")
     (send-sexp-to-mred
      `(test:button-push "Cancel"))
     (wait-for-frame "first")
     'passed))
  
  ;; after the first test, we should have one frame that will always
  ;; be in the group.
  
  (test
   'one-frame-registered
   (lambda (x) (equal? x (list "test" "first")))
   (lambda ()
     (send-sexp-to-mred
      `(send (make-object frame:basic% "test") show #t))
     (wait-for-frame "test")
     (send-sexp-to-mred
      `(begin0
         (map (lambda (x) (send x get-label)) (send (group:get-the-frame-group) get-frames))
         (send (get-top-level-focus-window) close)))))
  
  (test
   'two-frames-registered
   (lambda (x) (equal? x (list "test2" "test1" "first")))
   (lambda ()
     (send-sexp-to-mred
      '(send (make-object frame:basic% "test1") show #t))
     (wait-for-frame "test1")
     (send-sexp-to-mred
      '(send (make-object frame:basic% "test2") show #t))
     (wait-for-frame "test2")
     (send-sexp-to-mred
      `(begin0
         (let ([frames (send (group:get-the-frame-group) get-frames)])
           (for-each (lambda (x) 
                       (unless (equal? (send x get-label) "first")
                         (send x close)))
                     frames)
           (map (lambda (x) (send x get-label)) frames))))))
  
  (test
   'one-frame-unregistered
   (lambda (x) (equal? x (list "test1" "first")))
   (lambda ()
     (send-sexp-to-mred
      '(send (make-object frame:basic% "test1") show #t))
     (wait-for-frame "test1")
     (send-sexp-to-mred
      '(send (make-object frame:basic% "test2") show #t))
     (wait-for-frame "test2")
     (queue-sexp-to-mred
      `(send (get-top-level-focus-window) close))
     (send-sexp-to-mred
      `(let ([frames (send (group:get-the-frame-group) get-frames)])
         (for-each (lambda (x) 
                     (unless (equal? (send x get-label) "first")
                       (send x close)))
                   frames)
         (map (lambda (x) (send x get-label)) frames)))))
  
  (test
   'windows-menu
   (lambda (x)
     (equal? x (list "Bring Frame to Front..." "Most Recent Window" 
                     #f "first" "test")))
   (lambda ()
     (send-sexp-to-mred
      '(let ([frame (make-object frame:basic% "test")])
         (send frame show #t)))
     (wait-for-frame "test")
     (send-sexp-to-mred
      '(begin0
         (map
          (lambda (x) (and (is-a? x labelled-menu-item<%>) (send x get-label)))
          (send (car (send (send (get-top-level-focus-window) get-menu-bar) get-items)) get-items))
         (send (get-top-level-focus-window) close)))))
  
  (test
   'windows-menu-unshown
   (lambda (x)
     (equal? x (list "Bring Frame to Front..." "Most Recent Window"
                     #f "first" "test")))
   (lambda ()
     (send-sexp-to-mred
      '(let ([frame1 (make-object frame:basic% "test")]
             [frame2 (make-object frame:basic% "test-not-shown")])
         (send frame1 show #t)))
     (wait-for-frame "test")
     (send-sexp-to-mred
      '(begin0
         (map
          (lambda (x) (and (is-a? x labelled-menu-item<%>) (send x get-label)))
          (send (car (send (send (get-top-level-focus-window) get-menu-bar) get-items)) get-items))
         (send (get-top-level-focus-window) close)))))
  
  (test
   'windows-menu-sorted1
   (lambda (x)
     (equal? x (list "Bring Frame to Front..." "Most Recent Window"
                     #f "aaa" "bbb" "first")))
   (lambda ()
     (send-sexp-to-mred
      '(let ([frame (make-object frame:basic% "aaa")])
         (send frame show #t)))
     (wait-for-frame "aaa")
     (send-sexp-to-mred
      '(let ([frame (make-object frame:basic% "bbb")])
         (send frame show #t)))
     (wait-for-frame "bbb")
     (send-sexp-to-mred
      `(let ([frames (send (group:get-the-frame-group) get-frames)])
         (begin0
           (map
            (lambda (x) (and (is-a? x labelled-menu-item<%>) (send x get-label)))
            (send (car (send (send (car frames) get-menu-bar) get-items)) get-items))
           (for-each (lambda (x) 
                       (unless (equal? (send x get-label) "first")
                         (send x close)))
                     frames))))))
  
  (test
   'windows-menu-sorted2
   (lambda (x)
     (equal? x (list "Bring Frame to Front..." "Most Recent Window"
                     #f "aaa" "bbb" "first")))
   (lambda ()
     (send-sexp-to-mred
      '(let ([frame (make-object frame:basic% "bbb")])
         (send frame show #t)))
     (wait-for-frame "bbb")
     (send-sexp-to-mred
      '(let ([frame (make-object frame:basic% "aaa")])
         (send frame show #t)))
     (wait-for-frame "aaa")
     (send-sexp-to-mred
      `(let ([frames (send (group:get-the-frame-group) get-frames)])
         (begin0
           (map
            (lambda (x) (and (is-a? x labelled-menu-item<%>) (send x get-label)))
            (send (car (send (send (car frames) get-menu-bar) get-items)) get-items))
           (for-each (lambda (x) 
                       (unless (equal? (send x get-label) "first")
                         (send x close)))
                     frames)))))))
