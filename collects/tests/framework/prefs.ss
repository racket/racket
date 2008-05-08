(module prefs mzscheme
  (require "test-suite-utils.ss"
	   mzlib/list)
  
  (define ((check-eq? x) y) (eq? x y))
  (define pref-sym 'plt:not-a-real-preference)
  (define marshalling-pref-sym 'plt:not-a-real-preference-marshalling)
  (define default-test-sym 'plt:not-a-real-preference-default-test)
  
  (define saved-prefs-file
    (let loop ([n 0])
      (let ([candidate
             (build-path (find-system-path 'temp-dir)
                         (format "saved-prefs.~a" n))])
        (if (file-exists? candidate)
            (loop (+ n 1))
            candidate)))) 
  
  (define prefs-file (find-system-path 'pref-file))
  
  (when (file-exists? prefs-file)
    (copy-file prefs-file saved-prefs-file)
    (delete-file prefs-file)
    (printf "saved preferences file from ~s to ~s\n" prefs-file saved-prefs-file))
  
  (shutdown-mred)
  
  (test
   'preference-unbound
   (check-eq? 'passed)
   `(with-handlers ([exn:unknown-preference?
                     (lambda (x)
                       'passed)])
      (preferences:get ',pref-sym)))
  (test 'preference-set-default/get
        (check-eq? 'passed)
        `(begin (preferences:set-default ',pref-sym 'passed symbol?)
                (preferences:get ',pref-sym)))
  (test 'preference-set/get
        (check-eq? 'new-pref)
        `(begin (preferences:set ',pref-sym 'new-pref)
                (preferences:get ',pref-sym)))

  (test 'preference-marshalling
        (check-eq? 'the-answer)
        `(begin (preferences:set-default ',marshalling-pref-sym (lambda () 'the-answer) procedure?)
                (preferences:set-un/marshall ',marshalling-pref-sym
                                             (lambda (f) (f))
                                             (lambda (v) (lambda () v)))
                (begin0 ((preferences:get ',marshalling-pref-sym))
                        (preferences:set ',marshalling-pref-sym (lambda () 2)))))
  (shutdown-mred)
  (test 'preference-marshalling
        (check-eq? 2)
        `(begin (preferences:set-default ',marshalling-pref-sym (lambda () 'the-answer) procedure?)
                (preferences:set-un/marshall ',marshalling-pref-sym
                                             (lambda (f) (f))
                                             (lambda (v) (lambda () v)))
                ((preferences:get ',marshalling-pref-sym))))
  
  (with-handlers ([eof-result? (lambda (x) (void))])
    (send-sexp-to-mred '(begin (preferences:set 'framework:verify-exit #f)
                               (exit:exit)
                               
                               ;; do this yield here so that exit:exit
                               ;; actually exits on this interaction.
                               ;; right now, exit:exit queue's a new event to exit
                               ;; instead of just exiting immediately.
                               (yield (make-semaphore 0)))))
  
  (test 'preference-get-after-restart
        (check-eq? 'new-pref)
        `(begin (preferences:set-default ',pref-sym 'passed symbol?)
                (preferences:get ',pref-sym)))

  (test 'preference-no-set-default-stage1
        (check-eq? 'stage1)
        `(begin (preferences:set-default ',default-test-sym 'default symbol?)
                (preferences:set ',default-test-sym 'new-value)
                'stage1))
  (shutdown-mred)
  (test 'preference-no-set-default-stage2
        (check-eq? 'stage2)
        `(begin 'stage2))
  (shutdown-mred)
  (test 'preference-no-set-default-stage3
        (check-eq? 'new-value)
        `(begin (preferences:set-default ',default-test-sym 'default symbol?)
                (preferences:get ',default-test-sym)))
  
  (test 'dialog-appears
        (lambda (x) (eq? 'passed x))
        (lambda ()
          (send-sexp-to-mred '(begin (send (make-object frame:basic% "frame") show #t)
                                     (preferences:show-dialog)))
          (wait-for-frame "Preferences")
          (send-sexp-to-mred '(begin (preferences:hide-dialog)
                                     (let ([f (get-top-level-focus-window)])
                                       (if f
                                           (if (string=? "Preferences" (send f get-label))
                                               'failed
                                               'passed)
                                           'passed))))))
  
  (when (file-exists? saved-prefs-file)
    (printf "restoring preferences file from ~s to ~s\n" saved-prefs-file prefs-file)
    (when (file-exists? prefs-file)
      (delete-file prefs-file))
    (copy-file saved-prefs-file prefs-file)
    (delete-file saved-prefs-file)))

