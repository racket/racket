(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "unitsig.ss")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           "patchlevel.ss" "check.ss"
           (lib "external.ss" "browser"))

  ;; either 'yes, 'no, or something else, see `enabled?' below for a reason
  (preferences:set-default 'updates:enabled? 'unset symbol?)
  (preferences:set-default 'updates:last 0 integer?)
  ;; how often do we check; default: check every week
  (preferences:set-default 'updates:frequency (* 60 60 24 7) integer?)
  ;; time to wait if user chooses "later"; default: in two weeks
  (define later-delay (* 60 60 24 14))

  ;; This is used to check if updates:enabled? is true or false.  The problem
  ;; is that we don't want to set a default of #t or #f, so make it 'unset and
  ;; change it only when users explicitly set it.  This makes it possible to
  ;; have the default be #f, but without making it always #f for all users, and
  ;; in the future it is possible to change it to default to a different
  ;; default.
  (define (enabled? v)
    (case v [(yes) #t] [(no) #f] [else #t])) ; default to #t

  (define (check-for-updates)
    ;; wait until the definitions are instantiated, return top-level window
    (define (wait-for-definitions)
      (let ([ws (get-top-level-windows)])
        (if (null? ws)
          (begin (sleep 1) (wait-for-definitions))
          (car ws))))
    ;; show a message and a disable button
    (define hide-message void) ; set by show-message
    (define (show-message top)
      (sleep 3) ; wait to make this appearance visible
      (let* ([info    (send top get-info-panel)]
             [panel   (make-object horizontal-panel% info)]
             [message (make-object message% "Checking for updates..." panel)]
             [button  (make-object button% "Disable" panel disable)])
        (send info change-children (lambda (l) (cons panel (remq panel l))))
        (sleep 1) ; wait before and after check to make it visible
        (set! hide-message
              (lambda now?
                (unless (and (pair? now?) (car now?)) (sleep 1))
                (send info change-children (lambda (l) (remq panel l)))
                (set! hide-message void)))))
    ;; disable handler
    (define abort void) ; used to abort an active check
    (define (disable . _)
      (abort) (preferences:set 'updates:enabled? 'no))
    ;; main checker
    (define (check top)
      (show-message top)
      (let ([r #f])
        ;; run the check in a thread, with a chance to abort it
        (let ([t (thread (lambda () (set! r (check-version))))])
          (set! abort (lambda () (kill-thread t)))
          (thread-wait t)
          (set! abort void))
        ;; do nothing if we have a good version, if there was an error, or if
        ;; there is a suggested alpha -- only show a message if there is a
        ;; newer version
        (when (and (pair? r) (eq? 'newer (car r)))
          (hide-message 'now)
          (case (message-box/custom
                 "Outdated PLT Version"
                 (string-append "PLT Scheme v"(cadr r)" is now available")
                 "Quit && &Take Me There" "Remind Me &Later" "&Disable Checking"
                 ;; don't use `top' for the parent -- some wants a non-modal
                 ;; dialog that can be pushed back as a reminder instead of
                 ;; dismissed
                 #f '(default=2) #f)
            ;; go there
            [(1) (send-url "http://download.plt-scheme.org/")
                 (sleep 1)
                 ((application-quit-handler))]
            ;; later
            [(2) (preferences:set 'updates:last
                                  (- (+ (current-seconds) later-delay)
                                     (preferences:get 'updates:frequency)))]
            ;; disable
            [(3) (preferences:set 'updates:enabled? 'no)]
            ;; only other option is escape -- check again in the normal time
            )))
      (hide-message))
    ;; start the check if enabled and enough time passed
    (when (enabled? (preferences:get 'updates:enabled?))
      (let ([cur  (current-seconds)]
            [last (preferences:get 'updates:last)]
            [freq (preferences:get 'updates:frequency)])
        (when '(> (- cur last) freq)
          '(preferences:set 'updates:last cur)
          (check (wait-for-definitions))))))

  (provide tool@)
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      (define (phase1) (void))
      (define (phase2)
        (preferences:add-to-warnings-checkbox-panel
         (lambda (panel)
           (let ([b (make-object check-box%
                      "Periodically check for newer PLT Scheme versions"
                      panel
                      (lambda (b e)
                        (preferences:set 'updates:enabled?
                                         (if (send b get-value) 'yes 'no))))])
             (preferences:add-callback
              'updates:enabled?
              (lambda (p v) (send b set-value (enabled? v))))
             (send b set-value
                   (enabled? (preferences:get 'updates:enabled?))))))
        (thread check-for-updates))
      (when (> patchlevel 0) (version:add-spec 'p patchlevel)))))
