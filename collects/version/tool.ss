(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "unitsig.ss")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           "patchlevel.ss" "check.ss"
           (lib "external.ss" "browser"))

  ;; either 'yes, 'no, or something else, see `enabled?' below for a reason
  (preferences:set-default 'updates:enabled? 'unset symbol?)
  (preferences:set-default 'updates:last 0 integer?)
  ;; how often do we check; default: check every three days
  (preferences:set-default 'updates:frequency (* 60 60 24 3) integer?)
  ;; time to wait if user chooses "later"; default: in a week
  (define later-delay (* 60 60 24 3))

  ;; This is used to check if updates:enabled? is true or false.  The problem
  ;; is that we don't want to set a default of #t or #f, so make it 'unset and
  ;; change it only when users explicitly set it.  This makes it possible to
  ;; have the default be #f, but without making it always #f for all users, and
  ;; in the future it is possible to change it to default to a different
  ;; default.
  (define (enabled? v)
    (case v [(yes) #t] [(no) #f] [else #f])) ; default to #f

  (define (check-for-updates)
    ;; wait until the definitions are instantiated, return top-level window
    (define (wait-for-definitions)
      (let ([ws (get-top-level-windows)])
        (if (null? ws)
          (begin (sleep 1) (wait-for-definitions))
          (car ws))))
    (define (check top)
      (let ([r (check-version)])
        ;; do nothing if we have a good version, if there was an error, or if
        ;; there is a suggested alpha -- only show a message if there is a
        ;; newer version
        (when (and (pair? r) (eq? 'newer (car r)))
          (case (message-box/custom
                 "Outdated PLT Version"
                 (string-append "PLT Scheme v"(cadr r)"is now available")
                 "Quit && &Take Me There" "Remind Me &Later" "&Stop Checking"
                 top '(default=2) #f)
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
            ))))
    (when (enabled? (preferences:get 'updates:enabled?))
      (let ([cur  (current-seconds)]
            [last (preferences:get 'updates:last)]
            [freq (preferences:get 'updates:frequency)])
        (when (> (- cur last) freq)
          (preferences:set 'updates:last cur)
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
                      "Check for newer PLT Scheme versions"
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
