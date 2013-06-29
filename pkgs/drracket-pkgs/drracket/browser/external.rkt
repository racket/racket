#lang racket/base
  (require string-constants
           racket/gui
           racket/class
           racket/file
           racket/list
           racket/match
           (prefix-in raw: net/sendurl)
           net/url
           (prefix-in fw: framework))
  (provide send-url
           (rename-out [raw:browser-preference? browser-preference?])
           update-browser-preference
           install-help-browser-preference-panel
           add-to-browser-prefs-panel)

  ; : -> bool
  (define (unix-browser?)
    (eq? (system-type) 'unix))

  (fw:preferences:set-default
   'external-browser
   (let ([pref (get-preference 'external-browser 
                               (lambda () #f)
                               #:timeout-lock-there (lambda (path) #f))])
     (and (raw:browser-preference? pref) pref))
   raw:browser-preference?)

  (define http-proxy-preference 'plt:http-proxy)

  ;; proxy-pref? : any -> boolean
  ;; determines if the input is a valid setting for the
  ;; http-proxy-preference pref
  (define (proxy-pref? x)
    (match x
      [#f #t]
      [`("http" ,(? string?) ,(? integer?)) #t]
      [else #f]))

  ;; sync-current-proxy-servers : proxy-pref -> void
  ;; syncs current-proxy-servers parameter with the proxy-pref-val
  (define (sync-current-proxy-servers pref-val)
    (let* ([ops (current-proxy-servers)]
           [removed (remove-all-proxies "http" ops)])
      (current-proxy-servers
       (if pref-val (cons pref-val removed) removed))))

  (define (remove-all-proxies scheme proxies)
    (filter (lambda (x) (and (pair? x) (not (equal? (car x) scheme))))
            proxies))

  (fw:preferences:set-default http-proxy-preference #f proxy-pref?)
  (sync-current-proxy-servers (fw:preferences:get http-proxy-preference))
  (void (fw:preferences:add-callback http-proxy-preference (lambda (p v) (sync-current-proxy-servers v))))

  (define send-url
    (if (unix-browser?)
      (lambda (url . args)
        (when (or (get-preference 'external-browser (lambda () #f))
                  ;; either the preference doesn't exist or is #f
                  (update-browser-preference url))
          (apply raw:send-url url args)))
      raw:send-url))

  ; : str -> void
  ; to prompt the user for a browser preference and update the preference
  (define (update-browser-preference url)
    (or (not (unix-browser?))
        (choose-browser url)))

  ; : (U symbol #f) -> void
  ; to set the default browser
  (define (set-browser! browser)
    (fw:preferences:set 'external-browser browser))

  ;; Tries to put low-level prefs three times, sleeping a bit in
  ;; between, then gives up.
  (define (try-put-preferences names vals)
    (let loop ([tries 0])
      (unless (= tries 3)
        (put-preferences names vals
                         (lambda (lock-file)
                           (sleep 0.2)
                           (loop (add1 tries)))))))

  (define unix-browser-names
    (map (lambda (s)
           (string-titlecase (regexp-replace* #rx"-" (symbol->string s) " ")))
         raw:unix-browser-list))

  ;; : (U str #f) -> (U symbol #f)
  ;; to prompt the user for a browser preference
  ;;  #f for the URL indicates a pre-emptive request by Help Desk,
  ;;  and in that case, the user can choose to use the internal
  ;;  broswer.
  (define (choose-browser url)
    (let* ([title (string-constant choose-browser)]
           [d (make-object dialog% title)]
           [main-pane (make-object vertical-pane% d)]
           [internal-ok? (not url)]
           [ok? #f]
           [orig-external (fw:preferences:get 'external-browser)])
      (make-object message% title main-pane)
      ;; No need to show the URL (it can be very long)
      ;; (when url
      ;;   (make-object message% (format "URL: ~a" url) main-pane))
      (let-values ([(panel callbacks) (make-help-browser-preference-panel internal-ok? #f (lambda (f) (f main-pane)))])
        (let*-values ([(button-pane) (instantiate horizontal-panel% (main-pane)
                                                  (alignment '(right center)))]
                      [(ok-button cancel-button)
                       (fw:gui-utils:ok/cancel-buttons
                        button-pane
                        (lambda (b e) (set! ok? #t) (send d show #f))
                        (lambda (b e)
                          (fw:preferences:set 'external-browser orig-external)
                          (send d show #f)))]
                      [(enable-button) (lambda (_n _v)
                                         (queue-callback
                                          (lambda ()
                                            (send ok-button enable (fw:preferences:get 'external-browser)))))])
          (send ok-button enable #f)
          (set! callbacks
                (cons
                 (fw:preferences:add-callback 'external-browser enable-button)
                 callbacks)))
        (send d show #t)
        (map (lambda (f) (f)) callbacks)
        ok?)))

  (define panel-installed? #f)
  (define prefs-panel #f)
  (define synchronized? #f)
  (define additions null)

  (define (install-help-browser-preference-panel)
    (unless panel-installed?
      (set! panel-installed? #t)
      (make-help-browser-preference-panel
       #t #t
       (lambda (f) (fw:preferences:add-panel
                    (string-constant browser)
                    (lambda (parent)
                      (let-values ([(panel cbs) (f parent)])
                        (set! prefs-panel panel)
                        (map (lambda (f) (f panel)) additions)
                        (set! additions null)
                        panel)))))))

  (define (add-to-browser-prefs-panel proc)
    (if prefs-panel
      (proc prefs-panel)
      (set! additions (append additions (list proc)))))

  (define (make-help-browser-preference-panel set-help? ask-later? mk)
    (mk
     (lambda (parent)
       (define callbacks null)
       (let ([pref-panel (instantiate vertical-panel% ()
                                      [parent parent]
                                      [alignment '(left center)])])

         ;; -------------------- external browser for Unix --------------------
         (when (unix-browser?)
           (unless synchronized?
             ;; Keep 'external-browser in sync
             (fw:preferences:add-callback 'external-browser
               (lambda (name browser)
                 (try-put-preferences (list 'external-browser) (list browser)))))

           (letrec ([v-panel (instantiate group-box-panel% ()
                                          (parent pref-panel)
                                          (alignment '(right center))
                                          (stretchable-height #f)
                                          (label (string-constant external-browser-choice-title)))]
                    [h-panel (instantiate horizontal-panel% ()
                                          (parent v-panel)
                                          (alignment '(center bottom)))]
                    [none-index (length raw:unix-browser-list)]
                    [custom-index (add1 none-index)]
                    [r (instantiate radio-box% ()
                                    (label #f)
                                    (choices (append unix-browser-names
                                                     (list (string-constant no-browser)
                                                           (string-constant browser-command-line-label))))
                                    (parent h-panel)
                                    (callback
                                     (lambda (radio event)
                                       (let ([n (send radio get-selection)])
                                         (set-browser!
                                          (cond
                                            [(= n none-index) #f]
                                            [(= n custom-index) (get-custom)]
                                            [else (list-ref raw:unix-browser-list n)]))))))]
                    [select-custom
                     (lambda (_ __)
                       (send r set-selection custom-index)
                       (set-browser! (get-custom)))]
                    [get-custom
                     (lambda () (cons (send pre get-value) (send post get-value)))]
                    [template-panel (instantiate horizontal-panel% (h-panel)
                                                 (spacing 0)
                                                 (stretchable-height #f))]
                    [pre (instantiate text-field% ()
                                      (label #f) (parent template-panel) (callback select-custom)
                                      (horiz-margin 0))]
                    [mess (instantiate message% () (label "<URL>") (parent template-panel)
                                       (horiz-margin 0))]
                    [post (instantiate text-field% ()
                                       (label #f) (parent template-panel) (callback select-custom)
                                       (horiz-margin 0))]
                    [note1 (instantiate message% ((string-constant browser-cmdline-expl-line-1)
                                                  v-panel))]
                    [note2 (instantiate message% ((string-constant browser-cmdline-expl-line-2)
                                                  v-panel))]
                    [refresh-controls (lambda (pref)
                                        (if (pair? pref)
                                          (begin
                                            (send r set-selection custom-index)
                                            (send pre set-value (car pref))
                                            (send post set-value (cdr pref)))
                                          (let init ([x raw:unix-browser-list] [n 0])
                                            (cond
                                              [(null? x) (send r set-selection n)]
                                              [else (if (eq? pref (car x))
                                                      (send r set-selection n)
                                                      (init (cdr x) (add1 n)))]))))])

             (unless ask-later?
               (send r enable none-index #f))

             (refresh-controls (fw:preferences:get 'external-browser))
             (set! callbacks
                   (cons (fw:preferences:add-callback 'external-browser
                           (lambda (name browser) (refresh-controls browser)))
                    callbacks))))

         ;; -------------------- proxy for doc downloads --------------------
         (when set-help?
           (letrec ([p (instantiate group-box-panel% ()
                                    [label (string-constant http-proxy)]
                                    [parent pref-panel]
                                    [stretchable-height #f]
                                    [alignment '(left top)])]
                    [rb (make-object radio-box%
                                     #f (list (string-constant proxy-direct-connection)
                                              (string-constant proxy-use-proxy))
                                     p
                                     (lambda (r e)
                                       (let ([proxy? (= 1 (send r get-selection))])
                                         (send proxy-spec enable proxy?)
                                         (if proxy?
                                             (update-proxy)
                                             (fw:preferences:set http-proxy-preference #f)))))]
                    [proxy-spec (instantiate horizontal-panel% (p)
                                             [stretchable-width #f]
                                             [stretchable-height #f]
                                             [alignment '(left center)])]
                    [update-proxy (lambda ()
                                    (let ([host (send host get-value)]
                                          [port (send port get-value)])
                                      (let ([ok? (and (regexp-match? #rx"^[-0-9a-zA-Z.]+$" host)
                                                      (regexp-match? #rx"^[0-9]+$" port)
                                                      (string->number port)
                                                      (<= 1 (string->number port) 65535))])
                                        (when ok?
                                          (fw:preferences:set
                                           http-proxy-preference
                                           (list "http" host (string->number port))))
                                        (send bad-host show (not ok?)))))]
                    [host (make-object text-field%
                                       (string-constant proxy-host)
                                       proxy-spec (lambda (x y) (update-proxy))
                                       "www.someplacethatisaproxy.domain.com")]
                    [port (make-object text-field%
                                       (string-constant proxy-port)
                                       proxy-spec (lambda (x y) (update-proxy)) "65535")]
                    [bad-host (make-object message%
                                           (string-constant proxy-bad-host)
                                           p)]
                    [update-gui
                     (lambda (proxy-val)
                       (send bad-host show #f)
                       (if proxy-val
                           (begin
                             (send rb set-selection 1)
                             (send proxy-spec enable #t)
                             (unless (string=? (cadr proxy-val) (send host get-value))
                               (send host set-value (cadr proxy-val)))
                             (unless (equal? (caddr proxy-val) (string->number (send port get-value)))
                               (send port set-value (number->string (caddr proxy-val)))))
                           (begin
                             (send rb set-selection 0)
                             (send proxy-spec enable #f)
                             (send host set-value "")
                             (send port set-value ""))))])

             (fw:preferences:add-callback http-proxy-preference
                                          (lambda (name val)
                                            (update-gui val)))
             (update-gui (fw:preferences:get http-proxy-preference))
             (send bad-host show #f)))

         (set! synchronized? #t)
         (values pref-panel callbacks)))))
