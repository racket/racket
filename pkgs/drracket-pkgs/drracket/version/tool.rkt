#lang racket/gui

(require racket/unit racket/class framework drracket/tool
         browser/external string-constants
         version/patchlevel
         version/check)

(define download-url "http://download.racket-lang.org/")

;; either 'yes, 'no, or something else, see `enabled?' below for a reason
(preferences:set-default 'updates:enabled? 'unset symbol?)
(preferences:set-default 'updates:last 0 integer?)
;; how often do we check; default: check every week
(preferences:set-default 'updates:frequency (* 60 60 24 7) integer?)
;; time to wait if user chooses "later"; default: in two weeks
(define later-delay (* 60 60 24 14))

;; This is used to check if updates:enabled? is true or false.  The problem is
;; that we don't want to set a default of #t or #f, so make it 'unset and
;; change it only when users explicitly set it.  This makes it possible to have
;; the default be #f, but without making it always #f for all users, and in the
;; future it is possible to change it to default to #t.
(define (is-enabled? v)
  (case v [(yes) #t] [(no) #f] [else #f])) ; default to #f

(define (check-for-updates . top?)
  (define enabled?  (is-enabled? (preferences:get 'updates:enabled?)))
  (define explicit? (pair? top?)) ; top => explicit check for updates
  (define top       (and (pair? top?) (car top?)))
  ;; wait until the definitions are instantiated, return top-level window
  (define (wait-for-definitions)
    (define ws (get-top-level-windows))
    (if (null? ws)
      (begin (sleep 1) (wait-for-definitions))
      (car ws)))
  #| ;; Cute code, but may resize the window if too much space, and people
     ;; didn't like this way of asking if you want update checks.
  ;; show a message and a disable button
  (define hide-message void) ; set by show-message
  (define (show-message first-time?)
    ;; No info display if we got some non-drscheme window by accident
    (cond
      [(with-handlers ([void (λ _ #f)]) (send top get-info-panel)) =>
       (λ (info)
         (sleep 3) ; wait to make this appearance visible
         (define -check "Checking for updates...")
         (define -about "About to auto-check for updates, you can")
         (define p (make-object horizontal-panel% info))
         (define m (make-object message% (if first-time? -about -check) p))
         (define b (make-object button% "Disable" p disable))
         (send info change-children (λ (l) (cons p (remq p l))))
         (when first-time?
           (define m1 (make-object message% "these checks" p))
           (sleep 20)
           (send p change-children (λ (l) (remq m1 l)))
           (send m set-label -check))
         (sleep 2) ; wait before and after check to make it visible
         (set! hide-message
               (λ ([now? #f])
                 (unless now? (sleep 1))
                 (send info change-children (λ (l) (remq p l)))
                 (set! hide-message void)))
         #t)] ; return #t so that the check starts
     [else #f])) ; no standard window -- return #f to skip the whole thing
  |#
  ;; show results in a dialog in a non-modal dialog (if it was not an
  ;; explicit call) , so the window can be left around as a reminder.
  (define (message style fmt . args)
    (define (run)
      (define-values [result new-enabled?]
        (message+check-box/custom
         (string-constant version:results-title)
         (apply format fmt args)
         (string-constant version:do-periodic-checks)
         (string-constant ok)
         (and (eq? 'newer style) (string-constant version:take-me-there))
         #f
         (and explicit? top)
         `(,@(case style
               [(#f) '()] [(newer) '(stop)] [else (list style)])
           ,@(if enabled? '(checked) '())
           default=1)))
      (unless (eq? enabled? new-enabled?)
        (preferences:set 'updates:enabled? (if new-enabled? 'yes 'no))
        (set! enabled? new-enabled?))
      result)
    (if explicit?
      (run)
      ;; non-modal
      (parameterize ([current-eventspace (make-eventspace)]) (run))))
  ;; main checker
  (define (check)
    (define result #f)
    ;; run the check in a thread, with a chance to abort it
    (let ([d #f])
      (define t (thread (λ () (set! result (check-version))
                              (when d (send d show #f)))))
      (unless (sync/timeout .4 t) ; still checking, pop message
        (when explicit?           ; unless it's an automatic check
          (queue-callback
           (λ ()
             (set! d (new (class dialog%
                            (super-new
                              [label (string-constant version:update-check)]
                              [parent #f])
                            (make-object message%
                              (string-constant version:connecting-server)
                              this)
                            (make-object button%
                              (string-constant abort) this
                              (λ (b e) (kill-thread t) (send this show #f))
                              '(border))
                            (send this center))))
             (send d show #t)))
          (sleep/yield .5))
        (thread-wait t)))
    (cond
      [(and (pair? result) (eq? 'newer (car result)))
       (when (equal? 2 (message 'newer "Racket v~a ~a ~a"
                                (cadr result)
                                (string-constant version:now-available-at)
                                download-url))
         ;; 2 = go there
         (send-url download-url)
         ;; (sleep 1) ((application-quit-handler))
         )]
      ;; implicit auto-check => show a message only if there is a newer
      ;; version => the rest are only for explicit calls
      [(not explicit?) (void)]
      [(eq? result 'ok)
       (message #f (string-constant version:plt-up-to-date))]
      [(not (pair? result)) (void)] ; either #f (canceled) or ok
      [else (case (car result)
              [(error)
               (message 'stop "~a: ~a~a"
                        (string-constant error) (cadr result)
                        (if (pair? (cddr result))
                          (string-append "\n" (caddr result)) ""))]
              [(ok-but)
               (message 'caution "~a,\n~a (v~a)"
                        (string-constant version:plt-up-to-date)
                        (string-constant version:but-newer-alpha)
                        (cadr result))]
              [else (error 'check-for-updates "internal error")])]))
  ;; start the check if enabled and enough time passed
  (when (or explicit? enabled?)
    (unless top (set! top (wait-for-definitions)))
    (let ([cur  (current-seconds)]
          [last (preferences:get 'updates:last)]
          [freq (preferences:get 'updates:frequency)])
      (when (or explicit? (> (- cur last) freq))
        (preferences:set 'updates:last cur)
        (check)))))

(provide tool@)
(define tool@
  (unit (import drscheme:tool^) (export drscheme:tool-exports^)

    (define (phase1) (void))
    (define (phase2)
      (preferences:add-to-warnings-checkbox-panel
       (λ (panel)
         (define b
           (make-object check-box%
             (string-constant version:do-periodic-checks)
             panel
             (λ (b e) (preferences:set 'updates:enabled?
                                       (if (send b get-value) 'yes 'no)))))
         (preferences:add-callback
          'updates:enabled?
          (λ (p v) (send b set-value (is-enabled? v))))
         (send b set-value
               (is-enabled? (preferences:get 'updates:enabled?)))))
      (drscheme:get/extend:extend-unit-frame
       (λ (f%)
         (class f%
           (define/override (help-menu:after-about m)
             (make-object menu-item%
               (string-constant version:update-menu-item) m
               (λ (b e) (check-for-updates this)))
             (super help-menu:after-about m))
           (super-new))))
      (thread check-for-updates))
    (when (> patchlevel 0) (version:add-spec 'p patchlevel))))
