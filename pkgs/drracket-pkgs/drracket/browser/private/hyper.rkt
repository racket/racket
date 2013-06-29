#| 
A test case:

#lang racket
   (require racket/gui browser framework)
   
   (define f%
     (frame:status-line-mixin 
      frame:basic%))
   
   (define f (new f%
                  (label "My Frame")
                  (width 400)
                  (height 300)))
   (define browser (new hyper-panel% 
                        (info-line? #f)
                        (parent (send f get-area-container))))
   (send f show #t)
   
   (send (send browser get-canvas)
	 goto-url
	 ;; The starting URL:
	 "http://www.htdp.org/";
	 ;; #f means not a relative URL:
	 #f)
|#

#lang racket/unit

(require racket/class
         (only-in racket/list last-pair)
         "sig.rkt"
         racket/path
         racket/file
         net/url-sig
         net/url-structs
         net/head
         mred/mred-sig
         framework
         string-constants
         setup/plt-installer-sig)

  (import html^
        mred^
        setup:plt-installer^
        url^)
  (export hyper^)
  (init-depend mred^)

(struct exn:file-saved-instead exn (pathname))
(struct exn:cancelled exn ())
(struct exn:tcp-problem exn ())

(define history-limit 20)

(struct hyperlink (anchor-start anchor-end url-string))

(struct hypertag (name position))

(define (same-page-url? a b)
  (or (eq? a b)
      (and (url? a) (url? b)
           ;; fragment can be different
           (equal? (url-scheme a) (url-scheme b))
           (equal? (url-host a) (url-host b))
           
           ;; assume that url-paths are all strings 
           ;; (other wise the pages are treated as different)
           (equal? (map path/param-path (url-path a)) 
                   (map path/param-path (url-path b)))
           
           (equal? (url-query a) (url-query b)))))


(define hyper-text<%>
  (interface ()
    init-browser-status-line
    update-browser-status-line
    close-browser-status-line
    url-allows-evaling?))

(define hyper-text-mixin
  (mixin ((class->interface text%) editor:keymap<%>) (hyper-text<%>)
         (inherit begin-edit-sequence end-edit-sequence lock erase clear-undos
                  change-style
                  set-modified auto-wrap
                  find-snip get-snip-position set-clickback get-canvas
                  insert last-position hide-caret
                  get-end-position set-autowrap-bitmap)
         
         (init-field url top-level-window)
         (init progress)
         (init-field [post-data #f])
         
         (define/pubment (url-allows-evaling? url)
           (cond
            [(port? url) #f]
            [(and (url? url)
                  (equal? "file" (url-scheme url)))
             (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
               (path-below?
                (normal-case-path (normalize-path (build-path (collection-path "racket") 
                                                              'up
                                                              'up)))
                (normal-case-path (normalize-path (apply build-path 
                                                         (map path/param-path (url-path url)))))))]
            [else (inner #f url-allows-evaling? url)]))
         
         (define doc-notes null)
         (define title #f)
         (define htmling? #f)
         (define redirection #f)
         (define hypertags-list (list (hypertag "top" 0)))
         (define hyper-delta (make-object style-delta% 'change-underline #t))
         (let ([mult (send hyper-delta get-foreground-mult)]
               [add (send hyper-delta get-foreground-add)])
           (send mult set 0 0 0)
           (send add set 0 0 255))
         
         (define/override (get-keymaps) (list* space-page-keymap hyper-keymap (super get-keymaps)))
         (define/public (get-hyper-keymap) hyper-keymap)
         
         (define/augment (after-set-position)
           (unless (zero? (get-end-position))
             (hide-caret #f))
           (inner (void) after-set-position))          

         ;; get-redirection : -> (union false? url?)
         ;; #f indicates no redirection, url is where it redirects to
         (define/public (get-redirection) redirection)
         
         (define/public (add-document-note note)
           (set! doc-notes (append doc-notes (list note))))
         (define/public (get-document-notes) doc-notes)
         
         (define/public (make-link-style start end) (change-style hyper-delta start end))
         (define/public (get-url) (and (url? url) url))
         
         (define/public post-url
           (lambda (url-string [post-data #f])
             (on-url-click
              (lambda (url-string post-data)
                (send (get-canvas) goto-url url-string (get-url) void post-data))
              url-string
              post-data)))
         
         (define/public (on-url-click f x post-data) 
           (let ([c (get-canvas)])
             (if c
                 (send c on-url-click f x post-data)
                 (f x post-data))))
         (define/public (get-title) (or title (and (url? url) (url->string url))))
         (define/public (set-title t) (set! title t))
         
         (define/public (add-tag name pos)
           (for-each (lambda (tag) 
                       (when (string=? name (hypertag-name tag))
                         (remove-tag  name)))
                     hypertags-list)
           (let ([new-tag (hypertag name pos)])
             (set! hypertags-list
                   (let insert-loop ([tags-left hypertags-list])
                     (cond [(null? tags-left)(cons new-tag '())]
                           [(> pos (hypertag-position (car tags-left)))
                            (cons new-tag tags-left)]
                           [else (cons (car tags-left)
                                       (insert-loop (cdr tags-left)))])))))
         
         (define/public (find-tag name)
           (if (and (integer? name) (positive? name))
               name
               (and (string? name)
                    (ormap (lambda (x) 
                             (and (string=? name (hypertag-name x)) 
                                  (hypertag-position x)))
                           hypertags-list))))
         (define/public (remove-tag name)
           (set! hypertags-list
                 (filter (lambda (x) (not (string=? name (hypertag-name x))))
                         hypertags-list)))
         (define/public (add-link start end url-string)
           (let* ([new-link (hyperlink start end url-string)])
             (set-clickback start end 
                            (lambda (x y z)
                              (post-url url-string)))))
         
         ;; remember the directory when the callback is added (during parsing)
         ;; and restore it during the evaluation of the callback.
         (define/public (add-racket-callback start end racket-string)
           (let ([dir (current-load-relative-directory)])
             (set-clickback 
              start end 
              (lambda (edit start end)
                (if (url-allows-evaling? url)
                    (parameterize ([current-load-relative-directory dir])
                      (eval-racket-string racket-string))
                    (message-box (string-constant help-desk)
                                 "<A RACKET= ...> disabled"))))))
         (define/public (add-thunk-callback start end thunk)
           (set-clickback 
            start end 
            (lambda (edit start end)
              (thunk))))
         
         (define/public (eval-racket-string s)
           (let ([v 
                  (dynamic-wind
                      begin-busy-cursor
                      (lambda () 
                        (with-handlers ([exn:fail? (build-html-error-message s)])
                          (eval (read (open-input-string s)))))
                      end-busy-cursor)])
             (when (string? v)
               (send (get-canvas) goto-url
                     (open-input-string v)
                     (get-url)))))

         (define/public (init-browser-status-line top-level-window)
           (send top-level-window open-status-line 'browser:hyper.rkt))
         (define/public (update-browser-status-line top-level-window s)
           (send top-level-window update-status-line 'browser:hyper.rkt s))
         (define/public (close-browser-status-line top-level-window)
           (send top-level-window close-status-line 'browser:hyper.rkt))

         (define/public reload
           ;; The reload function is called in a non-main thread,
           ;;  since this class is instantiated in a non-main thread.
           (lambda ([progress void])
             (when url
               (let ([s (make-semaphore)]
                     [closer-t #f]
                     [this-t (current-thread)])
                 (when top-level-window
                   (queue-callback
                    (lambda ()
                      (init-browser-status-line top-level-window)
                      (update-browser-status-line 
                       top-level-window 
                       (format "Visiting ~a"
                               (cond
                                [(url? url) (url->string url)]
                                [else "page"])))
                      ;; Yikes! We need to ensure that the browser status
                      ;;  line is closed, even if the reload thread dies.
                      ;; We use the usual trick of setting up a watcher
                      ;;  thread and then killing it off if its work
                      ;;  is not needed.
                      (set! closer-t 
                            (thread (lambda ()
                                      (sync (thread-dead-evt this-t))
                                      (queue-callback
                                       (lambda ()
                                         (close-browser-status-line top-level-window))))))
                      (semaphore-post s)))
                   (yield s))
                 (let ([headers (get-headers/read-from-port progress)])
                   ;; Page is a redirection?
                   (let ([m (regexp-match "^HTTP/[^ ]+ 30[12] " headers)])
                     (when m
                       (let ([loc (extract-field "location" headers)])
                         (when loc
                           (set! redirection 
                                 (cond
                                  [(url? url)
                                   (combine-url/relative url loc)]
                                  [else
                                   (string->url loc)])))))))
                 (when top-level-window
                   (queue-callback
                    (lambda ()
                      (kill-thread closer-t)
                      (close-browser-status-line top-level-window)
                      (semaphore-post s)))
                   (yield s))))))
         
         (define/private (get-headers/read-from-port progress)
           (cond
            [(port? url) 
             (read-from-port url empty-header progress)
             empty-header]
            [else
             (let* ([busy? #t]
                    [stop-busy (lambda ()
                                 (when busy?
                                   (set! busy? #f)))])
               (with-handlers ([(lambda (x) (and (exn:fail? x) busy?))
                                (lambda (x) 
                                  (call/input-url 
                                   url
                                   (if post-data 
                                       (case-lambda
                                        [(u) (post-pure-port u post-data)]
                                        [(u s) (post-pure-port u post-data s)])
                                       get-pure-port)
                                   (lambda (p)
                                     (stop-busy)
                                     (read-from-port p empty-header progress)
                                     empty-header)))])
                 (call/input-url 
                  url 
                  (if post-data 
                      (case-lambda
                       [(u) (post-impure-port u post-data)]
                       [(u s) (post-impure-port u post-data s)])
                      get-impure-port)
                  (lambda (p)
                    (let ([headers (purify-port p)])
                      (stop-busy)
                      (read-from-port p headers progress)
                      headers)))))]))
         
         (define/private (read-from-port p mime-headers progress)
           (let ([wrapping-on? #t])
             (dynamic-wind
                 (lambda ()
                   (lock #f)
                   (begin-edit-sequence #f)
                   (set! htmling? #t))
                 (lambda ()
                   (erase)
                   (clear-undos)
                   (let* ([mime-type (extract-field "content-type" mime-headers)]
                          [path-extension (and (not mime-type)
                                               (url? url)
                                               (let ([p (url-path url)])
                                                 (and (not (null? p))
                                                      (regexp-match #rx"[.][^.]*$"
                                                                    (path/param-path (car (last-pair p)))))))]
                          [html? (or (and mime-type (regexp-match #rx"text/html" mime-type))
                                     (member path-extension '(".html" ".htm")))]
                          [text? (or (and mime-type (regexp-match #rx"text/plain" mime-type))
                                     (member path-extension '(".txt"))
                                     (and (url? url)
                                          (equal? (url-scheme url) "file")))])
                     (cond
                      [(or (and mime-type (regexp-match #rx"application/" mime-type))
                           (and (url? url)
                                (not (null? (url-path url)))
                                (not text?)
                                        ; document-not-found produces HTML:
                                (not html?)))
                                        ; Save the file
                       (progress #f)
                       (let* ([orig-name (and (url? url)
                                              (let ([p (url-path url)])
                                                (and (not (null? p))
                                                     (let ([lp (path/param-path (car (last-pair p)))])
                                                       (and (not (string=? "" lp))
                                                            lp)))))]
                              [size (let ([s (extract-field "content-length" mime-headers)])
                                      (and s (let ([m (regexp-match #rx"[0-9]+" s)])
                                               (and m (string->number (car m))))))]
                              [install?
                               (and (and orig-name (regexp-match #rx"[.]plt$" orig-name))
                                    (let ([d (make-object dialog% (string-constant install?))]
                                          [d? #f]
                                          [i? #f])
                                      (make-object message%
                                                   (string-constant you-have-selected-an-installable-package)
                                                   d)
                                      (make-object message% 
                                                   (string-constant do-you-want-to-install-it?) d)
                                      (when size
                                        (make-object message%
                                                     (format (string-constant paren-file-size) size) d))
                                      (let ([hp (make-object horizontal-panel% d)])
                                        (send hp set-alignment 'center 'center)
                                        (send (make-object button% 
                                                           (string-constant download-and-install)
                                                           hp
                                                           (lambda (b e)
                                                             (set! i? #t)
                                                             (send d show #f))
                                                           '(border))
                                              focus)
                                        (make-object button% (string-constant download) hp
                                                     (lambda (b e)
                                                       (set! d? #t)
                                                       (send d show #f)))
                                        (make-object button% (string-constant cancel) hp
                                                     (lambda (b e)
                                                       (send d show #f))))
                                      (send d center)
                                      (send d show #t)
                                      (unless (or d? i?)
                                        (raise (exn:cancelled
                                                "Package Cancelled"
                                                (current-continuation-marks))))
                                      i?))]
                              [tmp-plt-filename
                               (if install?
                                   (make-temporary-file "tmp~a.rkt")
                                   (put-file 
                                    (if size
                                        (format 
                                         (string-constant save-downloaded-file/size)
                                         size)
                                        (string-constant save-downloaded-file))
                                    #f ; should be calling window!
                                    #f
                                    orig-name))])
                         (when tmp-plt-filename
                           (let* ([d (make-object dialog% (string-constant downloading) top-level-window)]
                                  [message (make-object message% 
                                                        (string-constant downloading-file...)
                                                        d)]
                                  [gauge (if size
                                             (make-object gauge% #f 100 d)
                                             #f)]
                                  [exn #f]
                                        ; Semaphores to avoid race conditions:
                                  [wait-to-start (make-semaphore 0)]
                                  [wait-to-break (make-semaphore 0)]
                                        ; Thread to perform the download:
                                  [t (thread
                                      (lambda ()
                                        (semaphore-wait wait-to-start)
                                        (with-handlers ([void
                                                         (lambda (x)
                                                           (when (not (exn:break? x))
                                                             (set! exn x)))])
                                          (semaphore-post wait-to-break)
                                          (with-output-to-file tmp-plt-filename
                                            (lambda ()
                                              (let loop ([total 0])
                                                (when gauge
                                                  (send gauge set-value 
                                                        (inexact->exact
                                                         (floor (* 100 (/ total size))))))
                                                (let ([bts (read-bytes 1024 p)])
                                                  (unless (eof-object? bts)
                                                    (write-bytes bts)
                                                    (loop (+ total (bytes-length bts)))))))
                                            'binary 'truncate))
                                        (send d show #f)))])
                             (send d center)
                             (make-object button% (string-constant &stop)
                                          d (lambda (b e)
                                              (semaphore-wait wait-to-break)
                                              (set! tmp-plt-filename #f)
                                              (send d show #f)
                                              (break-thread t)))
                                        ; Let thread run only after the dialog is shown
                             (queue-callback (lambda () (semaphore-post wait-to-start)))
                             (send d show #t)
                             (when exn 
                               (raise (exn:tcp-problem (exn-message exn) (current-continuation-marks)))))
                           (let ([sema (make-semaphore 0)])
                             (when (and tmp-plt-filename install?)
                               (run-installer tmp-plt-filename
                                              (lambda ()
                                                (semaphore-post sema)))
                               (yield sema))))
                         (raise
                          (if tmp-plt-filename
                              (exn:file-saved-instead
                               (if install?
                                   (string-constant package-was-installed)
                                   (string-constant download-was-saved))
                               (current-continuation-marks)
                               tmp-plt-filename)
                              (exn:cancelled "The download was cancelled."
                                                  (current-continuation-marks)))))]
                      [(or (and (url? url)
                                (not (null? (url-path url)))
                                (regexp-match #rx"[.]html?$"
                                              (path/param-path (car (last-pair (url-path url))))))
                           (port? url)
                           html?)
                                        ; HTML
                       (progress #t)
                       (let* ([directory
                               (or (if (and (url? url)
                                            (string=? "file" (url-scheme url)))
                                       (let ([path (apply build-path (map path/param-path (url-path url)))])
                                         (let-values ([(base name dir?) (split-path path)])
                                           (if (string? base)
                                               base
                                               #f)))
                                       #f)
                                   (current-load-relative-directory))])
                         (parameterize ([html-status-handler 
                                         (lambda (s)
                                           (when top-level-window
                                             (let ([t (current-thread)]
                                                   [sema (make-semaphore)])
                                               (queue-callback
                                                (lambda ()
                                                  (when (thread-running? t)
                                                    ;; Since t is running, the status line hasn't been
                                                    ;;  closed by the watcher thread (and there's no
                                                    ;;  race, because it can only be closed in the
                                                    ;;  handler thread)
                                                    (update-browser-status-line top-level-window s))
                                                  (semaphore-post sema)))
                                               (yield sema))))]
                                        [current-load-relative-directory directory]
                                        [html-eval-ok (url-allows-evaling? url)])
                           (html-convert p this)))]
                      [else
                                        ; Text
                       (progress #t)
                       (begin-edit-sequence)
                       (let loop ()
                         (let ([r (read-line p 'any)])
                           (unless (eof-object? r)
                             (insert r)
                             (insert #\newline)
                             (loop))))
                       (change-style (make-object style-delta% 'change-family 'modern)
                                     0 (last-position))
                       (set! wrapping-on? #f)
                       (end-edit-sequence)])))
                 (lambda ()
                   (end-edit-sequence)
                   (set! htmling? #f)
                   (set-modified #f)
                   (auto-wrap wrapping-on?)
                   (set-autowrap-bitmap #f)
                   (lock #t)
                   (close-input-port p)))))
         
         (inherit find-position get-snip-location
                  get-dc get-between-threshold
                  editor-location-to-dc-location
                  dc-location-to-editor-location)
         ;; use on-event rather than on-default-event since we want
         ;; to override the caret handling snip in the case that
         ;; an image-map-snip is there.
         (define/override (on-event event)
           (let* ([edge-close-b (box 0)]
                  [on-it-b (box #f)]
                  [dc-event-x (send event get-x)]
                  [dc-event-y (send event get-y)])
             (let-values ([(editor-event-x editor-event-y) 
                           (dc-location-to-editor-location dc-event-x dc-event-y)])
               (let ([pos (find-position editor-event-x editor-event-y #f on-it-b edge-close-b)])
                 (cond
                  [(and (unbox on-it-b)
                        (not (<= (abs (unbox edge-close-b))
                                 (get-between-threshold))))
                   (let ([snip (find-snip pos 'after-or-none)])
                     (cond
                      [(and snip (is-a? snip image-map-snip%))
                       (let ([bsnip-left (box 0)]
                             [bsnip-top (box 0)]
                             [bsnip-right (box 0)]
                             [bsnip-bot (box 0)])
                         (get-snip-location snip bsnip-left bsnip-top #f)
                         (get-snip-location snip bsnip-right bsnip-bot #t)
                         (let ([snip-left (unbox bsnip-left)]
                               [snip-top (unbox bsnip-top)]
                               [snip-right (unbox bsnip-right)]
                               [snip-bot (unbox bsnip-bot)])
                           (cond
                            [(and (<= snip-left editor-event-x snip-right)
                                  (<= snip-top editor-event-y snip-bot))
                             (let-values ([(x y) (editor-location-to-dc-location snip-left snip-top)])
                               (send snip on-event (get-dc) x y snip-left snip-top event))]
                            [else (super on-event event)])))]
                      [else (super on-event event)]))]
                  [else (super on-event event)])))))
         
         (super-new)
         
         ;; load url, but the user might break:
         (with-handlers ([exn:break? void])
                                        ;(printf "url: ~a\n" (if (url? url) (url->string url) url)) ;; handy for debugging help desk
           (reload progress))))

;; build-html-error-message : exn -> string[html]
(define ((build-html-error-message str) exn)
  (string-append
   "<html><head><title>Error Evaluating Racket</title></head>"
   "<body>"
   "<h2>Error Evaluating Racket Code</h2>"
   (format "<pre>\n~a\n</pre>" str)
   "<p><p>"
   (format "<font color=\"red\">~a</font>" 
           (regexp-replace* #rx"<" (regexp-replace* #rx">" (exn-message exn) "&lt;") "&gt;"))
   "</body></html>"))

(define hyper-text% (hyper-text-mixin text:keymap%))

(define space-page-keymap (make-object keymap%))
(add-text-keymap-functions space-page-keymap)
(send space-page-keymap map-function "space" "next-page")
(send space-page-keymap map-function "s:space" "previous-page")

(define hyper-keymap (make-object keymap%))
(send hyper-keymap add-function "rewind" 
      (lambda (txt evt)
        (call-with-hyper-panel
         txt
         (lambda (panel)
           (send panel rewind)))))
(send hyper-keymap add-function "forward" 
      (lambda (txt evt)
        (call-with-hyper-panel
         txt
         (lambda (panel)
           (send panel forward)))))
(send hyper-keymap add-function "do-wheel" 
      (lambda (txt evt)
        ;; Redirect the event to the canvas, which should
        ;;  handle the event
        (send (send txt get-canvas) on-char evt)))
(add-text-keymap-functions hyper-keymap)
(send hyper-keymap map-function "d:[" "rewind")
(send hyper-keymap map-function "a:[" "rewind")
(send hyper-keymap map-function "c:[" "rewind")
(send hyper-keymap map-function "d:left" "rewind")
(send hyper-keymap map-function "a:left" "rewind")
(send hyper-keymap map-function "c:left" "rewind")
(send hyper-keymap map-function "~c:m:left" "rewind")
(send hyper-keymap map-function "d:]" "forward")
(send hyper-keymap map-function "a:]" "forward")
(send hyper-keymap map-function "c:]" "forward")
(send hyper-keymap map-function "d:right" "forward")
(send hyper-keymap map-function "a:right" "forward")
(send hyper-keymap map-function "c:right" "forward")
(send hyper-keymap map-function "~c:m:right" "forward")
(send hyper-keymap map-function "wheelup" "do-wheel")
(send hyper-keymap map-function "pageup" "previous-page")
(send hyper-keymap map-function "wheeldown" "do-wheel")
(send hyper-keymap map-function "pagedown" "next-page")

;; call-with-hyper-panel : object ((is-a?/c hyper-panel<%>) -> void) -> void
(define (call-with-hyper-panel text f)
  (when (is-a? text hyper-text<%>)
    (let ([canvas (send text get-canvas)])
      (when canvas
        (let ([tlw (send canvas get-top-level-window)])
          (when (is-a? tlw hyper-frame<%>)
            (f (send tlw get-hyper-panel))))))))

;; path-below? : string[normalized-path] string[normalized-path] -> boolean
;; returns #t if subpath points to a place below top
(define (path-below? top longer)
  (let loop ([top (explode-path top)]
             [longer (explode-path longer)])
    (cond
     [(null? top) #t]
     [(null? longer) #f]
     [(equal? (car top) (car longer))
      (loop (cdr top) (cdr longer))]
     [else #f])))

(keymap:add-to-right-button-menu/before
 (let ([old (keymap:add-to-right-button-menu/before)])
   (lambda (menu editor event)
     (when (is-a? editor hyper-text<%>)
       (let* ([panel (let ([canvas (send editor get-canvas)])
                       (and canvas
                            (send (send canvas get-top-level-window) get-hyper-panel)))]
              [back
               (instantiate menu-item% ()
                            (label (string-append "< " (string-constant rewind-in-browser-history)))
                            (parent menu)
                            (callback
                             (lambda (_1 _2)
                               (when panel
                                 (send panel rewind)))))]
              [forward
               (instantiate menu-item% ()
                            (label (string-append (string-constant forward-in-browser-history) " >"))
                            (parent menu)
                            (callback
                             (lambda (_1 _2)
                               (when panel
                                 (send panel forward)))))])
         (send back enable (send panel can-rewind?))
         (send forward enable (send panel can-forward?))
         (instantiate separator-menu-item% ()
                      (parent menu))))
     (old menu editor event))))

(define hyper-canvas-mixin
  (mixin ((class->interface editor-canvas%)) ()
         (inherit get-editor set-editor refresh get-parent get-top-level-window)
         
         (define/public (get-editor%) hyper-text%)
         
         (define/public (current-page)
           (let ([e (get-editor)])
             (and e 
                  (let ([sbox (box 0)]
                        [ebox (box 0)])
                    (send e get-visible-position-range sbox ebox)
                    (list e (unbox sbox) (unbox ebox))))))
         (define/public (on-url-click k url post-data)
           (send (get-parent) on-url-click k url post-data))
         (define/public goto-url
           (lambda (in-url relative [progress void] [post-data #f])
             (let ([tlw (get-top-level-window)])
               (when (and tlw
                          (is-a? tlw hyper-frame<%>))
                 (let* ([pre-url (cond
                                  [(url? in-url) in-url]
                                  [(port? in-url) in-url]
                                  [(string? in-url)
                                   (if relative
                                       (combine-url/relative relative in-url)
                                       (string->url in-url))]
                                  [else (error 'goto-url "unknown url ~e\n" in-url)])]
                        [killable-cust (make-custodian)]
                        [hyper-panel (send tlw get-hyper-panel)]
                        [result
                         (let ([e-now (get-editor)])
                           (cond
                            [(and e-now
                                  (not post-data)
                                  (same-page-url? pre-url (send e-now get-url)))
                             (progress #t)
                             (cons e-now pre-url)]
                            [else
                             (send hyper-panel set-stop-callback 
                                   (lambda ()
                                     (custodian-shutdown-all killable-cust)))
                             (send hyper-panel enable-browsing #f)
                             (begin0
                              (make-editor/setup-kill killable-cust 
                                                      (get-editor%)
                                                      tlw
                                                      pre-url
                                                      progress
                                                      post-data
                                                      (lambda (x) (remap-url x)))
                              (send hyper-panel set-stop-callback void)
                              (send hyper-panel enable-browsing #t))]))])
                   (cond
                    [(pair? result)
                     (let* ([e (car result)]
                            [url (cdr result)]
                            [tag-pos (send e find-tag (and (url? url) (url-fragment url)))])
                       (unless (and tag-pos (positive? tag-pos))
                         (send e hide-caret #t))
                       (set-page (list e (or tag-pos 0) (send e last-position)) #t)
                       (when tag-pos (send e set-position tag-pos)))]
                    [(exn? result) 
                     (message-box (string-constant drracket)
                                  (exn-message result)
                                  tlw)]
                    [else (void)]))))))
         
         ;; remap-url : url? -> (union #f url?)
         ;; this method is intended to be overridden so that derived classes can change
         ;; the behavior of the browser. Calls to this method may be killed.
         (define/public (remap-url url)
           url)
         
         (define/public (after-set-page) (void))
         (define/public (set-page page notify?)
           (let ([e (car page)]
                 [spos (cadr page)]
                 [epos (caddr page)]
                 [curr (get-editor)]
                 [current (current-page)])
                                        ; Pre-size the editor to avoid visible reflow
             (when curr
               (let ([wbox (box 0)])
                 (send curr get-view-size wbox (box 0))
                 (when (send e auto-wrap)
                   (send e set-max-width (unbox wbox)))))
             (send e begin-edit-sequence)
             (when notify?
               (send (get-parent) leaving-page current (list e 0 0)))
             (set-editor e (and current (zero? (cadr current)) (zero? spos)))
             (send e scroll-to-position spos #f epos 'start)
             (send e end-edit-sequence)
             (after-set-page)
             (when (or (positive? spos) (not current) (positive? (cadr current)))
               (refresh))))
         (super-new)))

;; make-editor/setup-kill : custodian editor-class frame%-instance 
;;                          url (boolean??? -> void) ??? (url -> (union port #f url))
;;                       -> (union (cons editor (union #f url)) exn #f)
;; if cust is shutdown, the url will stop being loaded and a dummy editor is returned.
(define (make-editor/setup-kill cust html-editor% tlw init-url progress post-data remap-url)
  (let* ([c (make-channel)]
         [progs (make-channel)]
         [sent-prog? #f]
         [t (parameterize ([current-custodian cust])
              (thread
               (lambda ()
                 (with-handlers ([exn? (lambda (exn)
                                         (channel-put c exn))])
                   (channel-put
                    c
                    (make-editor/follow-redirections html-editor%
                                                     tlw
                                                     init-url
                                                     (lambda (v)
                                                       (channel-put progs v))
                                                     post-data
                                                     remap-url))))))]
         [ans #f])
    (let loop ()
      (yield
       (choice-evt
        (handle-evt c (lambda (x) (set! ans x)))
        (handle-evt progs (lambda (v)
                            (set! sent-prog? #t)
                            (progress v)
                            (loop)))
        (handle-evt (thread-dead-evt t)
                    (lambda (_)
                      (let ([t (new hyper-text% 
                                    (url #f)
                                    (top-level-window #f)
                                    (progress void))])
                        (send t insert "Stopped.")
                        (set! ans (cons t #f))))))))
    (unless sent-prog?
      (progress #f))
    ans))

;; make-editor/follow-redirections : editor-class frame%-instance
;;                                   url (boolean??? -> void) ??? (url -> (union port #f url))
;;                                -> (cons (union #f editor) (union #f url))
;; builds an html editor using make-editor and follows any redictions,
;; but stops after 10 redirections (just in case there are too many
;; of these things, give the user a chance to stop)
(define (make-editor/follow-redirections html-editor% tlw init-url progress post-data remap-url)
  (with-handlers ([(lambda (x) 
                     (or (exn:file-saved-instead? x)
                         (exn:cancelled? x)
                         (exn:tcp-problem? x)))
                   values])
    (let loop ([n 10]
               [unmapped-url init-url])
      (let ([url (if (url? unmapped-url)
                     (let ([rurl (remap-url unmapped-url)])
                       (unless (or (url? rurl)
                                   (input-port? rurl)
                                   (not rurl))
                         (error 'remap-url
                                "expected a url struct, an input-port, or #f, got ~e"
                                rurl))
                       rurl)
                     unmapped-url)])
        (if url
            (let ([html-editor (new html-editor%
                                    [url url]
                                    [top-level-window tlw]
                                    [progress progress]
                                    [post-data post-data])])
              (cond
               [(zero? n) 
                (cons html-editor url)]
               [(send html-editor get-redirection)
                =>
                (lambda (new-url) (loop (- n 1) new-url))]
               [else
                (cons html-editor url)]))
            #f)))))

(define hyper-canvas% (hyper-canvas-mixin canvas:basic%))

(define info-canvas%
  (class canvas%
    (inherit min-client-height get-dc stretchable-height
             get-parent enable refresh show)
    (field
     [text ""])
    [define/override on-paint
      (lambda ()
        (let ([dc (get-dc)])
          (send dc clear)
          (send dc draw-text text 4 2)))]
    [define/public erase-info (lambda ()
                                (unless (string=? text "")
                                  (set! text "")
                                  (let ([dc (get-dc)])
                                    (send dc clear))))]
    [define/public set-info (lambda (t)
                              (set! text t)
                              (if (string=? t "")
                                  (show #f)
                                  (let ([dc (get-dc)])
                                    (send dc clear)
                                    (show #t)
                                    (refresh))))]
    (super-instantiate ())
    (stretchable-height #f)
    (enable #f)
    (show #f)
    (let ([font (make-object font% (send normal-control-font get-point-size) 
                             'default 'normal 'normal)]
          [dc (get-dc)])
      (send dc set-font font)
      (send dc set-text-foreground (make-object color% "FOREST GREEN"))
      (let-values ([(w h d a) (send dc get-text-extent "X" font)])
        (min-client-height (+ 4 (inexact->exact (ceiling h))))))))

(define hyper-panel<%>
  (interface ()
    current-page
    rewind
    forward
    can-rewind?
    can-forward?
    get-canvas%
    make-canvas
    make-control-bar-panel
    
    set-init-page
    goto-init-page
    
    on-navigate
    filter-notes
    get-canvas
    on-url-click 
    reload
    leaving-page
    get-stop-button
    set-stop-callback
    
    enable-browsing))

(define hyper-panel-mixin
  (mixin (area-container<%>) (hyper-panel<%>)
         (init info-line?)
         (inherit reflow-container)
         (super-new)
         
         (define browsing-on? #t)
         (define/public (enable-browsing on?)
           (set! browsing-on? on?)
           (cond
            [on?
             (send stop-button enable #f)
             (when choice (send choice enable #t))
             (update-buttons)]
            [else 
             (send stop-button enable #t)
             (when home (send home enable #f))
             (when forw (send forw enable #f))
             (when back (send back enable #f))
             (when choice (send choice enable #f))]))
         
         (define/private (clear-info)
           (when info 
             (send info erase-info)))
         (define/private (update-info page)
           (when (and info page)
             (let ([notes (send (page->editor page) get-document-notes)])
               (send info set-info
                     (filter-notes notes (send (page->editor page) get-url))))))
         (define/private (go page)
           (clear-info)
           (send c set-page page #f)
           (update-info page)
           (update-buttons/set-page page)
           (on-navigate))
         
         (define/public (current-page) (send c current-page))
         (define/public (rewind)
           (unless (null? past)
             (let ([page (car past)])
               (set! future (cons (send c current-page) future))
               (set! past (cdr past))
               (go page))))
         (define/public (forward)
           (unless (null? future)
             (let ([page (car future)])
               (set! past (cons (send c current-page) past))
               (set! future (cdr future))
               (go page))))
         (define/public (can-forward?) (and browsing-on? (not (null? future))))
         (define/public (can-rewind?) (and browsing-on? (not (null? past))))
         [define/public get-canvas% (lambda () hyper-canvas%)]
         [define/public make-canvas (lambda (f) (make-object (get-canvas%) f))]
         [define/public make-control-bar-panel (lambda (f) (make-object horizontal-panel% f))]
         (field
          [past null]
          [future null] 
          
          
          ;; (union #f                             -- no init page
          ;;         string                        -- delayed init page
          ;;         url                           -- delayed init page
          ;;         (list editor number numer))   -- forced init page
          [init-page #f]

          [hp (make-control-bar-panel this)]
          [control-bar? (is-a? hp area-container<%>)]
          [back (and control-bar?
                     (make-object button%
                                  (string-append "< " (string-constant rewind-in-browser-history))
                                  hp
                                  (lambda (b ev) 
                                    (rewind))))]
          [forw (and control-bar?
                     (make-object button% 
                                  (string-append (string-constant forward-in-browser-history) " >")
                                  hp
                                  (lambda (b ev) 
                                    (forward))))])
         
         (define/private (home-callback)
           (cond
            [(or (url? init-page)
                 (string? init-page))
             
                                        ; handle stopping when loading the home page
             (with-handlers ([exn:break? 
                              (lambda (x) (void))])
               (send c goto-url init-page #f)
               (update-buttons))]
            [else 
             (send c set-page init-page #t)]))
         (field
          [home (and control-bar?
                     (make-object button% (string-constant home) hp
                                  (lambda (b ev)
                                    (home-callback))))])
         
         (define the-page #f)
         (define/private (update-buttons/set-page page)
           (unless init-page
             (set! init-page page))
           (set! the-page page)
           (update-buttons))
         (define/private (update-buttons)
           (when control-bar?
             (send home enable (or (url? init-page) (string? init-page)))
             (send back enable (pair? past))
             (send forw enable (pair? future))
             
             (send choice clear)
             (for-each
              (lambda (p)
                (send choice append 
                      (let ([s (send (car p) get-title)])
                        (if s 
                            (gui-utils:trim-string s 200)
                            (string-constant untitled)))))
              (append (reverse future)
                      (if the-page (list the-page) null)
                      past))
             (let ([c (send choice get-number)])
               (unless (zero? c)
                 (send choice set-selection (length future))))))
         (field
          [choice (and control-bar?
                       (make-object choice% #f null hp
                                    (lambda (ch e)
                                      (let* ([l (append (reverse past)
                                                        (list (send c current-page))
                                                        future)]
                                             [pos (- (send choice get-number) (send choice get-selection) 1)])
                                        (let loop ([l l][pre null][pos pos])
                                          (cond
                                           [(zero? pos)
                                            (set! past pre)
                                            (set! future (cdr l))
                                            (go (car l))]
                                           [else (loop (cdr l)
                                                       (cons (car l) pre)
                                                       (sub1 pos))]))))))]
          [stop-callback void]
          [stop-button
           (and control-bar?
                (new button%
                     (label (string-constant stop))
                     (parent hp)
                     (callback (lambda (x y) (stop-callback)))))])
         (define/public (get-stop-button) stop-button)
         (define/public (set-stop-callback bc) (set! stop-callback bc))
         (when stop-button (send stop-button enable #f))
         
         (field
          [info (and info-line?
                     (make-object info-canvas% this))]
          [c (make-canvas this)])
         
         ;; set-init-page : (union string url) -> void
         [define/public set-init-page
           (lambda (p)
             (set! init-page p))]
         [define/public goto-init-page
           (lambda ()
             (home-callback))]
         
         [define/public on-navigate (lambda () (void))]
         [define/public filter-notes (lambda (l) (apply string-append l))]
         [define/public get-canvas (lambda () c)]
         [define/public on-url-click (lambda (k url post-data) (k url post-data))]
         
         [define/public reload
           (lambda ()
             (let ([c (get-canvas)])
               (and c 
                    (let ([e (send c get-editor)])
                      (and e
                           (send e reload))))))]
         
         (define/public (leaving-page page new-page)
           (set! future null)
           (when page
             (set! past (cons page past)))
           (when (> (length past) history-limit)
             (set! past
                   (let loop ([l past])
                     (if (null? (cdr l))
                         null
                         (cons (car l) (loop (cdr l)))))))
           (clear-info)
           (update-buttons/set-page new-page)
           (update-info new-page))
         (when control-bar?
           (send choice stretchable-width #t)
           (send hp stretchable-height #f))
         (update-buttons/set-page #f)))

(define hyper-panel% (hyper-panel-mixin vertical-panel%))

(define hyper-frame<%>
  (interface ()
    get-hyper-panel
    get-hyper-panel%))

(define hyper-no-show-frame-mixin
  (mixin (frame:status-line<%>) (hyper-frame<%>)
         (field [p #f])
         (define/public get-hyper-panel% (lambda () hyper-panel%))
         (define/public get-hyper-panel (lambda () p))
         (inherit show get-area-container)
         (super-instantiate ())
         (set! p (make-object (get-hyper-panel%) #f (get-area-container)))))

(define hyper-frame-mixin
  (let ([m (mixin (hyper-frame<%> top-level-window<%>) ()
                  (init start-url)
                  (inherit show get-hyper-panel)
                  (super-instantiate ())
                  (show #t)
                  (send (send (get-hyper-panel) get-canvas) goto-url start-url #f))])
    (lambda (%)
      (m (hyper-no-show-frame-mixin %)))))

(define hyper-frame% (hyper-frame-mixin (frame:status-line-mixin frame:basic%)))
(define hyper-no-show-frame% (hyper-no-show-frame-mixin (frame:status-line-mixin frame:basic%)))

(define (editor->page e) (list e 0 0))
(define (page->editor e) (car e))

(define (same-page? a b)
  (eq? (car a) (car b)))

(define (open-url file)
  (make-object hyper-frame% file (string-constant browser) #f 500 450))

