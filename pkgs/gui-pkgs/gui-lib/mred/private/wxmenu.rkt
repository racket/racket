(module wxmenu racket/base
  (require racket/class
           racket/list
           (prefix-in wx: "kernel.rkt")
           (prefix-in wx: "wxme/keymap.rkt")
           "lock.rkt"
           "const.rkt"
           "helper.rkt"
           "wx.rkt")

  (provide (protect-out wx-menu-item%
                        wx-menu-bar%
                        wx-menu%))

  (define wx-menu-item%
    (class* wx:menu-item% (wx<%>)
      (init mr mn-dat can-enable?)
      (define menu-data mn-dat)
      (define mred mr)
      (define keymap #f)
      (define wx-menu #f)
      (define enabled? #t)
      (define ever-enabled? can-enable?)
      (public*
       [get-keymap (lambda () keymap)]
       [set-keymap (lambda (k) (set! keymap k))]
       [swap-keymap (lambda (parent k) 
                      (send (send (mred->wx parent) get-container) swap-item-keymap keymap k) 
                      (set-keymap k))]
       [get-mred (lambda () mred)]
       [get-menu-data (lambda () menu-data)] ; for meta-shortcuts
       [get-container (lambda () wx-menu)]
       [set-wx-menu (lambda (wx) (set! wx-menu wx))]
       [is-enabled? (lambda () enabled?)]
       [set-enabled (lambda (on?) (set! enabled? on?))]
       [ignore-enabled? (lambda () (not ever-enabled?))])
      (super-make-object)))

  (define wx-menu-bar%
    (class* wx:menu-bar% (wx<%>)
      (init mr)
      (inherit delete)
      (rename-super [super-append append]
	            [super-enable-top enable-top])
      (define mred mr)
      (define items null)
      (define disabled null)
      (define disabled? #f)
      (define keymap (make-object wx:keymap%))
      (public*
       [get-container (lambda () this)]
       [handle-key (lambda (event) 
                     (as-exit 
                      (lambda () 
                        (or (send keymap handle-key-event this event)
                            (and (menu-shortcut-in-label?)
                                 (send event get-meta-down)
                                 (char? (send event get-key-code))
                                 (let ([c (send event get-key-code)])
                                   (and (or (char-alphabetic? c)
                                            (char-numeric? c))
                                        (let ([re (key-regexp c)])
                                          (ormap
                                           (lambda (i)
                                             (let* ([data (send (mred->wx i) get-menu-data)]
                                                    [label (mcar data)]
                                                    [menu (mcdr data)])
                                               (if (regexp-match re label)
                                                   (send menu select this)
                                                   #f)))
                                           items)))))))))]
       [on-demand (lambda () (as-exit (lambda () (send mred on-demand))))]
       [get-mred (lambda () mred)]
       [get-items (lambda () items)]
       [append-item (lambda (item menu title)
                      (super-append menu title)
                      (when disabled?
                        (super-enable-top (length items) #f))
                      (set! items (append items (list item)))
                      (send keymap chain-to-keymap (send (mred->wx item) get-keymap) #f))]
       [all-enabled? (lambda () (not disabled?))]
       [enable-all (lambda (on?)
                     (set! disabled? (not on?))
                     (let loop ([n (sub1 (length items))])
                       (unless (negative? n)
                         (if on?
                             (unless (memq (list-ref items n) disabled)
                               (super-enable-top n #t))
                             (super-enable-top n #f))
                         (loop (sub1 n)))))]
       [delete-item (lambda (i)
                      (let ([p (position-of i)])
                        (set! items (remq i items))
                        (set! disabled (remq i disabled))
                        (delete #f p)
                        (send keymap remove-chained-keymap (send (mred->wx i) get-keymap))))]
       [position-of (lambda (i) (find-pos items i eq?))])
      (override*
       [enable-top (lambda (p on?)
                     (let ([i (list-ref items p)])
                       (if on?
                           (when (memq i disabled)
                             (set! disabled (remq i disabled))
                             (unless disabled?
                               (super-enable-top p #t)))
                           (unless (memq i disabled)
                             (set! disabled (cons i disabled))
                             (super-enable-top p #f)))))])
      (super-make-object)))

  (define wx-menu%
    (class* wx:menu% (wx<%>)
      (init mr popup-label popup-callback font)
      (define mred mr)
      (define items null)
      (define keymap (make-object wx:keymap%))
      (define popup-grabber #f)
      (inherit delete-by-position)
      (rename-super [super-delete delete]
	            [super-enable enable])
      (public*
       [get-container (lambda () this)]
       [get-keymap (lambda () keymap)]
       [get-mred (lambda () mred)]
       [get-items (lambda () items)]
       [append-item (lambda (i iwx) 
                      (set! items (append items (list i)))
                      (let ([k (send iwx get-keymap)])
                        (when k
                          (send keymap chain-to-keymap k #f))))]
       [delete-sep (lambda (i iwx)
                     (delete-by-position (find-pos items i eq?))
                     (set! items (remq i items)))]
       [swap-item-keymap (lambda (old-k new-k)
                           (when old-k (send keymap remove-chained-keymap old-k))
                           (when new-k (send keymap chain-to-keymap new-k #f)))]

       [popup-grab (lambda (c)
                     (if popup-grabber
                         #f
                         (begin
                           (set! popup-grabber c)
                           #t)))]
       [popup-release (lambda () (set! popup-grabber #f))]
       [get-popup-grabber (lambda () popup-grabber)])
      (override*
       [delete (lambda (id i) 
                 (super-delete id) 
                 (set! items (remq i items))
                 (let ([k (send (mred->wx i) get-keymap)])
                   (when k
                     (send keymap remove-chained-keymap k))))]
       [enable (lambda (iwx id on?)
                 ;; Only called if the item is not deleted
                 (unless (eq? (send iwx is-enabled?) (and on? #t))
                   (send iwx set-enabled (and on? #t))
                   (super-enable id on?)))])
      (super-make-object popup-label popup-callback font))))
