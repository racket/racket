#lang scheme/unit

(require mzlib/class
         mrlib/hierlist
         "sig.rkt"
         "../preferences.rkt"
         "../gui-utils.rkt"
         mred/mred-sig
         racket/path
         string-constants)

(import mred^
        [prefix finder: framework:finder^]
        [prefix group: framework:group^]
        [prefix text: framework:text^]
        [prefix frame: framework:frame^])
(export framework:handler^)
(init-depend framework:frame^)

(define-struct handler (name extension handler))

(define format-handlers '())

(define (make-insert-handler who name extension handler)
  (cond [(not (string? name))
         (error who "name was not a string")]
        [(not (or (procedure? extension)
                  (string? extension)
                  (and (list? extension) (andmap string? extension))))
         (error who
                "extension was not a string, list of strings, or a predicate")]
        [(not (procedure? handler))
         (error who "handler was not a function")]
        [else (make-handler name
                            (if (string? extension) (list extension) extension)
                            handler)]))

(define (insert-format-handler . args)
  (set! format-handlers
        (cons (apply make-insert-handler 'insert-format-handler args)
              format-handlers)))

(define (find-handler name handlers)
  (let/ec exit
    (let ([extension (if (string? name) (or (filename-extension name) "") "")])
      (for ([handler handlers])
        (let ([ext (handler-extension handler)])
          (when (or (and (procedure? ext) (ext name))
                    (and (pair? ext)
                         (ormap (λ (ext) (string=? ext extension))
                                ext)))
            (exit (handler-handler handler)))))
      #f)))

(define (find-format-handler name)
  (find-handler name format-handlers))

; Finding format & mode handlers by name
(define (find-named-handler name handlers)
  (let loop ([l handlers])
    (cond [(null? l) #f]
          [(string-ci=? (handler-name (car l)) name)
           (handler-handler (car l))]
          [else (loop (cdr l))])))

(define (find-named-format-handler name)
  (find-named-handler name format-handlers))

; Open a file for editing
(define current-create-new-window
  (make-parameter
   (λ (filename)
     (let ([frame (make-object frame:text% filename)])
       (send frame show #t)
       frame))))

(define (edit-file filename [make-default
                             (λ () ((current-create-new-window) filename))])
  (with-handlers ([(λ (x) #f) ;exn:fail?
                   (λ (exn)
                     (message-box
                      (string-constant error-loading)
                      (string-append
                       (format (string-constant error-loading-file/name)
                               (or filename
                                   (string-constant unknown-filename)))
                       "\n\n"
                       (if (exn? exn)
                         (format "~a" (exn-message exn))
                         (format "~s" exn))))
                     #f)])
    (gui-utils:show-busy-cursor
     (λ ()
       (if filename
         (let ([already-open (send (group:get-the-frame-group)
                                   locate-file
                                   filename)])
           (cond
             [already-open
              (send already-open make-visible filename)
              (send already-open show #t)
              already-open]
             [else
              (let ([handler (and (path? filename)
                                  (find-format-handler filename))])
                (add-to-recent filename)
                (if handler (handler filename) (make-default)))]))
         (make-default))))))

;; type recent-list-item = (list/p string? number? number?)

;; add-to-recent : path -> void
(define (add-to-recent filename)
  
  (define old-list (preferences:get 'framework:recently-opened-files/pos))
  (define old-ents (filter (λ (x) (recently-opened-files-same-enough-path? (car x) filename))
                           old-list))
  (define new-ent (if (null? old-ents)
                      (list filename 0 0)
                      (cons filename (cdr (car old-ents)))))
  (define added-in (cons new-ent
                         (remove* (list new-ent)
                                  old-list 
                                  (λ (l1 l2) 
                                    (recently-opened-files-same-enough-path? (car l1) (car l2))))))
  (define new-recent (size-down added-in
                                (preferences:get 'framework:recent-max-count)))
  (preferences:set 'framework:recently-opened-files/pos new-recent))

;; same-enough-path? : path path -> boolean
;; used to determine if the open-recent-files menu item considers two paths to be the same
(define (recently-opened-files-same-enough-path? p1 p2)
  (equal? (simplify-path (normal-case-path p1) #f)
          (simplify-path (normal-case-path p2) #f)))



;; size-down : (listof X) -> (listof X)[< recent-max-count]
;; takes a list of stuff and returns the
;; front of the list, up to `recent-max-count' items
(define (size-down new-recent n)
  (let loop ([n n] [new-recent new-recent])
    (cond [(zero? n) null]
          [(null? new-recent) null]
          [else (cons (car new-recent)
                      (loop (- n 1) (cdr new-recent)))])))

;; size-recently-opened-files : number -> void
;; sets the recently-opened-files/pos preference
;; to a size limited by `n'
(define (size-recently-opened-files n)
  (preferences:set
   'framework:recently-opened-files/pos
   (size-down (preferences:get 'framework:recently-opened-files/pos)
              n)))

;; set-recent-position : path number number -> void
;; updates the recent menu preferences
;; with the positions `start' and `end'
(define (set-recent-position filename start end)
  (let* ([recent-items
          (preferences:get 'framework:recently-opened-files/pos)]
         [new-recent-items
          (map (λ (x)
                 (if (recently-opened-files-same-enough-path? (path->string (car x))
                                                              (path->string filename))
                     (list* (car x) start end (cdddr x))
                     x))
               (preferences:get 'framework:recently-opened-files/pos))])
    (unless (equal? recent-items new-recent-items)
      (preferences:set 'framework:recently-opened-files/pos
                       new-recent-items))))

;; install-recent-items : (is-a?/c menu%) -> void?
(define (install-recent-items menu)
  (let ([recently-opened-files
         (preferences:get
          'framework:recently-opened-files/pos)])
    (unless (menu-items-still-same? recently-opened-files menu)
      (for ([item (send menu get-items)]) (send item delete))
      
      (for ([recent-list-item recently-opened-files])
        (new menu-item%
             [parent menu]
             [label (recent-list-item->menu-label recent-list-item)]
             [callback (λ (x y) (open-recent-list-item recent-list-item))]))
      (new separator-menu-item% [parent menu])
      (new menu-item%
           [parent menu]
           [label (string-constant show-recent-items-window-menu-item)]
           [callback (λ (x y) (show-recent-items-window))]))
    (void)))

(define (recent-list-item->menu-label recent-list-item)
  (let ([filename (car recent-list-item)])
    (gui-utils:quote-literal-label
     (path->string filename))))

;; this function must mimic what happens in install-recent-items
;; it returns #t if all of the labels of menus are the same, or approximation to
;; the menus actually being different
(define (menu-items-still-same? recently-opened-files menu)
  (let ([current-items 
         (map (λ (x) (and (is-a? x labelled-menu-item<%>) (send x get-label)))
              (send menu get-items))]
        ;; the new-items variable should match up to what install-recent-items actually does when it creates the menu 
        [new-items 
         (append 
          (for/list ([recent-list-item recently-opened-files])
            (recent-list-item->menu-label recent-list-item))
          (list #f 
                (string-constant show-recent-items-window-menu-item)))])
    (equal? current-items new-items)))

;; open-recent-list-item : recent-list-item -> void
(define (open-recent-list-item recent-list-item)
  (let* ([filename (car recent-list-item)]
         [start (cadr recent-list-item)]
         [end (caddr recent-list-item)])
    (cond
      [(file-exists? filename)
       (edit-file filename)]
      [else
       (preferences:set 'framework:recently-opened-files/pos
                        (remove* (list recent-list-item)
                                 (preferences:get 'framework:recently-opened-files/pos)
                                 (λ (l1 l2)
                                   (recently-opened-files-same-enough-path?
                                    (car l1)
                                    (car l2)))))
       (message-box (string-constant error)
                    (format (string-constant cannot-open-because-dne)
                            filename))])))

;; show-recent-items-window : -> void
(define (show-recent-items-window)
  (unless recent-items-window
    (set! recent-items-window (make-recent-items-window)))
  (send recent-items-window show #t))

;; make-recent-items-window : -> frame
(define (make-recent-items-window)
  (make-object (get-recent-items-window%)
    (string-constant show-recent-items-window-label)
    #f
    (preferences:get 'framework:recent-items-window-w)
    (preferences:get 'framework:recent-items-window-h)))

;; recent-items-window : (union #f (is-a?/c frame%))
(define recent-items-window #f)

(define recent-items-hierarchical-list%
  (class hierarchical-list%
    (define/override (on-double-select item)
      (send item open-item))
    (super-instantiate ())))

(define recent-items-super% (frame:standard-menus-mixin frame:basic%))

(define (set-recent-items-frame-superclass super%)
  (set! recent-items-super% super%))

(define (get-recent-items-window%)
  (class recent-items-super%

    ;; remove extraneous separators
    (define/override (file-menu:between-print-and-close menu) (void))
    (define/override (edit-menu:between-find-and-preferences menu) (void))

    (define/override (on-size w h)
      (preferences:set 'framework:recent-items-window-w w)
      (preferences:set 'framework:recent-items-window-h h))

    ;; refresh-hl : (listof recent-list-item) -> void
    (define/private (refresh-hl recent-list-items)
      (let ([ed (send hl get-editor)])
        (send ed begin-edit-sequence)
        (for ([item (send hl get-items)]) (send hl delete-item item))
        (for-each (λ (item) (add-recent-item item))
                  (if (eq? (preferences:get 'framework:recently-opened-sort-by)
                           'name)
                    (sort recent-list-items string<?
                          #:key (compose path->string car) #:cache-keys? #t)
                    recent-list-items))
        (send ed end-edit-sequence)))

    (define/private (add-recent-item recent-list-item)
      (let ([item (send hl
                        new-item (make-hierlist-item-mixin recent-list-item))])
        (send (send item get-editor)
              insert (path->string (car recent-list-item)))))

    (field [remove-prefs-callback
            (preferences:add-callback 'framework:recently-opened-files/pos
                                      (λ (p v) (refresh-hl v)))])

    (define/augment (on-close)
      (inner (void) on-close)
      (remove-prefs-callback)
      (set! recent-items-window #f))

    (super-new)

    (inherit get-area-container)
    (field [bp (make-object horizontal-panel% (get-area-container))]
           [hl (make-object recent-items-hierarchical-list%
                            (get-area-container) '())]
           [sort-by-name-button
            (make-object button%
              (string-constant recent-items-sort-by-name)
              bp
              (λ (x y) (set-sort-by 'name)))]
           [sort-by-age-button
            (make-object button%
              (string-constant recent-items-sort-by-age)
              bp
              (λ (x y) (set-sort-by 'age)))])

    (send bp stretchable-height #f)
    (send sort-by-name-button stretchable-width #t)
    (send sort-by-age-button stretchable-width #t)

    (define/private (set-sort-by flag)
      (preferences:set 'framework:recently-opened-sort-by flag)
      (case flag
        [(name)
         (send sort-by-age-button enable #t)
         (send sort-by-name-button enable #f)]
        [(age)
         (send sort-by-age-button enable #f)
         (send sort-by-name-button enable #t)])
      (refresh-hl (preferences:get 'framework:recently-opened-files/pos)))

    (set-sort-by (preferences:get 'framework:recently-opened-sort-by))))

;; make-hierlist-item-mixin : recent-item -> mixin(arg to new-item method of hierlist)
(define (make-hierlist-item-mixin recent-item)
  (λ (%)
    (class %
      (define/public (open-item)
        (open-recent-list-item recent-item))
      (super-instantiate ()))))

(define (open-file [directory #f])
  (let* ([parent (and (not (eq? 'macosx (system-type)))
                      (get-top-level-focus-window))]
         [file
          (parameterize ([finder:dialog-parent-parameter parent])
            (finder:get-file directory))])
    (and file
         (edit-file file))))
