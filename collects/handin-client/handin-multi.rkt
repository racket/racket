#lang racket/base

(require racket/class racket/port racket/gui/base framework browser/external
         "info.rkt" "client-gui.rkt" "this-collection.rkt")

(define handin-name (#%info-lookup 'name))
(define web-address (#%info-lookup 'web-address
                      (lambda () "http://racket-lang.org")))
(define selection-mode (#%info-lookup 'selection-mode (lambda () 'extended)))
(define selection-defaults
  (let ([sd (#%info-lookup 'selection-default (lambda () '("*.rkt")))])
    (if (string? sd) (list sd) sd)))
(define last-dir-key (make-my-key 'multifile:last-dir))
(preferences:set-default last-dir-key "" string?)
(define last-auto-key (make-my-key 'multifile:last-auto))
(preferences:set-default last-auto-key (car selection-defaults) string?)
(define geometry-key (make-my-key 'multifile:geometry))
(preferences:set-default geometry-key #f void)

(define update
  (and (#%info-lookup 'enable-auto-update (lambda () #f))
       (dynamic-require `(lib "updater.rkt" ,this-collection-name) 'update)))

;; ==========================================================================
(define magic #"<<<MULTI-SUBMISSION-FILE>>>")
(define (pack-files files)
  (let/ec return
    (parameterize ([current-output-port (open-output-bytes)])
      (printf "~a\n" magic)
      (for ([file (in-list files)])
        (let ([size (and (file-exists? file) (file-size file))])
          (unless size (return #f))
          (let ([buf (with-input-from-file file
                       (lambda () (read-bytes size)))])
            (unless (equal? size (bytes-length buf)) (return #f))
            (write (list file buf)) (newline))))
      (flush-output)
      (get-output-bytes (current-output-port)))))
(define ((unpack-files parent) buf)
  (let/ec return
    (define (error* msg)
      (message-box "Retrieve Error" msg parent)
      (return #f))
    (parameterize ([current-input-port (open-input-bytes buf)])
      (unless (equal? magic (read-bytes (bytes-length magic)))
        (error* "Error in retrieved content: bad format"))
      (let ([files
             (let loop ([files '()])
               (let ([f (with-handlers ([void void]) (read))])
                 (if (eof-object? f)
                   (reverse files) (loop (cons f files)))))]
            [overwrite-all? #f])
        (define (write? file)
          (define (del)
            ;; check if exists: users might rename files during questions
            (when (file-exists? file) (delete-file file)))
          (cond [(not (file-exists? file)) #t]
                [overwrite-all? (del) #t]
                [else (case (message-box/custom
                             "Retrieve"
                             (format "~s already exists, overwrite?" file)
                             "&Yes" "&No" "Yes to &All" parent
                             '(default=2 caution) 4)
                        [(1) (del) #t]
                        [(2) #f]
                        [(3) (set! overwrite-all? #t) (del) #t]
                        [(4) (error* "Aborting...")])]))
        (unless (and (list? files)
                     (andmap (lambda (x)
                               (and (list? x) (= 2 (length x))
                                    (string? (car x)) (bytes? (cadr x))))
                             files))
          (error* "Error in retrieved content: bad format"))
        (for ([file (in-list files)])
          (let ([file (car file)] [buf (cadr file)])
            (when (write? file)
              (with-output-to-file file
                (lambda () (display buf) (flush-output))))))
        (message-box "Retrieve" "Retrieval done" parent)))))

;; ==========================================================================
(define multifile-dialog%
  (class frame%
    ;; ----------------------------------------------------------------------
    (let ([g (preferences:get geometry-key)])
      (super-new [label (format "~a Handin" handin-name)]
                 [stretchable-width #t] [stretchable-height #t]
                 [width  (and g (car g))] [height (and g (cadr g))]
                 [x (and g (caddr g))] [y (and g (cadddr g))]))
    (define main-pane (new horizontal-pane% [parent this]))
    (define buttons-pane
      (new vertical-pane% [parent main-pane] [stretchable-width #f]))
    (define files-pane
      (new vertical-pane% [parent main-pane]))

    ;; ----------------------------------------------------------------------
    (define (close)
      (preferences:set geometry-key
                       (list (send this get-width) (send this get-height)
                             (send this get-x) (send this get-y)))
      ;; (preferences:save)
      (send this show #f))
    (define/augment (on-close) (close))

    ;; ----------------------------------------------------------------------
    (new button% [parent buttons-pane]
         [label (make-object bitmap% (in-this-collection "icon.png"))]
         [callback (lambda _ (send-url web-address))])
    (new pane% [parent buttons-pane])
    (let ([button (lambda (label callback)
                    (new button% [label label] [parent buttons-pane]
                         [stretchable-width #t] [callback callback]))])
      (button "&Submit"   (lambda _ (do-submit)))
      (button "&Retrieve" (lambda _ (do-retrieve)))
      (button "A&ccount"  (lambda _ (new manage-handin-dialog% [parent this])))
      (when update (button "&Update" (lambda _ (update this #t))))
      (button "C&lose"    (lambda _ (close))))

    ;; ----------------------------------------------------------------------
    (define files-list
      (new list-box% [label "&Files:"] [parent files-pane]
           [style `(,selection-mode vertical-label)] [enabled #f]
           [choices '("Drag something here," "or click below")]
           [min-height 100] [stretchable-width #t] [stretchable-height #t]))
    (define auto-select
      (new combo-field% [label "&Auto:"] [parent files-pane]
           [init-value (preferences:get last-auto-key)]
           [choices selection-defaults]
           [callback (lambda (t e)
                       (when (eq? (send e get-event-type) 'text-field-enter)
                         (preferences:set last-auto-key (send t get-value))
                         (do-selections '() '())))]))
    (define directory-pane
      (new horizontal-pane% [parent files-pane]
           [stretchable-width #t] [stretchable-height #f]))
    (define choose-dir-button
      (new button% [label "&Directory:"] [parent directory-pane]
           [callback (lambda _ (choose-dir))]))
    (define current-working-directory
      (new text-field% [label #f] [parent directory-pane] [init-value ""]
           [callback (lambda (t e)
                       (when (eq? (send e get-event-type) 'text-field-enter)
                         (set-dir (send t get-value))
                         (send t focus)))]))
    (let ([ldir (preferences:get last-dir-key)])
      ;; don't use init-value since it can get very long
      (send current-working-directory set-value ldir)
      (unless (equal? "" ldir) (current-directory ldir)))

    ;; ----------------------------------------------------------------------
    (define dir-selected? #f)
    (define (->string x)
      (cond [(string? x) x]
            [(path? x)   (path->string x)]
            [(bytes? x)  (bytes->string/utf-8 x)]
            [(symbol? x) (symbol->string x)]
            [else (error '->string "bad input: ~e" x)]))
    (define (get-selected+unselected)
      (if (send files-list is-enabled?)
        (let ([selected (send files-list get-selections)])
          (let loop ([i (sub1 (send files-list get-number))] [s '()] [u '()])
            (if (<= 0 i)
              (let ([f (send files-list get-string i)])
                (if (memq i selected)
                  (loop (sub1 i) (cons f s) u)
                  (loop (sub1 i) s (cons f u))))
              (list (reverse s) (reverse u)))))
        '(() ())))
    (define (set-dir dir)
      (let* ([dir (and dir (->string dir))]
             [dir (and dir (not (equal? "" dir)) (directory-exists? dir)
                       (->string (simplify-path (path->complete-path
                                                 (build-path dir 'same)))))]
             [sel+unsel (if (equal? dir (->string (current-directory)))
                          (get-selected+unselected) '(() ()))])
        (when dir
          (current-directory dir)
          (set! dir-selected? #t)
          (let ([t current-working-directory])
            (send t set-value dir)
            (send (send t get-editor) select-all))
          (preferences:set last-dir-key dir)
          (send files-list set
                (sort (map ->string (filter file-exists? (directory-list)))
                      string<?))
          (if (< 0 (send files-list get-number))
            (begin (apply do-selections sel+unsel)
                   (send files-list enable #t)
                   (send files-list focus))
            (begin (send files-list append "no files")
                   (send files-list enable #f))))))
    (define (choose-dir)
      (let ([ldir (preferences:get last-dir-key)])
        (set-dir
         (get-directory "Choose a directory with files to submit" this
                        (and (not (equal? ldir "")) ldir)))))
    (define (refresh-dir)
      (when dir-selected? (set-dir (current-directory))))
    (define auto-glob+regexp '(#f #f))
    (define (globs->regexps glob)
      (if (equal? (car auto-glob+regexp) glob)
        (cdr auto-glob+regexp)
        (let* ([regexps
                (map (lambda (glob)
                       (let* ([re (regexp-replace* #rx"[.]" glob "\\\\.")]
                              [re (regexp-replace* #rx"[?]" re ".")]
                              [re (regexp-replace* #rx"[*]" re ".*")]
                              [re (string-append "^" re "$")]
                              [re (with-handlers ([void (lambda _ #f)])
                                    (regexp re))])
                         re))
                     (regexp-split ";" glob))]
               [regexps (filter values regexps)]
               [regexps (if (pair? regexps)
                          (lambda (file)
                            (ormap (lambda (re) (regexp-match re file))
                                   regexps))
                          (lambda (_) #f))])
          (set! auto-glob+regexp (cons glob regexps))
          regexps)))
    (define (do-selections selected unselected)
      (define glob (send auto-select get-value))
      (define regexps (globs->regexps glob))
      (let loop ([n (sub1 (send files-list get-number))])
        (when (<= 0 n)
          (let ([file (send files-list get-string n)])
            (send files-list select n
                  (cond [(member file selected) #t]
                        [(member file unselected) #f]
                        [else (regexps file)]))
            (loop (sub1 n)))))
      (send (if (send files-list is-enabled?) files-list choose-dir-button)
            focus))

    ;; ----------------------------------------------------------------------
    (define/override (on-drop-file path)
      (cond [(directory-exists? path) (set-dir path)]
            [(file-exists? path)
             (let-values ([(dir name dir?) (split-path path)])
               (set-dir dir)
               (cond [(send files-list find-string (->string name))
                      => (lambda (i) (send files-list select i #t))]))]))
    (define/override (on-subwindow-char w e)
      (define (next) (super on-subwindow-char w e))
      (case (send e get-key-code)
        [(escape) (close)]
        [(f5) (refresh-dir)]
        ;; [(#\space) (if (eq? w files-list)
        ;;              (printf ">>> ~s\n" (send files-list get-selection))
        ;;              (next))]
        [else (next)]))

    ;; ----------------------------------------------------------------------
    (define (do-submit)
      (let ([files (car (get-selected+unselected))])
        (if (pair? files)
          (let ([content (pack-files files)])
            (if content
              (new handin-frame% [parent this] [on-retrieve #f]
                   [content content])
              (message-box "Handin" "Error when packing files" this)))
          (message-box "Handin" "No files" this))))
    (define (do-retrieve)
      (if dir-selected?
        (new handin-frame% [parent this] [content #f]
             [on-retrieve (unpack-files this)])
        (message-box "Handin" "No directory selected" this)))

    ;; ----------------------------------------------------------------------
    (send this accept-drop-files #t)
    (send choose-dir-button focus)
    (send this show #t)
    (when update (update this))))

(provide multifile-handin)
(define (multifile-handin) (new multifile-dialog%))
