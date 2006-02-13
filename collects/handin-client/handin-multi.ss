(module handin-multi mzscheme
  (require (lib "class.ss") (lib "list.ss") (lib "string.ss") (lib "port.ss")
           (lib "unitsig.ss") (lib "mred.ss" "mred")
           (lib "framework.ss" "framework") (lib "external.ss" "browser")
           "info.ss" "client-gui.ss" (only "updater.ss" update))

  (define handin-name       (#%info-lookup 'name))
  (define this-collection   (#%info-lookup 'collection))
  (define web-address       (#%info-lookup 'web-address))
  (define selection-mode    (#%info-lookup 'selection-mode))
  (define selection-defaults
    (let ([sd (#%info-lookup 'selection-default)])
      (if (string? sd) (list sd) sd)))
  (define (make-key sfx)
    (string->symbol (format "~a:~a" (string-downcase handin-name) sfx)))
  (define last-dir-key (make-key 'handin-last-dir))
  (preferences:set-default last-dir-key "" string?)
  (define last-auto-key (make-key 'handin-last-auto))
  (preferences:set-default last-auto-key (car selection-defaults) string?)
  (define geometry-key (make-key 'handin-geometry))
  (preferences:set-default geometry-key #f void)

  ;; ==========================================================================
  (define magic #"<<<MULTI-SUBMISSION-FILE>>>")
  (define (pack-files files)
    (let/ec return
      (parameterize ([current-output-port (open-output-bytes)])
        (printf "~a\n" magic)
        (for-each
         (lambda (file)
           (let ([size (and (file-exists? file) (file-size file))])
             (unless size (return #f))
             (let ([buf (with-input-from-file file
                          (lambda () (read-bytes size)))])
               (unless (equal? size (bytes-length buf)) (return #f))
               (write (list file buf)) (newline))))
         files)
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
                     (reverse! files) (loop (cons f files)))))]
              [overwrite-all? #f])
          (define (write? file)
            (define (del) (delete-file file) #t)
            (cond
             [(not (file-exists? file)) #t]
             [overwrite-all? (del)]
             [else (case (message-box/custom
                          "Retrieve"
                          (format "~s already exists, overwrite?" file)
                          "&Yes" "&No" "Yes to &All" parent
                          '(default=2 caution) 4)
                     [(1) (del)]
                     [(2) #f]
                     [(3) (set! overwrite-all? #t) (del)]
                     [(4) (error* "Aborting...")])]))
          (unless (and (list? files)
                       (andmap (lambda (x)
                                 (and (list? x) (= 2 (length x))
                                      (string? (car x)) (bytes? (cadr x))))
                               files))
            (error* "Error in retrieved content: bad format"))
          (for-each (lambda (file)
                      (let ([file (car file)] [buf (cadr file)])
                        (when (write? file)
                          (with-output-to-file file
                            (lambda () (display buf) (flush-output))))))
                    files)
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
        (preferences:save)
        (send this show #f))

      ;; ----------------------------------------------------------------------
      (new button% [parent buttons-pane]
           [label (make-object bitmap%
                               (build-path (collection-path this-collection)
                                           "icon.png"))]
           [callback (lambda _ (send-url web-address))])
      (new pane% [parent buttons-pane])
      (let ([button (lambda (label callback)
                      (new button% [label label] [parent buttons-pane]
                           [stretchable-width #t] [callback callback]))])
        (button "&Submit"   (lambda _ (do-submit)))
        (button "&Retrieve" (lambda _ (do-retrieve)))
        (button "A&ccount"  (lambda _ (manage-handin-account this)))
        (button "&Update"   (lambda _ (update this #t)))
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
                           (do-auto-select #t)))]))
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
      (define (get-files)
        (if (send files-list is-enabled?)
          (map (lambda (i) (send files-list get-string i))
               (send files-list get-selections))
          '()))
      (define (set-dir dir)
        (let* ([dir (and dir (->string dir))]
               [dir (and dir (not (equal? "" dir)) (directory-exists? dir)
                         (->string (simplify-path (path->complete-path
                                                   (build-path dir 'same)))))]
               [selected (if (equal? dir (->string (current-directory)))
                           (get-files) '())])
          (when dir
            (current-directory dir)
            (set! dir-selected? #t)
            (let ([t current-working-directory])
              (send t set-value dir)
              (send (send t get-editor) select-all))
            (preferences:set last-dir-key dir)
            (send files-list clear)
            (for-each (lambda (f)
                        (when (file-exists? f)
                          (send files-list append f)
                          (when (member f selected)
                            (send files-list select
                                  (sub1 (send files-list get-number))))))
                      (mergesort (map ->string (directory-list)) string<?))
            (if (< 0 (send files-list get-number))
              (begin (do-auto-select #f)
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
      (define (do-auto-select exactly?)
        (define glob (send auto-select get-value))
        (define regexps
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
                   [regexps (and (pair? regexps)
                                 (lambda (file)
                                   (ormap (lambda (re) (regexp-match re file))
                                          regexps)))])
              (set! auto-glob+regexp (cons glob regexps))
              regexps)))
        (when regexps
          (let loop ([n (sub1 (send files-list get-number))])
            (when (<= 0 n)
              (let* ([file    (send files-list get-string n)]
                     [select? (regexps file)])
                (when (or select? exactly?) (send files-list select n select?))
                (loop (sub1 n)))))
          (send (if (send files-list is-enabled?) files-list choose-dir-button)
                focus)))

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
        (let ([files (get-files)])
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
      (update this)))

  (provide multifile-handin)
  (define (multifile-handin) (new multifile-dialog%))

  )
