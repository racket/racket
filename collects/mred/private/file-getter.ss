(module file-getter mzscheme
  (require (lib "class.ss")
	   (lib "list.ss")
	   (prefix wx: "kernel.ss")
	   "helper.ss"
	   "mrtop.ss"
	   "mritem.ss"
	   "mrpanel.ss"
	   "mrtextfield.ss"
	   "messagebox.ss")
  (provide file-getter)

  (define last-visted-directory #f)

  (define (file-getter put? multi? dir? message parent directory filename)
    (define ok? #f)
    (define typed-name #f)
    (define dir
      (or (and directory
               (if (string? directory) (string->path directory) directory))
          last-visted-directory
          (current-directory)))
    (define f
      (make-object dialog%
        (if dir? "Select Directory" (if put? "Save" "Open")) parent 500 300))
    (define __
      (when message
        (let ([p (make-object vertical-pane% f)])
          (send p stretchable-height #f)
          (make-object message% (protect& message) p))))
    (define dir-pane (instantiate horizontal-pane% (f) (stretchable-height #f)))
    (define m (make-object message% (protect& (path->string dir)) dir-pane))
    (define lp (make-object horizontal-pane% f))
    (define (change-dir d)
      (let ([sd (send d get-string-selection)])
        (when sd
          (set! dir (simplify-path (build-path dir sd)))
          (reset-directory))))
    (define dirs
      (make-object
       (class list-box%
         (define/override (on-subwindow-char w e)
           (cond [(and (send e get-meta-down) (eq? (send e get-key-code) 'down))
                  (change-dir w)]
                 [(and (send e get-meta-down) (eq? (send e get-key-code) 'up))
                  (send dirs set-selection 0)
                  (change-dir dirs)]
                 [else (super on-subwindow-char w e)]))
         (super-instantiate ()))
       #f null lp
       (lambda (d e)
         (update-ok)
         (when (eq? (send e get-event-type) 'list-box-dclick) (change-dir d)))))
    (define dir-paths null)
    (define files
      (make-object
       list-box% #f null lp
       (lambda (d e)
         (update-ok)
         (when (eq? (send e get-event-type) 'list-box-dclick) (done)))
       (if multi? '(multiple) '(single))))
    (define file-paths null)
    (define (do-text-name)
      (let ([v (send dir-text get-value)])
        (if (or dir? (directory-exists? v))
          (begin (set! dir (string->path v)) (reset-directory))
          ;; Maybe specifies a file:
          (let-values ([(super file)
                        (with-handlers ([void #f])
                          (let-values ([(base name dir?) (split-path v)])
                            (let ([super (and (not dir?)
                                              (or (and (path? base)
                                                       (directory-exists? base)
                                                       base)
                                                  (and (eq? base 'relative)
                                                       (directory-exists? dir)
                                                       dir)))])
                              (if super
                                (values super name)
                                (values #f #f)))))])
            (if super
              (begin (set! dir super) (set! typed-name file) (done))
              (begin (set! dir (string->path v)) (reset-directory)))))))
    (define dir-text
      (make-object text-field% #f f
        (lambda (t e)
          (if (eq? (send e get-event-type) 'text-field-enter)
            (do-text-name)
            (begin ; typing in the box; disable the lists and enable ok
              (send dirs enable #f)
              (send files enable #f)
              (when create-button
                (send create-button enable #t))
              (send ok-button enable #t))))))
    (define bp (make-object horizontal-pane% f))
    (define dot-check
      (make-object check-box% "Show files/directories that start with \".\"" bp
        (lambda (b e) (reset-directory))))
    (define spacer (make-object vertical-pane% bp))
    (define create-button
      (and dir?
           (make-object button% "Create" bp
             (lambda (b e)
               (with-handlers ([void
                                (lambda (exn)
                                  (message-box "Error"
                                               (exn-message exn)
                                               f
                                               '(ok stop)))])
                 (make-directory (send dir-text get-value))
                 (do-text-name))))))
    (define cancel-button
      (make-object button% "Cancel" bp
        (lambda (b e) (set! ok? #f) (send f show #f))))
    (define ok-button
      (make-object button% (if dir? "Goto" "OK") bp
        (lambda (b e)
          (if (send (if dir? dirs files) is-enabled?)
            ;; normal mode
            (if dir?
              (change-dir dirs)
              (done))
            ;; handle typed text
            (do-text-name)))
        '(border)))
    (define (update-ok)
      (send ok-button enable
            (not (null? (send (if dir? dirs files) get-selections)))))
    (define select-this-dir
      (and dir? (make-object button% "<- &Select" dir-pane
                  (lambda (b e) (send f show #f) (done)))))
    (define (path-string-locale<? p1 p2)
      (string-locale<? (path->string p1) (path->string p2)))
    (define (reset-directory)
      (wx:begin-busy-cursor)
      (let ([dir-exists? (directory-exists? dir)])
        (send m set-label
              (protect&
               (if dir-exists?
                 (begin (unless directory (set! last-visted-directory dir))
                        (path->string dir))
                 (string-append "BAD DIRECTORY: " (path->string dir)))))
        (when select-this-dir (send select-this-dir enable dir-exists?))
        (when create-button (send create-button enable (not dir-exists?))))
      (send dir-text set-value (path->string dir))
      (let ([l (with-handlers ([void (lambda (x) null)]) (directory-list dir))]
            [dot? (send dot-check get-value)])
        (let-values ([(ds fs)
                      (let loop ([l l][ds null][fs null])
                        (cond
                         [(null? l)
                          (values (cons (string->path "..")
                                        (sort ds path-string-locale<?))
                                  (sort fs path-string-locale<?))]
                         [(and (not dot?)
                               (char=? (string-ref (path->string (car l)) 0)
                                       #\.))
                          (loop (cdr l) ds fs)]
                         [(file-exists? (build-path dir (car l)))
                          (loop (cdr l) ds (cons (car l) fs))]
                         [else (loop (cdr l) (cons (car l) ds) fs)]))])
          (set! dir-paths ds)
          (send dirs set (map path->string ds))
          (set! file-paths fs)
          (send files set (map path->string fs))
          (send dirs enable #t)
          (unless dir?
            (send files enable #t))
          (update-ok)
          (wx:end-busy-cursor))))
    (define (get-filename)
      (if dir?
        dir
        (let* ([mk (lambda (f) (simplify-path (build-path dir f)))]
               [l (map mk (if typed-name
                            (list typed-name)
                            (map (lambda (p)
                                   (list-ref (if dir? dir-paths file-paths) p))
                                 (send (if dir? dirs files) get-selections))))])
          (if multi? l (car l)))))
    (define (done)
      (let ([name (get-filename)])
        (unless (and put? (file-exists? name)
                     (eq? (message-box
                           "Warning"
                           (format "Replace ~s?" (path->string name))
                           f '(yes-no))
                          'no)
                     (set! typed-name #f))
          (set! ok? #t)
          (send f show #f))))
    (send bp stretchable-height #f)
    (send m stretchable-width #t)
    (reset-directory)
    (when filename
      (when (string? filename)
        (set! filename (string->path filename)))
      (let ([d (send dir-text get-value)])
        (send dir-text set-value (path->string (build-path d filename)))
        (set! typed-name filename)
        (send ok-button enable #t)))
    (when put? (send dir-text focus))
    (when dir? (send files enable #f))
    (send f center)
    (send f show #t)
    (and ok? (get-filename))))
