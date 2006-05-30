(module path-dialog mzscheme
  (require (lib "class.ss") (lib "list.ss") (lib "string.ss")
	   (prefix wx: "kernel.ss")
	   "helper.ss" "mrtop.ss" "mritem.ss" "mrpanel.ss" "mrtextfield.ss"
           "messagebox.ss" "mrmenu.ss")
  (provide path-dialog%)

  (define last-visted-directory #f)

  (define path-up-delta
    (let ([d (make-object wx:style-delta%)])
      (send d set-delta-foreground "BLUE")
      (send d set-delta 'change-underline #t)
      d))
  (define path-up-delta*
    (let ([d (make-object wx:style-delta%)])
      (send d set-delta-foreground "BLUE")
      (send d set-delta 'change-underline #t)
      d))

  ;; os-dependent stuff
  (define win? (eq? 'windows (system-type)))
  (define path-separator        (if win? "\\" "/"))
  (define up-dir-name           (string-append ".." path-separator))
  (define path-separator-re-str (if win? "/\\" "/"))
  (define (make-re . strs)
    (let ([l (map (lambda (s) (if (eq? / s) path-separator-re-str s)) strs)])
      (regexp (apply string-append l))))
  (define end-separators-re    (make-re "["/"]+$"))
  (define initial-/dir-part-re (make-re "^["/"]*([^"/"]+)"))
  (define initial-dir/-part-re (make-re "^.*?["/"]"))

  (define default-filters (if win? '(("Any" "*.*")) '(("Any" "*"))))

  (define simplify-path*
    (if win?
      (lambda (p . more)
        (apply simplify-path (regexp-replace*
                              #rx"/" (if (path? p) (path->string p) p) "\\\\")
               more))
      simplify-path))

  (define (build-path* dir path)
    (cond [(absolute-path? path) (if (string? path) (string->path path) path)]
          [(equal? "" path)      (if (string? dir)  (string->path dir)  dir)]
          [else (build-path dir path)]))

  (define (glob->regexps glob)
    (map glob->regexp (regexp-split #rx" *; *" glob)))

  ;; ==========================================================================
  (define path-dialog%
    (class dialog%
      ;; ----------------------------------------------------------------------
      ;; Arguments & Variables
      (init [message    #f]   ; message at the top of the dialog
            [parent     #f]   ; parent frame
            [directory  #f]   ; initial directory
            [filename   #f]   ; initial text for the input box
            [put?       #f]   ; selecting a new path?
            [dir?       #f]   ; are we selecting a directory?
            [existing?  #f]   ; must select an existing path?
            [new?       #f]   ; must select a new path?
            [multi?     #f]   ; selecting multiple paths?
            [can-mkdir? put?] ; is there a create-directory button?
            ;; (list-of (list filter-name filter-glob))
            ;; can use multiple globs with ";" separators
            ;; #f => disable, #t => use default
            [filters #t]
            ;; predicates are used to filter paths that are shown -- they are
            ;; applied on the file/dir name (as a string) (either as an
            ;; absolute path or relative while current-directory is set);
            ;; intended to be lightweight
            [show-file? #f]   ; a predicate for listing a file
            [show-dir?  #f]   ; a predicate for listing a directory
            ;; this predicate is used like the previous two, but it is used to
            ;; check if the "OK" button should be enabled or not; it is used
            ;; for both files and directories
            [ok? #f]
            ;; a verifier for the final result -- it will receive the result
            ;; that is about to be returned, and can return a different value
            ;; (any value) instead; if it throws an exception, an error dialog
            ;; is shown, and the dialog interaction continues (so it can be
            ;; used to verify results without dismissing the menu); it can also
            ;; throw a `(void)' value and the interaction continues but without
            ;; an error message; this is checked first, before the checks that
            ;; `exists?' or `new?' imply, but those checks are done on the
            ;; original result
            [guard #f]
            )

      (cond [(eq? filters #t) (set! filters default-filters)]
            [(null? filters)  (set! filters #f)])

      (when dir?
        (if show-file?
          (error 'path-dialog% "cannot use `show-file?' with `dir?'")
          (set! show-file? (lambda (_) #f)))
        (when filters (error 'path-dialog% "cannot use `filters' with `dir?'")))

      (define label
        (if dir?
          (if put? "Select New Directory" "Select Directory")
          (if put? "Save File" "Open File")))

      (super-new [label label] [parent parent] [width 300] [height 300])

      (define result #f)
      (define dir    #f)
      (define paths  '())

      ;; ----------------------------------------------------------------------
      ;; Utilities

      (define (return r)
        (set! result (if (string? r) (string->path r) r))
        (when (or (not r)
                  (not put?)
                  (not (or (file-exists? r) (directory-exists? r)))
                  (eq? 'yes (message-box "Warning"
                                         (format "Replace \"~a\"?" r)
                                         this '(yes-no))))
          (with-handlers ([exn? (lambda (e) (error-popup (exn-message e)))]
                          [void? (lambda (e) #t)]) ; do not return
            (when guard (set! result (guard result)))
            (send this show #f))))

      (define (error-popup fmt . args)
        (message-box (string-append label ": error") (apply format fmt args)
                     this '(stop ok)))

      (define (root? path)
        (let-values ([(base name dir?) (split-path path)]) (eq? base #f)))

      (define (name+dir path)
        (let*-values ([(path) (if (path? path) path (string->path path))]
                      [(path) (build-path* dir path)]
                      [(base name dir?) (split-path path)])
          (values (if dir? "" (path->string name))
                  (path->string (if dir? path base)))))

      (define (prefix? s1 s2 . proper?)
        (and ((if (and (pair? proper?) (car proper?)) < <=)
              (string-length s1) (string-length s2))
             (do ([i (sub1 (string-length s1)) (sub1 i)])
                 [(or (< i 0) (not (eq? (string-ref s1 i) (string-ref s2 i))))
                  (< i 0)])))

      ;; returns a list of strings, dirs first, all with a "/" suffix
      (define (sorted-dirlist dir)
        (define dotted? (send show-dotted get-value))
        (parameterize ([current-directory dir])
          (let loop ([paths (with-handlers ([void (lambda (x) '())])
                              (directory-list))]
                     [dirs '()] [files '()])
            (if (null? paths)
              (let ([ps (append! (sort! dirs string-locale<?)
                                 (sort! files string-locale<?))])
                (if (root? dir) ps (cons up-dir-name ps)))
              (let* ([path (car paths)]
                     [name (path->string (car paths))]
                     [paths (cdr paths)])
                (cond [(and (not dotted?) (eq? #\. (string-ref name 0)))
                       (loop paths dirs files)]
                      [(directory-exists? path)
                       (loop paths
                             (if (and show-dir? (not (show-dir? name)))
                               dirs (cons (string-append name path-separator)
                                          dirs))
                             files)]
                      [else (loop
                             paths dirs
                             (if (or (and globs
                                          (not (ormap (lambda (glob)
                                                        (regexp-match-positions
                                                         glob name))
                                                      globs)))
                                     (and show-file? (not (show-file? name))))
                               files (cons name files)))]))))))

      (define (find-completion str strs)
        (let ([strs (filter (lambda (p) (prefix? str p)) strs)])
          (cond [(null? strs) #f]
                [(null? (cdr strs)) (cons (car strs) #f)]
                [else (let ([m (apply min (map string-length strs))])
                        (do ([i (string-length str) (add1 i)])
                            [(or (>= i m)
                                 (not (let ([ch (string-ref (car strs) i)])
                                        (andmap (lambda (s)
                                                  (eq? ch (string-ref s i)))
                                                (cdr strs)))))
                             (cons (substring (car strs) 0 i) strs)]))])))

      ;; ----------------------------------------------------------------------
      ;; GUI Utilities & Handlers

      (define (set-dir newdir)
        (wx:begin-busy-cursor)
        (set! dir (simplify-path* (expand-path newdir)))
        (let ([dir (path->string dir)] [edit (send dir-text get-editor)])
          (send dir-text unlock)
          (send dir-text set-value dir)
          (let loop ([i 0])
            (let ([m (regexp-match-positions initial-/dir-part-re dir i)])
              (when m
                (send edit change-style path-up-delta (caadr m) (cdadr m))
                (send edit set-clickback (caar m) (cdar m)
                      (lambda _
                        (enter-text (substring dir 0 (cdar m)))
                        (do-enter)))
                (loop (cdar m)))))
          (send dir-text lock))
        (if (directory-exists? dir)
          (begin (set! paths (sorted-dirlist dir))
                 (send path-list set paths)
                 (send path-list enable #t))
          (begin (set! paths '())
                 (send path-list set (list "Bad Directory:" dir))
                 (send path-list enable #f)))
        (wx:end-busy-cursor))

      (define (is-ok? path isdir? isfile?)
        (let* ([exists? (or isfile? isdir?)]
               [exists-ok? (if dir? isdir? isfile?)])
          (and (cond [new? (not exists?)]
                     [existing? exists-ok?]
                     [put? (not isdir?)] ; maybe want to overwrite
                     [else #t])
               (or (not ok?) (ok? path)))))

      (define (set-ok?)
        ;; deal with button enabling disabling etc
        (let* ([value   (send text get-value)]
               [path    (build-path* dir value)]
               [empty?  (equal? "" value)]
               [isdir?  (directory-exists? path)]
               [isfile? (file-exists? path)])
          (send ok-button set-label
                (cond [(and dir? empty?) "Choose"]
                      [(and isdir? (not empty?)) "Go"]
                      [else "OK"]))
          (when create-button
            (send create-button enable (not (or isdir? isfile? empty?))))
          (send ok-button enable
                (or (and isdir? (not empty?)) ; go
                    (and (or (not empty?) dir?)
                         (parameterize ([current-directory dir])
                           (is-ok? (if empty? "." value) isdir? isfile?)))))))

      (define (new-selected-paths)
        (let ([sel (send path-list get-selections)])
          (when (and multi? (not dir?) (pair? sel) (pair? (cdr sel)))
            ;; make sure no dirs are chosen when files are
            (let ([dirs (filter (lambda (i)
                                  (regexp-match end-separators-re
                                                (send path-list get-string i)))
                                sel)])
              (unless (and (equal? dirs sel) (= 1 (length dirs)))
                (for-each (lambda (i) (send path-list select i #f))
                          dirs)
                (set! sel (remq* dirs sel)))))
          (cond [(and (pair? sel) (null? (cdr sel)))
                 (let* ([new (send path-list get-string (car sel))]
                        [new (regexp-replace end-separators-re new "")])
                   ;; if `multi?' is problematic on Windows since it needs the
                   ;; focus for the mouse wheel to move
                   (enter-text new enter-text-no-focus?))]
                [multi? (enter-text "" enter-text-no-focus?)])))
      (define enter-text-no-focus? (or win? multi?))

      (define (create-directory path) ; return #f on failure
        (with-handlers ([void (lambda (exn)
                                (error-popup (exn-message exn))
                                #f)])
          (let loop ([path path])
            (parameterize ([current-directory dir])
              (let-values ([(base file dir?) (split-path path)])
                (and (or (memq base '(#f relative)) ; root or here
                         (directory-exists? base)   ; or has base
                         (create-directory base))   ; or made base
                     (begin (make-directory path) #t))))))) ; => create

      (define (do-enter*)
        (let ([t (send text get-value)])
          (if (regexp-match #rx"[*?]" t)
            (begin (send text set-value "") (set-filter t))
            (do-enter))))
      (define (do-enter)
        (set-ok?)
        (when (send ok-button is-enabled?)
          (let* ([sel0  (send text get-value)]
                 [sel   (regexp-replace end-separators-re sel0 "")]
                 [/?    (not (equal? sel0 sel))]
                 [sel/  (string-append sel path-separator)]
                 [path  (and (not (equal? sel "")) (build-path* dir sel))]
                 [file? (and path (not /?) (member sel paths))]
                 [dir?  (and path (member sel/ paths))])
            (when (and path (not (or file? dir?)))
              ;; not in list, but maybe on disk (disk changed, hidden)
              (cond [(and (not /?) (file-exists? path)) (set! file? #t)]
                    [(directory-exists? path) (set! dir? #t)]))
            (cond [(not path) (return dir)] ; chose this directory -- return it
                  [dir? ; chose a directory -- go there
                   (set-dir path)
                   (unless (and (equal? up-dir-name sel/)
                                (member up-dir-name paths))
                     (send text* erase))
                   (send text* select-all)]
                  [file? (return path)] ; chose existing file -- return it
                  [(and /? (or (member sel paths) (file-exists? path)))
                   (error-popup "bad input: '~a' is a file" sel)]
                  [/? (error-popup "bad input: no '~a' directory~a" sel
                                   (if create-button
                                     " (use the NewDir button)"
                                     ""))]
                  [else (return path)])))) ; inexistent path -- return new file

      (define (enter-text str . no-focus?)
        (send text set-value str)
        (unless (and (pair? no-focus?) (car no-focus?)) (send text focus))
        (send text* select-all)
        (set-ok?))

      (define multi?? multi?) ; so it's accessible below
      (define/override (on-subwindow-char r e)
        (define key (send e get-key-code))
        (when (eq? r text) (text-callback)) ; make the text aware of all keys
        (cond [(and (eq? r text) (memq key '(up down)))
               ;; divert up/down in text to the path-list control
               ;; (must reimplement all list-box% functionality??)
               (let ([i (send path-list get-selection)]
                     [n (send path-list get-number)])
                 (case key
                   [(down)
                    (let ([i (if i (min (add1 i) (sub1 n)) 0)]
                          [v (sub1 (send path-list number-of-visible-items))])
                      (send path-list set-selection i)
                      (when (> (- i v) (send path-list get-first-visible-item))
                        (send path-list set-first-visible-item (- i v))))]
                   [(up)
                    (let ([i (if i (max (sub1 i) 0) 0)])
                      (send path-list set-selection i)
                      (when (< i (send path-list get-first-visible-item))
                        (send path-list set-first-visible-item i)))])
                 (new-selected-paths)
                 (when multi?? (send path-list focus))
                 #t)]
              ;; return is usually the same all over except for the path widget
              [(memq key '(#\return nupad-enter))
               (cond [(eq? r dir-text)
                      (let ([edit (send r get-editor)])
                        (send edit call-clickback
                              (send edit get-start-position)
                              (send edit get-end-position)))]
                     [(eq? r file-filter) (set-filter)]
                     [else (do-enter*)])
               #t]
              [else (super on-subwindow-char r e)]))

      ;;-----------------------------------------------------------------------
      ;; Delayed Filename Completion

      (define last-text-value "")
      (define last-text-start 0)
      (define last-text-end   0)
      (define last-text-completed? #f) ; is the last region a completion?

      (define (text-callback)
        (send completion-timer wait)
        (let* ([value   (send text get-value)]
               [len     (string-length value)]
               [start   (send text* get-start-position)]
               [end     (send text* get-end-position)]
               [change? (not (equal? value last-text-value))])
          (define (set-state!)
            (set! last-text-value value)
            (set! last-text-start start)
            (set! last-text-end   end)
            (set! last-text-completed? #f))
          (when change?
            ;; if entered an existing directory, go there
            (let loop ()
              (let ([m (regexp-match-positions initial-dir/-part-re value)])
                (when m
                  (let* ([pfx (substring value 0 (cdar m))]
                         [pfx (build-path* dir pfx)])
                    (when (directory-exists? pfx)
                      (set-dir pfx)
                      (set! last-text-value "")
                      (set! last-text-start 0)
                      (set! last-text-end   0)
                      (set! value (substring value (cdar m)))
                      (set! start (string-length value))
                      (set! end   start)
                      (set! change? #t)
                      (send text set-value value)
                      (send text* set-position start end #f #f 'local)
                      (loop)))))))
          (cond [;; a b c|D E F|
                 ;; a b c d|     => typed a character from completed text
                 (and last-text-completed?
                      (= len start end)
                      (< last-text-start len last-text-end)
                      (prefix? value last-text-value #t))
                 (send completion-timer stop)
                 ;; => shrink the completed part
                 (send text set-value last-text-value)
                 (set! change? #f)
                 (send text* set-position start last-text-end #f #f 'local)
                 (set! last-text-start start)]
                [;; a b c d
                 ;; a b c   => removed text
                 (prefix? value last-text-value #t)
                 ;; => disable pending completions if any
                 (send completion-timer stop)
                 (restore-path-list-state)
                 (set-state!)]
                [;;    a b c
                 ;;    any...| => typed some new text
                 (and (= start end len)
                      (not (prefix? value last-text-value)))
                 ;; => complete in a while
                 (send completion-timer reset)
                 (set-state!)]
                [;; something else changed? => stopped completing
                 (not (and (= last-text-start start)
                           (= last-text-end   end)
                           (not change?)))
                 (set-state!)]
                ;; otherwise there is no change
                )
          (when change? (set-ok?))))

      ;; Use the path-list for completion options
      (define saved-path-list-state #f)
      (define (save-path-list-state)
        (unless saved-path-list-state
          (set! saved-path-list-state
                (list (send path-list get-first-visible-item)
                      (send path-list get-selections)))))
      (define (restore-path-list-state)
        (when saved-path-list-state
          (send path-list set paths)
          (for-each (lambda (i) (send path-list select i))
                    (cadr saved-path-list-state))
          (send path-list set-first-visible-item (car saved-path-list-state))
          (set! saved-path-list-state #f)))

      ;; Timer for delaying completion
      (define completion-timer
        (new (class wx:timer% (super-new)
               (define running? #f)
               (define/override (start)
                 (set! running? #t)
                 (super start 400 #t))
               (define/override (stop)
                 (set! running? #f)
                 (super stop))
               (define/public (reset)
                 (when running? (send this stop))
                 (set! running? #t)
                 (super start 400 #t))
               (define/public (wait) ; delay if running
                 (when running? (reset)))
               (define/override (notify)
                 (set! running? #f)
                 (let* ([new (send text get-value)]
                        [found (and (not (equal? "" new))
                                    (find-completion new paths))]
                        [temp-paths? #f])
                   (when found
                     (let* ([options (cdr found)]
                            [found   (regexp-replace
                                      end-separators-re (car found) "")]
                            [start   (string-length new)]
                            [end     (string-length found)])
                       (when options
                         (save-path-list-state)
                         (send path-list set options)
                         (set! temp-paths? #t))
                       (unless (= start end)
                         (send text set-value found)
                         (send text* set-position start end #f #f 'local)
                         (set! last-text-value found)
                         (set! last-text-start start)
                         (set! last-text-end   end)
                         (set! last-text-completed? #t)))
                     (set-ok?))
                   (unless temp-paths? (restore-path-list-state)))))))

      ;; ----------------------------------------------------------------------
      ;; GUI Construction

      (when message
        (let ([p (make-object vertical-pane% this)])
          (send p stretchable-height #f)
          (make-object message% (protect& message) p)))

      (define dir-text
        (let ([c (class (if win? combo-field% text-field%) (super-new)
                    (define editor (send this get-editor))
                    (define/public (lock) (send editor lock #t))
                    (define/public (unlock) (send editor lock #f))
                    (when win?
                      (let ([m (send this get-menu)])
                        (for-each (lambda (r)
                                    (define l (path->string r))
                                    (make-object menu-item% l m
                                                 (lambda _ (enter-text l))))
                                  (filesystem-root-list))))
                    (lock))])
          (if win?
            (new c [label #f] [parent this] [init-value ""] [choices '()])
            (new c [label #f] [parent this] [init-value ""]))))

      (define path-list
        (new (class list-box%
               ;; make sure that if the focus is here, the text is synced
               (define/override (on-focus on?) (when on? (new-selected-paths)))
               (super-new))
             [parent this] [label #f] [choices '()]
             [min-height 100] [stretchable-width #t] [stretchable-height #t]
             [style (if multi? '(multiple) '(single))]
             [callback (lambda (t e)
                         (case (send e get-event-type)
                           [(list-box) (new-selected-paths)]
                           [(list-box-dclick) (do-enter)]))]))

      (define text
        (new text-field% [parent this] [label #f]
          [init-value (cond [(path? filename) (path->string filename)]
                            [(string? filename) filename]
                            [else ""])]
          [callback (lambda (t e)
                      (if (eq? (send e get-event-type) 'text-field-enter)
                        (do-enter*) (text-callback)))]))
      (define text* (send text get-editor))
      (send text* select-all)

      (define globs (and filters (glob->regexps (cadar filters))))
      (define (set-filter . new)
        (let ([filt (if (pair? new) (car new) (send file-filter get-value))])
          (when (pair? new) (send file-filter set-value filt))
          (set! globs (and (not (equal? "" filt)) (glob->regexps filt)))
          (set-dir dir)
          (send text focus)))
      (define file-filter
        (and filters
             (let* ([c (new combo-field% [parent this] [label "Filter:"]
                            [choices '()] [callback void]
                            [init-value (cadar filters)])]
                    [m (send c get-menu)])
               (for-each (lambda (f)
                           (make-object menu-item% (apply format "~a (~a)" f) m
                                        (lambda _ (set-filter (cadr f)))))
                         filters)
               c)))

      (define bp1
        (new horizontal-pane% [parent this] [stretchable-height #f]
             [alignment '(left center)]))
      (define show-dotted
        (new check-box% [parent bp1]
          [label "Show files/directories that start with \".\""]
          [callback (lambda (b e) (set-dir dir))]))
      (make-object vertical-pane% bp1) ; spacer
      (define create-button
        (and can-mkdir?
             (make-object button% "NewDir" bp1
               (lambda (b e)
                 (let ([path (simplify-path* (send text get-value) #f)])
                   (and (create-directory path)
                        (do-enter)))))))

      (define bp2
        (new horizontal-pane% [parent this] [stretchable-height #f]
             [alignment '(right center)]))
      (define cancel-button
        (make-object button% "Cancel" bp2 (lambda (b e) (return #f))))
      (define ok-button
        (make-object button% "Choose" bp2 (lambda (b e) (do-enter)) '(border)))
      (send ok-button set-label
            (if (and dir? (equal? "" (send text get-value))) "Choose" "OK"))

      ;; ----------------------------------------------------------------------
      ;; Initialization & `run'

      (set-dir (or directory last-visted-directory (current-directory)))
      (text-callback)
      (set-ok?)

      (send this center)
      (send text focus)

      (define/public (run)
        (send this show #t)
        (send completion-timer stop)
        result)))

  )
