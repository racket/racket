(module path-dialog mzscheme
  (require mzlib/class mzlib/list mzlib/string mzlib/file
           (prefix wx: "kernel.rkt")
           (prefix wx: racket/snip/private/style)
           racket/snip/private/prefs
           "helper.rkt" "mrtop.rkt" "mritem.rkt" "mrpanel.rkt"
           "mrtextfield.rkt" "messagebox.rkt" "mrmenu.rkt"
           (only racket/base compose1))
  (provide path-dialog%)

  (define last-visted-directory #f)

  (define path-up-delta
    (let ([d (make-object wx:style-delta%)])
      (send d set-delta-foreground "BLUE")
      (send d set-delta 'change-underline #t)
      d))

  ;; os-dependent stuff
  (define win? (eq? 'windows (system-type)))
  (define path-separator        (if win? "\\" "/"))
  (define path-separator-rx-str (if win? "/\\" "/"))
  (define (make-rx . strs)
    (let ([l (map (lambda (s) (if (eq? / s) path-separator-rx-str s)) strs)])
      (regexp (apply string-append l))))
  (define end-separators-rx    (make-rx "["/"]+$"))
  (define initial-/dir-part-rx (make-rx "^["/"]*([^"/"]+)"))
  (define initial-dir/-part-rx (make-rx "^.*?["/"]"))
  (define isfilter-rx #rx"[*?]")

  (define default-filters (if win? '(("Any" "*.*")) '(("Any" "*"))))

  (define (expand-path* p)
    (if (equal? "" p)
      p
      ;; expand-user-path throws an exception on bad usernames
      (with-handlers ([exn:fail:filesystem? (lambda (e) p)])
        (expand-user-path p))))

  (define simplify-path*
    (if win?
      (lambda (p . more)
        (apply simplify-path (regexp-replace*
                              #rx"/" (if (path? p) (path->string p) p) "\\\\")
               more))
      (lambda (p . more)
        (apply simplify-path (expand-path* p) more))))

  (define directory-exists*? (compose1 directory-exists? expand-path*))
  (define file-exists*?      (compose1 file-exists? expand-path*))
  (define absolute-path*?    (compose1 absolute-path? expand-path*))

  (define (build-path* dir path)
    (cond [(absolute-path*? path) (if (string? path) (string->path path) path)]
          [(equal? "" path)       (if (string? dir)  (string->path dir)  dir)]
          [else (build-path dir path)]))

  ;; returns a list of a glob-regexp-list and another one without hiding dots
  (define (glob->2regexps glob)
    (let ([globs (remove* '("") (regexp-split #rx" *; *" glob))])
      (map (lambda (hide-dots?)
             (map (lambda (glob) (glob->regexp glob hide-dots?)) globs))
           '(#t #f))))

  (define-struct pname (path string isdir? okstring? nulstring))
  (define (path->pname path isdir?)
    (let* ([name (if (member (path->string path) '("." ".."))
                   (path->string path) ; avoid errors
                   (path-element->string path))]
           [name (regexp-replace end-separators-rx name "")]
           [name (if (<= 199 (string-length name))
                   (string-append (substring name 0 195) "...")
                   name)]
           [name/ (if isdir? (string-append name path-separator) name)]
           [goodstr?  (equal? path (string->path name))]
           [no-globs? (not (regexp-match-positions isfilter-rx name))])
      (make-pname path name/ (and isdir? #t) ; must be a strict boolean
                  ;; most paths are `ok' strings, the ones that are not are
                  ;; * paths where the string name does not correspond to the
                  ;;   path, eg, a sequence of bytes that interprets badly when
                  ;;   using UTF-8
                  ;; * paths that are about 200 characters or longer (the
                  ;;   displayed name must be truncated) (part of the above)
                  ;; * paths that have an initial "~" in them (also part of the
                  ;;   above)
                  ;; * paths that contain `*' and `?', since they will be
                  ;;   considered as filters
                  ;; in these cases, the string is shown in the path-list gui,
                  ;; but cannot be entered in the filename field
                  (and goodstr? no-globs?)
                  ;; nulstring is usually the same as string, except when
                  ;; okstring? is #f -- in this case bad characters will be
                  ;; "\0"s instead of "?"s, globbing chars are also turned to
                  ;; "\0"s -- useful for completion, marking the place where
                  ;; things become untextual
                  ;; (note: use "\u01" due to a bug with "\0" for error char)
                  (let* ([bad #\u01]
                         [nulstr (if goodstr?
                                   name/
                                   (let ([s (bytes->string/locale
                                             (path->bytes path) bad)])
                                     (if isdir?
                                       (string-append s path-separator)
                                       s)))]
                         [nulstr (if no-globs?
                                   nulstr
                                   (regexp-replace* isfilter-rx nulstr
                                                    (string bad)))])
                    nulstr))))
  (define up-dir-pname (path->pname (string->path "..") #t))
  ;; compare two pnames, use the strings according to the locael, dirs come 1st
  (define (pname<? p1 p2)
    (let ([d1 (pname-isdir? p1)] [d2 (pname-isdir? p2)])
      (if (eq? d1 d2)
        (string-locale<? (pname-string p1) (pname-string p2))
        d1))) ; directories come first

  (define (prefix? s1 s2 . proper?)
    (and ((if (and (pair? proper?) (car proper?)) < <=)
          (string-length s1) (string-length s2))
         (do ([i (sub1 (string-length s1)) (sub1 i)])
             [(or (< i 0) (not (eq? (string-ref s1 i) (string-ref s2 i))))
              (< i 0)])))

  ;; find a completion for str in the strs list
  ;; * no strings   => #f
  ;; * single match => (cons matching-string #f)
  ;; * else         => (cons matching-prefix alternatives)
  ;; the first elt of a cons is often longer that the input string
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
  ;; like the above, but removes a suffix that indicates nonstring stuff
  (define (find-completion* str strs)
    (let ([r (find-completion str strs)])
      (and r (cons (regexp-replace #rx"\u01.*$" (car r) "") (cdr r)))))

  ;; ==========================================================================
  (define path-dialog%
    (class dialog%
      ;; ----------------------------------------------------------------------
      ;; Arguments & Variables
      (init [label      #f]   ; dialog title
            [message    #f]   ; message at the top of the dialog
            [parent     #f]   ; parent frame
            [directory  #f]   ; initial directory
            [filename   #f]   ; initial text for the input box
            [put?       #f]   ; selecting a new path?
            [dir?       #f]   ; are we selecting a directory?
            [existing?  (not put?)] ; must select an existing path?
            [new?       #f]   ; must select a new path?
            [multi?     #f]   ; selecting multiple paths?
            [can-mkdir? put?] ; is there a create-directory button?
            ;; (list-of (list filter-name filter-glob))
            ;; can use multiple globs with ";" separators
            ;; #f => disable, #t => use default
            [filters #t]
            ;; predicates that are used to filter paths that are shown -- they
            ;; are applied on the file/dir name (as a string) while
            ;; current-directory is set; intended to be lightweight
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
            ;; an error message
            [guard #f]
            )

      (cond [(eq? filters #t) (set! filters default-filters)]
            [(null? filters)  (set! filters #f)])

      (when (and new? existing?)
        (error 'path-dialog% "cannot use `new?' with `existing?'"))

      (when dir?
        (if show-file?
          (error 'path-dialog% "cannot use `show-file?' with `dir?'")
          (set! show-file? (lambda (_) #f)))
        (when filters (error 'path-dialog% "cannot use `filters' with `dir?'")))

      (unless label
        (set! label (if dir?
                      (if put? "Select New Directory" "Select Directory")
                      (if put? "Save File" "Open File"))))

      (define size
        (let ([s (get-preference* 'mred:path-dialog:size (lambda () #f))])
          (or (and (list? s) (= 2 (length s)) (andmap integer? s) s)
              '(300 300))))

      (super-new [label label] [parent parent]
                 [width (car size)] [height (cadr size)]
                 [style '(resize-border)])

      (define result #f)
      (define dir    #f)
      (define pnames '())
      (define pnames-nulstrings '()) ; cache nulstrings of pnames

      ;; ----------------------------------------------------------------------
      ;; Utilities

      (define (return r)
        (set! result (let loop ([r r])
                       (cond [(list? r) (map loop r)]
                             [(path? r) r]
                             [(string? r) (string->path r)]
                             [(not r) r]
                             [else (error-popup "internal error!")])))
        (when (or (not r)    ; when returning something
                  multi?     ; single
                  (not put?) ; and we're in put? mode
                  (not (or (file-exists*? r) ; and it exists
                           (directory-exists*? r)))
                  ;; then ask about continuing
                  (eq? 'yes (message-box "Warning"
                                         (format "Replace \"~a\"?" r)
                                         this '(yes-no))))
          (with-handlers ([exn? (lambda (e) (error-popup (exn-message e)))]
                          [void? (lambda (e) #t)]) ; no error, and no return
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

      ;; returns a list of strings, dirs first, all with a "/" suffix
      (define (sorted-dirlist dir)
        (define dotted? (send show-dotted get-value))
        (parameterize ([current-directory dir])
          (let loop ([paths (with-handlers ([void (lambda (x) '())])
                              (directory-list))]
                     [pnames '()])
            (if (null? paths)
              (let ([ps (sort pnames pname<?)])
                (if (root? dir) ps (cons up-dir-pname ps)))
              (let* ([path   (car paths)]
                     [paths  (cdr paths)]
                     [isdir? (directory-exists*? path)]
                     [pname  (path->pname path isdir?)]
                     [name   (pname-string pname)])
                (loop paths
                      (cond [(and (not dotted?)
                                  ;; globs used for no-dots in files
                                  (or (not globs) isdir?)
                                  (eq? #\. (string-ref name 0)))
                             pnames]
                            [(if isdir?
                               (or (not show-dir?) (show-dir? name))
                               (and (or (not globs)
                                        (ormap (lambda (glob)
                                                 (regexp-match-positions
                                                  glob name))
                                               ((if dotted? cadr car)
                                                globs)))
                                    (or (not show-file?)
                                        (show-file? name))))
                             (cons pname pnames)]
                            [else pnames])))))))

      ;; ----------------------------------------------------------------------
      ;; GUI Utilities & Handlers

      ;; may be set if there is a value that is non-textual (a pname entry
      ;; without an okstring, a directly clicked path)
      (define nonstring-path #f)

      (define set-path-list-pnames
        (case-lambda
          [() (set-path-list-pnames pnames)]
          [(pnames)
           (send path-list set (map pname-string pnames))
           (let loop ([pnames pnames] [i 0])
             (unless (null? pnames)
               (send path-list set-data i (car pnames))
               (loop (cdr pnames) (add1 i))))]))

      (define (set-dir newdir)
        (wx:begin-busy-cursor)
        (set! dir (simplify-path* newdir))
        ;; get a list of upward paths, display it with clickbacks
        (let* ([sep (cons path-separator #f)]
               [items ; a list of (cons name path)
                (let loop ([dir dir] [r '()])
                  (let-values ([(base name _) (split-path dir)])
                    (let ([n (cons (path->string name) dir)])
                      (if (path? base)
                        (loop base (list* n sep r))
                        (cons n r)))))]
               [edit (send dir-text get-editor)])
          (send dir-text unlock)
          (send dir-text set-value (apply string-append (map car items)))
          (let loop ([i 0] [items items])
            (unless (null? items)
              (let ([j (+ i (string-length (caar items)))])
                (when (cdar items)
                  (unless (zero? i) (send edit change-style path-up-delta i j))
                  (send edit set-clickback i
                        (if (zero? i) j (add1 j)) ; inclucde the sep
                        (lambda _ (enter-text (cdar items)) (do-enter))))
                (loop j (cdr items)))))
          (send dir-text lock))
        (clear-path-list-state)
        (if (directory-exists*? dir)
          (begin (set! pnames (sorted-dirlist dir))
                 (set! pnames-nulstrings (map pname-nulstring pnames))
                 (set-path-list-pnames)
                 (send path-list enable #t))
          (begin (set! pnames '())
                 (set! pnames-nulstrings '())
                 (send path-list set (list "Bad Directory:" (path->string dir)))
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
        ;; deal with button enabling/disabling etc
        (define-values (value path)
          (if nonstring-path
            (values (path->string nonstring-path) nonstring-path)
            (let ([val (send text get-value)])
              (values val (build-path* dir val)))))
        (let* ([empty? (equal? "" value)]
               [selections (send path-list get-selections)])
          ;; turn off other selections if there is a value
          (when (and multi? (not empty?))
            (let ([on? (if nonstring-path
                         (lambda (i)
                           (equal? nonstring-path
                                   (pname-path (send path-list get-data i))))
                         (lambda (i)
                           (let ([s (send path-list get-string i)])
                             (and (prefix? value s) ; optimize: save on replaces
                                  (equal? value (regexp-replace
                                                 end-separators-rx s ""))))))])
            (for-each (lambda (i) (send path-list select i (on? i)))
                      selections))
            (set! selections (send path-list get-selections)))
          (let* ([seln    (length selections)]
                 [isdir?  (directory-exists*? path)]
                 [isfile? (file-exists*? path)]
                 [choose? (and dir? empty? (= 0 seln))]
                 [go?     (and isdir? (not empty?) (<= seln 1))])
            ;; set ok-button label
            (send ok-button set-label
                  (cond [choose? "Choose"]
                        [go?     "Go"]
                        [else    "OK"]))
            (when create-button
              (send create-button enable
                    (not (or isdir? isfile? empty? (<= 1 seln)))))
            ;; decide if the ok-button is enabled
            (send ok-button enable
              (or choose? go?
                  (parameterize ([current-directory dir])
                    (if (<= seln 1)
                      (is-ok? (if empty? "." value) isdir? isfile?)
                      (andmap (lambda (i)
                                (let ([s (send path-list get-string i)])
                                  (is-ok?
                                   s (directory-exists*? s) (file-exists*? s))))
                              selections))))))))

      (define (new-selected-paths)
        (let ([sel (send path-list get-selections)])
          (when (and multi? (not dir?) (pair? sel) (pair? (cdr sel)))
            ;; make sure no dirs are chosen when files are
            (let ([dirs (filter (lambda (i)
                                  (pname-isdir? (send path-list get-data i)))
                                sel)])
              (unless (and (equal? dirs sel) (= 1 (length dirs)))
                (for-each (lambda (i) (send path-list select i #f))
                          dirs)
                (set! sel (remq* dirs sel)))))
          (cond [(and (pair? sel) (null? (cdr sel)))
                 (let* ([pname (send path-list get-data (car sel))]
                        [new (cond [(not (pname-okstring? pname))
                                    (pname-path pname)]
                                   [(pname-isdir? pname)
                                    (regexp-replace end-separators-rx
                                                    (pname-string pname) "")]
                                   [else (pname-string pname)])])
                   (enter-text new enter-text-no-focus?))]
                [multi? (enter-text "" enter-text-no-focus?)])))
      ;; uncomment the following to keep the focus on the text
      ;;   (`multi?' is problematic on Windows since it needs the focus for the
      ;;   mouse wheel to move)
      (define enter-text-no-focus? #t) ; (or win? multi?))

      (define (create-directory path) ; return #f on failure
        (with-handlers ([void (lambda (exn)
                                (error-popup (exn-message exn))
                                #f)])
          (let loop ([path path])
            (parameterize ([current-directory dir])
              (let-values ([(base file dir?) (split-path path)])
                (and (or (memq base '(#f relative)) ; root or here
                         (directory-exists*? base)  ; or has base
                         (create-directory base))   ; or made base
                     (begin (make-directory* path) #t))))))) ; => create

      (define (do-enter*)
        (let ([t (send text get-value)])
          (if (and file-filter (regexp-match isfilter-rx t))
            (begin (send text set-value "") (set-filter t))
            (do-enter))))
      (define (do-enter)
        (set-ok?)
        (when (send ok-button is-enabled?)
          (let* ([sel0 (if nonstring-path
                         (path->string nonstring-path)
                         (send text get-value))]
                 [sel  (regexp-replace end-separators-rx sel0 "")]
                 [/?   (not (equal? sel0 sel))]
                 [sel/ (string-append sel path-separator)]
                 [path (build-path* dir (or nonstring-path sel))]
                 [isfile? (and (not /?) (file-exists*? path))]
                 [isdir?  (directory-exists*? path)]
                 [selections (send path-list get-selections)]
                 [many?   (and multi? (equal? "" sel)
                               (<= 1 (length selections)))])
            (cond [(and isdir? (not (equal? "" sel0)))
                   ;; chose a directory -- go there
                   (set-dir path) (send* text* (erase) (select-all)) (set-ok?)]
                  [(and /? isfile?)
                   (error-popup "bad input: '~a' is a file" sel)]
                  [(and /? (not isdir?))
                   (error-popup "bad input: no '~a' directory~a" sel
                                (if create-button
                                  " (use the NewDir button)" ""))]
                  [many? (return (map (lambda (i)
                                        (let ([s (send path-list get-data i)])
                                          (build-path* dir (pname-path s))))
                                      selections))]
                  [multi? (return (list path))]
                  [else (return path)]))))

      (define (enter-text str . no-focus?)
        ;; str can actually be a path if selected an entry with a non-okstring,
        ;; or a path that is set directly (eg, from the top path navigation
        ;; thing)
        (set! nonstring-path (and (path? str) str))
        (send text set-value (if nonstring-path "" str))
        (reset-last-text-state)
        (unless (and (pair? no-focus?) (car no-focus?)) (send text focus))
        (send text* select-all)
        (set-ok?))

      (define multi?? multi?) ; so it's accessible below
      (define/override (on-subwindow-char r e)
        (define key (send e get-key-code))
        (when (eq? r text) (text-callback key)) ; make `text' aware of all keys
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
              [(memq key '(#\return numpad-enter))
               (cond [(eq? r dir-text)
                      (let ([edit (send r get-editor)])
                        (send edit call-clickback
                              (send edit get-start-position)
                              (send edit get-end-position)))]
                     [(eq? r file-filter) (set-filter #f)]
                     [else (do-enter*)])
               #t]
              [else (super on-subwindow-char r e)]))

      ;; remember size
      (define/override (on-size width height)
        (let ([new (list width height)])
          (unless (equal? new size)
            (set! size new)
            (put-preferences* '(mred:path-dialog:size) (list size)))))

      ;;-----------------------------------------------------------------------
      ;; Delayed Filename Completion

      (define last-text-value "")
      (define last-text-start 0)
      (define last-text-end   0)
      (define last-text-completed? #f) ; is the last region a completion?
      (define completion-disabled? #f) ; are we allowed to insert completions?

      (define (reset-last-text-state)
        (set! last-text-value "")
        (set! last-text-start 0)
        (set! last-text-end   0)
        (set! last-text-completed? #f))

      (define (text-callback . key)
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
            (set! last-text-completed? #f)
            (set! completion-disabled? #f))
          (set! nonstring-path #f)
          (when change?
            ;; if entered an existing directory, go there
            (let loop ()
              (let ([m (regexp-match-positions initial-dir/-part-rx value)])
                (when m
                  (let* ([pfx (substring value 0 (cdar m))]
                         [pfx (build-path* dir pfx)])
                    (when (directory-exists*? pfx)
                      (set-dir pfx)
                      (reset-last-text-state)
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
                      (<= last-text-start len last-text-end)
                      (prefix? value last-text-value #t))
                 ;; in some situations this is called with the same text before
                 ;; the new character is inserted -- do nothing in that case,
                 ;; not even set-ok?
                 (when (< last-text-start len last-text-end)
                   (send completion-timer stop)
                   ;; => shrink the completed part
                   (send text set-value last-text-value)
                   (send text* set-position start last-text-end #f #f 'local)
                   (set! last-text-start start))
                 (set! change? #f)]
                [;; a b c d
                 ;; a b c   => backspaced some text
                 (and (= start end len) (prefix? value last-text-value #t))
                 ;; filter as usual, but disallow completion text
                 ;;   (otherwise it will pop up again, annoyingly)
                 (send completion-timer reset)
                 (set-state!)
                 (set! completion-disabled? #t)]
                [;; a b c
                 ;; any...| => typed some new text
                 (and (= start end len) change?)
                 (send completion-timer reset)
                 (set-state!)]
                [;; something else changed? => stopped completing
                 (not (and (= last-text-start start)
                           (= last-text-end   end)
                           (not change?)))
                 (set-state!)]
                [;; hit right at end of text and no change
                 (and (= len start end) (pair? key) (eq? 'right (car key)))
                 (send completion-timer fire)]
                ;; otherwise there is no change and nothing to do
                )
          (when change? (set-ok?))))

      ;; Use the path-list for completion options
      (define saved-path-list-state #f)
      (define (clear-path-list-state)
        (set! saved-path-list-state #f))
      (define (save-path-list-state)
        (unless saved-path-list-state
          (set! saved-path-list-state
                (list (send path-list get-first-visible-item)
                      (send path-list get-selections)))))
      (define (restore-path-list-state)
        (when saved-path-list-state
          (set-path-list-pnames)
          (for-each (lambda (i) (send path-list select i))
                    (cadr saved-path-list-state))
          (send path-list
                set-first-visible-item (car saved-path-list-state))
          (set! saved-path-list-state #f)))

      ;; Timer for delaying completion
      (define completion-timer
        (new (class wx:timer% (super-new)
               (define delay 400)
               (define running? #f)
               (define/override (start)
                 (set! running? #t)
                 (super start delay #t))
               (define/override (stop)
                 (set! running? #f)
                 (super stop))
               (define/public (reset)
                 (when running? (send this stop))
                 (start))
               (define/public (wait) ; delay if running
                 (when running? (reset)))
               (define/public (fire)
                 (stop)
                 (notify))
               (define/override (notify)
                 (set! running? #f)
                 (let* ([new (send text get-value)]
                        [found (and (not (equal? "" new))
                                    (find-completion* new pnames-nulstrings))]
                        [temp-paths? #f])
                   (when found
                     (let* ([options (cdr found)]
                            [found   (regexp-replace
                                      end-separators-rx (car found) "")]
                            [start   (string-length new)]
                            [end     (string-length found)])
                       (when options
                         (save-path-list-state)
                         (set-path-list-pnames
                          (filter (lambda (p)
                                    (member (pname-nulstring p) options))
                                  pnames))
                         (set! temp-paths? #t))
                       (unless (or completion-disabled? (= start end))
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
        (let* ([c%
                (if win?
                    (class combo-field%
                      (super-new [choices '()])
                      (define/override (on-popup e)
                        (let ([m (send this get-menu)])
                          (for-each (lambda (i) (send i delete))
                                    (send m get-items))
                          (for-each (lambda (r)
                                      (define l (path->string r))
                                      (make-object menu-item% l m
                                                   (lambda _
                                                     (enter-text r)
                                                     (do-enter))))
                                    (filesystem-root-list)))))
                    text-field%)]
               [c (class c%
                    (super-new)
                    (define editor (send this get-editor))
                    (define/public (lock)   (send editor lock #t))
                    (define/public (unlock) (send editor lock #f))
                    (lock))])
          (new c [label #f] [parent this] [init-value ""])))

      (define path-list
        (new (class list-box%
               ;; make sure that if the focus is here, the text is synced
               ;; (questionable behavior)
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
        (new text-field% [parent this] [label "Filename:"]
          [init-value (cond [(path? filename) (path->string filename)]
                            [(string? filename) filename]
                            [else ""])]
          [callback (lambda (t e)
                      (if (eq? (send e get-event-type) 'text-field-enter)
                        (do-enter*) (text-callback)))]))
      (define text* (send text get-editor))
      (send text* select-all)

      (define last-set-glob (and filters (cadar filters)))
      (define globs         (and filters (glob->2regexps last-set-glob)))
      (define (set-filter new . keep-focus?)
        (when new (send file-filter set-value new))
        (let ([filt (or new (send file-filter get-value))])
          (unless (equal? last-set-glob filt)
            (set! last-set-glob filt)
            (set! globs (and (not (equal? "" filt)) (glob->2regexps filt)))
            (set-dir dir))
          (unless (and (pair? keep-focus?) (car keep-focus?))
            (send text focus))))
      (define file-filter
        (and filters
             (let* ([c (new (class combo-field% (super-new)
                              (define/override (on-focus on?)
                                (unless on? (set-filter #f #t))))
                            [parent this] [label "Filter:"]
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
