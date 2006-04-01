
(module finder mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   "sig.ss"
	   "../gui-utils.ss"
           (lib "class.ss")
	   (lib "mred-sig.ss" "mred")
	   (lib "string.ss")
	   (lib "list.ss")
	   (lib "file.ss")
	   (lib "etc.ss"))

  (provide finder@)

  (define finder@
    (unit/sig framework:finder^
      (import mred^
              [preferences : framework:preferences^]
	      [keymap : framework:keymap^])

      (rename [-put-file put-file]
	      [-get-file get-file])
      
      (define dialog-parent-parameter (make-parameter #f))

      (define filter-match?
	(λ (filter name msg)
	  (let-values ([(base name dir?) (split-path name)])
	    (if (regexp-match-exact? filter (path->bytes name))
		#t
		(begin
		  (message-box (string-constant error) msg)
		  #f)))))
      
      (define (set-last-directory dir) (preferences:set 'framework:last-directory dir))
      (define (get-last-directory) (preferences:get 'framework:last-directory))
      
      (define make-relative
	(λ (s) s))
      
      (define build-updir
	(λ (dir)
	  (let-values ([(base _1 _2) (split-path dir)])
	    (or base dir))))

      (define default-filters (make-parameter '(("Any" "*.*"))))
      (define default-extension (make-parameter ""))

      ; the finder-dialog% class controls the user interface for dialogs
      
      (define finder-dialog%
	(class dialog% 
          (init parent-win)
          (init-field save-mode?)
          (init-field replace-ok?)
          (init-field multi-mode?)
          (init-field result-box)
          (init start-dir)
          (init start-name)
          (init prompt)
          (init-field file-filter)
          (init-field file-filter-msg)
	  
          (inherit center show)
          
	  (define default-width 500)
          (define default-height 400)
          (define dirs #f)
          (define current-dir #f)
	  
	  (define/private set-listbox-directory ; sets directory in listbox 
            (λ (dir) ; dir is normalized
              (when (directory-exists? dir)
                (gui-utils:show-busy-cursor
                 (λ ()
                   (set! current-dir dir)
                   (set-last-directory dir)
                   (let-values 
                       ([(dir-list menu-list)
                         (let loop ([this-dir dir]
                                    [dir-list null]
                                    [menu-list null])
                           (let-values ([(base-dir in-dir dir?) (split-path this-dir)])
                             (when (eq? (system-type) 'windows)
                               (string-lowercase! in-dir))
                             (let* ([dir-list (cons this-dir dir-list)]
                                    [menu-list (cons (path->string in-dir) menu-list)])
                               (if base-dir
                                   (loop base-dir dir-list menu-list)
                                   ; No more
                                   (values dir-list menu-list)))))])
                     (set! dirs (reverse dir-list))
                     (send dir-choice clear)
                     (let loop ([choices (reverse menu-list)])
                       (unless (null? choices)
                         (send dir-choice append (car choices))
                         (loop (cdr choices))))
                     (send dir-choice set-selection 0))
                   
                   (send name-list clear)
                   (send name-list set
                         (sort
                          (let ([no-periods? 
                                 (not (preferences:get
                                       'framework:show-periods-in-dirlist))])
                            (let loop ([l (directory-list dir)])
                              (if (null? l)
                                  null
                                  (let ([s (car l)]
                                        [rest (loop (cdr l))])
                                    (cond
                                      [(and no-periods?
                                            (let ([str (path->string s)])
                                              (<= 1 (string-length str))
                                              (char=? (string-ref str 0) #\.)))
                                       rest]
                                      [(directory-exists? (build-path dir s))
                                       (cons (path->string s) rest)]
                                      [(or (not file-filter)
                                           (regexp-match-exact? file-filter (path->string s)))
                                       (cons (path->string s)
                                             rest)]
                                      [else rest])))))
                          string<?))
                   (send name-list set-selection-and-edit 0))))))
          
          (define/private set-edit
            (λ ()
              (let* ([file (send name-list get-string-selection)])
                (send directory-field set-value
                      (path->string
                       (if file
                           (build-path current-dir file)
                           current-dir))))))
	  
	  [define/public do-period-in/exclusion
            (λ (check-box event)
              (preferences:set
               'framework:show-periods-in-dirlist
               (send check-box get-value))
              (set-listbox-directory current-dir))]
          
          [define/public do-dir
            (λ (choice event)
              (let ([which (send choice get-selection)])
                (if (< which (length dirs))
                    (set-listbox-directory (list-ref dirs which)))))]
          
          [define/public do-name-list
            (λ (list-box evt)
              (if (eq? (send evt get-event-type) 'list-box-dclick)
                  (let ([dir (send directory-field get-value)])
                    (if (directory-exists? dir)
                        (set-listbox-directory (normal-case-path (normalize-path dir)))
                        (if multi-mode?
                            (do-add)
                            (do-ok))))
                  (when (send list-box get-string-selection)
                    (set-edit))))]
          
          [define/public do-result-list
            (λ () #f)]
          
          [define/public do-ok
            (λ args
              
              (if multi-mode?
                  
                  (let loop ([n (sub1 (send result-list get-number))]
                             [result null])
                    (if (< n 0)
                        (begin
                          (set-box! result-box result)
                          (show #f))
                        (loop (sub1 n) 
                              (cons (send result-list get-string n)
                                    result))))
                  ; not multi-mode
                  
                  (let ([name (send name-list get-string-selection)]
                        [non-empty? (> (send name-list get-number) 0)])
                    
                    (cond
                      
                      [(and save-mode? 
                            non-empty?
                            (not (string? name))) 'nothing-selected]
                      
                      [(and save-mode? 
                            non-empty?
                            (string=? name ""))
                       (let ([file (send directory-field get-value)])
                         (if (directory-exists? file)
                             (set-listbox-directory (normal-case-path (normalize-path file)))
                             (message-box 
                              (string-constant error)
                              (string-constant must-specify-a-filename))))]
                      
                      [(and save-mode? 
                            non-empty?
                            file-filter 
                            (not (regexp-match-exact? file-filter name)))
                       (message-box (string-constant error) file-filter-msg)]
                      
                      [else
                       
                       ; if dir in edit box, go to that dir
                       
                       (let ([dir-name (send directory-field get-value)])
                         
                         (if (directory-exists? dir-name)
                             (set-listbox-directory (normal-case-path (normalize-path dir-name)))
                             
                             ; otherwise, try to return absolute path
                             
                             (let* ([relative-name (make-relative name)]
                                    [file-in-edit (file-exists? dir-name)]
                                    [file (if (or file-in-edit
                                                  (not relative-name)
                                                  save-mode?)
                                              dir-name
                                              (build-path current-dir relative-name))])
                               
                               ; trying to open a file that doesn't exist
                               
                               (if (and (not save-mode?) (not file-in-edit))
                                   (message-box 
                                    (string-constant error)
                                    (format (string-constant file-does-not-exist) dir-name))
                                   
                                   ; saving a file, which may exist, or
                                   ; opening an existing file
                                   
                                   (if (or (not save-mode?)
                                           (not (file-exists? file))
                                           replace-ok?
                                           (eq? (message-box 
                                                 (string-constant warning)
                                                 (format
                                                  (string-constant ask-because-file-exists)
                                                  file)
                                                 #f
                                                 '(yes-no))
                                                'yes))
                                       (let ([normal-path
                                              (with-handlers 
                                                  ([(λ (_) #t)
                                                    (λ (_)
                                                      (message-box
                                                       (string-constant warning)
                                                       (format
                                                        (string-constant dne-or-cycle)
                                                        file))
                                                      #f)])
                                                (normal-case-path
                                                 (normalize-path file)))])
                                         (when normal-path 
                                           (set-box! result-box normal-path)
                                           (show #f))))))))]))))]
          
          [define/public add-one
            (λ (name)
              (unless (or (directory-exists? name)
                          (send result-list find-string name))
                (send result-list append
                      (normal-case-path (normalize-path name)))))]
          
          [define/public do-add
            (λ ()
              (let ([name (send name-list get-string-selection)])
                (if (string? name)
                    (let ([name (build-path current-dir
                                            (make-relative name))])
                      (add-one name)))))]
          
          [define/public do-add-all
            (λ ()
              (let loop ([n 0])
                (when (< n (send name-list get-number))
                  (let ([name (send name-list get-string n)])
                    (let ([name (build-path current-dir
                                            (make-relative name))])
                      (add-one name)
                      (loop (add1 n)))))))]
          
          [define/public do-remove
            (λ ()
              (let loop ([n 0])
                (if (< n (send result-list get-number))
                    (if (send result-list is-selected? n)
                        (begin
                          (send result-list delete n)
                          (loop n))
                        (loop (add1 n))))))]
          
          [define/public do-cancel
            (λ ()
              (set-box! result-box #f)
              (show #f))]
	  
	  (define/augment on-close (λ () #f))
	  
          (super-new (label (if save-mode? 
                                (string-constant put-file)
                                (string-constant get-file)))
                     (parent parent-win)
                     (width default-width)
                     (height default-height)
                     (style '(resize-border)))
	  
          [define main-panel (make-object vertical-panel% this)]
          
          [define top-panel (make-object horizontal-panel% main-panel)]
          
          (make-object message% prompt top-panel)
          
          [define dir-choice (make-object choice% #f null top-panel
                               (λ (choice event) (do-dir choice event)))]
          
          [define middle-panel (make-object horizontal-panel% main-panel)]
          [define left-middle-panel (make-object vertical-panel% middle-panel)]
          [define right-middle-panel (when multi-mode? 
                                       (make-object vertical-panel% middle-panel))]
          
          [define name-list%
            
            (class list-box%
              
              (inherit 
                get-string-selection
                get-string
                get-selection
                get-number
                get-first-visible-item
                number-of-visible-items
                set-first-visible-item
                set-selection)
              
              (define/override (on-subwindow-char _ key)
                (let ([code (send key get-key-code)]
                      [num-items (get-number)]
                      [curr-pos (get-selection)])
                  (cond 
                    [(or (equal? code 'numpad-return)
                         (equal? code #\return))
                     (if multi-mode?
                         (do-add)
                         (do-ok))]
                    
                    ; look for letter at beginning of a filename
                    [(char? code)
                     (let ([next-matching
                            (let loop ([pos (add1 curr-pos)])
                              (cond
                                [(>= pos num-items) #f]
                                [else
                                 (let ([first-char (string-ref (get-string pos) 0)])
                                   (if (char-ci=? code first-char)
                                       pos
                                       (loop (add1 pos))))]))])
                       (if next-matching
                           (set-selection-and-edit next-matching)
                           
                           ;; didn't find anything forward; start again at front of list
                           (let loop ([pos 0]
                                      [last-before 0])
                             (cond
                               [(<= pos num-items)
                                (let ([first-char (string-ref (get-string pos) 0)])
                                  (cond
                                    [(char-ci=? code first-char)
                                     (set-selection-and-edit pos)]
                                    [(char-ci<=? first-char code)
                                     (loop (+ pos 1)
                                           pos)]
                                    [else
                                     (set-selection-and-edit last-before)]))]
                               [else (set-selection-and-edit last-before)]))))]
                    
                    ; movement keys
                    [(and (eq? code 'up) 
                          (> curr-pos 0))
                     (set-selection-and-edit (sub1 curr-pos))]
                    
                    [(and (eq? code 'down)
                          (< curr-pos (sub1 num-items)))
                     (let* ([num-vis (number-of-visible-items)] 
                            [curr-first (get-first-visible-item)]
                            [new-curr-pos (add1 curr-pos)]
                            [new-first (if (< new-curr-pos (+ curr-first num-vis))
                                           curr-first ; no scroll needed
                                           (add1 curr-first))])
                       (set-first-visible-item new-first)
                       (set-selection-and-edit new-curr-pos))]
                    
                    [(and (eq? code 'prior)
                          (> curr-pos 0))
                     (let* ([num-vis (number-of-visible-items)]
                            [new-first (- (get-first-visible-item) num-vis)])
                       (set-first-visible-item (max new-first 0))
                       (set-selection-and-edit (max 0 (- curr-pos num-vis))))]
                    
                    [(and (eq? code 'next)
                          (< curr-pos (sub1 num-items)))
                     (let* ([num-vis (number-of-visible-items)]
                            [new-first (+ (get-first-visible-item) num-vis)])
                       (set-first-visible-item
                        (min new-first (- (get-number) num-vis)))
                       (set-selection-and-edit 
                        (min (sub1 num-items) (+ curr-pos num-vis))))]
                    
                    [else #f])))
              
              [define/public set-selection-and-edit
                 (λ (pos)
                   (when (> (get-number) 0)
                     (let* ([first-item (get-first-visible-item)]
                            [last-item (sub1 (+ (number-of-visible-items) 
                                                first-item))])
                       (if (or (< pos first-item) (> pos last-item))
                           (set-first-visible-item pos))
                       (set-selection pos)))
                   (set-edit))]
              [define/public on-default-action
                (λ ()
                  (when (> (get-number) 0)
                    (let* ([which (get-string-selection)]
                           [dir (build-path current-dir
                                            (make-relative which))])
                      (if (directory-exists? dir)
                          (set-listbox-directory (normal-case-path
                                                  (normalize-path dir)))
                          (if multi-mode?
                              (do-add)
                              (do-ok))))))]
              
              (super-new))]
          
          [define name-list (make-object name-list%
                              #f null left-middle-panel (λ (x y) (do-name-list x y))
                              '(single))]
          
          [define save-panel (when save-mode? (make-object horizontal-panel% main-panel))]
          
          [define directory-panel (make-object horizontal-panel% main-panel)]
          
          [define dot-panel (when (eq? 'unix (system-type))
                              (make-object horizontal-panel% main-panel))]
          
          [define bottom-panel (make-object horizontal-panel% main-panel)]
          
          [define directory-field
            (keymap:call/text-keymap-initializer
             (λ ()
               (make-object text-field%
                 (string-constant full-pathname)
                 directory-panel
                 (λ (txt evt)
                   (when (eq? (send evt get-event-type) 'text-field-enter)
                     (let ([dir (send directory-field get-value)])
                       (if (directory-exists? dir)
                           (set-listbox-directory (normal-case-path
                                                   (normalize-path dir)))
                           (if multi-mode?
                               (do-add)
                               (do-ok)))))))))]
          
          [define result-list
            (when multi-mode?
              (make-object list-box%
                #f
                null
                right-middle-panel
                (λ (x y) (do-result-list))
                '(multiple)))]
          [define add-panel 
            (when multi-mode? 
              (make-object horizontal-panel% left-middle-panel))]
          
          [define remove-panel 
            (when multi-mode? 
              (make-object horizontal-panel% right-middle-panel))]
          
          [define/private do-updir
            (λ () 
              (set-listbox-directory (build-updir current-dir))
              (set-focus-to-name-list))]
          
          [define/private set-focus-to-name-list
            (λ ()
              (send name-list focus))]
	  
          
          (when (eq? (system-type) 'unix)
            (let ([dot-cb
                   (make-object check-box%
                     (string-constant show-dot-files)
                     dot-panel
                     (λ (x y) (do-period-in/exclusion x y)))])
              (send dot-panel stretchable-height #f)
              (send dot-cb set-value 
                    (preferences:get 'framework:show-periods-in-dirlist))))
          
          (send directory-panel stretchable-height #f)
          
          (when multi-mode?
            (send add-panel stretchable-height #f)
            (send remove-panel stretchable-height #f)
            (send result-list stretchable-width #t))
          
          (make-object button% 
            (string-constant up-directory-button-label)
            top-panel
            (λ (button evt) (do-updir)))
          
          (send dir-choice stretchable-width #t)
          (send name-list stretchable-width #t)
          (send top-panel stretchable-height #f)
          (send bottom-panel stretchable-height #f)
          
          (when save-mode?
            (send save-panel stretchable-height #f))
	  
	  [define add-button (when multi-mode?
                               (make-object horizontal-panel% add-panel)
                               (make-object button%
                                 (string-constant add-button-label)
                                 add-panel
                                 (λ (x y) (do-add))))]
          [define add-all-button (when multi-mode?
                                   (begin0
                                     (make-object button%
                                       (string-constant add-all-button-label)
                                       add-panel 
                                       (λ (x y) (do-add-all)))
                                     (make-object horizontal-panel% add-panel)))]
          [define remove-button (when multi-mode?
                                  (make-object horizontal-panel% remove-panel)
                                  (begin0
                                    (make-object button% 
                                      (string-constant remove-button-label)
                                      remove-panel
                                      (λ (x y) (do-remove)))
                                    (make-object horizontal-panel% remove-panel)))]
          (make-object vertical-panel% bottom-panel) 
          [define ok-button
            (make-object button% (string-constant ok) bottom-panel 
              (λ (x y) (do-ok))
              (if multi-mode? '() '(border)))]
          [define cancel-button (make-object button% 
                                  (string-constant cancel)
                                  bottom-panel
                                  (λ (x y) (do-cancel)))]
          (make-object grow-box-spacer-pane% bottom-panel)
          
          (cond
            [(and start-dir
                  (directory-exists? start-dir))
             (set-listbox-directory (normal-case-path
                                     (normalize-path start-dir)))]
            [(get-last-directory)
             =>
             (λ (dir)
               (set-listbox-directory dir))]
            [else (set-listbox-directory (current-directory))])
          
          (send ok-button min-width (send cancel-button get-width))
          
          (center 'both)
          
          (show #t)))
      
      ; make-common takes a dialog-maker
      ; used to make one dialog object per session, now created each time
      (define make-common
	(λ (make-dialog)
	  (λ args
	    (let ([result-box (box #f)])
	      (apply make-dialog result-box args)
	      (unbox result-box)))))
      
					; the common versions of these functions have their visual
					; interfaces under Scheme control
      
      (define common-put-file
	(make-common
	 (opt-lambda (result-box 
		      [name #f]
		      [in-directory #f]
		      [replace? #f]
		      [prompt (string-constant select-file)]
		      [filter #f]
		      [filter-msg (string-constant file-wrong-form)]
		      [parent-win (dialog-parent-parameter)])
	   (let* ([directory (if (and (not in-directory)
				      (string? name))
				 (path-only name)
				 in-directory)]
		  [saved-directory (get-last-directory)]
		  [name (or (and (string? name)
				 (file-name-from-path name))
			    name)])
             (new finder-dialog% 
                  (parent-win parent-win)
                  (save-mode? #t)
                  (replace-ok? replace?)
                  (multi-mode? #f)
                  (result-box result-box )
                  (start-dir directory)
                  (start-name name)
                  (prompt prompt)
                  (file-filter filter)
                  (file-filter-msg filter-msg))
	     (when in-directory (set-last-directory saved-directory))))))
      
      (define common-get-file
	(make-common
	 (opt-lambda
	     (result-box 
	      [directory #f]
	      [prompt (string-constant select-file)]
	      [filter #f]
	      [filter-msg (string-constant file-wrong-form)]
	      [parent-win (dialog-parent-parameter)])
	   (let ([saved-directory (get-last-directory)])
             (new finder-dialog% 
                  (parent-win parent-win)
                  (save-mode? #f)
                  (replace-ok? #f)
                  (multi-mode? #f)
                  (result-box result-box)
                  (start-dir directory)
                  (start-name #f)
                  (prompt prompt)
                  (file-filter filter)
                  (file-filter-msg filter-msg))
	     (when directory (set-last-directory saved-directory))))))
      
      (define common-get-file-list
	(make-common
	 (opt-lambda (result-box 
		      [directory #f]
		      [prompt (string-constant select-files)]
		      [filter #f]
		      [filter-msg (string-constant file-wrong-form)]
		      [parent-win (dialog-parent-parameter)])
      	   (new finder-dialog% 
                (parent-win parent-win)
                (save-mode? #f)
                (replace-ok? #f)
                (multi-mode? #t)
                (result-box result-box)
                (start-dir directory)
                (start-name #f)
                (prompt prompt)
                (file-filter filter)
                (file-filter-msg filter-msg)))))
      
      ; the std- and common- forms both have opt-lambda's, with the same
      ; list of args.  Should the opt-lambda's be placed in the dispatching function?
      
      (define std-put-file
	(opt-lambda ([name #f]
		     [directory #f]
		     [replace? #f]
		     [prompt (string-constant select-file)]
		     [filter #f]
		     [filter-msg (string-constant file-wrong-form)]
		     [parent-win (dialog-parent-parameter)])
	  (let* ([directory (if (and (not directory)
				     (string? name))
				(path-only name)
				directory)]
		 [name (or (and (string? name)
				(file-name-from-path name))
			   name)]
		 [f (put-file 
		     prompt 
		     parent-win
		     directory 
		     name
		     (default-extension)
		     '()
		     (default-filters))])

	    (if (or (not f)
		    (and filter 
			 (not (filter-match? filter 
					     f
					     filter-msg))))
		#f
		(let* ([f (normal-case-path (normalize-path f))]
		       [dir (path-only f)]
		       [name (file-name-from-path f)])
		  (cond
		   [(not (and (path-string? dir) (directory-exists? dir)))
		    (message-box (string-constant error)
                                 (string-constant dir-dne))
		    #f]
		   [(or (not name) (equal? name ""))
		    (message-box (string-constant error)
                                 (string-constant empty-filename))
		    #f]
		   [else f]))))))
      
      (define std-get-file
	(opt-lambda ([directory #f]
		     [prompt (string-constant select-file)]
		     [filter #f]
		     [filter-msg (string-constant file-wrong-form)]
		     [parent-win (dialog-parent-parameter)])
	  (let ([f (get-file
		    prompt 
		    parent-win
		    directory)])

	    (if f
		(if (or (not filter) (filter-match? filter f filter-msg))
		    (let ([f (normalize-path f)])
		      (cond
		       [(directory-exists? f)
			(message-box (string-constant error)
                                     (string-constant that-is-dir-name))
			#f]
		       [(not (file-exists? f))
			(message-box (string-constant error) 
                                     (string-constant file-dne))
			#f]
		       [else f]))
		    #f)
		#f))))
      
					; external interfaces to file functions
      
      (define -put-file
	(λ args
	  (let ([actual-fun 
		 (case (preferences:get 'framework:file-dialogs)
		   [(std) std-put-file]
		   [(common) common-put-file])])
	    (apply actual-fun args))))
      
      (define -get-file
	(λ args
	  (let ([actual-fun
		 (case (preferences:get 'framework:file-dialogs)
		   [(std) std-get-file]
		   [(common) common-get-file])])
	    (apply actual-fun args)))))))
