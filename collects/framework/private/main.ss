(module main mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   "../gui-utils.ss"
           (lib "string-constant.ss" "string-constants")
	   (lib "mred-sig.ss" "mred"))
  
  (provide main@)

  (define main@
    (unit/sig framework:main^
      (import mred^
	      [preferences : framework:preferences^]
	      [exit : framework:exit^]
	      [group : framework:group^]
              [handler : framework:handler^]
              [editor : framework:editor^]
              [color-prefs : framework:color-prefs^]
              [scheme : framework:scheme^])
      
      (application-preferences-handler (λ () (preferences:show-dialog)))
      
      (preferences:set-default 'framework:basic-canvas-background
                               (send the-color-database find-color "white")
                               (λ (x) (is-a? x color%)))
      (preferences:set-un/marshall 
       'framework:basic-canvas-background
       (λ (clr) (list (send clr red) (send clr green) (send clr blue)))
       (λ (lst) (and (pair? lst)
                          (pair? (cdr lst))
                          (pair? (cddr lst))
                          (null? (cdddr lst))
                          (make-object color% (car lst) (cadr lst) (caddr lst)))))
      
      (preferences:set-default 'framework:special-option-key #f boolean?)
      (preferences:add-callback 'framework:special-option-key (λ (p v) (special-option-key v)))
      (special-option-key (preferences:get 'framework:special-option-key))
      
      (preferences:set-default 'framework:fraction-snip-style 'mixed (λ (x) (memq x '(mixed improper))))
      
      (preferences:set-default 'framework:standard-style-list:font-name
                               (get-family-builtin-face 'modern)
                               string?)
      
      (preferences:set-default
       'framework:standard-style-list:font-size
       (let* ([txt (make-object text%)]
              [stl (send txt get-style-list)]
              [bcs (send stl basic-style)])
         (send bcs get-size))
       (λ (x) (and (number? x) (exact? x) (integer? x) (positive? x))))
      
      (preferences:set-default
       'framework:standard-style-list:smoothing
       'default
       (λ (x) 
         (memq x '(unsmoothed partly-smoothed smoothed default))))
      
      (editor:set-standard-style-list-pref-callbacks)
      
      (preferences:set-default 'framework:paren-match-color
                               (let ([gray-level
                                      ;; old gray-level 192
                                      (if (eq? (system-type) 'windows)
                                          (* 3/4 256)
                                          (- (* 7/8 256) 1))])
                                 (make-object color% gray-level gray-level gray-level))
                               (λ (x) (is-a? x color%)))
      
      (preferences:set-un/marshall
       'framework:paren-match-color
       (λ (c) (list (send c red) (send c green) (send c blue)))
       (λ (l) (make-object color% (car l) (cadr l) (caddr l))))
      
      (preferences:set-default 'framework:recently-opened-files/pos 
                               null 
                               (λ (x) (and (list? x) 
                                                (andmap
                                                 (λ (x) 
                                                   (and (list? x)
                                                        (= 3 (length x))
                                                        (path? (car x))
                                                        (number? (cadr x))
                                                        (number? (caddr x))))
                                                 x))))
      
      (preferences:set-un/marshall
       'framework:recently-opened-files/pos
       (λ (l) (map (λ (ele) (cons (path->bytes (car ele)) (cdr ele))) l))
       (λ (l) 
         (let/ec k
           (unless (list? l)
             (k '()))
           (map (λ (x)
                  (unless (and (list? x)
                               (= 3 (length x))
                               (bytes? (car x))
                               (number? (cadr x))
                               (number? (caddr x)))
                    (k '()))
                  (cons (bytes->path (car x)) (cdr x)))
                l))))
      
      (preferences:set-default 'framework:last-directory 
                               (find-system-path 'doc-dir) 
                               (λ (x) (or (not x) path-string?)))
      
      (preferences:set-un/marshall 'framework:last-directory 
                                   (λ (x) (and (path? x) (path->bytes x)))
                                   (λ (x)
                                     (and (bytes? x)
                                          (bytes->path x))))

      (preferences:set-default 'framework:recent-max-count 
                               50 
                               (λ (x) (and (number? x)
                                                (x . > . 0) 
                                                (integer? x))))
      (preferences:add-callback
       'framework:recent-max-count
       (λ (p v)
         (handler:size-recently-opened-files v)))
      
      (preferences:set-default 'framework:last-url-string "" string?)
      (preferences:set-default 'framework:recently-opened-sort-by 'age 
                               (λ (x) (or (eq? x 'age) (eq? x 'name))))
      (preferences:set-default 'framework:recent-items-window-w 400 number?)
      (preferences:set-default 'framework:recent-items-window-h 600 number?)
      (preferences:set-default 'framework:open-here? #f boolean?)
      (preferences:set-default 'framework:show-delegate? #f boolean?)
      (preferences:set-default 'framework:search-using-dialog? #t boolean?)
      (preferences:set-default 'framework:windows-mdi #f boolean?)
      (preferences:set-default 'framework:menu-bindings #t boolean?)
      (preferences:set-default 'framework:verify-change-format #f boolean?)
      (preferences:set-default 'framework:auto-set-wrap? #t boolean?)
      (preferences:set-default 'framework:display-line-numbers #t boolean?)
      (preferences:set-default 'framework:show-status-line #t boolean?)
      (preferences:set-default 'framework:col-offsets #f boolean?)
      
      (preferences:set-default
       'framework:print-output-mode
       'standard
       (λ (x) (or (eq? x 'standard) (eq? x 'postscript))))
      
      (preferences:set-default 'framework:highlight-parens #t boolean?)
      (preferences:set-default 'framework:fixup-parens #t boolean?)
      (preferences:set-default 'framework:paren-match #t boolean?)
      (let ([hash-table (make-hash-table)])
	(for-each (λ (x) 
                    (hash-table-put! hash-table x 'define))
                  '())
	(for-each (λ (x) 
		    (hash-table-put! hash-table x 'begin))
		  '(case-lambda
                     match-lambda match-lambda*
                     cond
                     delay
                     unit compound-unit compound-unit/sig
                     public private override
                     inherit sequence))
	(for-each (λ (x) 
		    (hash-table-put! hash-table x 'lambda))
		  '(
		    cases
                       instantiate super-instantiate
                     syntax/loc quasisyntax/loc
                     
                     
                     λ lambda let let* letrec recur
                     letrec-values
                     with-syntax
		     with-continuation-mark
                     module
		     match match-let match-let* match-letrec
                     let/cc let/ec letcc catch
                     let-syntax letrec-syntax fluid-let-syntax letrec-syntaxes+values
                     
                     kernel-syntax-case
                     syntax-case syntax-case* syntax-rules
                     let-signature fluid-let
                     let-struct let-macro let-values let*-values
                     case when unless 
                     let-enumerate
                     class class* class-asi class-asi* class*/names
                     class100 class100* class100-asi class100-asi* class100*/names
                     rec
                     make-object mixin
                     define-some do opt-lambda
		     send* with-method
                     define-record
                     local catch shared
                     unit/sig unit/lang
                     with-handlers
                     interface
                     parameterize
                     call-with-input-file call-with-input-file* with-input-from-file
                     with-input-from-port call-with-output-file
                     with-output-to-file with-output-to-port))
	(preferences:set-default 
         'framework:tabify
         (list hash-table #rx"^begin" #rx"^def" #f)
         (λ (x)
           (and (list? x)
                (= (length x) 4)
                (hash-table? (car x))
                (andmap (λ (x) (or (regexp? x) (not x))) (cdr x)))))
	(preferences:set-un/marshall
	 'framework:tabify 
	 (λ (t) (cons (hash-table-map (car t) list)
                           (cdr t)))
	 (λ (l) 
           (and (list? l)
                (= (length l) 4)
                (andmap (λ (x) (or (regexp? x) (not x)))
                        (cdr l))
                (andmap (λ (x) (and (list? x)
                                         (= 2 (length x))
                                         (andmap symbol? x)))
                        (car l))
                (let ([h (make-hash-table)])
                  (for-each (λ (x) (apply hash-table-put! h x)) (car l))
                  (cons h (cdr l)))))))
      
      
      (preferences:set-default 'framework:autosave-delay 300 number?)
      (preferences:set-default 'framework:autosaving-on? #t boolean?)
      (preferences:set-default 'framework:backup-files? #t boolean?)
      (preferences:set-default 'framework:verify-exit #t boolean?)
      (preferences:set-default 'framework:delete-forward? 
			       (not (eq? (system-type) 'unix))
			       boolean?)
      (preferences:set-default 'framework:show-periods-in-dirlist #f boolean?)
      (preferences:set-default
       'framework:file-dialogs
       'std
       (λ (x)
	 (or (eq? x 'common)
	     (eq? x 'std))))
      
      ;; scheme prefs
      
      (for-each (λ (line)
                  (let ([sym (car line)]
                        [color (cadr line)])
                    (color-prefs:register-color-pref (scheme:short-sym->pref-name sym)
                                                     (scheme:short-sym->style-name sym)
                                                     color)))
                (scheme:get-color-prefs-table))
      (preferences:set-default 'framework:coloring-active #t boolean?)

      (preferences:set-default 'framework:default-text-color 
                               (send the-color-database find-color "Black")
                               (λ (x) (is-a? x color%)))
      
      (preferences:set-un/marshall 'framework:default-text-color 
                                   (λ (c) (list (send c red) (send c green) (send c blue)))
                                   (λ (lst) 
                                     (make-object color% (car lst) (cadr lst) (caddr lst))))
      (preferences:add-callback 'framework:default-text-color
                                (λ (p v)
                                  (editor:set-default-font-color v)))
      
      ;; groups
      
      (preferences:set-default 'framework:exit-when-no-frames #t boolean?)

      (exit:insert-can?-callback
       (λ ()
         (send (group:get-the-frame-group) can-close-all?)))    
      
      (exit:insert-on-callback
       (λ ()
         (send (group:get-the-frame-group) on-close-all)
         (preferences:silent-save) ;; the prefs may have changed as a result of closing the windows...
         ))
      
      (exit:insert-can?-callback 
       (λ ()
         (or (preferences:save)
             (exit-anyway?))))
      
      (define (exit-anyway?)
        (gui-utils:get-choice
         (string-constant still-locked-exit-anyway?)
         (string-constant yes)
         (string-constant no)
         (string-constant drscheme)))

      ;; reset these -- they are only for the test suite.
      ;; they do not need to be set across starting up and shutting down
      ;; the application.
      ;(preferences:set 'framework:file-dialogs 'std)
      
      (void))))
