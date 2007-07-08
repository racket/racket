(module main (lib "a-unit.ss")
  (require (lib "class.ss")
	   "sig.ss"
           "../preferences.ss"
   	   (lib "mred-sig.ss" "mred"))
  
  (import mred^
          [prefix preferences: framework:preferences^]
          [prefix exit: framework:exit^]
          [prefix group: framework:group^]
          [prefix handler: framework:handler^]
          [prefix editor: framework:editor^]
          [prefix color-prefs: framework:color-prefs^]
          [prefix scheme: framework:scheme^])
  (export framework:main^)
  (init-depend framework:preferences^ framework:exit^ framework:editor^
               framework:color-prefs^ framework:scheme^)
  
  (preferences:low-level-put-preferences preferences:put-preferences/gui)
  
  (application-preferences-handler (λ () (preferences:show-dialog)))
  
  (preferences:set-default 'framework:square-bracket:cond/offset
                           '(("case-lambda" 0)
                             ("cond" 0)
                             ("field" 0)
                             ("provide/contract" 0)
                             ("new" 1)
                             ("case" 1)
                             ("syntax-case" 2)
                             ("syntax-case*" 3))
                           (λ (x) (and (list? x) (andmap (λ (x) (and (pair? x)
                                                                     (string? (car x))
                                                                     (pair? (cdr x))
                                                                     (number? (cadr x))
                                                                     (null? (cddr x))))
                                                         x))))
  (preferences:set-default 'framework:white-on-black? #f boolean?)
  
  (preferences:set-default 'framework:square-bracket:letrec
                           '("let" 
                             "let*" "let-values" "let*-values"
                             "let-syntax" "let-struct" "let-syntaxes"
                             "letrec"
                             "letrec-syntaxes" "letrec-syntaxes+values" "letrec-values"
                             "parameterize"
                             "with-syntax")
                           (λ (x) (and (list? x) (andmap string? x))))
  
  (preferences:set-default 'framework:case-sensitive-search?
                           #f
                           boolean?)
  (color-prefs:set-default/color-scheme 'framework:basic-canvas-background "white" "black")
  
  (preferences:set-default 'framework:special-meta-key #f boolean?)
  (preferences:add-callback 'framework:special-meta-key (λ (p v) (map-command-as-meta-key v)))
  (map-command-as-meta-key (preferences:get 'framework:special-meta-key))
  
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
  
  (color-prefs:set-default/color-scheme
   'framework:paren-match-color
   (let ([gray-level
          ;; old gray-level 192
          (if (eq? (system-type) 'windows)
              (* 3/4 256)
              (- (* 7/8 256) 1))])
     (make-object color% gray-level gray-level gray-level))
   (make-object color% 50 50 50))
  
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
  (preferences:set-default 'framework:fixup-open-parens #t boolean?)
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
                 lambda/kw
                 letrec-values
                 with-syntax
                 with-continuation-mark
                 module
                 match match-let match-let* match-letrec
                 let/cc let/ec letcc catch
                 let-syntax letrec-syntax fluid-let-syntax letrec-syntaxes+values
                 
                 kernel-syntax-case
                 syntax-case syntax-case* syntax-rules syntax-id-rules
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
  (preferences:set-default 'framework:delete-forward? #t boolean?)
  (preferences:set-default 'framework:show-periods-in-dirlist #f boolean?)
  (preferences:set-default 'framework:file-dialogs 'std
                           (λ (x) (and (memq x '(common std)) #t)))
  
  ;; scheme prefs
  
  (for-each (λ (line white-on-black-line)
              (let ([sym (car line)]
                    [color (cadr line)]
                    [white-on-black-color (cadr white-on-black-line)])
                (color-prefs:register-color-preference
                 (scheme:short-sym->pref-name sym)
                 (scheme:short-sym->style-name sym)
                 color
                 white-on-black-color)))
            (scheme:get-color-prefs-table)
            (scheme:get-white-on-black-color-prefs-table))
  (preferences:set-default 'framework:coloring-active #t boolean?)
  
  (color-prefs:set-default/color-scheme 'framework:default-text-color "black" "white")
  (preferences:add-callback 'framework:default-text-color
                            (λ (p v)
                              (editor:set-default-font-color v)))
  (editor:set-default-font-color (preferences:get 'framework:default-text-color))
  
  (color-prefs:set-default/color-scheme 'framework:delegatee-overview-color
                                        "light blue"
                                        (make-object color% 62 67 155))
                                        
  
  ;; groups
  
  (preferences:set-default 'framework:exit-when-no-frames #t boolean?)
  (preferences:set 'framework:exit-when-no-frames #t)
  
  (exit:insert-can?-callback
   (λ ()
     (send (group:get-the-frame-group) can-close-all?)))    
  
  (exit:insert-on-callback
   (λ ()
     (send (group:get-the-frame-group) on-close-all)))
  
  ;; reset these -- they are only for the test suite.
  ;; they do not need to be set across starting up and shutting down
  ;; the application.
  ;(preferences:set 'framework:file-dialogs 'std)
  
  ;; setup the color scheme stuff
  
  
  (void))
