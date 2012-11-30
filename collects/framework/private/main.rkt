#lang racket/unit
(require racket/class
         "sig.rkt"
         "../preferences.rkt"
         mred/mred-sig)

(import mred^
        [prefix preferences: framework:preferences^]
        [prefix exit: framework:exit^]
        [prefix group: framework:group^]
        [prefix handler: framework:handler^]
        [prefix editor: framework:editor^]
        [prefix color-prefs: framework:color-prefs^]
        [prefix racket: framework:racket^]
        [prefix early-init: framework:early-init^]
        [prefix color: framework:color^])
(export framework:main^)
(init-depend framework:preferences^ framework:exit^ framework:editor^
             framework:color-prefs^ framework:racket^ framework:early-init^)

(preferences:low-level-get-preference preferences:get-preference/gui)
(preferences:low-level-put-preferences preferences:put-preferences/gui)

(application-preferences-handler (λ () (preferences:show-dialog)))

(preferences:set-default 'framework:aspell-dict #f (λ (x) (or (not x) (string? x))))

(preferences:set-default 'framework:line-spacing-add-gap?
                         (not (eq? (system-type) 'windows))
                         boolean?)

;; used to time how long it takes to set a preference; the value is not actually used.
(preferences:set-default 'drracket:prefs-debug #f (λ (x) #t))

(preferences:set-default 'framework:spell-check-on? #f boolean?)

(preferences:set-default 'framework:always-use-platform-specific-linefeed-convention #f boolean?)

(preferences:set-default 'framework:overwrite-mode-keybindings #f boolean?)

(preferences:set-default 'framework:ask-about-paste-normalization #t boolean?)
(preferences:set-default 'framework:do-paste-normalization #t boolean?)

(preferences:set-default 'framework:replace-visible? #f boolean?)
(preferences:set-default 'framework:anchored-search #f boolean?)

(let ([search/replace-string-predicate
       (λ (l) 
         (and (list? l)
              (andmap 
               (λ (x) (or (string? x) (is-a? x snip%)))
               l)))])
  (preferences:set-default 'framework:search-string 
                           '() 
                           search/replace-string-predicate)
  (preferences:set-default 'framework:replace-string 
                           '() 
                           search/replace-string-predicate))

;; marshalling for this one will just lose information. Too bad.
(preferences:set-un/marshall 'framework:search-string 
                             (λ (l)
                               (map (λ (x) 
                                      (if (is-a? x snip%)
                                          (send x get-text 0 (send x get-count))
                                          x))
                                    l))
                             values)

(preferences:set-default 'framework:paren-color-scheme 'basic-grey symbol?)
  
(preferences:set-default 'framework:square-bracket:cond/offset
                         '(("case-lambda" 0)
                           ("match-lambda" 0)
                           ("match-lambda*" 0)
                           ("cond" 0)
                           ("field" 0)
                           ("provide/contract" 0)
                           ("match" 1)
                           ("new" 1)
                           ("case" 1)
                           ("match" 1)
                           ("match*" 1)
                           ("syntax-rules" 1)
                           ("syntax-case" 2)
                           ("syntax-case*" 3)
                           ("kernel-syntax-case" 2)
                           ("kernel-syntax-case*" 3))
                         (λ (x) (and (list? x) (andmap (λ (x) (and (pair? x)
                                                                   (string? (car x))
                                                                   (pair? (cdr x))
                                                                   (number? (cadr x))
                                                                   (null? (cddr x))))
                                                       x))))
(preferences:set-default 'framework:square-bracket:local
                         '("local")
                         (λ (x) (and (list? x) (andmap string? x))))

(define all-fors
  (let ()
    (define base-fors
      '(for for/list for/hash for/hasheq for/hasheqv for/and for/or 
         for/lists for/first for/last for/fold for/vector for/flvector
         for/sum for/product))
    (define untyped-fors
      (append base-fors
              (map (λ (x) (string->symbol (regexp-replace #rx"^for" (symbol->string x) "for*")))
                   base-fors)))
    (define all-fors
      (append untyped-fors
              (map (λ (x) (string->symbol (string-append (symbol->string x) ":")))
                   untyped-fors)))
    all-fors))

(preferences:set-default 'framework:square-bracket:letrec
                         (append (map symbol->string all-fors)
                                 '("let" 
                                   "let*" "let-values" "let*-values"
                                   "let-syntax" "let-struct" "let-syntaxes"
                                   "match-let" "match-let*" "match-letrec"
                                   "letrec"
                                   "letrec-syntaxes" "letrec-syntaxes+values" "letrec-values"
                                   "parameterize"
                                   "with-syntax"))
                         (λ (x) (and (list? x) (andmap string? x))))

(preferences:set-default 'framework:white-on-black? #f boolean?)

(preferences:set-default 'framework:case-sensitive-search?
                         #f
                         boolean?)
(color-prefs:set-default/color-scheme 'framework:basic-canvas-background "white" "black")

(preferences:set-default 'framework:special-meta-key #f boolean?)
(preferences:add-callback 'framework:special-meta-key (λ (p v) (map-command-as-meta-key v)))
(map-command-as-meta-key (preferences:get 'framework:special-meta-key))

(preferences:set-default 'framework:fraction-snip-style 'mixed (λ (x) (memq x '(mixed improper decimal))))

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

(let ([gray-level
       ;; old gray-level 192
       (if (eq? (system-type) 'windows)
           (* 3/4 256)
           (- (* 7/8 256) 1))])
  (define default-color (make-object color% 0 0 0 (- 1. (/ gray-level 255))))
  (define w-o-b-default-color (make-object color% 255 255 255 (/ 50 255)))
  (color-prefs:set-default/color-scheme 'framework:paren-match-color
                                        default-color
                                        w-o-b-default-color)
  
  ;; when the preference is currently set to the old color,
  ;; then just update it to the new one (if someone really
  ;; wants the old default, they can still have a color that is
  ;; off by one from the old default which should be ok)
  (define current-color (preferences:get 'framework:paren-match-color))
  (cond
    [(and (= (send current-color red) gray-level)
          (= (send current-color green) gray-level)
          (= (send current-color blue) gray-level)
          (= (send current-color alpha) 1.0))
     (preferences:set 'framework:paren-match-color default-color)]
    [(and (= (send current-color red) 50)
          (= (send current-color green) 50)
          (= (send current-color blue) 50)
          (= (send current-color alpha) 1.0))
     (preferences:set 'framework:paren-match-color w-o-b-default-color)]))

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
(preferences:set-default 'framework:show-delegate? #f boolean?)
(preferences:set-default 'framework:windows-mdi #f boolean?)
(preferences:set-default 'framework:menu-bindings #t boolean?)
(preferences:set-default 'framework:verify-change-format #f boolean?)
(preferences:set-default 'framework:auto-set-wrap? #f boolean?)
(preferences:set-default 'framework:display-line-numbers #t boolean?)
(preferences:set-default 'framework:show-status-line #t boolean?)
(preferences:set-default 'framework:col-offsets #f boolean?)

(preferences:set-default
 'framework:print-output-mode
 'standard
 (λ (x) (or (eq? x 'standard) (eq? x 'postscript) (eq? x 'pdf))))

(preferences:set-default 'framework:highlight-parens #t boolean?)
(preferences:set-default 'framework:fixup-parens #t boolean?)
(preferences:set-default 'framework:fixup-open-parens #f boolean?)
(preferences:set-default 'framework:paren-match #t boolean?)
(let ([hash-table (make-hasheq)])
  (for-each (λ (x) 
              (hash-set! hash-table x 'define))
            '(struct
              local
                     
              struct: define-struct: define-typed-struct define-struct/exec:
              define: pdefine:
              define-type define-predicate
              match-define))
  (for-each (λ (x) 
              (hash-set! hash-table x 'begin))
            '(case-lambda case-lambda: pcase-lambda:
               match-lambda match-lambda*
               cond
               delay
               unit compound-unit compound-unit/sig
               public private override
               inherit sequence))
  (for-each (λ (x) 
              (hash-set! hash-table x 'lambda))
            `(
              cases
                 instantiate super-instantiate
               syntax/loc quasisyntax/loc
               match match* match-let match-let* match-letrec
               
               λ lambda let let* letrec recur
               lambda/kw
               letrec-values
               with-syntax
               with-continuation-mark
               module module* module+
               match match-let match-let* match-letrec
               let/cc let/ec letcc catch
               let-syntax letrec-syntax fluid-let-syntax letrec-syntaxes+values
               
               let: letrec: let*:
               let-values: letrec-values: let*-values:
               let/cc: let/ec:
               lambda: λ:
               plambda: opt-lambda: popt-lambda:

               splicing-let splicing-letrec splicing-let-values
               splicing-letrec-values splicing-let-syntax
               splicing-letrec-syntax splicing-let-syntaxes
               splicing-letrec-syntaxes splicing-letrec-syntaxes+values
               splicing-local splicing-syntax-parameterize

               ,@all-fors

               do:
               
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
               catch shared
               unit/sig unit/lang
               with-handlers
               interface
               parameterize
               call-with-input-file call-with-input-file* with-input-from-file
               with-input-from-port call-with-output-file
               with-output-to-file with-output-to-port 

	       for-all
               
               type-case
	       ))
  (preferences:set-default 
   'framework:tabify
   (list hash-table #rx"^begin" #rx"^def" #f)
   (λ (x)
     (and (list? x)
          (= (length x) 4)
          (hash? (car x))
          (andmap (λ (x) (or (regexp? x) (not x))) (cdr x)))))
  (preferences:set-un/marshall
   'framework:tabify 
   (λ (t) (cons (hash-map (car t) list)
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
          (let ([h (make-hasheq)])
            (for-each (λ (x) (apply hash-set! h x)) (car l))
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
               (racket:short-sym->pref-name sym)
               (racket:short-sym->style-name sym)
               color
               white-on-black-color)))
          (racket:get-color-prefs-table)
          (racket:get-white-on-black-color-prefs-table))
(preferences:set-default 'framework:coloring-active #t boolean?)

(color-prefs:set-default/color-scheme 'framework:default-text-color "black" "white")
(preferences:add-callback 'framework:default-text-color
                          (λ (p v)
                            (editor:set-default-font-color v)))
(editor:set-default-font-color (preferences:get 'framework:default-text-color))

(color-prefs:set-default/color-scheme 'framework:misspelled-text-color "black" "white")

(color-prefs:set-default/color-scheme 'framework:delegatee-overview-color
                                      "light blue"
                                      (make-object color% 62 67 155))

 
(let ([delta (make-object style-delta%)]
      [style (send (editor:get-standard-style-list) find-named-style color:misspelled-text-color-style-name)])
  (if style
      (send style set-delta delta)
      (send (editor:get-standard-style-list) new-named-style color:misspelled-text-color-style-name
            (send (editor:get-standard-style-list) find-or-create-style
                  (send (editor:get-standard-style-list) find-named-style "Standard")
                  delta))))
(let ([update-style-list
       (λ (v)
         (define sl (editor:get-standard-style-list))
         (define style (send sl find-named-style color:misspelled-text-color-style-name))
         (define delta (new style-delta%))
         (send style get-delta delta)
         (send delta set-delta-foreground v)
         (send style set-delta delta))])
  (preferences:add-callback
   'framework:misspelled-text-color
   (λ (p v) (update-style-list v)))
  (update-style-list 
   (preferences:get 'framework:misspelled-text-color)))

;; groups

(preferences:set-default 'framework:exit-when-no-frames #t boolean?)
(unless (preferences:get 'framework:exit-when-no-frames)
  (preferences:set 'framework:exit-when-no-frames #t))

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
