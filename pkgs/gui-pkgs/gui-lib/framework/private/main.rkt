#lang racket/unit
(require racket/class
         racket/contract
         racket/list
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

(preferences:set-default 'framework:color-scheme 'classic symbol?)

(preferences:set-default 'framework:column-guide-width
                         '(#f 102)
                         (list/c boolean? (and/c exact-integer? (>=/c 2))))

(preferences:set-default 'framework:aspell-dict #f (λ (x) (or (not x) (string? x))))

(preferences:set-default 'framework:line-spacing-add-gap?
                         (not (eq? (system-type) 'windows))
                         boolean?)

;; used to time how long it takes to set a preference; the value is not actually used.
(preferences:set-default 'drracket:prefs-debug #f (λ (x) #t))

;; 'framework:spell-check-on? is only for people who had set
;; prefs in old versions; it isn't used except to provide the
;; default values for the newer prefs: 'framework:spell-check-strings?
;; and 'framework:spell-check-text?
(preferences:set-default 'framework:spell-check-on? #f boolean?)
(preferences:set-default 'framework:spell-check-strings? 
                         (preferences:get 'framework:spell-check-on?)
                         boolean?)
(preferences:set-default 'framework:spell-check-text?
                         (preferences:get 'framework:spell-check-on?)
                         boolean?)

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
  
(define cond/offset-defaults
  '(("case-lambda" 0)
    ("match-lambda" 0)
    ("match-lambda*" 0)
    ("cond" 0)
    ("field" 0)
    ("provide/contract" 0)
    ("match" 1)
    ("new" 1)
    ("case" 1)
    ("datum-case" 1)
    ("match" 1)
    ("match*" 1)
    ("syntax-rules" 1)
    ("syntax-case" 2)
    ("syntax-case*" 3)
    ("kernel-syntax-case" 2)
    ("kernel-syntax-case*" 3)))
(preferences:set-default 'framework:square-bracket:cond/offset
                         cond/offset-defaults
                         (listof (list/c string? exact-nonnegative-integer?)))

(preferences:set-un/marshall
 'framework:square-bracket:cond/offset
 (λ (val) 
   (define deletions (for/list ([line (in-list cond/offset-defaults)]
                                #:unless (ormap (λ (val-line)
                                                  (equal? (car line) (car val-line)))
                                                val))
                       (list-ref line 0)))
   (define additions/changes (for/list ([line (in-list val)]
                                        #:when (not (member line cond/offset-defaults)))
                               line))
   (list additions/changes deletions))
 (λ (marsh)
   (cond
     [((listof (list/c string? exact-nonnegative-integer?)) marsh)
      ;; old style prefs: don't try to find any deletions, as they are
      ;; probably caused by a stale defaults setting
      (define ht (make-hash (map (λ (x) (cons (list-ref x 0) (list-ref x 1))) cond/offset-defaults)))
      (for ([line (in-list marsh)])
        (hash-set! ht (list-ref line 0) (list-ref line 1)))
      (hash-map ht list)]
     [((list/c (listof (list/c string? exact-nonnegative-integer?)) (listof string?)) marsh)
      ;; new style-pref
      (define additions/changes (list-ref marsh 0))
      (define deletions (list-ref marsh 1))
      (define ht (make-hash (map (λ (x) (cons (list-ref x 0) (list-ref x 1))) cond/offset-defaults)))
      (for ([del (in-list deletions)])
        (hash-remove! ht del))
      (for/list ([line (in-list additions/changes)])
        (hash-set! ht (list-ref line 0) (list-ref line 1)))
      (hash-map ht list)])))

 
(define (set-square-bracket-nonum-pref pref-sym defaults)
  (preferences:set-default pref-sym defaults (listof string?))
  (preferences:set-un/marshall 
   pref-sym
   (λ (val)
     (define additions '())
     (define deletions '())
     (for ([itm (in-list val)])
       (unless (member itm defaults)
         (set! additions (cons itm additions))))
     (for ([def (in-list defaults)])
       (unless (member def val)
         (set! deletions (cons def deletions))))
     (list additions deletions))
   (λ (marshed)
     (cond
       [((listof string?) marshed)
        ;; this is the old preference; in this case
        ;; we ignore any deletions while unmarshalling
        ;; as those are likely caused by a defaults
        ;; set that got bigger
        (remove-duplicates (append marshed defaults))]
       [((list/c (listof string?) (listof string?)) marshed)
        (define additions (list-ref marshed 0))
        (define deletions (list-ref marshed 1))
        (append additions (remove* deletions defaults))]))))
  

(set-square-bracket-nonum-pref 'framework:square-bracket:local
                               '("local"))

(define all-fors
  (let ()
    (define base-fors
      '(for for/list for/hash for/hasheq for/hasheqv for/and for/or 
         for/lists for/first for/last for/fold for/vector for/flvector
         for/sum for/product for/set))
    (define untyped-fors
      (append base-fors
              (map (λ (x) (string->symbol (regexp-replace #rx"^for" (symbol->string x) "for*")))
                   base-fors)))
    (define all-fors
      (append untyped-fors
              (map (λ (x) (string->symbol (string-append (symbol->string x) ":")))
                   untyped-fors)))
    all-fors))

(set-square-bracket-nonum-pref 'framework:square-bracket:letrec
                               (append (map symbol->string all-fors)
                                       '("let" 
                                         "let*" "let-values" "let*-values"
                                         "let-syntax" "let-struct" "let-syntaxes"
                                         "match-let" "match-let*" "match-letrec"
                                         "letrec"
                                         "letrec-syntaxes" "letrec-syntaxes+values" "letrec-values"
                                         "parameterize" "parameterize*"
                                         "with-syntax" "with-handlers")))

(preferences:set-default 'framework:white-on-black? #f boolean?)

(preferences:set-default 'framework:case-sensitive-search?
                         #f
                         boolean?)
(color-prefs:add-color-scheme-entry 'framework:basic-canvas-background "white" "black")

(preferences:set-default 'framework:special-meta-key #f boolean?)
(preferences:add-callback 'framework:special-meta-key (λ (p v) (map-command-as-meta-key v)))
(map-command-as-meta-key (preferences:get 'framework:special-meta-key))

(preferences:set-default 'framework:fraction-snip-style 
                         'mixed (λ (x) (memq x '(mixed improper decimal))))

(preferences:set-default 'framework:standard-style-list:font-name
                         (get-family-builtin-face 'modern)
                         string?)

(preferences:set-default
 'framework:standard-style-list:font-size
 (vector (hash)
         (let* ([txt (make-object text%)]
                [stl (send txt get-style-list)]
                [bcs (send stl basic-style)])
           (send bcs get-size)))
 (vector/c 
  ;; font sizes for specific monitor configurations
  (hash/c 
   ;; a particular monitor configuration: the widths and heights
   (non-empty-listof (list/c exact-nonnegative-integer? 
                             exact-nonnegative-integer?))
   ;; the font size for that configuration
   exact-nonnegative-integer?
   #:flat? #t)
  ;; default font size, when none of the configs above apply
  exact-nonnegative-integer?
  #:flat? #t))

(preferences:set-un/marshall
 'framework:standard-style-list:font-size
 values
 (λ (x)
   (if (exact-nonnegative-integer? x)
       ;; coerce old pref settings to new
       (vector (hash) x)
       x)))

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
  (color-prefs:add-color-scheme-entry 'framework:paren-match-color
                                      default-color
                                      w-o-b-default-color)
  
  ;; when the preference is currently set to the old color,
  ;; then just update it to the new one (if someone really
  ;; wants the old default, they can still have a color that is
  ;; off by one from the old default which should be ok)
  (define current-color (color-prefs:lookup-in-color-scheme 'framework:paren-match-color))
  (cond
    [(and (= (send current-color red) gray-level)
          (= (send current-color green) gray-level)
          (= (send current-color blue) gray-level)
          (= (send current-color alpha) 1.0))
     (color-prefs:set-in-color-scheme 'framework:paren-match-color default-color)]
    [(and (= (send current-color red) 50)
          (= (send current-color green) 50)
          (= (send current-color blue) 50)
          (= (send current-color alpha) 1.0))
     (color-prefs:set-in-color-scheme 'framework:paren-match-color w-o-b-default-color)]))

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
(let ([defaults-ht (make-hasheq)])
  (for-each (λ (x) (hash-set! defaults-ht x 'define))
            '(struct
              local
                     
              struct: define-struct: define-typed-struct define-struct/exec:
              define: pdefine:
              define-type define-predicate
              match-define))
  (for-each (λ (x) (hash-set! defaults-ht x 'begin))
            '(case-lambda case-lambda: pcase-lambda:
               match-lambda match-lambda*
               cond
               delay
               unit compound-unit compound-unit/sig
               public private override require
               inherit sequence))
  (for-each (λ (x) (hash-set! defaults-ht x 'lambda))
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
               parameterize parameterize*
               call-with-input-file call-with-input-file* with-input-from-file
               with-input-from-port call-with-output-file
               with-output-to-file with-output-to-port 

	       for-all
               
               type-case))
  (preferences:set-default 
   'framework:tabify
   (list defaults-ht #rx"^begin" #rx"^def" #f)
   (list/c hash? (or/c #f regexp?) (or/c #f regexp?) (or/c #f regexp?)))
  (define old-style-pred? (listof (list/c symbol? symbol?)))
  (define pref-pred?
    (list/c (or/c 
             ;; old-style prefs
             old-style-pred?
             
             ;; new-style prefs
             (list/c (listof (list/c symbol? symbol?))   ;; additions to defaults
                     (listof (list/c symbol? symbol?)))) ;; deletions
            
            (or/c regexp? #f)
            (or/c regexp? #f)
            (or/c regexp? #f)))
  
  (define (ht->addition/deletion-lists ht)
    (define additions '())
    (define deletions '())
    (for ([(k v) (in-hash ht)])
      (unless (hash-ref defaults-ht k #f)
        (set! additions (cons (list k v) additions))))
    (for ([(k v) (in-hash defaults-ht)])
      (unless (hash-ref ht k #f)
        (set! deletions (cons (list k v) deletions))))
    (list additions deletions))
  
  (define (addition/deletion-lists->ht lsts)
    (define additions (list-ref lsts 0))
    (define deletions (list-ref lsts 1))
    (define ht (hash-copy defaults-ht))
    (for ([pr (in-list deletions)])
      (define k (list-ref pr 0))
      (define v (list-ref pr 1))
      (when (equal? (hash-ref ht k #f) v)
        (hash-remove! ht k)))
    (for ([pr (in-list additions)])
      (define k (list-ref pr 0))
      (define v (list-ref pr 1))
      (hash-set! ht k v))
    ht)
  
  (preferences:set-un/marshall
   'framework:tabify 
   (λ (t) (cons (ht->addition/deletion-lists (list-ref t 0))
                (cdr t)))
   (λ (l) 
     (and (pref-pred? l)
          (cond
            [(old-style-pred? (list-ref l 0))
             ;; when migrating prefs from the old style,
             ;; get rid of any apparent deletions, as
             ;; they are likely unintentional, a result 
             ;; of moving defaults
             (define h (make-hasheq))
             (for-each (λ (x) (apply hash-set! h x)) (list-ref l 0))
             (define lsts (ht->addition/deletion-lists h))
             (cons (addition/deletion-lists->ht (list (list-ref l 0)
                                                      '()))
                   (cdr l))]
            [else 
             (cons (addition/deletion-lists->ht (list-ref l 0))
                   (cdr l))])))))


(preferences:set-default 'framework:autosave-delay 30 number?)
(preferences:set-default 'framework:autosaving-on? #t boolean?)
(preferences:set-default 'framework:backup-files? #t boolean?)
(preferences:set-default 'framework:verify-exit #t boolean?)
(preferences:set-default 'framework:delete-forward? #t boolean?)
(preferences:set-default 'framework:show-periods-in-dirlist #f boolean?)
(preferences:set-default 'framework:file-dialogs 'std
                         (λ (x) (and (memq x '(common std)) #t)))

(for ([line (in-list (racket:get-color-prefs-table))]
      [white-on-black-line (in-list (racket:get-white-on-black-color-prefs-table))])
  (define sym (car line))
  (define color (cadr line))
  (define white-on-black-color (cadr white-on-black-line))
  (color-prefs:add-color-scheme-entry (racket:short-sym->pref-name sym)
                                      #:style (racket:short-sym->style-name sym)
                                      color
                                      white-on-black-color))
  
(preferences:set-default 'framework:coloring-active #t boolean?)

(color-prefs:add-color-scheme-entry 'framework:default-text-color "black" "white")
(color-prefs:register-color-scheme-entry-change-callback
 'framework:basic-canvas-background
 (λ (v)
   (editor:set-default-font-color
    (color-prefs:lookup-in-color-scheme 'framework:default-text-color)
    v)))
(color-prefs:register-color-scheme-entry-change-callback
 'framework:default-text-color
 (λ (v)
   (editor:set-default-font-color 
    v 
    (color-prefs:lookup-in-color-scheme 'framework:basic-canvas-background))))
(editor:set-default-font-color 
 (color-prefs:lookup-in-color-scheme 'framework:default-text-color)
 (color-prefs:lookup-in-color-scheme 'framework:basic-canvas-background))

(color-prefs:add-color-scheme-entry 'framework:misspelled-text-color "black" "white")

(color-prefs:set-default/color-scheme 'framework:delegatee-overview-color
                                      "light blue"
                                      (make-object color% 62 67 155))

 
(let ([delta (make-object style-delta%)]
      [style (send (editor:get-standard-style-list) find-named-style
                   color:misspelled-text-color-style-name)])
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
  (color-prefs:register-color-scheme-entry-change-callback
   'framework:misspelled-text-color
   (λ (v) (update-style-list v)))
  (update-style-list 
   (color-prefs:lookup-in-color-scheme 'framework:misspelled-text-color)))

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
