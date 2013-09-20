#lang racket/unit

(require racket/class
         racket/list
         racket/runtime-path
         racket/contract
         racket/struct-info
         setup/getinfo
         mred
         framework
         framework/splash
         drracket/private/drsig
         "language-object-contract.rkt"
         "wrap-tool-inputs.rkt"
         mrlib/switchable-button
         net/url
         string-constants)

(import [prefix drracket:frame: drracket:frame^]
        [prefix drracket:unit: drracket:unit^]
        [prefix drracket:rep: drracket:rep^]
        [prefix drracket:get/extend: drracket:get/extend^]
        [prefix drracket:language: drracket:language^]
        [prefix drracket:language-configuration: drracket:language-configuration^]
        [prefix drracket:help-desk: drracket:help-desk^]
        [prefix drracket:init: drracket:init^]
        [prefix drracket:debug: drracket:debug^]
        [prefix drracket:eval: drracket:eval^]
        [prefix drracket:modes: drracket:modes^]
        [prefix drracket:tracing: drracket:tracing^]
        [prefix drracket:module-language: drracket:module-language^]
        [prefix drracket:module-language-tools: drracket:module-language-tools/int^]
        [prefix drracket:tools-drs: drracket:tools-drs^]
        [prefix drracket: drracket:interface^])
(export drracket:tools^)

;; An installed-tool is
;; (make-installed-tool directory-record module-spec string/#f string/#f string/#f string/#f boolean)
(define-struct installed-tool (dir spec bitmap name url drracket?) #:transparent)

;; installed-tools : (list-of installed-tool)
(define installed-tools null)

;; candidate-tools : (listof installed-tool)
(define candidate-tools null)

;; successfully-loaded-tool = 
;; (make-successfully-loaded-tool 
;;    module-spec (union #f (instanceof bitmap%)) (union #f string) (union #f string)
;;    (-> void) (-> void))
(define-struct successfully-loaded-tool (spec bitmap name url phase1 phase2))

;; successfully-loaded-tools : (listof successfully-loaded-tool)
;; this list contains the tools that successfully were loaded
;; it is updated in load/invoke-tool. 
(define successfully-loaded-tools null)

;; successful-tool = (make-successful-tool module-spec 
;;                                         (union #f (instanceof bitmap%))
;;                                         (union #f string)
;;                                         (union #f string))
(define-struct successful-tool (spec bitmap name url))

;; successful-tools : (listof successful-tool)
(define successful-tools null)

;; get-successful-tools : -> (listof successful-tool)
(define (get-successful-tools) successful-tools)

;; load/invoke-all-tools : -> void
(define (load/invoke-all-tools phase1-extras phase2-extras)
  (rescan-installed-tools!)
  (set! current-phase 'loading-tools)
  (set! candidate-tools (filter candidate-tool? installed-tools))
  (for-each load/invoke-tool candidate-tools)
  (run-phases phase1-extras phase2-extras))

;; rescan-installed-tools! : -> void
(define (rescan-installed-tools!)
  (set! installed-tools (all-installed-tools)))

;; all-installed-tools : -> (list-of installed-tool)
(define (all-installed-tools)
  (apply append
         (map installed-tools-for-directory
              (all-tool-directories))))

;; all-tool-directories : -> (list-of directory-record)
(define (all-tool-directories)
  (find-relevant-directory-records
   '(drracket-tools drracket-tool-icons drracket-tool-names drracket-tool-urls
     tools tool-icons tool-names tool-urls)))

;; installed-tools-for-directory : directory-record -> (list-of installed-tool)
(define (installed-tools-for-directory coll-dir)
  (append (installed-tools-for-directory/keys coll-dir 'tools 'tool-icons 'tool-names 'tool-urls #f)
          (installed-tools-for-directory/keys coll-dir 'drracket-tools 'drracket-tool-icons 'drracket-tool-names 'drracket-tool-urls #t)))

;; installed-tools-for-directory/keys : directory-record symbol symbol symbol symbol boolean -> (list-of installed-tool)
(define (installed-tools-for-directory/keys coll-dir tools-key tool-icons-key tool-names-key tool-urls-key drracket-tool?)
  (let ([table (with-handlers ((exn:fail? values))
                 (get-info/full (directory-record-path coll-dir)))])
    (cond
      [(not table)
       null]
      [(exn? table)
       (message-box (string-constant drscheme)
                    (format (string-constant error-loading-tool-title)
                            (directory-record-path coll-dir)
                            (let ([sp (open-output-string)])
                              (parameterize ([current-error-port sp]
                                             [current-error-port sp])
                                (drracket:init:original-error-display-handler (exn-message table) table))
                              (get-output-string sp)))
                    #f
                    '(ok stop))
       null]
      [else
       (let* ([tools (table tools-key (lambda () null))]
              [tool-icons (table tool-icons-key (lambda () (map (lambda (x) #f) tools)))]
              [tool-names (table tool-names-key (lambda () (map (lambda (x) #f) tools)))]
              [tool-urls (table tool-urls-key (lambda () (map (lambda (x) #f) tools)))])
         (unless (= (length tools) (length tool-icons))
           (message-box (string-constant drscheme)
                        (format (string-constant tool-tool-icons-same-length)
                                coll-dir tools tool-icons)
                        #f
                        '(ok stop))
           (set! tool-icons (map (lambda (x) #f) tools)))
         (unless (= (length tools) (length tool-names))
           (message-box (string-constant drscheme)
                        (format (string-constant tool-tool-names-same-length)
                                coll-dir tools tool-names)
                        #f
                        '(ok stop))
           (set! tool-names (map (lambda (x) #f) tools)))
         (unless (= (length tools) (length tool-urls))
           (message-box (string-constant drscheme)
                        (format (string-constant tool-tool-urls-same-length)
                                coll-dir tools tool-urls)
                        #f
                        '(ok stop))
           (set! tool-urls (map (lambda (x) #f) tools)))
         (map (lambda (t i n u) (make-installed-tool coll-dir t i n u drracket-tool?))
              tools tool-icons tool-names tool-urls))])))

;; candidate-tool? : installed-tool -> boolean
;; Predicate for tools selected for execution in this
;; run of DrRacket (depending on env variables and preferences)
(define candidate-tool?
  (cond
    [(getenv "PLTNOTOOLS")
     (printf "PLTNOTOOLS: skipping tools\n") (flush-output)
     (lambda (it) #f)]
    [(getenv "PLTONLYTOOL")
     => (lambda (onlys)
          (let* ([allowed (let ([exp (read (open-input-string onlys))])
                            (cond 
                             [(symbol? exp) (list exp)]
                             [(pair? exp) exp]
                             [else '()]))]
                 [directory-ok? (lambda (x) 
                                  (let-values ([(base name dir) (split-path x)])
                                    (memq (string->symbol (path->string name))
                                          allowed)))])
            (printf "PLTONLYTOOL: only loading ~s\n" allowed) (flush-output)
            (lambda (it)
              (directory-ok?
               (directory-record-path
                (installed-tool-dir it))))))]
    [else
     (lambda (it)
       (eq? (or (get-tool-configuration it)
                (default-tool-configuration it))
            'load))]))

;; get-tool-configuration : installed-tool -> symbol/#f
;; Get tool configuration preference or #f if no preference set.
(define (get-tool-configuration it)
  (let ([p (assoc (installed-tool->key it) (toolspref))])
    (and p (cadr p))))

;; default-tool-configuration : installed-tool -> (union 'load 'skip)
(define (default-tool-configuration it)
  (preferences:get 'drracket:default-tools-configuration))

(define toolspref
  (case-lambda
    [() (preferences:get 'drracket:tools-configuration)]
    [(v) (preferences:set 'drracket:tools-configuration v)]))

(define (installed-tool->key it)
  (list (directory-record-spec (installed-tool-dir it))
        (installed-tool-spec it)))

(define (installed-tool-full-path it)
  (apply build-path
         (directory-record-path (installed-tool-dir it))
         (let ([path-parts (installed-tool-spec it)])
           (cond [(list? path-parts)
                  (append (cdr path-parts) (list (car path-parts)))]
                 [else (list path-parts)]))))

(define (installed-tool->module-spec it)
  (let* ([dirrec (installed-tool-dir it)]
         [key (directory-record-spec dirrec)]
         [maj (directory-record-maj dirrec)]
         [min (directory-record-min dirrec)]
         [parts (let ([parts0 (installed-tool-spec it)])
                  (if (list? parts0)
                      parts0
                      (list parts0)))]
         [file (car parts)]
         [rest-parts (cdr parts)])
    (case (car key)
      ((lib)
       `(lib ,(string-append
               (apply string-append
                      (map (lambda (s)
                             (string-append s "/"))
                           (append (cdr key) rest-parts)))
               file)))
      ((planet)
       `(planet ,file (,@(cdr key) ,maj ,min) ,@rest-parts)))))

;; installed-tool-is-loaded : installed-tool -> boolean
(define (installed-tool-is-loaded? it)
  (let ([path (installed-tool-full-path it)])
    (ormap (lambda (st) (equal? path (successful-tool-spec st)))
           (get-successful-tools))))


;                                                                                        
;                                                                                        
;                                                                                        
;  ;;;;                        ;;;;  ;;  ;;                             ;;;;             
;  ;;;;                        ;;;;  ;;  ;;                             ;;;;             
;  ;;;;   ;;;;   ;;;;;;;    ;;;;;;;  ;;     ;;;; ;;;  ;;;  ;;;   ;;;;   ;;;; ;;;   ;;;   
;  ;;;;  ;;;;;;  ;;;;;;;;  ;;;;;;;;  ;;;;;; ;;;;;;;;; ;;;  ;;;  ;;;;;;  ;;;; ;;;  ;;;;;  
;  ;;;; ;;;;;;;;     ;;;; ;;;;;;;;;  ;;;;;; ;;;; ;;;;  ;;;;;;  ;;;;;;;; ;;;;;;;  ;;;; ;; 
;  ;;;; ;;;; ;;;  ;;;;;;; ;;;; ;;;; ;; ;;;; ;;;; ;;;;  ;;;;;;  ;;;; ;;; ;;;;;;;  ;;;;;;; 
;  ;;;; ;;;;;;;; ;;  ;;;; ;;;;;;;;; ;; ;;;; ;;;; ;;;;  ;;;;;;  ;;;;;;;; ;;;; ;;; ;;;;;   
;  ;;;;  ;;;;;;  ;;;;;;;;  ;;;;;;;; ;; ;;;; ;;;; ;;;;   ;;;;    ;;;;;;  ;;;; ;;;  ;;;;;; 
;  ;;;;   ;;;;    ;; ;;;;   ;;;;;;; ;; ;;;; ;;;; ;;;;   ;;;;     ;;;;   ;;;; ;;;   ;;;;  
;                                   ;;                                                   
;                                                                                        
;                                                                                        



;; load/invoke-tool : installed-tool -> void
(define (load/invoke-tool it)
  (load/invoke-tool* (directory-record-path (installed-tool-dir it))
                     (installed-tool-spec it)
                     (installed-tool-bitmap it)
                     (installed-tool-name it)
                     (installed-tool-url it)
                     (installed-tool-drracket? it)))

;; load/invoke-tool* :   path
;;                       (listof string[sub-collection-name]) 
;;                       (union #f (cons string[filename] (listof string[collection-name])))
;;                       (union #f string)
;;                       (union #f string)
;;                       boolean
;;                    -> void
;; `coll' is a collection to load the tool from
;; `in-path' is the `coll'-relative collection-path spec for the tool module file
;; `icon-spec' is the collection-path spec for the tool's icon, if there is one.
;; `name' is the name of the tool (only used in about box)
(define (load/invoke-tool* coll-dir in-path icon-spec name tool-url drracket?)
  (let* ([icon-path 
          (cond
            [(string? icon-spec)
             (build-path coll-dir icon-spec)]
            [(and (list? icon-spec)
                  (andmap string? icon-spec))
             (apply collection-file-path icon-spec)]
            [else #f])]
         [tool-bitmap
          (and icon-path
               (install-tool-bitmap name icon-path))])
    (let/ec k
      (unless (or (string? in-path)
                  (and (list? in-path)
                       (not (null? in-path))
                       (andmap string? in-path)))
        (message-box (string-constant drscheme)
                     (format (string-constant invalid-tool-spec)
                             coll-dir in-path)
                     #f
                     '(ok stop))
        (k (void)))
      (let* ([tool-path
              (if (string? in-path) 
                  (build-path coll-dir in-path)
                  (apply build-path coll-dir (append (cdr in-path) (list (car in-path)))))]
             [unit 
               (with-handlers ([exn:fail? 
                                (lambda (x)
                                  (show-error
                                   (format (string-constant error-invoking-tool-title)
                                           coll-dir in-path)
                                   x)
                                  (k (void)))])
                 (dynamic-require tool-path 'tool@))])
        (with-handlers ([exn:fail? 
                         (lambda (x)
                           (show-error 
                            (format (string-constant error-invoking-tool-title)
                                    coll-dir in-path)
                            x))])
          (let-values ([(phase1-thunk phase2-thunk) 
                        (if drracket?
                            (invoke-tool unit (string->symbol (or name (path->string coll-dir))))
                            (drracket:tools-drs:invoke-drs-tool unit (string->symbol (or name (path->string coll-dir)))))])
            (set! successfully-loaded-tools 
                  (cons (make-successfully-loaded-tool
                         tool-path
                         tool-bitmap
                         name
                         tool-url 
                         phase1-thunk
                         phase2-thunk)
                        successfully-loaded-tools))))))))

;; invoke-tool : unit/sig string -> (values (-> void) (-> void))
;; invokes the tools and returns the two phase thunks.
(define (invoke-tool unit tool-name)
  (define-unit-binding unit@ unit (import drracket:tool^) (export drracket:tool-exports^))
  (language-object-abstraction drracket:language:object/c #f)
  (wrap-tool-inputs 
   (let ()
     (define-values/invoke-unit unit@
       (import drracket:tool^) (export drracket:tool-exports^))
     (values phase1 phase2))
   tool-name
   #f))

;; show-error : string (union exn TST) -> void
(define (show-error title x)
  (parameterize ([drracket:init:error-display-handler-message-box-title
                  title])
    ((error-display-handler)
     (if (exn? x)
         (format "~a\n\n~a" title (exn-message x))
         (format "~a\n\nuncaught exception: ~s" title x))
     x)))


;; install-tool-bitmap : string path -> bitmap
;; adds the tool's bitmap to the splash screen
(define (install-tool-bitmap name bitmap-path)
  (let/ec k
    (let ([bitmap
           (with-handlers ([exn:fail:filesystem? (lambda (x) (k (void)))])
             (make-object bitmap% bitmap-path 'unknown/mask))])
      (unless (and (is-a? bitmap bitmap%)
                   (send bitmap ok?))
        (k #f))
      (let ([splash-eventspace (get-splash-eventspace)]
            [splash-canvas (get-splash-canvas)]
            [splash-width (get-splash-width)]
            [splash-height (get-splash-height)])
        
        (unless (and (eventspace? splash-eventspace)
                     (is-a? splash-canvas canvas%))
          (k (void)))
        
        (parameterize ([current-eventspace splash-eventspace])
          (queue-callback
           (lambda ()
             (let ([bdc (make-object bitmap-dc%)]
                   [translated-tool-bitmap-y 
                    (max 0 (- splash-height tool-bitmap-y tool-bitmap-size))])
               
               ;; add the bitmap, but centered at its position
               ;; (used to truncate the bitmap
               ;; if it was too large, but no longer)
               ((dynamic-require 'framework/splash 'add-splash-icon)
                bitmap
                (floor (+ tool-bitmap-x
                          (- (/ tool-bitmap-size 2)
                             (/ (send bitmap get-width) 2))))
                (floor (+ translated-tool-bitmap-y
                          (- (/ tool-bitmap-size 2)
                             (/ (send bitmap get-height) 2)))))
               
               (set! tool-bitmap-x (+ tool-bitmap-x tool-bitmap-size tool-bitmap-gap))
               (when ((+ tool-bitmap-x tool-bitmap-gap tool-bitmap-size) . > . splash-width)
                 (set! tool-bitmap-y (+ tool-bitmap-y tool-bitmap-size tool-bitmap-gap))
                 (set! tool-bitmap-x tool-bitmap-gap))
               (when ((+ tool-bitmap-y tool-bitmap-gap tool-bitmap-size) . > . splash-width)
                 (set! tool-bitmap-y tool-bitmap-gap)))))))
      bitmap)))

(define tool-bitmap-gap 3)
(define tool-bitmap-x tool-bitmap-gap)
(define tool-bitmap-y tool-bitmap-gap)
(define tool-bitmap-size 32)

;     
;     
;            ;;                             ;                          ;;;  
;             ;                           ;;;            ;;;          ;   ; 
;             ;                             ;           ;  ;          ;   ; 
;     ; ;;;   ; ;;   ;;;;    ;;;    ;;;     ;           ;             ;   ; 
;      ;   ;  ;;  ;      ;  ;   ;  ;   ;    ;            ;   ;           ;  
;      ;   ;  ;   ;   ;;;;   ;;;   ;;;;;    ;           ;;; ;           ;   
;      ;   ;  ;   ;  ;   ;      ;  ;        ;          ;   ;           ;    
;      ;   ;  ;   ;  ;   ;  ;   ;  ;   ;    ;          ;   ;;         ;   ; 
;      ;;;;  ;;; ;;;  ;;; ;  ;;;    ;;;   ;;;;;         ;;;  ;        ;;;;; 
;      ;                                                                    
;      ;                                                                    
;     ;;;                                                                   
;       

;; run-phases : -> void
(define (run-phases phase1-extras phase2-extras)
  (drracket:module-language-tools:no-more-online-expansion-handlers)
  (let* ([after-phase1 (run-one-phase 'phase1
                                      (string-constant tool-error-phase1)
                                      successfully-loaded-tool-phase1
                                      successfully-loaded-tools
                                      phase1-extras)]
         [after-phase2 (run-one-phase 'phase2
                                      (string-constant tool-error-phase2)
                                      successfully-loaded-tool-phase2
                                      after-phase1
                                      phase2-extras)])
    (set! current-phase 'init-complete)
    (set! successful-tools
          (map (lambda (x) (make-successful-tool
                            (successfully-loaded-tool-spec x)
                            (successfully-loaded-tool-bitmap x)
                            (successfully-loaded-tool-name x)
                            (successfully-loaded-tool-url x)))
               after-phase2))))

;; run-one-phase : string 
;;                 (successfully-loaded-tool -> (-> void))
;;                 (listof successfully-loaded-tool)
;;                 (-> void)
;;              -> (listof successfully-loaded-tool)
;; filters out the tools that raise exceptions during the phase.
;; extras is the thunk for DrRacket init stuff on this phase.
(define (run-one-phase _the-phase err-fmt selector tools extras)
  (set! current-phase _the-phase)
  (extras)
  (let loop ([tools tools])
    (cond
      [(null? tools) null]
      [else 
       (let ([tool (car tools)])
         (let ([phase-thunk (selector tool)])
           (with-handlers ([exn:fail?
                            (lambda (exn) 
                              (show-error
                               (format err-fmt 
                                       (successfully-loaded-tool-spec tool)
                                       (successfully-loaded-tool-name tool))
                               exn)
                              (loop (cdr tools)))])
             (phase-thunk)
             (cons tool (loop (cdr tools))))))])))

;; current-phase : (union #f 'loading-tools 'phase1 'phase2 'init-complete)
(define current-phase #f)
(define (get-current-phase) current-phase)

;; only-in-phase : sym (union #f 'loading-tools 'phase1 'phase2 'init-complete) ... -> void
;; raises an error unless one of `phases' is the current phase
(define (only-in-phase func . phases)
  (unless (memq current-phase phases)
    (error func "can only be called in phase: ~a"
           (apply string-append 
                  (map (lambda (x) (format "~e " x))
                       (filter (lambda (x) x) phases))))))

;; Preferences GUI

(define load-action "Load the tool")
(define skip-action "Skip the tool")

(define (add-prefs-panel)
  (preferences:add-panel
   "Tools"
   (lambda (parent)
     (define main (new vertical-panel% (parent parent)))
     (define advisory
       (new message%
            (parent main)
            (label "Changes to tool configuration will take effect the next time you start DrRacket.")))
     (define listing
       (new list-box%
            (parent main)
            (label "Installed tools")
            (choices null)
            (callback (lambda _ (on-select-tool)))))
     (define info
       (new vertical-panel%
            (parent main)
            (style '(border))
            (stretchable-height #f)))
     (define location
       (new text-field%
            (parent info)
            (label "Tool: ")))
     (define location-editor (send location get-editor))
     (define configuration
       (new radio-box%
            (label "Load the tool when DrRacket starts?")
            (parent info)
            (choices (list load-action skip-action #| default-action |#))
            (callback (lambda _ (on-select-policy)))))

     (define (populate-listing!)
       (send listing clear)
       (for-each
        (lambda (entry+it)
          (send listing append
                (car entry+it)
                (cdr entry+it)))
        (sort (map (lambda (it) (cons (tool-list-entry it) it))
                   installed-tools)
              (lambda (a b)
                (string<? (car a) (car b))))))
     (define (tool-list-entry it)
       (let ([name (or (installed-tool-name it)
                       (format "unnamed tool ~a"
                               (installed-tool->module-spec it)))])
         (cond [(installed-tool-is-loaded? it)
                (string-append name " (loaded)")]
               [(not (memq it candidate-tools))
                (string-append name " (skipped)")]
               [else
                (string-append name " (failed to load)")])))
     (define (on-select-tool)
       (let ([it (get-selected-tool)])
         (send* location-editor
           (begin-edit-sequence)
           (lock #f)
           (erase)
           (insert
            (if it
                (format "~s" (installed-tool->module-spec it))
                ""))
           (lock #t)
           (end-edit-sequence))
         (send configuration set-selection
               (case (and it (get-tool-configuration it))
                 ((load) 0)
                 ((skip) 1)
                 ((#f) 0))) ;; XXX (or 2, if default is an option)
         (send configuration enable (and it #t))
         (void)))
     (define (on-select-policy)
       (let ([it (get-selected-tool)]
             [policy
              (case (send configuration get-selection)
                ((0) 'load)
                ((1) 'skip))])
         (when it
           (let ([key (installed-tool->key it)])
             (case policy
               ((load)
                (toolspref (cons (list key 'load)
                                 (let ([ts (toolspref)])
                                   (remove (assoc key ts) ts)))))
               ((skip)
                (toolspref (cons (list key 'skip)
                                 (let ([ts (toolspref)])
                                   (remove (assoc key ts) ts)))))
               ((#f)
                (toolspref (let ([ts (toolspref)])
                             (remove (assoc key ts) ts))))))))
       (void))
     (define (get-selected-tool)
       (let ([index (send listing get-selection)])
         (and index (send listing get-data index))))
     (populate-listing!)
     (send location-editor lock #t)
     main)))
