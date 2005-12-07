;; WARNING: printf is rebound in this module to always use the 
;;          original stdin/stdout of drscheme, instead of the 
;;          user's io ports, to aid any debugging printouts.
;;          (esp. useful when debugging the users's io)

(module language mzscheme
  (require "drsig.ss"
           (lib "string-constant.ss" "string-constants")
           (lib "pconvert.ss")
           (lib "pretty.ss")
           (lib "etc.ss")
	   (lib "unitsig.ss")
	   (lib "struct.ss")
           (lib "class.ss")
           (lib "file.ss")
           (lib "list.ss")
           (lib "embed.ss" "compiler")
           (lib "launcher.ss" "launcher")
	   (lib "mred.ss" "mred")
	   (lib "framework.ss" "framework")
           (lib "syntax-browser.ss" "mrlib"))

  (provide language@)

  (define language@
    (unit/sig drscheme:language^
      (import [drscheme:debug : drscheme:debug^]
              [drscheme:teachpack : drscheme:teachpack^]
              [drscheme:tools : drscheme:tools^]
              [drscheme:help-desk : drscheme:help-desk^])
      
      (define original-output-port (current-output-port))
      (define (printf . args) (apply fprintf original-output-port args)) 
      
      (define-struct text/pos (text start end))
      ;; text/pos = (make-text/pos (instanceof text% number number))
      ;; this represents a portion of a text to be processed.
      
      (define language<%>
	(interface ()
	  marshall-settings
          unmarshall-settings
          default-settings
	  default-settings?
          
          order-manuals
          
          front-end/complete-program
          front-end/interaction
	  config-panel
	  on-execute
          first-opened
          render-value/format
          render-value
          
          create-executable
          
          get-language-position
          get-language-name
          get-style-delta
          get-language-numbers
          get-one-line-summary
          get-language-url
          get-comment-character))
      
      (define module-based-language<%>
	(interface ()
	  marshall-settings
          unmarshall-settings
          default-settings
	  default-settings?

          get-module
          get-transformer-module
          use-namespace-require/copy?
	  config-panel

          get-reader
	  
          on-execute
          get-init-code
          use-mred-launcher
          
          render-value/format
          render-value
          
          get-language-position 
          get-language-numbers
          get-one-line-summary
          get-language-url))
      
      (define simple-module-based-language<%>
	(interface ()
          get-module
          get-language-position
          get-language-numbers
          get-one-line-summary
          get-language-url
          get-reader))
      
      
                                          
          ;                  ;;;          
                               ;          
                               ;          
  ;;;   ;;;   ;;; ;  ; ;;;     ;     ;;;  
 ;   ;    ;    ; ; ;  ;   ;    ;    ;   ; 
  ;;;     ;    ; ; ;  ;   ;    ;    ;;;;; 
     ;    ;    ; ; ;  ;   ;    ;    ;     
 ;   ;    ;    ; ; ;  ;   ;    ;    ;   ; 
  ;;;   ;;;;; ;; ; ;; ;;;;   ;;;;;;  ;;;  
                      ;                   
                      ;                   
                     ;;;                  

      
      (define simple-module-based-language%
        (class* object% (simple-module-based-language<%>)
          (init-field module
                      language-position
                      (language-numbers (map (λ (x) 0) language-position))
                      (one-line-summary "")
                      (language-url #f)
                      (documentation-reference #f)
                      (reader (λ (src port)
				(let ([v (parameterize ([read-accept-reader #t])
					   (read-syntax src port))])
                                  (if (eof-object? v)
				      v
				      (namespace-syntax-introduce v))))))
          (define/public (get-module) module)
	  (define/public (get-language-position) language-position)
          (define/public (get-language-numbers) language-numbers)
          (define/public (get-one-line-summary) one-line-summary)
          (define/public (get-language-url) language-url)
	  (define/public (get-reader) reader)
          (super-instantiate ())))
      

                                                                                                  
                                ;;         ;;;                 ;;                              ;; 
                                 ;           ;                  ;                               ; 
         ;                       ;           ;                  ;                               ; 
          ;   ;;; ;    ;;;    ;;;; ;;  ;;    ;     ;;;          ;;;;   ;;;;    ;;;    ;;;    ;;;; 
           ;   ; ; ;  ;   ;  ;   ;  ;   ;    ;    ;   ;         ;   ;      ;  ;   ;  ;   ;  ;   ; 
 ;;;;;      ;  ; ; ;  ;   ;  ;   ;  ;   ;    ;    ;;;;;  ;;;;;  ;   ;   ;;;;   ;;;   ;;;;;  ;   ; 
           ;   ; ; ;  ;   ;  ;   ;  ;   ;    ;    ;             ;   ;  ;   ;      ;  ;      ;   ; 
          ;    ; ; ;  ;   ;  ;   ;  ;   ;    ;    ;   ;         ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
         ;    ;; ; ;;  ;;;    ;;; ;  ;;; ; ;;;;;;  ;;;         ; ;;;    ;;; ;  ;;;    ;;;    ;;; ;
                                                                                                  
                                                                                                  
                                                                                                  
      
      ;; simple-module-based-language->module-based-language : module-based-language<%>
      ;; transforms a simple-module-based-language into a module-based-language<%>
      (define simple-module-based-language->module-based-language-mixin
	(mixin (simple-module-based-language<%>) (module-based-language<%>)
	  (define/public (get-transformer-module) 'mzscheme)
          (define/public (use-namespace-require/copy?) #f)
          (define/public (use-mred-launcher) #t)
          
          (inherit get-module)
          (define/public (marshall-settings settings)
	    (simple-settings->vector settings))
          (define/public (unmarshall-settings printable)
            (and (vector? printable)
                 (= (vector-length printable)
                    (procedure-arity make-simple-settings))
                 (boolean? (vector-ref printable 0))
                 (memq (vector-ref printable 1) '(constructor quasiquote write current-print))
                 (memq (vector-ref printable 2) 
                       '(mixed-fraction 
                         mixed-fraction-e
                         repeating-decimal 
                         repeating-decimal-e))
                 (boolean? (vector-ref printable 3))
                 (boolean? (vector-ref printable 4))
                 (memq (vector-ref printable 5) '(none debug debug/profile test-coverage))
                 (apply make-simple-settings (vector->list printable))))
          (define/public (default-settings) 
            (make-simple-settings #t 'write 'mixed-fraction-e #f #t 'debug))
          (define/public (default-settings? x)
	    (equal? (simple-settings->vector x)
		    (simple-settings->vector (default-settings))))
          (define/public (config-panel parent)
	    (simple-module-based-language-config-panel parent))
	  
          (define/public (on-execute setting run-in-user-thread)
	    (initialize-simple-module-based-language setting run-in-user-thread))
          (define/public (get-init-code setting teachpacks)
            (simple-module-based-language-get-init-code setting teachpacks))
          
          (define/public (render-value/format value settings port width)
            (simple-module-based-language-render-value/format value settings port width))
          (define/public (render-value value settings port)
            (simple-module-based-language-render-value/format value settings port 'infinity))
	  (super-instantiate ())))

      ;; settings for a simple module based language
      (define-struct simple-settings (case-sensitive 
                                      printing-style
                                      fraction-style
                                      show-sharing
                                      insert-newlines
                                      annotations))
      ;;  case-sensitive  : boolean
      ;;  printing-style  : (union 'write 'constructor 'quasiquote 'current-print)
      ;;  fraction-style  : (union 'mixed-fraction 'mixed-fraction-e 'repeating-decimal 'repeating-decimal-e)
      ;;  show-sharing    : boolean
      ;;  insert-newlines : boolean
      ;;  annotations     : (union 'none 'debug 'debug/profile 'test-coverage)
      (define simple-settings->vector (make-->vector simple-settings))

      ;; simple-module-based-language-config-panel : parent -> (case-> (-> settings) (settings -> void))
      (define (simple-module-based-language-config-panel _parent)
	(letrec ([parent (instantiate vertical-panel% ()
                           (parent _parent)
                           (alignment '(center center)))]
                 
                 [input-panel (instantiate group-box-panel% ()
                                (label (string-constant input-syntax))
                                (parent parent)
                                (alignment '(left center)))]
                 
                 [dynamic-panel (instantiate group-box-panel% ()
                                  (label (string-constant dynamic-properties))
                                  (parent parent)
                                  (alignment '(left center)))]
                 
                 [output-panel (instantiate group-box-panel% ()
                                 (label (string-constant output-syntax))
                                 (parent parent)
                                 (alignment '(left center)))]
                 
                 [case-sensitive (make-object check-box%
                                   (string-constant case-sensitive-label)
                                   input-panel
                                   void)]
                 [debugging (instantiate radio-box% ()
                              (label #f)
                              (choices 
                               (list (string-constant no-debugging-or-profiling)
                                     (string-constant debugging)
                                     (string-constant debugging-and-profiling)
                                     (string-constant test-coverage)))
                              (parent dynamic-panel)
                              (callback void))]
                 [output-style (make-object radio-box%
                                 (string-constant output-style-label)
                                 (list (string-constant constructor-printing-style)
                                       (string-constant quasiquote-printing-style)
                                       (string-constant write-printing-style)
                                       (string-constant print-printing-style))
                                 output-panel
                                 (λ (rb evt)
                                   (let ([on? (not (= (send rb get-selection) 3))])
                                     (send fraction-style enable on?)
                                     (send show-sharing enable on?)
                                     (send insert-newlines enable on?))))]
                 [fraction-style
                  (make-object check-box% (string-constant decimal-notation-for-rationals)
                    output-panel
                    void)]
                 [show-sharing (make-object check-box%
                                 (string-constant sharing-printing-label)
                                 output-panel
                                 void)]
                 [insert-newlines (make-object check-box%
                                    (string-constant use-pretty-printer-label)
                                    output-panel
                                    void)])
          
	  (case-lambda
            [()
             (make-simple-settings
              (send case-sensitive get-value)
              (case (send output-style get-selection)
                [(0) 'constructor]
                [(1) 'quasiquote]
                [(2) 'write]
                [(3) 'current-print])
              (if (send fraction-style get-value)
                  'repeating-decimal-e
                  'mixed-fraction-e)
              (send show-sharing get-value)
              (send insert-newlines get-value)
              (case (send debugging get-selection)
                [(0) 'none]
                [(1) 'debug]
                [(2) 'debug/profile]
                [(3) 'test-coverage]))]
            [(settings)
             (send case-sensitive set-value (simple-settings-case-sensitive settings))
             (send output-style set-selection
                   (case (simple-settings-printing-style settings)
                     [(constructor) 0]
                     [(quasiquote) 1]
                     [(write) 2]
                     [(current-print) 3]))
             (let ([on? (not (eq? 'current-print (simple-settings-printing-style settings)))])
               (send fraction-style enable on?)
               (send show-sharing enable on?)
               (send insert-newlines enable on?))
             (send fraction-style set-value (eq? (simple-settings-fraction-style settings)
                                                 'repeating-decimal-e))
             (send show-sharing set-value (simple-settings-show-sharing settings))
             (send insert-newlines set-value (simple-settings-insert-newlines settings))
             (send debugging set-selection
                   (case (simple-settings-annotations settings)
                     [(none) 0]
                     [(debug) 1]
                     [(debug/profile) 2]
                     [(test-coverage) 3]))])))

      ;; simple-module-based-language-render-value/format : TST settings port (union #f (snip% -> void)) (union 'infinity number) -> void
      (define (simple-module-based-language-render-value/format value settings port width)
        (if (eq? (simple-settings-printing-style settings) 'current-print)
            (parameterize ([current-output-port port])
              ((current-print) value))
            (let ([converted-value (simple-module-based-language-convert-value value settings)])
              (setup-printing-parameters 
               (λ ()
                 (cond
                   [(simple-settings-insert-newlines settings)
                    (if (number? width)
                        (parameterize ([pretty-print-columns width])
                          (pretty-print converted-value port))
                        (pretty-print converted-value port))]
                   [else
                    (parameterize ([pretty-print-columns 'infinity])
                      (pretty-print converted-value port))
                    (newline port)]))
               settings
               width))))
      
      ;; setup-printing-parameters : (-> void) -> void
      (define (setup-printing-parameters thunk settings width)
        (let ([use-number-snip?
                   (λ (x)
                     (and (number? x)
                          (exact? x)
                          (real? x)
                          (not (integer? x))))])
          (parameterize ([pretty-print-columns width]
                         [pretty-print-size-hook
                          (λ (value display? port)
                            (cond
			      [(not (port-writes-special? port)) #f]
                              [(is-a? value snip%) 1]
                              [(use-number-snip? value) 1]
                              [(syntax? value) 1]
                              [(to-snip-value? value) 1]
                              [else #f]))]
                         [pretty-print-print-hook
                          (λ (value display? port)
                            (cond
                              [(is-a? value snip%)
                               (write-special value port)
                               1]
                              [(use-number-snip? value)
                               (write-special
                                (case (simple-settings-fraction-style settings)
                                  [(mixed-fraction) 
                                   (number-snip:make-fraction-snip value #f)]
                                  [(mixed-fraction-e)
                                   (number-snip:make-fraction-snip value #t)]
                                  [(repeating-decimal)
                                   (number-snip:make-repeating-decimal-snip value #f)]
                                  [(repeating-decimal-e)
                                   (number-snip:make-repeating-decimal-snip value #t)])
                                port)
                               1]
                              [(syntax? value)
                               (write-special (render-syntax/snip value) port)]
                              [else (write-special (value->snip value) port)]))]
                         [print-graph
                          ;; only turn on print-graph when using `write' printing 
                          ;; style because the sharing is being taken care of
                          ;; by the print-convert sexp construction when using
                          ;; other printing styles.
                          (and (eq? (simple-settings-printing-style settings) 'write)
                               (simple-settings-show-sharing settings))])
            (thunk))))
      
      ;; drscheme-inspector : inspector
      (define drscheme-inspector (current-inspector))
      
      ;; simple-module-based-language-convert-value : TST settings -> TST
      (define (simple-module-based-language-convert-value value settings)
        (case (simple-settings-printing-style settings)
          [(write) value]
          [(current-print) value]
          [(constructor)
           (parameterize ([constructor-style-printing #t]
                          [show-sharing (simple-settings-show-sharing settings)]
			  [current-print-convert-hook (leave-snips-alone-hook (current-print-convert-hook))])
             (print-convert value))]
          [(quasiquote)
           (parameterize ([constructor-style-printing #f]
                          [show-sharing (simple-settings-show-sharing settings)]
			  [current-print-convert-hook (leave-snips-alone-hook (current-print-convert-hook))])
             (print-convert value))]))
      
      ;; leave-snips-alone-hook : any? (any? -> printable) any? -> printable
      (define ((leave-snips-alone-hook sh) expr basic-convert sub-convert)
	(if (is-a? expr snip%)
	    expr
	    (sh expr basic-convert sub-convert)))

      ;; initialize-simple-module-based-language : setting ((-> void) -> void)
      (define (initialize-simple-module-based-language setting run-in-user-thread)
        (run-in-user-thread
         (λ ()
           (let ([annotations (simple-settings-annotations setting)])
             (when (memq annotations '(debug debug/profile test-coverage))
               (current-eval 
                (drscheme:debug:make-debug-eval-handler
                 (current-eval)))
               (error-display-handler 
                (drscheme:debug:make-debug-error-display-handler
                 (error-display-handler))))
             (drscheme:debug:profiling-enabled (eq? annotations 'debug/profile))
             (drscheme:debug:test-coverage-enabled (eq? annotations 'test-coverage)))
           (global-port-print-handler
            (λ (value port)
              (let ([converted-value (simple-module-based-language-convert-value value setting)])
                (setup-printing-parameters 
                 (λ ()
                   (parameterize ([pretty-print-columns 'infinity])
                     (pretty-print converted-value port)))
                 setting
                 'infinity))))
           (current-inspector (make-inspector))
           (read-case-sensitive (simple-settings-case-sensitive setting)))))
      
      ;; simple-module-based-language-get-init-code : setting teachpack-cache -> sexp[module]
      (define (simple-module-based-language-get-init-code setting teachpack-cache)
        `(module mod-name mzscheme
           (require (lib "pconvert.ss")
                    (lib "pretty.ss"))
           
           (provide init-code)
           
           (define (executable-error-value->string-handler val size)
             (let ([o (open-output-string)])
               (render-value val o)
               (let ([s (get-output-string o)])
                 (if ((string-length s) . <= . size)
                     s
                     (string-append
                      (substring s 0 (- size 3))
                      "...")))))
           
           (define (render-value value port)
             (parameterize ([pretty-print-columns 'infinity])
               (pretty-print (convert-value value) port)))
           
           (define (convert-value value)
             ,(case (simple-settings-printing-style setting)
                [(write) `value]
                [(current-print) `value]
                [(constructor)
                 `(parameterize ([constructor-style-printing #t]
                                 [show-sharing ,(simple-settings-show-sharing setting)])
                    (print-convert value))]
                [(quasiquote)
                 `(parameterize ([constructor-style-printing #f]
                                 [show-sharing ,(simple-settings-show-sharing setting)])
                    (print-convert value))]))
           
           ,(if (memq (simple-settings-annotations setting) '(debug debug/profile test-coverage))
                `(require (lib "errortrace.ss" "errortrace"))
                `(void))

           (define (init-code)
             ,(drscheme:teachpack:launcher-init-code teachpack-cache)
             (current-inspector (make-inspector))
             (error-value->string-handler executable-error-value->string-handler)
             (read-case-sensitive ,(simple-settings-case-sensitive setting)))))
      


                                                                      
               ;;;                                                    
                 ;                                                    
         ;       ;                                                    
          ;      ;    ;;;;  ; ;;;    ;;; ;;;  ;;  ;;;;    ;;; ;  ;;;  
           ;     ;        ;  ;;  ;  ;   ;  ;   ;      ;  ;   ;  ;   ; 
 ;;;;;      ;    ;     ;;;;  ;   ;  ;   ;  ;   ;   ;;;;  ;   ;  ;;;;; 
           ;     ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;     
          ;      ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
         ;     ;;;;;;  ;;; ;;;;  ;;  ;;;;   ;;; ;  ;;; ;  ;;;;   ;;;  
                                        ;                    ;        
                                        ;                    ;        
                                     ;;;                  ;;;         

      
      
      ;; module-based-language->language : module-based-language -> language<%>
      ;; given a module-based-language, implements a language
      (define module-based-language->language-mixin
	(mixin (module-based-language<%>) (language<%>)
	  (inherit get-module get-transformer-module use-namespace-require/copy?
                   get-init-code use-mred-launcher get-reader)
          
          (define/public (first-opened) (void))
          (define/public (get-comment-character) (values ";  " #\;))
          (define/public (order-manuals x) (values x #t))
          
          (inherit get-language-position)
          (define/public (get-language-name)
            (let ([pos (get-language-position)])
              (if (null? pos)
                  "<<unknown>>"
                  (car (last-pair pos)))))
          (define/public (get-style-delta) #f)
	  (define/override (on-execute setting run-in-user-thread)
	    (super on-execute setting run-in-user-thread)
            (initialize-module-based-language (use-namespace-require/copy?)
                                              (get-module)
                                              (get-transformer-module)
                                              run-in-user-thread))
          (define/public (front-end/complete-program port settings teachpack-cache)
            (module-based-language-front-end port (get-reader)))
          (define/public (front-end/interaction port settings teachpack-cache)
            (module-based-language-front-end port (get-reader)))
          (define/public (create-executable setting parent program-filename teachpacks)
            (create-module-based-language-executable parent 
                                                     program-filename
                                                     (get-module)
                                                     (get-transformer-module)
                                                     (get-init-code setting teachpacks)
                                                     (use-mred-launcher)
                                                     (use-namespace-require/copy?)))
          (super-instantiate ())))

      ;; create-module-based-language-executable :  
      ;; (is-a?/c area-container<%>) string module-spec module-spec sexp (union boolean? 'ask) boolean?
      ;;  -> void
      (define (create-module-based-language-executable parent
                                                       program-filename
                                                       module-language-spec
                                                       transformer-module-language-spec
                                                       init-code
                                                       mred-launcher
                                                       use-copy?)
        (let ([executable-specs (create-executable-gui parent
                                                       program-filename
                                                       #t 
						       (if (boolean? mred-launcher)
							   (if mred-launcher
							       'mred
							       'mzscheme)
							   #t))])
          (when executable-specs
            (let* ([type (car executable-specs)]
                   [base (cadr executable-specs)]
                   [executable-filename (caddr executable-specs)]
                   [create-executable
                    (if (eq? type 'launcher)
                        create-module-based-launcher 
                        create-module-based-stand-alone-executable)])
              (create-executable
               program-filename
               executable-filename 
               module-language-spec
               transformer-module-language-spec
               init-code
               (if (boolean? mred-launcher)
                   mred-launcher
                   (eq? base 'mred))
               use-copy?)))))
      
      
      ;; create-executeable-gui : (union #f (is-a?/c top-level-area-container<%>))
      ;;                          (union #f string?)
      ;;                          (union #t 'launcher 'stand-alone)
      ;;                          (union #t 'mzscheme 'mred)
      ;;                       -> (union #f (list (union 'no-show 'launcher 'stand-alone)
      ;;                                          (union 'no-show 'mzscheme 'mred)
      ;;                                          string[filename]))
      (define (create-executable-gui parent program-filename show-type show-base)
        (define dlg (make-object dialog% (string-constant create-executable-title) parent))
        (define filename-panel (make-object horizontal-panel% dlg))
        (define filename-text-field (instantiate text-field% ()
                                      (label (string-constant filename))
                                      (parent filename-panel)
                                      (init-value (path->string (default-executable-filename program-filename #f)))
                                      (min-width 400)
                                      (callback void)))
        (define filename-browse-button (instantiate button% ()
                                         (label (string-constant browse...))
                                         (parent filename-panel)
                                         (callback
                                          (λ (x y) (browse-callback)))))
        (define type/base/help-panel (instantiate horizontal-panel% ()
                                       (parent dlg)
                                       (alignment '(center center))))
        (define type/base-panel (instantiate vertical-panel% ()
                                  (parent type/base/help-panel)
                                  (stretchable-width #f)))
        (define type-panel (make-object horizontal-panel% type/base-panel))
        (define type-rb (and (boolean? show-type)
                             (instantiate radio-box% ()
                               (label (string-constant executable-type))
                               (choices (list (string-constant launcher)
                                              (string-constant stand-alone)))
                               (parent type-panel)
                               (callback void))))
        (define base-panel (make-object horizontal-panel% type/base-panel))
        (define base-rb (and (boolean? show-base)
                             (instantiate radio-box% ()
                               (label (string-constant executable-base))
                               (choices (list "MzScheme" "MrEd"))
                               (parent base-panel)
                               (callback void))))
        
        (define help-button (make-object button% 
                              (string-constant help)
                              type/base/help-panel
                              (λ (x y)
                                (send dlg show #f)
                                (drscheme:help-desk:goto-help "drscheme" "Executables"))))

        (define button-panel (instantiate horizontal-panel% ()
                               (parent dlg)
                               (alignment '(right center))))
        
        (define-values (ok-button cancel-button)
          (gui-utils:ok/cancel-buttons
           button-panel
           (λ (x y)
             (when (check-filename)
               (set! cancelled? #f)
               (send dlg show #f)))
           (λ (x y) (send dlg show #f))
           (string-constant create)
           (string-constant cancel)))

        (define (browse-callback)
          (let ([ftf (send filename-text-field get-value)])
            (let-values ([(base name _) 
                          (if (path-string? ftf)
                              (split-path ftf)
                              (values (current-directory) "" #f))])
              (let* ([mzscheme? (currently-mzscheme-binary?)]
                     [launcher? (currently-launcher?)]
                     [filename 
                      (put-executable/defaults
                       dlg
                       base
                       name
                       launcher?
                       (not mzscheme?)
                       (if launcher?
                           (if mzscheme?
                               (string-constant save-a-mzscheme-launcher)
                               (string-constant save-a-mred-launcher))
                           (if mzscheme?
                               (string-constant save-a-mzscheme-stand-alone-executable)
                               (string-constant save-a-mred-stand-alone-executable))))])
                (when filename
                  (send filename-text-field set-value (path->string filename)))))))
        
        (define (currently-mzscheme-binary?)
          (cond
            [base-rb
             (= 0 (send base-rb get-selection))]
            [else (eq? show-base 'mzscheme)]))
          
        (define (currently-launcher?)
          (cond
            [type-rb
             (= 0 (send type-rb get-selection))]
            [else (eq? show-type 'launcher)]))
        
        (define (check-filename)
          (let ([filename-str (send filename-text-field get-value)]
                [mred? (not (currently-mzscheme-binary?))])
            (let-values ([(extension style filters)
                          (if (currently-launcher?)
                              (if mred?
                                  (mred-launcher-put-file-extension+style+filters)
                                  (mzscheme-launcher-put-file-extension+style+filters))
                              (embedding-executable-put-file-extension+style+filters mred?))])
              
              (cond
                [(string=? "" filename-str)
                 (message-box (string-constant drscheme)
                              (string-constant please-choose-an-executable-filename)
                              dlg)
                 #f]
                [(not (users-name-ok? extension dlg (string->path filename-str)))
                 #f]
                [(or (directory-exists? filename-str)
                     (file-exists? filename-str))
                 (ask-user-can-clobber? filename-str)]
                [else #t]))))
           
        ;; ask-user-can-clobber-directory? : (is-a?/c top-level-window<%>) string -> boolean
        (define (ask-user-can-clobber? filename)
          (message-box (string-constant drscheme)
                       (format (string-constant are-you-sure-delete?) filename)
                       dlg
                       '(yes-no)))
            
        (define cancelled? #t)
        
        (send dlg show #t)
        (cond
          [cancelled? #f]
          [else
           (list
            (if type-rb
                (case (send type-rb get-selection)
                  [(0) 'launcher]
                  [(1) 'stand-alone])
                'no-show)
            (if base-rb
                (case (send base-rb get-selection)
                  [(0) 'mzscheme]
                  [(1) 'mred])
                'no-show)
            (send filename-text-field get-value))]))

      ;; put-executable : parent string boolean boolean -> (union false? string)
      ;; invokes the put-file dialog with arguments specific to building executables
      (define (put-executable parent program-filename launcher? mred? title)
        (let-values ([(base name dir) (split-path program-filename)])
          (let ([default-name (default-executable-filename name mred?)])
            (put-executable/defaults
             parent
             base
             default-name
             launcher?
             mred? 
             title))))
      
      ;; put-executable/defaults : parent string string boolean boolean -> (union false? string)
      (define (put-executable/defaults parent default-dir default-name launcher? mred? title)
        (let-values ([(extension style filters)
                      (if launcher?
                          (if mred?
                              (mred-launcher-put-file-extension+style+filters)
                              (mzscheme-launcher-put-file-extension+style+filters))
                          (embedding-executable-put-file-extension+style+filters mred?))])
          (let* ([dir? (if launcher?
                           (if mred?
                               (mred-launcher-is-directory?)
                               (mzscheme-launcher-is-directory?))
                           (embedding-executable-is-directory? mred?))]
                 [users-name
                  (if dir?
                      (get-directory title
                                     parent
                                     default-dir
                                     style)
                      (put-file title
                                parent
                                default-dir
                                default-name
                                extension
                                style
                                filters))])
            (and users-name
                 (users-name-ok? extension parent users-name)
                 (or (not dir?)
                     (gui-utils:get-choice
                      (format (string-constant warning-directory-will-be-replaced)
                              users-name)
                      (string-constant yes)
                      (string-constant no)
                      (string-constant drscheme)
                      #f
                      parent))
                 users-name))))
      
      ;; users-name-ok? : string (union #f frame% dialog%) string -> boolean
      ;; returns #t if the string is an acceptable name for
      ;; a saved executable, and #f otherwise.
      (define (users-name-ok? extension parent name)
        (or (not extension)
            (let ([suffix-m (regexp-match #rx"[.][^.]*$" (path->string name))])
              (or (and suffix-m
                       (string=? (substring (car suffix-m) 1) extension))
                  (begin
                    ;; FIXME: change the message to be platform-neutral and to
                    ;; use `extension' for the message
                    (case (system-type)
                      [(macosx) 
                       (message-box (string-constant drscheme)
                                    (format
                                     (string-constant macosx-executables-must-end-with-app)
                                     name)
                                    parent)]
                      [(windows) 
                       (message-box (string-constant drscheme)
                                    (format (string-constant windows-executables-must-end-with-exe)
                                            name)
                                    parent)])
                    #f)))))
      
      ;; default-executable-filename : path -> path
      (define (default-executable-filename program-filename mred?)
        (let* ([ext (filename-extension program-filename)]
               [program-bytename (path->bytes program-filename)]
               ;; ext-less : bytes
               [ext-less (if ext
                             (subbytes program-bytename
                                       0
                                       (- (bytes-length program-bytename)
                                          (bytes-length ext)
                                          1 ;; sub1 for the period in the extension
                                          ))
                             program-bytename)])
          (bytes->path
           (case (system-type)
             [(windows) (bytes-append ext-less #".exe")]
             [(macosx) (if mred?
                           (bytes-append ext-less #".app")
                           ext-less)]
             [else ext-less]))))
      
      ;; create-module-based-stand-alone-executable : ... -> void (see docs)
      (define (create-module-based-stand-alone-executable program-filename 
                                                          executable-filename
                                                          module-language-spec
                                                          transformer-module-language-spec
                                                          init-code
                                                          gui?
                                                          use-copy?)
      
        (with-handlers ([(λ (x) #f) ;exn:fail?
                         (λ (x)
                           (message-box 
                            (string-constant drscheme)
                            (format "~a" (exn-message x)))
                           (void))])
          (define init-code-tmp-filename (make-temporary-file "drs-standalone-exectable-init~a"))
          (define bootstrap-tmp-filename (make-temporary-file "drs-standalone-exectable-bootstrap~a"))
          (let ([init-code-mod-name
                 (let-values ([(base name dir?) 
                               (split-path init-code-tmp-filename)])
                   (string->symbol (path->string name)))])
          
            (call-with-output-file bootstrap-tmp-filename
              (λ (port)
                (write `(let () ;; cannot use begin, since it gets flattened to top-level (and re-compiled!)
                          (,(if use-copy? 'namespace-require/copy 'namespace-require) ',module-language-spec)
                          (namespace-transformer-require ',transformer-module-language-spec)
                          ((dynamic-require ',init-code-mod-name 'init-code)))
                       port))
              'truncate
              'text)
            
            (let ([new-init-code 
                   (list*
                    (car init-code)
                    init-code-mod-name
                    (cddr init-code))])
              (call-with-output-file init-code-tmp-filename
                (λ (port)
                  (write new-init-code port))
                'truncate 'text)))
          
          (let* ([pre-to-be-embedded-module-specs0
                  (if (equal? module-language-spec transformer-module-language-spec)
                      (list module-language-spec)
                      (list module-language-spec
                            transformer-module-language-spec))]
                 [pre-to-be-embedded-module-specs1
                  (if gui?
                      (cons '(lib "mred.ss" "mred")
                            pre-to-be-embedded-module-specs0)
                      pre-to-be-embedded-module-specs0)]
                 [pre-to-be-embedded-module-specs2
                  (cons `(file ,(path->string init-code-tmp-filename))
                        pre-to-be-embedded-module-specs1)]
                 [pre-to-be-embedded-module-specs3
                  (append (drscheme:teachpack:launcher-modules-to-embed
                           (preferences:get 'drscheme:teachpacks))
                          pre-to-be-embedded-module-specs2)]
                 [pre-to-be-embedded-module-specs4
                  (filter (λ (x) (not (eq? x 'mzscheme)))
                          pre-to-be-embedded-module-specs3)]
                 [to-be-embedded-module-specs
                  (map (λ (x) (list #f x))
                       pre-to-be-embedded-module-specs4)])

            (make-embedding-executable 
             executable-filename
             gui?
             #f ;; verbose?
             to-be-embedded-module-specs
             (list 
              bootstrap-tmp-filename
              program-filename)
             #f
             (if gui?
                 (list "-mvqZ")
                 (list "-mvq"))))
          (delete-file init-code-tmp-filename)
          (delete-file bootstrap-tmp-filename)
          (void)))

      (define (condense-scheme-code-string s)
        (let ([i (open-input-string s)]
              [o (open-output-string)])
          (let loop ()
            (let ([c (read-char i)])
              (unless (eof-object? c)
                (let ([next (λ ()
                              (display c o)
                              (loop))])
                  (case c
                    [(#\space)
                     (if (char=? #\( (peek-char i))
                         (loop)
                         (next))]
                    [(#\))
                     (if (eq? #\space (peek-char i))
                         (begin
                           (display #\) o)
                           (read-char i)
                           (loop))
                         (next))]
                    [(#\\)
                     (begin
                       (display #\\ o)
                       (display (read-char i) o)
                       (loop))]
                    [(#\" #\|)
                     (display c o)
                     (let loop ()
                       (let ([v (read-char i)])
                         (cond
                           [(eq? v c) (next)]
                           [(eq? v #\\)
                            (display v o)
                            (display (read-char i) o)
                            (loop)]
                           [else (display v o)
                                 (loop)])))]
                    [else (next)])))))
          (get-output-string o)))
      
      (define (create-module-based-launcher program-filename 
                                            executable-filename
                                            module-language-spec
                                            transformer-module-language-spec
                                            init-code
                                            gui?
                                            use-copy?)

        (with-handlers ([(λ (x) #f) ;exn:fail?
                         (λ (x)
                           (message-box 
                            (string-constant drscheme)
                            (format "~a" (exn-message x)))
                           (void))])
          
          ((if gui? make-mred-launcher make-mzscheme-launcher)
           (list
            "-qmvt"
            (path->string
             (build-path (collection-path "drscheme" "private") 
                         "launcher-bootstrap.ss"))
            "--"
            (condense-scheme-code-string (format "~s" init-code))
            (path->string program-filename)
            (format "~s" module-language-spec)
            (format "~s" transformer-module-language-spec)
            (format "~s" use-copy?)
            (format "~s" (if gui?  
                             (list 'mzscheme '(lib "mred.ss" "mred"))
                             (list 'mzscheme))))
           (path->string executable-filename))))
      
      ;; initialize-module-based-language : boolean module-spec module-spec ((-> void) -> void)
      (define (initialize-module-based-language use-copy?
                                                module-spec
                                                transformer-module-spec
                                                run-in-user-thread)
        (run-in-user-thread
         (λ ()
           (with-handlers ([(λ (x) #t)
                            (λ (x)
                              (display (exn-message x))
                              (newline))])
	     (if use-copy?
		 (namespace-require/copy module-spec)
		 (namespace-require module-spec))
	     (namespace-transformer-require transformer-module-spec)))))

      ;; module-based-language-front-end : (port reader -> (-> (union sexp syntax eof)))
      ;; type reader = type-spec-of-read-syntax (see mz manual for details)
      (define (module-based-language-front-end port reader)
        (λ () 
          (reader (object-name port) port)))
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  snip/value extensions
      ;;
      
      (define to-snips null)
      (define-struct to-snip (predicate? >value))
      (define (add-snip-value predicate constructor)
        (set! to-snips (cons (make-to-snip predicate constructor) to-snips)))
      
      (define (value->snip v)
        (ormap (λ (to-snip) (and ((to-snip-predicate? to-snip) v)
                                      ((to-snip->value to-snip) v)))
               to-snips))
      (define (to-snip-value? v)
        (ormap (λ (to-snip) ((to-snip-predicate? to-snip) v)) to-snips))
      
      

                                                                      
                                             ;                        
                ;                                                     
                ;                                                     
  ;;;  ;;; ;;; ;;;;;   ;;;  ; ;;;    ;;;   ;;;     ;;;  ; ;;;    ;;;  
 ;   ;   ; ;    ;     ;   ;  ;;  ;  ;   ;    ;    ;   ;  ;;  ;  ;   ; 
 ;;;;;    ;     ;     ;;;;;  ;   ;   ;;;     ;    ;   ;  ;   ;   ;;;  
 ;       ; ;    ;     ;      ;   ;      ;    ;    ;   ;  ;   ;      ; 
 ;   ;  ;   ;   ;   ; ;   ;  ;   ;  ;   ;    ;    ;   ;  ;   ;  ;   ; 
  ;;;  ;;   ;;   ;;;   ;;;  ;;;  ;;  ;;;   ;;;;;   ;;;  ;;;  ;;  ;;;  
                                                                      
                                                                      

      (define language-extensions null)
      (define (get-language-extensions) 
        (drscheme:tools:only-in-phase
         'drscheme:language:get-default-mixin
         'phase2)
        language-extensions)

      (define (default-mixin x) x)
      (define (get-default-mixin)
        (drscheme:tools:only-in-phase
         'drscheme:language:get-default-mixin
         'phase2)
        default-mixin)
      
      (define (extend-language-interface extension<%> default-impl)
        (drscheme:tools:only-in-phase
         'drscheme:language:extend-language-interface
         'phase1)
        (set! default-mixin (compose default-impl default-mixin))
        (set! language-extensions (cons extension<%> language-extensions))))))

