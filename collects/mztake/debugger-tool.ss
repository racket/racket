;                                       ;                                                      
;  ;;       ;;        ;;;;;;;;;         ;                    ;                                 
;  ;;       ;;            ;             ;                    ;                                 
;  ; ;     ; ;            ;             ;                    ;                                 
;  ; ;     ; ; ;;;;;;     ;       ;;;;  ;    ;    ;;;        ;         ;;;;  ; ;;;;     ;;;;;; 
;  ;  ;   ;  ;      ;     ;      ;    ; ;   ;    ;   ;       ;        ;    ; ;;    ;   ;     ; 
;  ;  ;   ;  ;     ;      ;           ; ;  ;    ;     ;      ;             ; ;      ; ;      ; 
;  ;   ; ;   ;    ;       ;       ;;;;; ; ;     ;     ;      ;         ;;;;; ;      ; ;      ; 
;  ;   ; ;   ;   ;        ;      ;    ; ;;;     ;;;;;;;      ;        ;    ; ;      ; ;      ; 
;  ;    ;    ;   ;        ;     ;     ; ;  ;    ;            ;       ;     ; ;      ; ;      ; 
;  ;         ;  ;         ;     ;     ; ;   ;   ;            ;       ;     ; ;      ; ;      ; 
;  ;         ; ;          ;     ;    ;; ;    ;   ;    ;      ;       ;    ;; ;      ;  ;    ;; 
;  ;         ; ;;;;;;     ;      ;;;; ; ;     ;   ;;;;       ;;;;;;;  ;;;; ; ;      ;   ;;;; ; 
;                                                                                            ; 
;                                                                                      ;    ;  
;                                                                                       ;;;;   

(module debugger-tool mzscheme
  (require "mztake.ss"
           (lib "etc.ss")
           (lib "list.ss")
           (lib "class.ss")
           (lib "unitsig.ss")
           (lib "contract.ss")
           (lib "mred.ss" "mred")
           (lib "tool.ss" "drscheme")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants"))
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      ;############################MZTAKE LANGUAGE RELATED FUNCTIONS##############################################
      (define (phase1) (void))
      (define (phase2)
        (drscheme:language-configuration:add-language 
	 (make-object ((drscheme:language:get-default-mixin) (make-mztake-language mztake-language%)))))
      
      (define (make-mztake-language base)
	(class (drscheme:language:module-based-language->language-mixin
		(drscheme:language:simple-module-based-language->module-based-language-mixin
		 base))
          (field (watch-list empty))
	  (rename [super-on-execute on-execute])
          (inherit get-language-position)
          (define/override (on-execute settings run-in-user-thread)
            (let ([drs-eventspace (current-eventspace)])
              (super-on-execute settings run-in-user-thread)
              (run-in-user-thread
               (lambda ()
                 (let ([new-watch (namespace-variable-value 'render)]
                       [set-evspc (namespace-variable-value 'set-eventspace)])
                   (set-evspc drs-eventspace)
                   (set! watch-list
                         ((if (weak-member new-watch watch-list)
                              identity
                              (lambda (r) (cons (make-weak-box new-watch) r)))
                          (filter weak-box-value watch-list))))))))
          
          (rename (super:render-value/format render-value/format)
                  (super:render-value        render-value))
          (override render-value/format render-value)
          (define (render-value/format value settings port put-snip width)
            (super:render-value/format (watch watch-list value put-snip)
                                       settings port put-snip width))
          (define (render-value value settings port put-snip)
            (super:render-value (watch watch-list value put-snip)
                                settings port put-snip))
	  (define/override (use-namespace-require/copy?) #t)
	  (super-instantiate ())))
      
      
      (define mztake-language%
	(class* object% (drscheme:language:simple-module-based-language<%>)
	  (define/public (get-language-numbers)
	    '(1000 -400))
	  (define/public (get-language-position)
	    (list (string-constant experimental-languages) "MzTake"))
	  (define/public (get-module)
	    '(lib "mztake-syntax.ss" "mztake"))
	  (define/public (get-one-line-summary)
	    (format "MzTake Debugger (~a)" mztake-version))
          (define/public (get-language-url) #f)
	  (define/public (get-reader)
	    (lambda (name port offsets)
	      (let ([v (read-syntax name port offsets)])
		(if (eof-object? v)
		    v
		    (namespace-syntax-introduce v)))))
	  (super-instantiate ())))
      
      ;;;;;;;;;;;;;FRTIME REPL STUFF;;;;;;;;;;;;;;;;;;;;;;
      (define (weak-member obj lis)
        (let ([cmp (lambda (v) (eq? v obj))])
          (let loop ([lis lis])
            (and (cons? lis)
                 (or
                  (cond
                    [(weak-box-value (first lis)) => cmp]
                    [else false])
                  (loop (rest lis)))))))
      
      (define (watch watch-list value as-snip?)
        (foldl
         (lambda (wb acc)
           (cond
             [(weak-box-value wb)
              => (lambda (f) (f acc as-snip?))]
             [else acc]))
         value
         watch-list))
      ;###########################################################################################################
      
      
      (define debugger-bitmap
        (drscheme:unit:make-bitmap
         "Syntax Location"
         (build-path (collection-path "mztake") "stock_macro-check-brackets-16.png")))
      
      (define (debugger-unit-frame-mixin super%)
        (class super%
          
          (inherit get-button-panel get-interactions-text get-definitions-text get-menu-bar)
          
          (super-instantiate ())
          
          (define debugger-button 
            (make-object button%
              (debugger-bitmap this)
              (get-button-panel)
              (lambda (button evt)
                (let* ([pos (send (get-definitions-text) get-start-position)]
                       [line (send (get-definitions-text) position-paragraph pos)]
                       [column (- pos (send (get-definitions-text) line-start-position
                                            (send (get-definitions-text) position-line pos)))])
                  
                  (message-box "Syntax Location"
                               (format "Line: ~a~nColumn: ~a" (add1 line) column))))))
          (send (get-button-panel) change-children
                (lambda (_) (cons debugger-button (remq debugger-button _))))))
      
      (drscheme:get/extend:extend-unit-frame debugger-unit-frame-mixin))))