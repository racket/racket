
(module interfaces mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss"))
  (provide (all-defined))

  ;; Signatures
  
  (define-signature browser^
    (;; browse-syntax : syntax -> void
     browse-syntax
     
     ;; browse-syntaxes : (list-of syntax) -> void
     browse-syntaxes
     
     ;; make-syntax-browser : -> syntax-browser<%>
     make-syntax-browser
     
     ;; syntax-widget/controls%
     syntax-widget/controls%
     
     ;; syntax-browser-frame%
     syntax-browser-frame%))

  (define-signature prefs^
    (;; pref:width : pref of number
     pref:width
     
     ;; pref:height : pref of number
     pref:height
     
     ;; pref:props-percentage : pref of number in (0,1)
     pref:props-percentage))

  (define-signature keymap^
    (;; syntax-keymap% implements syntax-keymap<%>
     syntax-keymap%))
  
  (define-signature context-menu^
    (;; context-menu%
     context-menu%))

  (define-signature snip^
    (;; syntax-snip : syntax -> snip
     syntax-snip
     
     ;; syntax-snip%
     syntax-snip%))

  (define-signature snipclass^
    (;; snip-class
     snip-class))
  
  (define-signature widget^
    (;; syntax-widget%
     syntax-widget%))

  (define-signature implementation^
    ([unit widget : widget^]
     [unit snip : snip^]))
  
  ;; Class Interfaces

  ;; syntax-controller<%>
  ;; A syntax-controller coordinates state shared by many different syntax views.
  ;; Syntax views can share:
  ;;   - selection
  ;;   - partitioning configuration
  ;;   - property display
  (define syntax-controller<%>
    (interface ()
      ;; select-syntax : syntax -> void
      select-syntax
      
      ;; get-selected-syntax : -> syntax/#f
      get-selected-syntax
      
      ;; get-properties-controller : -> syntax-properties-controller<%>
      get-properties-controller
      
      ;; add-view-colorer : syntax-colorer<%> -> void
      add-view-colorer
      
      ;; get-view-colorers : -> (list-of syntax-colorer<%>)
      get-view-colorers
      
      ;; add-selection-listener : syntax -> void
      add-selection-listener
      ))

  ;; syntax-properties-controller<%>
  (define syntax-properties-controller<%>
    (interface ()
      ;; set-syntax : syntax -> void
      set-syntax
      
      ;; show : boolean -> void
      #;show
      
      ;; props-shown? : -> boolean
      props-shown?))

  ;; syntax-configuration<%>
  (define syntax-configuration<%>
    (interface ()
      ;; get-primary-partition : -> partition<%>
      get-primary-partition
      
      ;; get-secondary-partition : -> partition<%>
      get-secondary-partition
      
      ;; update-identifier=? : ... -> void
      update-identifier=?))

  
  ;; syntax-colorer<%>
  (define syntax-colorer<%>
    (interface ()
      select-syntax
      apply-styles))

  ;;----------
  
  ;; Convenience widget, specialized for displaying stx and not much else
  (define syntax-browser<%>
    (interface ()
      add-syntax
      add-text
      add-separator
      erase-all
      select-syntax
      get-text
      ))
  
  (define partition<%>
    (interface ()
      ;; get-partition : any -> number
      get-partition
      
      ;; same-partition? : any any -> number
      same-partition?
      
      ;; count : -> number
      count))
      
  ;; Internal interfaces
  
  (define syntax-pp-snip-controller<%>
    (interface ()
      on-select-syntax
      ))

  (define color-controller<%>
    (interface ()
      get-primary-partition
      get-secondary-partition
      ))

  (define syntax-pp<%>
    (interface ()
      pretty-print-syntax

      get-range
      get-identifier-list
      flat=>stx
      stx=>flat))

  (define typesetter<%>
    (interface ()
      get-output-port
      get-current-position))

  (define range<%>
    (interface ()
      get-start
      set-start
      get-ranges
      add-range
      all-ranges))

  )
