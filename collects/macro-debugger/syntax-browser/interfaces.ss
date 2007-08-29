
(module interfaces mzscheme
  (require (lib "class.ss"))
  (provide (all-defined))

  ;; displays-manager<%>
  (define displays-manager<%>
    (interface ()
      ;; add-syntax-display : display<%> -> void
      add-syntax-display

      ;; remove-all-syntax-displays : -> void
      remove-all-syntax-displays))

  ;; selection-manager<%>
  (define selection-manager<%>
    (interface ()
      ;; set-selected-syntax : syntax -> void
      set-selected-syntax

      ;; get-selected-syntax : -> syntax
      get-selected-syntax

      ;; listen-selected-syntax : (syntax -> void) -> void
      listen-selected-syntax))

  ;; mark-manager<%>
  ;; Manages marks, mappings from marks to colors
  (define mark-manager<%>
    (interface ()
      ;; get-primary-partition : -> partition
      get-primary-partition))

  ;; secondary-partition<%>
  (define secondary-partition<%>
    (interface (displays-manager<%>)
      ;; get-secondary-partition : -> partition<%>
      get-secondary-partition

      ;; set-secondary-partition : partition<%> -> void
      set-secondary-partition

      ;; listen-secondary-partition : (partition<%> -> void) -> void
      listen-secondary-partition

      ;; get-identifier=? : -> (cons string procedure)
      get-identifier=?

      ;; set-identifier=? : (cons string procedure) -> void
      set-identifier=?

      ;; listen-identifier=? : ((cons string procedure) -> void) -> void
      listen-identifier=?))

  ;; controller<%>
  (define controller<%>
    (interface (displays-manager<%>
                selection-manager<%>
                mark-manager<%> 
                secondary-partition<%>)))

  ;; host<%>
  (define host<%>
    (interface ()
      ;; get-controller : -> controller<%>
      get-controller

      ;; add-keymap : text snip
      add-keymap
      ))


  ;; display<%>
  (define display<%>
    (interface ()
      ;; refresh : -> void
      refresh

      ;; highlight-syntaxes : (list-of syntax) color -> void
      highlight-syntaxes

      ;; get-start-position : -> number
      get-start-position

      ;; get-end-position : -> number
      get-end-position

      ;; get-range : -> range<%>
      get-range))

  ;; range<%>
  (define range<%>
    (interface ()
      ;; get-ranges : datum -> (list-of (cons number number))
      get-ranges

      ;; all-ranges : (list-of Range)
      ;; Sorted outermost-first
      all-ranges

      ;; get-identifier-list : (list-of identifier)
      get-identifier-list))

  ;; A Range is (make-range datum number number)
  (define-struct range (obj start end))


  ;; syntax-prefs<%>
  (define syntax-prefs<%>
    (interface ()
      pref:width
      pref:height
      pref:props-percentage
      pref:props-shown?))

  ;; widget-hooks<%>
  (define widget-hooks<%>
    (interface ()
      ;; setup-keymap : -> void
      setup-keymap

      ;; shutdown : -> void
      shutdown
      ))

  ;; keymap-hooks<%>
  (define keymap-hooks<%>
    (interface ()
      ;; make-context-menu : -> context-menu<%>
      make-context-menu

      ;; get-context-menu% : -> class
      get-context-menu%))

  ;; context-menu-hooks<%>
  (define context-menu-hooks<%>
    (interface ()
      add-edit-items
      after-edit-items
      add-selection-items
      after-selection-items
      add-partition-items
      after-partition-items))


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

  )
