(module mxmain mzscheme

  ; dummy entries to make Setup happy
  ; these are the names defined in mxPrims[] in src/mysterx.cxx

  (provide
   mx-version
   block-while-browsers
   com-invoke
   com-set-property!
   com-get-property
   com-get-properties
   com-set-properties
   com-methods
   com-events
   com-method-type
   com-get-property-type
   com-set-property-type
   com-event-type
   com-help
   com-object-type
   com-is-a?
   com-currency?
   com-date?
   com-date->date
   date->com-date
   com-scode?
   com-scode->number
   number->com-scode
   com-currency->number
   number->com-currency
   com-object?
   com-iunknown?
   com-register-event-handler
   com-unregister-event-handler
   com-all-coclasses
   com-all-controls
   coclass->html
   progid->html
   cocreate-instance-from-coclass
   cocreate-instance-from-progid
   com-get-active-object-from-coclass
   coclass
   progid
   set-coclass!
   set-coclass-from-progid!
   com-object-eq?
   com-register-object
   com-release-object
   com-add-ref
   com-ref-count
   com-terminate
   make-browser
   browser-show
   navigate
   go-back
   go-forward
   refresh
   iconize
   restore
   current-url
   register-navigate-handler
   current-document
   print-document
   document?
   document-title
   document-insert-html
   document-append-html
   document-replace-html
   document-find-element
   document-find-element-by-id-or-name
   document-elements-with-tag
   document-objects
   element-insert-html
   element-append-html
   element-insert-text
   element-append-text
   element-replace-html
   element-get-html
   element-get-text
   element-focus
   element-selection
   element-set-selection!
   element-attribute
   element-set-attribute!
   element-click
   element-tag
   element-font-family
   element-set-font-family!
   element-font-style
   element-set-font-style!
   element-font-variant
   element-set-font-variant!
   element-font-weight
   element-set-font-weight!
   element-font
   element-set-font!
   element-background
   element-set-background!
   element-background-attachment
   element-set-background-attachment!
   element-background-image
   element-set-background-image!
   element-background-repeat
   element-set-background-repeat!
   element-background-position
   element-set-background-position!
   element-text-decoration
   element-set-text-decoration!
   element-text-transform
   element-set-text-transform!
   element-text-align
   element-set-text-align!
   element-margin
   element-set-margin!
   element-padding
   element-set-padding!
   element-border
   element-set-border!
   element-border-top
   element-set-border-top!
   element-border-bottom
   element-set-border-bottom!
   element-border-left
   element-set-border-left!
   element-border-right
   element-set-border-right!
   element-border-color
   element-set-border-color!
   element-border-width
   element-set-border-width!
   element-border-style
   element-set-border-style!
   element-border-top-style
   element-set-border-top-style!
   element-border-bottom-style
   element-set-border-bottom-style!
   element-border-left-style
   element-set-border-left-style!
   element-border-right-style
   element-set-border-right-style!
   element-style-float
   element-set-style-float!
   element-clear
   element-set-clear!
   element-display
   element-set-display!
   element-visibility
   element-set-visibility!
   element-list-style-type
   element-set-list-style-type!
   element-list-style-position
   element-set-list-style-position!
   element-list-style-image
   element-set-list-style-image!
   element-list-style
   element-set-list-style!
   element-position
   element-overflow
   element-set-overflow!
   element-pagebreak-before
   element-set-pagebreak-before!
   element-pagebreak-after
   element-set-pagebreak-after!
   element-css-text
   element-set-css-text!
   element-cursor
   element-set-cursor!
   element-clip
   element-set-clip!
   element-filter
   element-set-filter!
   element-style-string
   element-text-decoration-none
   element-set-text-decoration-none!
   element-text-decoration-underline
   element-set-text-decoration-underline!
   element-text-decoration-overline
   element-set-text-decoration-overline!
   element-text-decoration-linethrough
   element-set-text-decoration-linethrough!
   element-text-decoration-blink
   element-set-text-decoration-blink!
   element-pixel-top
   element-set-pixel-top!
   element-pixel-left
   element-set-pixel-left!
   element-pixel-width
   element-set-pixel-width!
   element-pixel-height
   element-set-pixel-height!
   element-pos-top
   element-set-pos-top!
   element-pos-left
   element-set-pos-left!
   element-pos-width
   element-set-pos-width!
   element-pos-height
   element-set-pos-height!
   element-font-size
   element-set-font-size!
   element-color
   element-set-color!
   element-background-color
   element-set-background-color!
   element-background-position-x
   element-set-background-position-x!
   element-background-position-y
   element-set-background-position-y!
   element-letter-spacing
   element-set-letter-spacing!
   element-vertical-align
   element-set-vertical-align!
   element-text-indent
   element-set-text-indent!
   element-line-height
   element-set-line-height!
   element-margin-top
   element-set-margin-top!
   element-margin-bottom
   element-set-margin-bottom!
   element-margin-left
   element-set-margin-left!
   element-margin-right
   element-set-margin-right!
   element-padding-top
   element-set-padding-top!
   element-padding-bottom
   element-set-padding-bottom!
   element-padding-left
   element-set-padding-left!
   element-padding-right
   element-set-padding-right!
   element-border-top-color
   element-set-border-top-color!
   element-border-bottom-color
   element-set-border-bottom-color!
   element-border-left-color
   element-set-border-left-color!
   element-border-right-color
   element-set-border-right-color!
   element-border-top-width
   element-set-border-top-width!
   element-border-bottom-width
   element-set-border-bottom-width!
   element-border-left-width
   element-set-border-left-width!
   element-border-right-width
   element-set-border-right-width!
   element-width
   element-set-width!
   element-height
   element-set-height!
   element-top
   element-set-top!
   element-left
   element-set-left!
   element-z-index
   element-set-z-index!
   event?
   get-event
   event-tag
   event-id
   event-from-tag
   event-from-id
   event-to-tag
   event-to-id
   event-keycode
   event-shiftkey
   event-ctrlkey
   event-altkey
   event-x
   event-y
   event-keypress?
   event-keydown?
   event-keyup?
   event-mousedown?
   event-mousemove?
   event-mouseover?
   event-mouseout?
   event-mouseup?
   event-click?
   event-dblclick?
   event-error?
   block-until-event
   process-win-events
   release-type-table
   com-omit
   %%initialize-dotnet-runtime)

  (error "mxmain.ss: you seem to be missing mxmain.dll; you need to build MysterX in plt\\src\\mysterx\\")

  (define mx-version #f)
  (define block-while-browsers #f)
  (define com-invoke #f)
  (define com-set-property! #f)
  (define com-get-property #f)
  (define com-get-properties #f)
  (define com-set-properties #f)
  (define com-methods #f)
  (define com-events #f)
  (define com-method-type #f)
  (define com-get-property-type #f)
  (define com-set-property-type #f)
  (define com-event-type #f)
  (define com-help #f)
  (define com-object-type #f)
  (define com-is-a? #f)
  (define com-currency? #f)
  (define com-date? #f)
  (define com-date->date #f)
  (define date->com-date #f)
  (define com-scode? #f)
  (define com-scode->number #f)
  (define number->com-scode #f)
  (define com-currency->number #f)
  (define number->com-currency #f)
  (define com-object? #f)
  (define com-iunknown? #f)
  (define com-register-event-handler #f)
  (define com-unregister-event-handler #f)
  (define com-all-coclasses #f)
  (define com-all-controls #f)
  (define coclass->html #f)
  (define progid->html #f)
  (define cocreate-instance-from-coclass #f)
  (define cocreate-instance-from-progid #f)
  (define com-get-active-object-from-coclass #f)
  (define coclass #f)
  (define progid #f)
  (define set-coclass! #f)
  (define set-coclass-from-progid! #f)
  (define com-object-eq? #f)
  (define com-register-object #f)
  (define com-release-object #f)
  (define com-add-ref #f)
  (define com-ref-count #f)
  (define com-terminate #f)
  (define make-browser #f)
  (define browser-show #f)
  (define navigate #f)
  (define go-back #f)
  (define go-forward #f)
  (define refresh #f)
  (define iconize #f)
  (define restore #f)
  (define current-url #f)
  (define register-navigate-handler #f)
  (define current-document #f)
  (define print-document #f)
  (define document? #f)
  (define document-title #f)
  (define document-insert-html #f)
  (define document-append-html #f)
  (define document-replace-html #f)
  (define document-find-element #f)
  (define document-find-element-by-id-or-name #f)
  (define document-elements-with-tag #f)
  (define document-objects #f)
  (define element-insert-html #f)
  (define element-append-html #f)
  (define element-insert-text #f)
  (define element-append-text #f)
  (define element-replace-html #f)
  (define element-get-html #f)
  (define element-get-text #f)
  (define element-focus #f)
  (define element-selection #f)
  (define element-set-selection! #f)
  (define element-attribute #f)
  (define element-set-attribute! #f)
  (define element-click #f)
  (define element-tag #f)
  (define element-font-family #f)
  (define element-set-font-family! #f)
  (define element-font-style #f)
  (define element-set-font-style! #f)
  (define element-font-variant #f)
  (define element-set-font-variant! #f)
  (define element-font-weight #f)
  (define element-set-font-weight! #f)
  (define element-font #f)
  (define element-set-font! #f)
  (define element-background #f)
  (define element-set-background! #f)
  (define element-background-attachment #f)
  (define element-set-background-attachment! #f)
  (define element-background-image #f)
  (define element-set-background-image! #f)
  (define element-background-repeat #f)
  (define element-set-background-repeat! #f)
  (define element-background-position #f)
  (define element-set-background-position! #f)
  (define element-text-decoration #f)
  (define element-set-text-decoration! #f)
  (define element-text-transform #f)
  (define element-set-text-transform! #f)
  (define element-text-align #f)
  (define element-set-text-align! #f)
  (define element-margin #f)
  (define element-set-margin! #f)
  (define element-padding #f)
  (define element-set-padding! #f)
  (define element-border #f)
  (define element-set-border! #f)
  (define element-border-top #f)
  (define element-set-border-top! #f)
  (define element-border-bottom #f)
  (define element-set-border-bottom! #f)
  (define element-border-left #f)
  (define element-set-border-left! #f)
  (define element-border-right #f)
  (define element-set-border-right! #f)
  (define element-border-color #f)
  (define element-set-border-color! #f)
  (define element-border-width #f)
  (define element-set-border-width! #f)
  (define element-border-style #f)
  (define element-set-border-style! #f)
  (define element-border-top-style #f)
  (define element-set-border-top-style! #f)
  (define element-border-bottom-style #f)
  (define element-set-border-bottom-style! #f)
  (define element-border-left-style #f)
  (define element-set-border-left-style! #f)
  (define element-border-right-style #f)
  (define element-set-border-right-style! #f)
  (define element-style-float #f)
  (define element-set-style-float! #f)
  (define element-clear #f)
  (define element-set-clear! #f)
  (define element-display #f)
  (define element-set-display! #f)
  (define element-visibility #f)
  (define element-set-visibility! #f)
  (define element-list-style-type #f)
  (define element-set-list-style-type! #f)
  (define element-list-style-position #f)
  (define element-set-list-style-position! #f)
  (define element-list-style-image #f)
  (define element-set-list-style-image! #f)
  (define element-list-style #f)
  (define element-set-list-style! #f)
  (define element-position #f)
  (define element-overflow #f)
  (define element-set-overflow! #f)
  (define element-pagebreak-before #f)
  (define element-set-pagebreak-before! #f)
  (define element-pagebreak-after #f)
  (define element-set-pagebreak-after! #f)
  (define element-css-text #f)
  (define element-set-css-text! #f)
  (define element-cursor #f)
  (define element-set-cursor! #f)
  (define element-clip #f)
  (define element-set-clip! #f)
  (define element-filter #f)
  (define element-set-filter! #f)
  (define element-style-string #f)
  (define element-text-decoration-none #f)
  (define element-set-text-decoration-none! #f)
  (define element-text-decoration-underline #f)
  (define element-set-text-decoration-underline! #f)
  (define element-text-decoration-overline #f)
  (define element-set-text-decoration-overline! #f)
  (define element-text-decoration-linethrough #f)
  (define element-set-text-decoration-linethrough! #f)
  (define element-text-decoration-blink #f)
  (define element-set-text-decoration-blink! #f)
  (define element-pixel-top #f)
  (define element-set-pixel-top! #f)
  (define element-pixel-left #f)
  (define element-set-pixel-left! #f)
  (define element-pixel-width #f)
  (define element-set-pixel-width! #f)
  (define element-pixel-height #f)
  (define element-set-pixel-height! #f)
  (define element-pos-top #f)
  (define element-set-pos-top! #f)
  (define element-pos-left #f)
  (define element-set-pos-left! #f)
  (define element-pos-width #f)
  (define element-set-pos-width! #f)
  (define element-pos-height #f)
  (define element-set-pos-height! #f)
  (define element-font-size #f)
  (define element-set-font-size! #f)
  (define element-color #f)
  (define element-set-color! #f)
  (define element-background-color #f)
  (define element-set-background-color! #f)
  (define element-background-position-x #f)
  (define element-set-background-position-x! #f)
  (define element-background-position-y #f)
  (define element-set-background-position-y! #f)
  (define element-letter-spacing #f)
  (define element-set-letter-spacing! #f)
  (define element-vertical-align #f)
  (define element-set-vertical-align! #f)
  (define element-text-indent #f)
  (define element-set-text-indent! #f)
  (define element-line-height #f)
  (define element-set-line-height! #f)
  (define element-margin-top #f)
  (define element-set-margin-top! #f)
  (define element-margin-bottom #f)
  (define element-set-margin-bottom! #f)
  (define element-margin-left #f)
  (define element-set-margin-left! #f)
  (define element-margin-right #f)
  (define element-set-margin-right! #f)
  (define element-padding-top #f)
  (define element-set-padding-top! #f)
  (define element-padding-bottom #f)
  (define element-set-padding-bottom! #f)
  (define element-padding-left #f)
  (define element-set-padding-left! #f)
  (define element-padding-right #f)
  (define element-set-padding-right! #f)
  (define element-border-top-color #f)
  (define element-set-border-top-color! #f)
  (define element-border-bottom-color #f)
  (define element-set-border-bottom-color! #f)
  (define element-border-left-color #f)
  (define element-set-border-left-color! #f)
  (define element-border-right-color #f)
  (define element-set-border-right-color! #f)
  (define element-border-top-width #f)
  (define element-set-border-top-width! #f)
  (define element-border-bottom-width #f)
  (define element-set-border-bottom-width! #f)
  (define element-border-left-width #f)
  (define element-set-border-left-width! #f)
  (define element-border-right-width #f)
  (define element-set-border-right-width! #f)
  (define element-width #f)
  (define element-set-width! #f)
  (define element-height #f)
  (define element-set-height! #f)
  (define element-top #f)
  (define element-set-top! #f)
  (define element-left #f)
  (define element-set-left! #f)
  (define element-z-index #f)
  (define element-set-z-index! #f)
  (define event? #f)
  (define get-event #f)
  (define event-tag #f)
  (define event-id #f)
  (define event-from-tag #f)
  (define event-from-id #f)
  (define event-to-tag #f)
  (define event-to-id #f)
  (define event-keycode #f)
  (define event-shiftkey #f)
  (define event-ctrlkey #f)
  (define event-altkey #f)
  (define event-x #f)
  (define event-y #f)
  (define event-keypress? #f)
  (define event-keydown? #f)
  (define event-keyup? #f)
  (define event-mousedown? #f)
  (define event-mousemove? #f)
  (define event-mouseover? #f)
  (define event-mouseout? #f)
  (define event-mouseup? #f)
  (define event-click? #f)
  (define event-dblclick? #f)
  (define event-error? #f)
  (define block-until-event #f)
  (define process-win-events #f)
  (define release-type-table #f)
  (define com-omit #f)
  (define %%initialize-dotnet-runtime #f))
