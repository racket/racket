(module interactions-box mzscheme
  
  (require
   (lib "mred.ss" "mred")
   (lib "class.ss")
   (lib "etc.ss")
   (lib "list.ss")
   (lib "embedded-gui.ss" "embedded-gui")
   (lib "match.ss")
   (lib "unitsig.ss")
   (lib "tool.ss" "drscheme")
   (lib "framework.ss" "framework")
   (lib "readerr.ss" "syntax")
   (lib "parser.ss" "profj")
   (lib "string-constant.ss" "string-constants")
   (lib "text-syntax-object.ss" "test-suite" "private")
   (lib "print-to-text.ss" "test-suite" "private")
   (lib "make-snipclass.ss" "test-suite" "private")
   "table.ss")
  
  (provide interactions-box@
           interactions-box^)
  
  (define-signature interactions-box^ (interactions-box%))
  
  (define interactions-box@
    (unit/sig interactions-box^
      (import drscheme:tool^ text->syntax-object^)
      
      (define interactions-box%
        (class* editor-snip% (readable-snip<%>)
          (inherit set-snipclass)
          (init [interactions-to-copy #f])
          
          #;(any? (union integer? false?) (union integer? false?) (union integer? false?) . -> . any?)
          ;; Called to get the syntax object representing this box
          (define/public read-special
            (opt-lambda (source (line false) (column false) (position false))
              #;((is-a?/c text%) . -> . syntax-object?)
              (define (text->syntax-object text)
                (match (text->syntax-objects text)
                  [() (raise-read-error
                       (string-constant profjBoxes-empty-error)
                       source line #f position 1)]
                  [(stx) stx]
                  [(stx next rest-stx ...)
                   (raise-read-error (string-constant profjBoxes-too-many-expressions-error)
                                     text
                                     (syntax-line next)
                                     (syntax-column next)
                                     (syntax-position next)
                                     (syntax-span next))]))
              ;(lambda (level class-loc box-pos input-spec)
              (let ([level 'beginner] [class-loc #f] [box-pos #f] [input-spec #f])
                #`(begin
                    #,@(send interactions map-children
                             (lambda (interaction)
                               (if (is-a? interaction interaction%)
                                   (with-syntax ([display-output
                                                  (lambda (value)
                                                    (send interaction display-output value))])
                                     #`(display-output
                                        #,(text->syntax-object (send interaction get-input))
                                        ;#,(parse-interactions
                                        ;   (open-input-text-editor (send interaction get-input))
                                        ;   (send interaction get-input)
                                        ;   level)
                                        ))
                                   #'(void))))))))
          
          #;(-> void)
          ;; tells the test-box to take the caret
          (define/public (take-caret)
            (let ([first-box (send (send interactions get-first-child) get-input)])
              (send pb set-caret-owner
                    (send (send first-box get-admin) get-snip)
                    'display)))
          
          ;;;;;;;;;;
          ;; Reading/Writing
          
          #;(-> (is-a?/c interactions-box%))
          ;; Make an interaction box that is a copy of this interaction box
          (define/override (copy)
            (new interactions-box% (interactions-to-copy interactions)))
          
          #;((is-a?/c editor-stream-out%) . -> . void?)
          ;; Writes this interactions box to file
          (define/override (write f)
            (send interactions write f))
            
          #;((is-a?/c editor-stream-in%) . -> . void?)
          ;; Reads interactions from file
          (define/public (read-from-file f)
            (send interactions read-from-file f))
          
          ;;;;;;;;;;
          ;; Layout
          
          (field
           [pb (new aligned-pasteboard%)]
           [main (new vertical-alignment% (parent pb))]
           [header (new horizontal-alignment% (parent main))])
          
          (new horizontal-alignment% (parent header)) ; left spacer
          (new snip-wrapper%
               (snip (make-object string-snip%
                       (string-constant profjBoxes-interactions-label)))
               (parent header))
          (new horizontal-alignment% (parent header)) ; right spacer
          
          (field [interactions (new (table interaction%)
                                    (parent main)
                                    (copy-constructor interactions-to-copy))])
          
          (super-new (editor pb))
          (unless interactions-to-copy
            (send interactions add-new))
          (set-snipclass sc)))
      
      (define sc (make-snipclass interactions-box% "interactions-box%"))
  
      ;; One interaction laid out horizontally
      (define interaction%
        (class* horizontal-alignment% (table-item<%>)
          (inherit get-parent next)
          (init [copy-constructor #f])
          
          #;(-> (is-a?/c text%))
          ;; The first text in the item that can be typed into
          (define/public (get-first-text) (get-input))
          
          #;(-> (is-a?/c text%))
          ;; The input of this interaction
          (define/public (get-input) input-text)
          
          #;(-> void?)
          ;; Resets the interaction to an inital state
          (define/public (reset)
            (send* output-text
              (lock false)
              (erase)
              (lock true))
            (send output show false))
          
          #;(string? . -> . void?)
          ;; Sets the output to the given value
          (define/public (display-output val)
            (let ([blue-text (new style-delta%)])
              (send blue-text set-delta-foreground "blue")
              (send* output-text 
                (lock false)
                (change-style blue-text 'start 'end #f))
              ;(print-to-text output-text (list val))
              (send* output-text
                (lock true))
              (send output show true)))
          
          #;(-> void?)
          ;; Insert a new interaction after this interaction
          (define/public (make-new)
            (send (get-parent) add-new this)
            (send (send (next) get-input) set-caret-owner false 'global))
          
          #;((is-a?/c editor-stream-out%) . -> . void?)
          ;; Writes the interaction to file
          (define/public (write f)
            (send input-text write-to-file f))
          
          #;((is-a?/c editor-stream-in%) . -> . void?)
          ;; Reads the interaction from file
          (define/public (read-from-file f)
            (send input-text read-from-file f))
          
          (super-new)
          
          (define program-editor%
            ((drscheme:unit:get-program-editor-mixin)
             (interaction-text this)))
          
          (field [input-text (new program-editor%)]
                 [output-text (new text%)])
          
          (when copy-constructor
            (send (send copy-constructor get-input) copy-self-to input-text))
          
          (field [io (new vertical-alignment% (parent this))]
                 [input (new horizontal-alignment% (parent io))])
          (new embedded-message% (label " > ") (parent input))
          (new snip-wrapper%
               (snip (new stretchable-editor-snip%
                          (editor input-text)
                          (stretchable-height false)
                          (with-border? false)
                          (min-width 100)))
               (parent input))
          (new embedded-text-button%
               (parent input)
               (label "Ctrl + Enter")
               (callback (lambda (b e) (make-new))))
          
          (field [output (new vertical-alignment% (parent io) (show? false))])
          (new snip-wrapper%
               (snip (new stretchable-editor-snip%
                          (editor output-text)
                          (stretchable-height false)
                          (with-border? false)))
               (parent output))
          ))
      
      #;((is-a? interaction%) . -> . (is-a?/c text%))
      ;; A text that is a program editor and also has keybindings that move around the
      ;; interactions from the given interaction
      (define (interaction-text interaction)
        (class scheme:text%
          
          #;((is-a?/c interaction%) . -> . void?)
          ;; Send the mouse cursor to the given interaction's input field
          ;; NOTE: This function not considered harmful.
          (define (goto inter)
            (when (is-a? inter interaction%)
              (let ([text (send inter get-input)])
                (send text set-caret-owner false 'global))))
      
          (field [movement-keymap (make-object keymap%)])
          
          (send* movement-keymap
            (add-function "goto-next-interaction"
                          (lambda (ignored event)
                            (goto (send interaction next))))
            (map-function ":c:right" "goto-next-interaction")
            (add-function "goto-prev-interaction"
                          (lambda (ignored event)
                            (goto (send interaction prev))))
            (map-function ":c:left" "goto-prev-interaction")
            (add-function "make-new"
                          (lambda (ignored event)
                            (send interaction make-new)))
            (map-function ":c:return" "make-new")
            (add-function "delete"
                          (lambda (ignored event)
                            (let ([next (send interaction next)])
                              (send (send interaction get-parent) delete-child interaction)
                              (goto next))))
            (map-function ":c:delete" "delete"))
          
          #;(-> (listof keymap%))
          ;; the list of keymaps associated with this text
          (define/override (get-keymaps)
            (cons movement-keymap (super get-keymaps)))
          
          (super-new)
          ))
      ))
  )