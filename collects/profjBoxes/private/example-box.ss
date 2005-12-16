(module example-box mzscheme
  
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
   (lib "parser.ss" "profj")
   (lib "readerr.ss" "syntax")
   (lib "make-snipclass.ss" "test-suite" "private")
   (lib "string-constant.ss" "string-constants")
   "table.ss"
   "box-helpers.ss")
  
  (provide example-box@ example-box^)
  
  ;; This is wrong but it's a good enough prototype
  (define re:java-id (regexp "[A-Za-z_]+"))
  (define min-field-width 50)
  
  (define-signature example-box^ (example-box%))
  (define example-box@
    (unit/sig example-box^
      (import drscheme:tool^)
      
      ;; A readable-snip<%> of an examples box to allow GUI contruction of data examples.
      (define example-box%
        (class* (decorated-editor-snip-mixin editor-snip%) (readable-snip<%>)
          (inherit set-snipclass)
          
          (init [examples-to-copy #f])
          
          #;(any? (union integer? false?) (union integer? false?) (union integer? false?)
                  . -> .
                  any?)
          ;; Called to get the syntax object representing this box
          (define/public read-special
            (opt-lambda (source (line false) (column false) (position false))
              #;(((is-a?/c text%))
                 (natural-number? (union natural-number? false?))
                 . opt-> .
                 id?)
              ;; Make an id out of the given text
              ;; STATUS: I'm parsing the ID with a regexp that's probablly not
              ;; the correct Java variable regexp. Furthermore, I need to parse
              ;; it differently if it's a class name vs. field name.
              ;;EDITED BY KATHY:: Commented this out because the result reading the example box
              ;;is much different than Mike expected it would be
              #;(define (text->java-id atext)
                (let ([str (send atext get-text)])
                  (match (regexp-match-positions re:java-id str 0 false)
                    [((m-start . m-end))
                     (datum->syntax-object
                      false
                      (string->symbol (substring str m-start m-end))
                      (list atext
                            1
                            m-start
                            (add1 m-start)
                            (- m-end m-start)))]
                    ;; STATUS: Here I need to provide for a better form of
                    ;; highlighting using the GUI because there won't always
                    ;; be text in the box to highlight.
                    [else (raise-read-error
                           (string-constant profjBoxes-bad-java-id-error)
                           atext 1 1 1 (send atext last-position))])))
              ;;EDITED BY KATHY:: Same reason as stated above
              #;#`(begin #,@(send examples map-children
                                (lambda (example)
                                  (with-syntax ([name (text->java-id
                                                       (send example get-name))]
                                                [value (parse-expression
                                                        (open-input-text-editor
                                                         (send example get-value))
                                                        (send example get-value)
                                                        level)])
                                    #'(define name value)))))
              (syntax-property #`(parse-example-box (list #,@(send examples map-children
                                                                   (lambda (example)
                                                                     (with-syntax ([type (send example get-type)]
                                                                                   [id (send example get-name)]
                                                                                   [value (send example get-value)])
                                                                       #'(list type id value))))))
                               'example-box #t)
              ))
          
          #;(-> void?)
          ;; Gives this box the cursor focus
          (define/public (take-caret)
            (let ([first-box (send (send examples get-first-child) get-type)])
              (send pb set-caret-owner
                    (send (send first-box get-admin) get-snip)
                    'display)))
          
          ;;;;;;;;;;
          ;; Saving and copying
          
          #;((is-a?/c editor-stream-out%) . -> . void?)
          ;; Writes the examples box to file
          (define/override (write f)
            (send examples write f))
          
          #;((is-a?/c editor-stream-in%) . -> . void?)
          ;; Reads the examples state in from an editor-stream
          (define/public (read-from-file f)
            (send examples read-from-file f))
          
          #;(-> (is-a?/c test-case-box%))
          ;; Makes a copy of this example box.
          (define/override (copy)
            (new example-box% (examples-to-copy examples)))
          
          ;;;;;;;;;;
          ;; Layout
          
          (define/override (get-color) "purple")

          (field [pb (new aligned-pasteboard%)])
          (send pb lock-alignment true)
          (field [main (new vertical-alignment% (parent pb))]
                 [header (new horizontal-alignment% (parent main))])
          
          ;; Since I don't have an icon I'll just center this for now and leave out the image
          (new horizontal-alignment% (parent header)) ; left spacer
          #;(new snip-wrapper% (parent header) (snip (make-object image-snip%)))
          (new embedded-message%
               (parent header)
               (label (string-constant profjBoxes-examples-label)))
          (new horizontal-alignment% (parent header)) ; right spacer
          
          (field
           [examples (new (table example%)
                          (parent main)
                          (copy-constructor examples-to-copy))]
           [button-bar (new horizontal-alignment% (parent main))]
           [add-button (new embedded-text-button%
                            (parent button-bar)
                            (label (string-constant profjBoxes-add-new-example-button))
                            (callback (lambda (b e) (send examples add-new))))])
          (super-new (editor pb))
          (unless examples-to-copy
            (send examples add-new))
          (send pb lock-alignment false)
          (set-snipclass sc)))
      
      (define sc (make-snipclass example-box% "example-box%"))
      
      ;; An example layed out in a horizontal manner. Allows access to the pieces of an example.
      (define example%
        (class* horizontal-alignment% (table-item<%>)
          (inherit get-parent get-pasteboard next prev)
          (init (copy-constructor #f))
          
          (field
           [program-editor%
            (cue-text-mixin
             (tabbable-text-mixin
              ((drscheme:unit:get-program-editor-mixin)
               (editor:keymap-mixin text:basic%))))]
           [type (new (single-line-text-mixin program-editor%)
                      (cue-text (string-constant profjBoxes-type))
                      (behavior '(on-char)))]
           [name (new (single-line-text-mixin program-editor%)
                      (cue-text (string-constant profjBoxes-name))
                      (behavior '(on-char)))]
           [value (new program-editor%
                       (cue-text (string-constant profjBoxes-value))
                       (behavior '(on-char)))])
          
          #;(-> (is-a?/c text%))
          ;; The first text in the item that can be typed into
          (define/public (get-first-text) (get-type))
          
          (define/public (get-type) type)
          (define/public (get-name) name)
          (define/public (get-value) value)
          
          #;((is-a?/c editor-stream-out%) . -> . void?)
          ;; Write the example to file
          (define/public (write f)
            (send type write-to-file f)
            (send name write-to-file f)
            (send value write-to-file f))
          
          #;((is-a?/c editor-stream-out%) . -> . void?)
          ;; Read the state of the example in from file
          (define/public (read-from-file f)
            (send type read-from-file f)
            (send name read-from-file f)
            (send value read-from-file f))
          
          (super-new)
          
          (when copy-constructor
            (send (send copy-constructor get-type) copy-self-to type)
            (send (send copy-constructor get-name) copy-self-to name)
            (send (send copy-constructor get-value) copy-self-to value))
          
          ;;;;;;;;;;
          ;; Tabbing
          
          (when (is-a? (prev) example%)
            (set-tabbing (send (prev) get-value) type))
          (set-tabbing type name value)
          (if (is-a? (next) example%)
              (set-tabbing value (send (next) get-type))
              (send value set-ahead (lambda ()
                                      (send (get-parent) add-new))))
          
          ;;;;;;;;;;
          ;; Layout
          
          (send (get-pasteboard) lock-alignment true)
          (new snip-wrapper%
               (parent this)
               (snip (new editor-snip%
                          (editor type)
                          (min-width min-field-width))))
          (new snip-wrapper%
               (parent this)
               (snip (new editor-snip%
                          (editor name)
                          (min-width min-field-width))))
          (new embedded-message% (parent this) (label " = "))
          (new snip-wrapper%
               (parent this)
               (snip (new editor-snip%
                          (editor value)
                          (min-width min-field-width))))
          (new embedded-message% (parent this) (label " ; "))
          (new horizontal-alignment% (parent this)) ; spacer
          (new embedded-text-button%
               (parent this)
               (label "Delete")
               (callback (lambda (b e) (send (get-parent) delete-child this))))
          (send (get-pasteboard) lock-alignment false)
          ))
      ))
  )