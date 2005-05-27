(module table mzscheme
  
  (require
   (lib "class.ss")
   (lib "embedded-gui.ss" "embedded-gui")
   (lib "etc.ss")
   (lib "contract.ss"))
  
  ;; An interface for things that can be items of a table
  (define table-item<%>
    (interface ()
      #;(-> (is-a?/c text%))
      ;; The first text in the item that can be typed into
      get-first-text
      
      #;((is-a?/c editor-stream-out%) . -> . void?)
      ;; Writes the interaction to file
      write
     
      #;((is-a?/c editor-stream-in%) . -> . void?)
      ;; Reads the interaction from file
      read-from-file))
  
  (provide/contract
   (table ((implementation?/c table-item<%>) . -> . (implementation?/c alignment<%>)))
   (table-item<%> interface?))
  
  ;; A table for holding element that must be accessed end writen to file
  (define (table table-item-class%)
    (class vertical-alignment%
      (inherit get-pasteboard)
      (inherit-field head tail)
      (init [copy-constructor #f])
      
      ;;;;;;;;;;
      ;; Accessors
      
      #;(-> (is-a?/c example%))
      ;; The first example in the example field
      (define/public (get-first-child)
        (send head next))
          
      #;(((is-a?/c example%) . -> . any?) . -> . (listof any?))
      ;; A list of the results of applying f to each example in the examples field.
      (define/public (map-children f)
        (send head map-to-list f))
          
      #;(((is-a?/c example%) . -> . void?) . -> . void?)
      ;; For eaches over the children
      (define/public (for-each-child f)
        (send head for-each f))
      
      ;;;;;;;;;;
      ;; Reading/Writing
      
      #;((is-a?/c editor-stream-out%) . -> . void?)
      ;; Write the examples to file
      (define/public (write f)
        (let ([num-items (length (map-children void))])
          (send f put num-items)
          (for-each-child (lambda (c) (send c write f)))))
      
      #;((is-a?/c editor-stream-in%) . -> . void?)
      ;; Reads the examples field's state in from the stream
      (define/public (read-from-file f)
        (send (get-pasteboard) lock-alignment true)
        ;; Delete all examples
        (send head for-each (lambda (c) (send c show false)))
        (send head next tail)
        (send tail prev head)
        ;; Read in all the examples to the file.
        (let* ([num-examples (box 0)])
          (send f get num-examples)
          (let loop ([n (unbox num-examples)])
            (unless (zero? n)
              (let ([example (new table-item-class% (parent this))])
                (send example read-from-file f)
                (loop (sub1 n))))))
        (send (get-pasteboard) lock-alignment false))

      ;;;;;;;;;;
      ;; Adding
      
      #;(-> (is-a?/c alignment<%>))
      ;; Adds a new example to the examples field.
      (define/public add-new
        (opt-lambda ((after #f))
          (let ([item (new table-item-class% (parent this) (after after))])
            (send (send item get-first-text) set-caret-owner false 'global)
            item)))
      
      #;((is-a?/c example%) . -> . (is-a?/c example%))
      ;; Adds a new example that is a copy of the given example
      (define (add-new-copy example-to-copy)
        (new table-item-class%
             (parent this)
             (copy-constructor example-to-copy)))
      
      ;;;;;;;;;;
      ;; Constructor
      
      (super-new)
      (when copy-constructor
        (send (get-pasteboard) lock-alignment true)
        (send copy-constructor for-each-child add-new-copy)
        (send (get-pasteboard) lock-alignment false))
      ))
  )