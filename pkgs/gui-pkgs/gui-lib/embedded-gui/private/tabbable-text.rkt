
(module tabbable-text mzscheme
  
  (require
   mzlib/class
   mzlib/list
   mzlib/etc
   framework
   mred
   mzlib/contract)
  
  (define tabbable-text<%>
    (interface ()
      #;(-> void)
      ;; Takes the caret into this text
      
      set-caret-owner
      #;((-> void) . -> . void)
      ;; The thunk to execute when tabbing ahead
      set-ahead
      
      #;((-> void) . -> . void)
      ;; The thunk to execute when tabbing back
      set-back))
  
  (provide/contract
   (tabbable-text<%> interface?)
   (tabbable-text-mixin mixin-contract)
   (set-tabbing (() (listof (is-a?/c tabbable-text<%>)) . ->* . (void?))))
  
  (define tabbable-text-mixin
    (mixin (editor:keymap<%>) (tabbable-text<%>)
      
      (init-field
       [ahead void]
       [back void])
      
      #;(-> (listof keymap%))
      ;; the list of keymaps associated with this text
      (define/override (get-keymaps)
        (let ([keymap (make-object keymap%)])
          (send keymap add-function "tab-ahead"
                (lambda (ignored event) (ahead)))
          (send keymap map-function ":tab" "tab-ahead")
          (send keymap add-function "tab-back"
                (lambda (ignored event) (back)))
          (send keymap map-function "s:tab" "tab-back")
          (cons keymap (super get-keymaps))))
      
      (define/public (set-ahead t) (set! ahead t))
      (define/public (set-back t) (set! back t))
      
      (super-new)))
  
  ;; sets the tabbing of all of the texts in the order of the list
  (define (set-tabbing . l)
    (cond
      [(or (empty? l) (empty? (rest l))) (void)]
      [else
       (send (first l) set-ahead
             (lambda () (send (second l) set-caret-owner false 'global)))
       (send (second l) set-back
             (lambda () (send (first l) set-caret-owner false 'global)))
       (apply set-tabbing (rest l))]))
  )
