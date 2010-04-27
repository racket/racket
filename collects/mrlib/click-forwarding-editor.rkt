(module click-forwarding-editor mzscheme
  
  (require
   mzlib/class
   mzlib/contract
   mzlib/etc
   mred)
  
  (provide/contract
   (click-forwarding-editor-mixin mixin-contract))
  
  ;; mixin to forward clicks to children snips within the editor
  (define (click-forwarding-editor-mixin super%)
    (class super%
      (inherit get-snip-location global-to-local local-to-global
               find-snip get-dc set-caret-owner)
      
      ;; on-event ((is-a?/c mouse-event%) . -> . void?)
      ;; overridden to give focus to child snips when clicked
      (define/override (on-event event)
        (if (memq (send event get-event-type)
                  '(left-down left-up middle-down middle-up right-down right-up))
            (let ([snip (find-snip/global (send event get-x) (send event get-y))])
              (if (is-a? snip snip%)
                  (forward-event snip event)
                  (super on-event event)))
            (super on-event event)))
      
      ;; forward-event ((is-a?/c snip%) (is-a?/c mouse-event%) . -> . void?)
      ;; send the event to the snip
      (define/private (forward-event snip event)
        (let ([editorx (box 0)]
              [editory (box 0)])
          (get-snip-location snip editorx editory false)
          (let ([x (box (unbox editorx))]
                [y (box (unbox editory))])
            (local-to-global x y)
            (send snip on-event (get-dc) (unbox x) (unbox y)
                  (unbox editorx) (unbox editory) event)
            (set-caret-owner snip 'display))))
      
      ;; find-snip/global (number? number? . -> . (union (is-a?/c snip%) false?))
      ;; finds the snip in the pasteboard that is at x y in the global display
      (define/private (find-snip/global x y)
        (let ([new-x (box x)]
              [new-y (box y)])
          (global-to-local new-x new-y)
          (cond
            [(is-a? this text%)
             (let ([pos (send this find-position (unbox new-x) (unbox new-y))])
               (find-snip pos 'after-or-none))]
            [(is-a? this pasteboard%)
             (find-snip (unbox new-x) (unbox new-y))])))
      
      (super-instantiate ())
      ))
  )
