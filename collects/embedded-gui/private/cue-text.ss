#| This module provides a text that has the an init argument of a string
   that is displayed in the text initially. This text acts as a cue or a
   label for that text and goes away when it is typed into.
|#
#| TODO: Adjust read methods like get-text and write-to-file so that if the
   cue text is still in the editor when they are called the result is that
   the text appears empty.
|#
(module cue-text mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "framework.ss" "framework"))
  
  (provide cue-text-mixin
           cue-text%)
  
  (define cue-text-mixin
    (mixin ((class->interface text%)) ()
      (inherit insert change-style erase)
      (init [cue-text ""]
            [color "gray"])
      (init-field
       [behavior '(on-focus)])
      (field [first-focus? true])
      
      #;(-> void)
      ;; Clears the cue-text if it's still there.
      (define (clear-cue-text)
        (when first-focus?
          (set! first-focus? false)
          (erase)))
      
      #;(boolean? . -> . void)
      ;; Called when this text% gains or loses focus
      (define/override (on-focus on?)
        (when (and on? (member 'on-focus behavior))
          (clear-cue-text))
        (super on-focus on?))
      
      #;((is-a?/c keyevent%) . -> . void)
      ;; Called when the keyboard is used with focus in this text
      (define/override (on-local-char akeyevent)
        (when (member 'on-char behavior)
          (clear-cue-text))
        (super on-local-char akeyevent))
      
      ;; Insert the cue text into the text% on instantiation
      (super-new)
      (let ([grey-text (new style-delta%)])
        (send grey-text set-delta-foreground color)
        (change-style grey-text 'start 'end #f))
      (insert cue-text)))
  
  (define cue-text% (cue-text-mixin text%))
  
  #|
  (define f (new frame% (label "f") (width 400) (height 400)))
  (define e (new text%))
  (define c (new editor-canvas% (editor e) (parent f)))
  (define t (new cue-text% (cue-text "Hello, World!")))
  (define s (new editor-snip% (editor t)))
  (send e insert s)
  (send f show #t)
  |#
  )