(module pasteboard-lib mzscheme
  
  (require
   mzlib/class
   mred
   mzlib/contract
   mzlib/etc
   "interface.rkt"
   "snip-lib.rkt")
  
  (provide/contract
   (pasteboard-root ((is-a?/c aligned-pasteboard<%>) . -> . (is-a?/c aligned-pasteboard<%>)))
   (pasteboard-parent
    ((is-a?/c pasteboard%) . -> . (or/c (is-a?/c editor-canvas%) (is-a?/c editor-snip%) false/c))))
  
  ;; gets the top most aligned pasteboard in the tree of pasteboards and containers
  (define (pasteboard-root pasteboard)
    (let ([parent (pasteboard-parent pasteboard)])
      (cond
        [(is-a? parent canvas%)
         pasteboard]
        [(is-a? parent snip%)
         (let ([grand-parent (snip-parent parent)])
           (if (is-a? grand-parent aligned-pasteboard<%>)
               (pasteboard-root grand-parent)
               pasteboard))]
        [else pasteboard])))

  ;; gets the canvas or snip that the pasteboard is displayed in
  ;; status: what if there is more than one canvas? should this be allowed? probably not.
  (define (pasteboard-parent pasteboard)
    (let ([admin (send pasteboard get-admin)])
      (cond
        [(is-a? admin editor-snip-editor-admin<%>)
         (send admin get-snip)]
        [(is-a? admin editor-admin%)
         (send pasteboard get-canvas)]
        [else false]))))
