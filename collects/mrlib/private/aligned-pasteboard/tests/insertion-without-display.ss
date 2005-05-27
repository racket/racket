(require (lib "aligned-pasteboard.ss" "mrlib"))
(with-handlers ([exn? (lambda (x) #f)])
  (send (new pasteboard%) insert (new aligned-editor-snip% (editor (new horizontal-pasteboard%))))
  #t)