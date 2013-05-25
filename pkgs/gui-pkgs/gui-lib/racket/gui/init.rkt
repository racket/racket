(module init scheme/gui
  (require scheme/init)

  (current-load text-editor-load-handler)

  (provide (all-from-out scheme/gui)
           (all-from-out scheme/init)))
