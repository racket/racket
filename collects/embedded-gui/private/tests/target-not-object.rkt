(require
 mzlib/class
 embedded-gui)

(let* ([pb (new aligned-pasteboard%)]
       [interactions (new vertical-alignment% (parent pb))])
  (send interactions map-to-list (lambda (x) x))
  #t)
