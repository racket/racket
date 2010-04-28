
(load-relative "loadtest.rkt")

(load-relative "basic.rkt")
(load-relative "unicode.rkt")
(load-relative "rx.rkt")
(load-relative "read.rkt")
(load-relative "macro.rkt")
(load-relative "syntax.rkt")
(load-relative "procs.rkt")
(load-relative "stx.rkt")
(load-relative "module.rkt")
(load-relative "number.rkt")
(load-relative "unsafe.rkt")
(load-relative "object.rkt")
(load-relative "struct.rkt")
(load-relative "unit.rkt")
(load-relative "unitsig.rkt")
(load-relative "thread.rkt")
(load-relative "logger.rkt")
(load-relative "sync.rkt")
(load-relative "deep.rkt")
(load-relative "contmark.rkt")
(load-relative "prompt.rkt")
(load-relative "will.rkt")
(load-relative "namespac.rkt")
(load-relative "modprot.rkt")
(load-relative "chaperone.rkt")
(unless (or building-flat-tests? in-drscheme?)
  (load-relative "param.rkt"))
(load-relative "port.rkt")
(load-relative "file.rkt")
(load-relative "path.rkt")
(unless (or building-flat-tests? in-drscheme?)
  (load-relative "optimize.rkt"))
(unless building-flat-tests?
  (load-relative "name.rkt"))

;; Ok, so this isn't really all of them. Here are more:
; thrport.rkt

; See also README
