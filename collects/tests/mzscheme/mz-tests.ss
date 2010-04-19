
(load-relative "loadtest.ss")

(load-relative "basic.ss")
(load-relative "unicode.ss")
(load-relative "rx.ss")
(load-relative "read.ss")
(load-relative "macro.ss")
(load-relative "syntax.ss")
(load-relative "procs.ss")
(load-relative "stx.ss")
(load-relative "module.ss")
(load-relative "number.ss")
(load-relative "unsafe.ss")
(load-relative "object.ss")
(load-relative "struct.ss")
(load-relative "unit.ss")
(load-relative "unitsig.ss")
(load-relative "thread.ss")
(load-relative "logger.ss")
(load-relative "sync.ss")
(load-relative "deep.ss")
(load-relative "contmark.ss")
(load-relative "prompt.ss")
(load-relative "will.ss")
(load-relative "namespac.ss")
(load-relative "modprot.ss")
(load-relative "chaperone.ss")
(unless (or building-flat-tests? in-drscheme?)
  (load-relative "param.ss"))
(load-relative "port.ss")
(load-relative "file.ss")
(load-relative "path.ss")
(unless (or building-flat-tests? in-drscheme?)
  (load-relative "optimize.ss"))
(unless building-flat-tests?
  (load-relative "name.ss"))

;; Ok, so this isn't really all of them. Here are more:
; thrport.ss

; See also README
