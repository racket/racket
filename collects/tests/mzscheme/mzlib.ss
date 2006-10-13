
; Test MzLib
; See also pptest.ss and ztest.ss

(load-relative "loadtest.ss")

(load-relative "md5.ss")

(load-relative "etc.ss")

(load-relative "structlib.ss")

(load-relative "async-channel.ss")

(load-relative "restart.ss")

(load-relative "function.ss")

(load-relative "string.ss")

(load-relative "filelib.ss")

(load-relative "portlib.ss")

(load-relative "threadlib.ss")

(load-relative "date.ss")

(load-relative "compat.ss")

(load-relative "cmdline.ss")

(load-relative "pconvert.ss")

(load-relative "pretty.ss")

(load-relative "control.ss")

;; (load-relative "package.ss")

(load-relative "contract-test.ss")

(load-relative "match-test.ss")

(load-relative "kw.ss")

; Next-to-last, because it `require's mzscheme
(load-relative "shared.ss")

; Last - so macros are not present by accident
(load-relative "macrolib.ss")

(report-errs)
