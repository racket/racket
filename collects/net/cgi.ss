(reference-library "macro.ss")
(reference-library "cgiu.ss" "web")

(define sendmail-program-file "/usr/lib/sendmail")

(invoke-open-unit/sig mzlib:cgi@ #f mzlib:cgi-imports^)
