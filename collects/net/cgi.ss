(reference-library "macro.ss")
(reference-library "mail.ss" "net")

(reference-library "cgiu.ss" "net")

(invoke-open-unit/sig mzlib:cgi@ #f mzlib:sendmail^)
