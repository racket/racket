(require-library "macro.ss")
(require-library "mail.ss" "net")

(require-library "cgiu.ss" "net")

(invoke-open-unit/sig mzlib:cgi@ #f mzlib:sendmail^)
