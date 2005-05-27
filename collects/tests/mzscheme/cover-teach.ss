
(define teach-dir (build-path (collection-path "lang") "private"))
(define compiled-file (build-path teach-dir "compiled" "teach.zo"))
(define saved-file (string-append compiled-file ".save"))

(define compiled? (file-exists? compiled-file))
(when compiled?
  (rename-file-or-directory compiled-file saved-file))

(require (lib "errortrace.ss" "errortrace"))

(execute-counts-enabled #t)
(dynamic-require '(lib "teach.ss" "lang" "private") #f)
(execute-counts-enabled #f)

(when compiled?
  (rename-file-or-directory saved-file compiled-file))

(load "beginner.ss")
(require mzscheme)
(load "beginner-abbr.ss")
(require mzscheme)
(load "intermediate.ss")
(require mzscheme)
(load "advanced.ss")
(require mzscheme)

(with-output-to-file "teach-annotated.ss"
  (lambda ()
    (annotate-executed-file (build-path (collection-path "lang") "private" "teach.ss")))
  'truncate/replace)

