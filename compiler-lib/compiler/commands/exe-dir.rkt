#lang scheme/base
(require scheme/cmdline
         raco/command-name
         compiler/distribute)

(define verbose (make-parameter #f))
(define exe-embedded-collects-path (make-parameter #f))
(define exe-dir-add-collects-dirs (make-parameter null))

(define-values (dest-dir source-files)
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("--collects-path") path "Set <path> as main collects for executables"
    (exe-embedded-collects-path path)]
   #:multi
   [("++collects-copy") dir "Add collects in <dir> to directory"
    (exe-dir-add-collects-dirs (append (exe-dir-add-collects-dirs) (list dir)))]
   #:once-each
   [("-v") "Verbose mode"
    (verbose #t)]
   #:args (dest-dir . executable)
   (values dest-dir executable)))

(assemble-distribution
 dest-dir
 source-files
 #:collects-path (exe-embedded-collects-path)
 #:copy-collects (exe-dir-add-collects-dirs))
(when (verbose)
  (printf " [output to \"~a\"]\n" dest-dir))

(module test racket/base)
