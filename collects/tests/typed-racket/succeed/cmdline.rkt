#lang typed-scheme

(require scheme/cmdline)

(: version-str String)
(define version-str "0.1")

(current-command-line-arguments (vector "counter.mch"))

(: verbose-mode (Parameter Boolean))
(define verbose-mode (make-parameter #f))

(: optimize-level (Parameter Integer))
(define optimize-level (make-parameter 0))

(: model-checking-mode (Parameter Any))
(define model-checking-mode (make-parameter 'sat))

(: file-to-model-check String)
(define file-to-model-check
 (command-line
  #:program "eboc" ;; Should be name of executable
  #:once-each
  [("-v" "--verbose") "Compile with verbose messages"
                      (verbose-mode #t)]
  [("-m" "--mode") #{mode : String} "Mode to run the model checker on (sat, satbin)"
                   (model-checking-mode (string-append mode))]
  #:once-any
  [("-o" "--optimize-1") "Compile with optimization level 1"
                         (optimize-level 1)]
  ["--optimize-2"        (; show help on separate lines
                          "Compile with optimization level 2,"
                          "which includes all of level 1")
                         (optimize-level 2)]

  #:args (#{filename : String}) ; expect one command-line argument: <filename>
  ; return the argument as a filename to compile
  filename))
