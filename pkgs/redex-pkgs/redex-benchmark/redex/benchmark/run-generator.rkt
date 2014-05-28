#lang racket/base

(require "private/logging.rkt"
         "private/gen-run.rkt"
         racket/cmdline
         racket/match
         racket/port)


(define secs 60)
(define gen-mod #f)
(define check-mod #f)
(define run-name (string-append
                  "run-"
                  (symbol->string (timestamp))))
(define log-dir (current-directory))
(define log? #f)
(define verbose? #f)

(define (log-name name type)
  (string-append name "-" (symbol->string type) ".rktd"))

(command-line
 #:once-each
 [("-m" "--minutes") minutes "Minutes to run for (default 1)"
                     (set! secs (* 60 (string->number minutes)))]
 [("-s" "--seconds") seconds "Seconds to run for (default 60)"
                     (set! secs (string->number seconds))]
 [("-g" "--gen-mod") generator-module "Generator module path"
                     (call-with-input-string 
                      generator-module
                      (λ (in) (set! gen-mod (read in))))]
 [("-c" "--check-mod") check-module "Generator module path"
                       (call-with-input-string 
                        check-module
                        (λ (in) (set! check-mod (read in))))]
 [("-n" "--name") name "Test run name"
                  (set! run-name name)]
 [("-d" "--log-directory") log-directory "Directory to write log files to"
                           (set! log-dir log-directory)]
 [("-v" "--verbose") "Verbose"
                     (set! verbose? #t)]
 [("-l" "--log") "Turn logging on"
                 (set! log? #t)])

(unless gen-mod
  (printf "Must specify a generator module (-g <module path>)\n")
  (exit))
(unless check-mod
  (printf "Must specify a check module (-c <module path>\n")
  (exit))

(when verbose?
  (printf "Running ~a for ~a seconds... \t[~s ~s]\n" run-name secs gen-mod check-mod))

(match-define (run-results tries time cexps) 
  (if log?
      (parameterize ([bmark-log-directory log-dir])
        (benchmark-logging-to (log-name run-name (dynamic-require gen-mod 'type))
                         (λ () (run-gen-and-check/mods gen-mod check-mod secs #:name run-name))))
      (run-gen-and-check/mods gen-mod check-mod secs #:name run-name)))

(when verbose?
  (printf "Finished running ~a in ~a seconds, found ~a counterexamples\n" run-name (exact->inexact (/ time 1000)) cexps))
