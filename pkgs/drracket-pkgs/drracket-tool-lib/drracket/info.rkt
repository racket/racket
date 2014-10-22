#lang info

(define raco-commands
  (list (list "dependencies-graph"
              'drracket/private/raco-module-browser
              "opens a GUI window showing transitive module dependencies (aka `Module Browser')"
              #f)))
