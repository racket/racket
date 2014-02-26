#lang s-exp typed-racket/base-env/extra-env-lang

;; This module provides base types for contracted racket/snip
;; identifiers that need special handling

(require racket/snip/private/snip
         racket/snip/private/snip-admin
         racket/snip/private/style
         (for-syntax (only-in (rep type-rep) make-Instance))
         "gui-types.rkt"
         (for-syntax (submod "gui-types.rkt" #%type-decl)))

(type-environment
 [snip% (parse-type #'Snip%)]
 [snip-admin% (parse-type #'Snip-Admin%)]
 [snip-class% (parse-type #'Snip-Class%)]
 [string-snip% (parse-type #'String-Snip%)]
 [style-delta% (parse-type #'Style-Delta%)]
 [style-list% (parse-type #'Style-List%)])
