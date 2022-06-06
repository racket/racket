#lang racket/base
(require racket/cmdline
         compiler/private/mach-o)

(define remove? #f)

(define file
  (command-line
   #:once-each
   [("--remove-signature") "Remove signature"
                           (set! remove? #t)]
   [("-s") identity "Add ad hoc-signature"
           (unless (equal? identity "-")
             (error "identity must be `-`"))]
   [("--entitlements") plist-file "With entitlements"
                       (void)]
   #:args (file)
   file))

(void
 (if remove?
     (remove-signature file)
     (add-ad-hoc-signature file)))
