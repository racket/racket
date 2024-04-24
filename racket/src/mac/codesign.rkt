#lang racket/base
(require racket/cmdline
         racket/file
         compiler/private/mach-o)

(define remove? #f)
(define entitlements #f)

(define file
  (command-line
   #:once-each
   [("--remove-signature") "Remove signature"
                           (set! remove? #t)]
   [("-s") identity "Add ad hoc-signature"
           (unless (equal? identity "-")
             (error "identity must be `-`"))]
   [("--entitlements") plist-file "With entitlements"
                       (set! entitlements (file->bytes plist-file))]
   #:args (file)
   file))

(void
 (if remove?
     (remove-signature file)
     (add-ad-hoc-signature file #:entitlements entitlements)))
