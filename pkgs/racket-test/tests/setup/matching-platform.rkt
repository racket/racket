#lang racket/base
(require setup/matching-platform
         rackunit
         racket/string)

(check-equal? #t (platform-spec? (system-type)))
(check-equal? #t (platform-spec? "string"))
(check-equal? #t (platform-spec? #rx"regexp"))
(check-equal? #f (platform-spec? #"bytes"))
(check-equal? #f (platform-spec? 11))

(check-equal? #t (matching-platform? (system-type)))
(check-equal? #f (matching-platform? 'no-such-system-type))
(check-equal? #t (matching-platform? 'unix #:system-type 'unix))
(check-equal? #t (matching-platform? 'windows #:system-type 'windows))

(check-equal? #t (matching-platform? (path->string (system-library-subpath #f))))
(check-equal? #f (matching-platform? "no-such-platform"))
(check-equal? #t (matching-platform? "no-such-platform" #:system-library-subpath (build-path "no-such-platform")))
(check-equal? #f (matching-platform? "no" #:system-library-subpath (build-path "no-such-platform")))

(check-equal? #t (matching-platform? #rx"."))
(check-equal? #t (matching-platform? (regexp-quote (path->string (system-library-subpath #f)))))
(check-equal? #f (matching-platform? #rx"^no-such-platform$"))
(check-equal? #t (matching-platform? #rx"^no-such-platform$" #:system-library-subpath (build-path "no-such-platform")))
