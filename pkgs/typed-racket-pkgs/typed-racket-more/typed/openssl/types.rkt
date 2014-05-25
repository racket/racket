#lang typed/racket/base

#| Datatypes used by typed/openssl, based on the untyped openssl module. |#

(provide SSL-Protocol
         SSL-Client-Context ssl-client-context?
         SSL-Server-Context ssl-server-context?
         SSL-Context
         SSL-Listener ssl-listener?
         ;; ssl-port? is provided below
         SSL-Verify-Source
         )

(define-type SSL-Protocol
  (U 'sslv2-or-v3 'sslv2 'sslv3 'tls))

(require/opaque-type SSL-Client-Context ssl-client-context? openssl)

(require/opaque-type SSL-Listener ssl-listener? openssl)
(provide SSL-Listener ssl-listener?)

(require/typed/provide openssl
  ;; XXX Would be better if we could make a type SSL-Port as a subtype
  ;; of Port, but for now that's impossible so we'll just provide this
  ;; predicate that guarantees Port-ness.
  [ssl-port? (-> Any Boolean ;; TODO: Add this -->  : #:+ Port
                 )]
  )

(require/opaque-type SSL-Server-Context ssl-server-context? openssl)

(define-type SSL-Context (U SSL-Client-Context SSL-Server-Context))



;;;; Host Verification ;;;;

(define-type SSL-Verify-Source
  (U Path-String
     (List 'directory Path-String)
     (List 'win32-store String)
     (List 'macosx-keychain Path-String)))

