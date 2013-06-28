#lang racket/signature

;; -- exceptions raised --
(struct mime-error () #:omit-constructor)
(struct unexpected-termination (msg) #:omit-constructor)
(struct missing-multipart-boundary-parameter () #:omit-constructor)
(struct malformed-multipart-entity (msg) #:omit-constructor)
(struct empty-mechanism () #:omit-constructor)
(struct empty-type () #:omit-constructor)
(struct empty-subtype () #:omit-constructor)
(struct empty-disposition-type () #:omit-constructor)

;; -- basic mime structures --
(struct message (version entity fields))
(struct entity (type subtype charset encoding
                disposition params id
                description other fields
                parts body))
(struct disposition (type filename creation
                     modification read
                     size params))

;; -- mime methods --
mime-analyze
