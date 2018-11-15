#lang racket/base
(require "../common/check.rkt"
         "../path/path.rkt"
         "../path/relativity.rkt"
         "../network/port-number.rkt")

(provide make-security-guard
         security-guard?
         current-security-guard

         security-guard-check-file
         security-guard-check-file-link
         security-guard-check-network

         unsafe-make-security-guard-at-root)

(struct security-guard (parent
                        file-guard
                        network-guard
                        link-guard))

(define root-security-guard
  (security-guard #f void void void))

(define/who current-security-guard
  (make-parameter root-security-guard
                  (lambda (v)
                    (check who security-guard? v)
                    v)))

(define/who (make-security-guard parent
                                 file-guard
                                 network-guard
                                 [link-guard void])
  (check who security-guard? parent)
  (check who (procedure-arity-includes/c 3) file-guard)
  (check who (procedure-arity-includes/c 4) network-guard)
  (check who #:or-false (procedure-arity-includes/c 3) link-guard)
  (security-guard parent file-guard network-guard (or link-guard void)))

(define/who (unsafe-make-security-guard-at-root [file-guard void]
                                                [network-guard void]
                                                [link-guard void])
  (check who (procedure-arity-includes/c 3) file-guard)
  (check who (procedure-arity-includes/c 4) network-guard)
  (check who (procedure-arity-includes/c 3) link-guard)
  (security-guard #f file-guard network-guard link-guard))

(define/who (security-guard-check-file check-who given-path guards)
  (check who symbol? check-who)
  (check who path-string? #:or-false given-path)
  (check who (lambda (l)
               (and (list? l)
                    (for/and ([s (in-list l)])
                      (or (eq? s 'read)
                          (eq? s 'write)
                          (eq? s 'execute)
                          (eq? s 'delete)
                          (eq? s 'exists)))))
         #:contract "(or/c 'read 'write 'execute 'delete 'exists)"
         guards)
  (define path (->path given-path))
  (let loop ([sg (current-security-guard)])
    (when sg
      ((security-guard-file-guard sg) check-who path guards)
      (loop (security-guard-parent sg)))))

(define/who (security-guard-check-file-link check-who given-path given-dest)
  (check who symbol? check-who)
  (check who (lambda (p) (and (path-string? p) (complete-path? p)))
         #:contract "(and/c path? complete-path?)"
         given-path)
  (check who path-string? given-dest)
  (define path (->path given-path))
  (define dest (->path given-dest))
  (let loop ([sg (current-security-guard)])
    (when sg
      ((security-guard-link-guard sg) check-who path dest)
      (loop (security-guard-parent sg)))))

(define/who (security-guard-check-network check-who given-host port mode)
  (check who symbol? check-who)
  (check who string? #:or-false given-host)
  (check who listen-port-number? #:or-false port)
  (check who (lambda (s)
               (or (eq? s 'client)
                   (eq? s 'server)))
         #:contract "(or/c 'client 'server)"
         mode)
  (define host (and given-host (string->immutable-string given-host)))
  (let loop ([sg (current-security-guard)])
    (when sg
      ((security-guard-network-guard sg) check-who host port mode)
      (loop (security-guard-parent sg)))))
