#lang racket/base
(require unstable/contract
         "http/response-structs.rkt"
         web-server/servlet/servlet-structs)

(current-response/c (coerce/c normalize-response))