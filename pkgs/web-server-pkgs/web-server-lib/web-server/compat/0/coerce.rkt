#lang racket/base
(require "http/response-structs.rkt"
         web-server/servlet/servlet-structs)
(set-any->response! normalize-response)
