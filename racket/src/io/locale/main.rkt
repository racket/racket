#lang racket/base
(require "parameter.rkt"
         "string.rkt"
         "collate.rkt"
         "recase.rkt")

(provide current-locale
         locale-string-encoding
         system-language+country

         bytes->string/locale
         string->bytes/locale

         string-locale<?
         string-locale=?
         string-locale>?
         string-locale-ci<?
         string-locale-ci=?
         string-locale-ci>?

         string-locale-downcase
         string-locale-upcase)

