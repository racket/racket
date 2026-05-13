#lang racket/base

(require
 "private/template.rkt"
 "private/render.rkt"
 "private/parse.rkt"
 "private/expand.rkt"
) ; end require

(provide
 template
 template?
 template-strings
 template-interpolations
 interpolation
 interpolation?
 interpolation-value
 interpolation-syntax
 interpolation-kind
 interpolation-source
 render-template
 render-fstring
 template->sql
 html-render
 parse-template-string
 tpl
 fpl
) ; end provide
