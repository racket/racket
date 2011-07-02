#lang racket/base

(require "private/decorated-editor-snip.rkt")

(provide 
 (rename-out [editor-snip:decorated% decorated-editor-snip%])
 (rename-out [editor-snip:decorated-snipclass% decorated-editor-snipclass%])
 (rename-out [editor-snip:decorated-mixin decorated-editor-snip-mixin])
 (rename-out [editor-snip:decorated<%> decorated-editor-snip<%>]))
