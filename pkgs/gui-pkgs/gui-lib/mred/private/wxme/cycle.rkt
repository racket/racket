#lang racket/base

(define-syntax-rule (decl id set-id)
  (begin
    (provide id set-id)
    (define id #f)
    (define (set-id v) (set! id v))))

(decl text% set-text%!)
(decl pasteboard% set-pasteboard%!)
(decl editor-stream-in% set-editor-stream-in%!)
(decl editor-stream-out% set-editor-stream-out%!)
(decl editor-snip% set-editor-snip%!)
(decl editor-snip-editor-admin% set-editor-snip-editor-admin%!)

(decl extended-editor-snip% set-extended-editor-snip%!)
(decl extended-text% set-extended-text%!)
(decl extended-pasteboard% set-extended-pasteboard%!)

(decl get-editor-data-class set-get-editor-data-class!)

(decl editor-get-file set-editor-get-file!)
(decl editor-put-file set-editor-put-file!)

(decl popup-menu% set-popup-menu%!)

(decl printer-dc% set-printer-dc%!)

