#lang scheme/base
(require "private/provide-structs.ss"
         scheme/contract)

(provide-structs
 [body-id ([value string?])]
 [hover-property ([text string?])]
 [script-property ([type string?]
                   [script (or/c path-string? (listof string?))])]
 [css-addition ([path (or/c path-string? (cons/c 'collects (listof bytes?)))])]
 [html-defaults ([prefix-path (or/c bytes? path-string? (cons/c 'collects (listof bytes?)))]
                 [style-path (or/c bytes? path-string? (cons/c 'collects (listof bytes?)))]
                 [extra-files (listof (or/c path-string? (cons/c 'collects (listof bytes?))))])]

 [url-anchor ([name string?])]
 [attributes ([assoc (listof (cons/c symbol? string?))])])
