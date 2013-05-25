#lang racket/base
(require racket/class
         racket/contract/base
         "draw/private/contract.rkt"
	 "draw/private/color.rkt"
         "draw/private/point.rkt"
         "draw/private/font.rkt"
         "draw/private/font-dir.rkt"
         "draw/private/font-syms.rkt"
         "draw/private/pen.rkt"
         "draw/private/brush.rkt"
         "draw/private/gradient.rkt"
         "draw/private/region.rkt"
         "draw/private/bitmap.rkt"
         "draw/private/dc-path.rkt"
         "draw/private/dc-intf.rkt"
         "draw/private/bitmap-dc.rkt"
         "draw/private/record-dc.rkt"
         "draw/private/post-script-dc.rkt"
         "draw/private/ps-setup.rkt"
         "draw/private/svg-dc.rkt"
         "draw/private/gl-config.rkt"
         "draw/private/gl-context.rkt")

(provide color-database<%>
	 the-color-database
         font-list% the-font-list make-font
         font-name-directory<%> the-font-name-directory
	 the-pen-list
         the-brush-list
         dc<%>
         recorded-datum->procedure
         ps-setup% current-ps-setup
         get-face-list
         get-family-builtin-face
         gl-context<%>
         make-bitmap
         make-platform-bitmap
         read-bitmap
         make-monochrome-bitmap

         ;; predicates/contracts
         brush-style/c
         pen-cap-style/c
         pen-join-style/c
         pen-style/c
         font-family/c
         font-weight/c
         font-style/c
         font-smoothing/c
         font-hinting/c)

(provide/contract [color%            color%/c]
                  [point%            point%/c]
                  [font%             font%/c]
                  [pen%              pen%/c]
                  [pen-list%         pen-list%/c]
                  [brush%            brush%/c]
                  [brush-list%       brush-list%/c]
                  [bitmap-dc%        bitmap-dc%/c]
                  [post-script-dc%   post-script-dc%/c]
                  [pdf-dc%           pdf-dc%/c]
                  [svg-dc%           svg-dc%/c]
                  [record-dc%        record-dc%/c]
                  [linear-gradient%  linear-gradient%/c]
                  [radial-gradient%  radial-gradient%/c]
                  [region%           region%/c]
                  [dc-path%          dc-path%/c]
                  [gl-config%        gl-config%/c]
                  [bitmap%           bitmap%/c]
                  [make-color        make-color/c]
                  [make-pen          make-pen/c]
                  [make-brush        make-brush/c])

