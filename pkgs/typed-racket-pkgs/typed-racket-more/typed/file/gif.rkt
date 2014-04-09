#lang typed/racket/base

(require typed/private/utils)

(require/opaque-type GIF-Stream gif-stream? file/gif)

(define-type GIF-Colormap (Listof (Vector Integer Integer Integer)))

(require/typed/provide file/gif
  [gif-start ( Output-Port Integer Integer Integer (U #f GIF-Colormap) -> GIF-Stream )]
  [gif-add-image ( GIF-Stream Integer Integer Integer Integer Any (U #f GIF-Colormap) Bytes -> Void )]
  [gif-add-control ( GIF-Stream (U 'any 'keep 'restore-bg 'restore-prev) Any Integer (U #f Integer) -> Void)]
  [gif-add-loop-control ( GIF-Stream Integer -> Void )]
  [gif-add-comment ( GIF-Stream Bytes -> Void )]
  [gif-end ( GIF-Stream -> Void )]
  [quantize ( Bytes -> (values Bytes GIF-Colormap (U #f Integer)))])

(provide gif-stream? GIF-Stream GIF-Colormap)
