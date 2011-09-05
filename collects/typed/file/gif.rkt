#lang typed/racket/base

(require typed/private/utils)

(require/opaque-type GIF-Stream gif-stream? file/gif)

(require/typed/provide file/gif
  [gif-start ( Output-Port Number Number Number (U #f (Listof (Vectorof Number))) -> Void )]
  [gif-add-image ( GIF-Stream Number Number Number Number Boolean (U #f Number) String -> Void )]
  [gif-add-control ( GIF-Stream Symbol Boolean Number (U #f Number) -> Void)]
  [gif-add-loop-control ( GIF-Stream Number -> Void )]
  [gif-add-comment ( GIF-Stream String -> Void )]
  [gif-end ( GIF-Stream -> Void )]
  [quantize ( String -> (values String (Listof (Vectorof Number)) (U #f (Vectorof Number))))])

(provide gif-stream? GIF-Stream)
