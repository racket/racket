("render-language: cannot render the result of define-union-language"
 ([rl (render-language L2)]) ([L2 L2] [L L])
 (let ()
   (define-language L (e any))
   (define-union-language L2 L)
   rl))
