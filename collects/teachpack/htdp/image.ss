;; this is like (lib "htdp" "image.ss") 
;; except that it provides things with
;; provide-primitives for better error
;; reporting in the teaching languages.

(module image mzscheme
  (require htdp/image (lib "prim.ss" "lang"))  
  (provide (all-from htdp/image))
)
