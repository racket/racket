;; this is like htdp/image
;; except that it provides things with
;; provide-primitives for better error
;; reporting in the teaching languages.

(module image mzscheme
  (require htdp/image lang/prim)
  (provide (all-from htdp/image))
)
