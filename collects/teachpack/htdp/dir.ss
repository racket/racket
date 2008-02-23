#cs(module dir mzscheme
  (require (all-except htdp/dir file-size)
	   (rename htdp/dir file--size file-size))
  (provide (rename file--size file-size)
	   (all-from-except htdp/dir file--size)))
