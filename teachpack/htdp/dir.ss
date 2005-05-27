#cs(module dir mzscheme
  (require (all-except (lib "dir.ss" "htdp") file-size)
	   (rename (lib "dir.ss" "htdp") file--size file-size))
  (provide (rename file--size file-size)
	   (all-from-except (lib "dir.ss" "htdp") file--size)))
