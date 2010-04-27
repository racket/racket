#lang scheme/base
(require (prefix-in 2: 2htdp/image)
         (prefix-in 1: htdp/image)
         htdp/error)

(provide image? scene? image-width image-height text 2:image?
         check-image check-scene check-scene-result)

(define (scene? x)
  ;; be sure to check 2:image? first so that
  ;; bitmaps are always okay (more specifically,
  ;; so that we don't reject a 2htdp/image universe
  ;; program that uses a bitmap (bitmap pinholes
  ;; are not at (0,0).)).
  (or (2:image? x)
      (1:scene? x)))

(define (image? x) (or (1:image? x) (2:image? x)))

(define (text a b c) (2:text a b c))

(define (image-width x)
  (check-arg 'image-width (image? x) 'image 1 x)
  (if (2:image? x)
      (2:image-width x)
      (1:image-width x)))

(define (image-height x)
  (check-arg 'image-height (image? x) 'image 1 x)
  (if (2:image? x)
      (2:image-height x)
      (1:image-height x)))

;; Symbol Any String String *-> Void
(define (check-image tag i rank . other-message)
  (if (and (pair? other-message) (string? (car other-message)))
      (check-arg tag (image? i) (car other-message) rank i)
      (check-arg tag (image? i) "image" rank i)))

(define (check-scene tag i rank)
  (define error "image with pinhole at (~s,~s)")
  (if (2:image? i)
      i
      (if (1:image? i)
          (check-arg tag (1:scene? i) "scene" rank (image-pins i))
          (check-arg tag #f         "scene" rank i))))

(define (check-scene-result tname i)
  (if (2:image? i)
      i
      (if (1:image? i) 
          (check-result tname 1:scene? "scene" i (image-pins i))
          (check-result tname (lambda _ #f) "scene" i))))

(define (image-pins i)
  (format "image with pinhole at (~s,~s)" (1:pinhole-x i) (1:pinhole-y i)))
