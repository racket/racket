#lang racket/base

(provide (struct-out import))

(struct import (name shape int-name [pos #:mutable]))
