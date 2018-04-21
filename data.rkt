#lang typed/racket
(require "math.rkt")
(provide (all-defined-out))

(define *entities* #())

(define-type Entity Exact-Positive-Integer)

(struct transform ([position : Vec3]))
