#lang racket

(require reloadable)
(require ffi/vector)
(require "g.rkt")

(provide run)

(define shid (make-shader "vert.glsl" "frag.glsl"))

(define m (read-mesh "/tmp/x.scm"))

(define pos (f32vector 0.0 -5.0 1.0))

(define (run)
  (when (key-down? 'up)
    (f32vector-set! pos 0 (+ (f32vector-ref pos 0) 1)))
  (when (key-down? 'down)
    (f32vector-set! pos 0 (- (f32vector-ref pos 0) 1)))
  (when (key-down? 'right)
    (f32vector-set! pos 1 (+ (f32vector-ref pos 1) 1)))
  (when (key-down? 'left)
    (f32vector-set! pos 1 (- (f32vector-ref pos 1) 1)))
  (clear_frame 0.1 0.2 0.3)
  (inject_mvp shid pos)
  (draw m))

