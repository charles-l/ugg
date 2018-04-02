#lang racket

(require reloadable)
(require ffi/vector)
(require "g.rkt")

(provide run)

(define a-shader (make-shader "vert.glsl" "frag.glsl" '((mvp . mat4))))

(define m (read-mesh "./x.scm"))

(define pos (make-vec3 0.0 -5.0 1.0))

(define (run)
  (when (key-down? 'up)
    (set-vec3-x! pos (+ (vec3-x pos) 1)))
  (when (key-down? 'down)
    (set-vec3-x! pos (- (vec3-x pos) 1)))
  (when (key-down? 'right)
    (set-vec3-y! pos (+ (vec3-y pos) 1)))
  (when (key-down? 'left)
    (set-vec3-y! pos (- (vec3-y pos) 1)))
  (clear_frame 0.1 0.2 0.3)
  (define mvp (calculate_mvp pos))
  (with-shader a-shader `((mvp . ,mvp))
               (thunk
                 (draw m))))

