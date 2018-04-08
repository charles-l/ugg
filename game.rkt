#lang racket

(require reloadable)
(require ffi/vector)
(require "g.rkt")

(provide run)

(define +max-debug-lines+ 32)
(define a-shader (make-shader "vert.glsl" "frag.glsl" '((mvp . mat4))))
(define mono-shader (make-shader "vert.glsl" "mono.glsl" '((mvp . mat4) (color . vec3))))

(define m (read-mesh "./x.scm"))

(define *debug*
  (make-line (make-vec3 0.0 0.0 0.0)
             (make-vec3 8.0 8.0 8.0)
             (make-vec3 1.0 4.0 1.0)
             (make-vec3 1.0 8.0 1.0)
             #:buffer-n (* +max-debug-lines+ 2)
             ))

(define pos (make-vec3 0.0 -5.0 1.0))

(define (run)
  (when (key-down? 'up)
    (set-vec3-x! pos (+ (vec3-x pos) 1)))
  (when (key-down? 'down)
    (set-vec3-x! pos (- (vec3-x pos) 1)))
  (when (key-down? 'right)
    (set-vec3-y! pos (+ (vec3-y pos) 1)))
  (when (key-down? 'left)
    (set-vec3-y! pos (- (vec3-y pos) 1))
    (%append-verts! *debug* `((,(exact->inexact (random 4)) ,(exact->inexact (random 4)) ,(exact->inexact (random 4)))
                              (,(exact->inexact (random 4)) ,(exact->inexact (random 4)) ,(exact->inexact (random 4)))))
    (println (mesh-verts *debug*)))
  (clear_frame 0.1 0.2 0.3)
  (define mvp (calculate_mvp pos))
  (with-shader a-shader `((mvp . ,mvp) (color . ,(make-vec3 1.0 1.0 1.0)))
               (thunk
                 (draw m)))
  (with-shader mono-shader `((mvp . ,mvp) (color . ,(make-vec3 1.0 1.0 1.0))) #:draw-mode 'line
               (thunk
                 (draw-lines *debug* #:connected? #f))))

