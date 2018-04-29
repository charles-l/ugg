#lang racket

(require reloadable)
(require ffi/vector)
(require "math.rkt")
(require "g.rkt")
(require "data.rkt")
(require "systems.rkt")

(provide run handler yaw)

(define +max-debug-lines+ 32)
#;(define mono-shader (make-shader "vert.glsl" "mono.glsl" (list (mvp . mat4) (color . vec3))))

(module mydata typed/racket
  (provide (all-defined-out))
  (require "data.rkt")
  (require "math.rkt")
  (require/typed "systems.rkt"
                 (read-mesh (String -> mesh-c))
                 (make-plane (Nonnegative-Integer -> mesh-c)))
  (define-shader a "vert.glsl" "frag.glsl"
                 (mvp : Mat4)
                 (color : Vec3)
                 (tex : Texture))

  (define-entity monkey
                 (mesh (read-mesh "./x.sexp"))
                 (pos (transform (v 0.0 4.0 0.0))))

  (define-entity p
                 (mesh (make-plane 4))
                 (pos (transform (v 0.0 0.0 0.0)))))
(require 'mydata)

(define m (read-mesh "./x.sexp"))
(define t (load_texture "img.png"))

(define *debug*
  (make-line (make-vec3 0.0 0.0 0.0)
             (make-vec3 8.0 8.0 8.0)
             (make-vec3 1.0 4.0 1.0)
             (make-vec3 1.0 8.0 1.0)
             #:buffer-n (* +max-debug-lines+ 2)
             ))

(define cam-pos (make-vec3 0.0 0.0 -4.0))
(define yaw 0.0)

(define pitch 0.0)
(define +pitch-max+ 90)
(define +pitch-min+ -90)

(require ffi/unsafe)
(define (handler event dt)
  (case (union-ref event 0)
    ((mouse-motion)
     (let ((e (union-ref event 2)))
       (set! yaw (+ yaw
                    (* dt 200
                       (/ (exact->inexact
                            (sdl-mouse-motion-event-xrel e)) 100))))
       (set! pitch (clamp
                     +pitch-min+
                     +pitch-max+
                     (+ pitch
                        (* dt 200
                           (/ (exact->inexact
                                (sdl-mouse-motion-event-yrel e)) 100)))))))))

(define projection (make_projection))

(define (run dt)
  (define view (calculate_view cam-pos yaw pitch))
  (define vp (m* projection view))
  (when (key-down? 'w)
    (set! cam-pos (mat4_relative_move view cam-pos 0.0 -0.5 (* dt 10))))
  (when (key-down? 's)
    (set! cam-pos (mat4_relative_move view cam-pos 0.0 0.5 (* dt 10))))
  (when (key-down? 'd)
    (set! cam-pos (mat4_relative_move view cam-pos -0.5 0.0 (* dt 10))))
  (when (key-down? 'a)
    (set! cam-pos (mat4_relative_move view cam-pos 0.5 0.0 (* dt 10))))

  (clear_frame 0.1 0.2 0.3)
  (use-a-shader! (m* vp (translate-m 2.0 2.0 2.0)) (v 1.0 1.0 1.0) t)
  (draw m))

