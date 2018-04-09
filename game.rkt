#lang racket

(require reloadable)
(require ffi/vector)
(require "g.rkt")

(provide run handler)

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

(define cam-pos (make-vec3 0.0 0.0 -4.0))
(define yaw 0.0)
(define pitch 0.0)

(require ffi/unsafe)
(define (handler event)
  (case (union-ref event 0)
    ((mouse-motion)
     (let ((e (union-ref event 2)))
       (set! yaw (+ yaw
                    (/ (exact->inexact
                         (sdl-mouse-motion-event-xrel e)) 100)))
       (set! pitch (+ pitch
                    (/ (exact->inexact
                         (sdl-mouse-motion-event-yrel e)) 100)))
       ))))

(define (run)
  (when (key-down? 'up)
    (set-vec3-y! cam-pos (+ (vec3-y cam-pos) 1)))
  (when (key-down? 'down)
    (set-vec3-y! cam-pos (- (vec3-y cam-pos) 1)))
  (when (key-down? 'right)
    (set-vec3-z! cam-pos (+ (vec3-z cam-pos) 1)))
  (when (key-down? 'left)
    (set-vec3-z! cam-pos (- (vec3-z cam-pos) 1))
    (%append-verts! *debug* `((,(exact->inexact (random 4)) ,(exact->inexact (random 4)) ,(exact->inexact (random 4)))
                              (,(exact->inexact (random 4)) ,(exact->inexact (random 4)) ,(exact->inexact (random 4))))))
  (clear_frame 0.1 0.2 0.3)
  (define mvp (calculate_mvp cam-pos yaw pitch))
  (with-shader a-shader `((mvp . ,mvp) (color . ,(make-vec3 1.0 1.0 1.0)))
               (thunk
                 (draw m)))
  (with-shader mono-shader `((mvp . ,mvp) (color . ,(make-vec3 1.0 1.0 1.0))) #:draw-mode 'line
               (thunk
                 (draw-lines *debug* #:connected? #f))))

