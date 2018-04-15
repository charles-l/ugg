#lang racket

(require reloadable)
(require ffi/vector)
(require "g.rkt")

(provide run handler yaw)

(define +max-debug-lines+ 32)
(define a-shader (make-shader "vert.glsl" "frag.glsl" '((mvp . mat4) (color . vec3))))
(define mono-shader (make-shader "vert.glsl" "mono.glsl" '((mvp . mat4) (color . vec3))))

(define m (read-mesh "./x.sexp"))
(define m-pos (make_translate_matrix (make-vec3 0.0 4.0 0.0)))
(define p (make-plane 4))

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
(define (clamp a b v)
  (max (min b v) a))

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

(define mid (make_id_mat))
(define (m* . ms) (foldr HMM_MultiplyMat4 mid ms))

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

#;(%append-verts! *debug* `((,(exact->inexact (random 4)) ,(exact->inexact (random 4)) ,(exact->inexact (random 4)))
                              (,(exact->inexact (random 4)) ,(exact->inexact (random 4)) ,(exact->inexact (random 4)))))
  (clear_frame 0.1 0.2 0.3)
  (with-shader a-shader `((mvp . ,(m* vp m-pos)) (color . ,(make-vec3 1.0 1.0 1.0)))
               (thunk
                 (draw m)))
  (with-shader mono-shader `((mvp . ,vp) (color . ,(make-vec3 1.0 0.0 0.0)))
               (thunk
                 (draw p)))
  (with-shader mono-shader `((mvp . ,vp) (color . ,(make-vec3 1.0 1.0 1.0))) #:draw-mode 'line
               (thunk
                 (draw-lines *debug* #:connected? #f))))

