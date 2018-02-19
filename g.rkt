#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector)

(require math/matrix)
(require k-infix)

(define-ffi-definer define-g (ffi-lib "./g"))

(define _win-ptr (_cpointer 'SDL_Window))

(define-g get_win (_fun -> _win-ptr))
(define-g init_screen (_fun _string -> _void))
(define-g main_loop (_fun (_fun -> _void) -> _void))
(define-g gen_vao (_fun -> _uint32))
(define-g clear_frame (_fun -> _void))
(define-g gen_vbo (_fun _pointer _size -> _uint32))
(define-g draw_array (_fun _uint32 _uint32 -> _void))
(define-g glBindVertexArray (_fun _uint32 -> _void))

(define (make-mesh positions)
  (let* ((n (gen_vao))
         (r (gen_vbo (f32vector->cpointer positions)
                     (f32vector-length positions))))
    (glBindVertexArray 0)
    (cons n r)))

(define (projection-matrix fov near far)
  (define scale ($ 1 / (tan (fov * 1/2 * pi / 180))))
  (matrix [[scale               0 0 0]
           [0 scale               0 0]
           [0 0 0                  -1]
           [0 0 ($ - far / (far - near)) 0]]))

(define perspective (projection-matrix 20 1 400))

(init_screen "game")
(define m (make-mesh (f32vector -1.0 -1.0 0.0
                                 1.0 -1.0 0.0
                                 0.0  1.0 0.0)))

(println m)

(main_loop (lambda ()
             (clear_frame)
             (draw_array (car m) (cdr m))
             (println 'hi)))
