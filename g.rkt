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
(define-g compile_shader (_fun _string _uint32 -> _uint32))
(define-g link_program (_fun _uint32 _uint32 -> _uint32))
(define-g glUseProgram (_fun _uint -> _void))
(define-g glGetUniformLocation (_fun _uint _string -> _int))
(define-g inject_mvp (_fun _uint -> _void))

(define (make-mesh positions)
  (let* ((n (gen_vao))
         (r (gen_vbo (f32vector->cpointer positions)
                     (f32vector-length positions))))
    (glBindVertexArray 0)
    (cons n r)))

(define (make-shader vert-path frag-path)
  (define FRAGMENT-SHADER #x8B30)
  (define VERTEX-SHADER #x8B31)
  (let ((v (compile_shader (file->string vert-path) VERTEX-SHADER))
        (f (compile_shader (file->string frag-path) FRAGMENT-SHADER)))
    (link_program v f)))

(define (projection-matrix fov near far)
  (define scale ($ 1 / (tan (fov * 1/2 * pi / 180))))
  (matrix [[scale               0 0 0]
           [0 scale               0 0]
           [0 0 0                  -1]
           [0 0 ($ - far / (far - near)) 0]]))

(init_screen "game")

(define perspective (projection-matrix 20 1 400))

(define shid (make-shader "vert.glsl" "frag.glsl"))

(define m (make-mesh (f32vector -1.0 -1.0 0.0
                                 1.0 -1.0 0.0
                                 0.0  1.0 0.0)))

(println m)

(glUseProgram shid)
(define mvpid (glGetUniformLocation shid "mvp"))

(main_loop (lambda ()
             (clear_frame)
             (inject_mvp mvpid)
             (draw_array (car m) (cdr m))))
