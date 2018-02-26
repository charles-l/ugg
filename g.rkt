#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector)

(provide init_screen
         make-shader
         read-mesh
         glUseProgram
         glGetUniformLocation
         main_loop
         clear_frame
         inject_mvp
         draw)

(define-ffi-definer define-g (ffi-lib "./g"))

(define _win-ptr (_cpointer 'SDL_Window))

(define-g get_win (_fun -> _win-ptr))
(define-g init_screen (_fun _string -> _void))
(define-g main_loop (_fun (_fun -> _void) -> _void))
(define-g gen_vao (_fun -> _uint32))
(define-g clear_frame (_fun -> _void))
(define-g gen_fvbo (_fun _pointer _uint32 _size -> _uint32))
(define-g gen_uvbo (_fun _pointer _uint32 _size -> _uint32))
(define-g draw_array (_fun _uint32 _uint32 _uint32 _size -> _void))
(define-g glBindVertexArray (_fun _uint32 -> _void))
(define-g compile_shader (_fun _string _uint32 -> _uint32))
(define-g link_program (_fun _uint32 _uint32 -> _uint32))
(define-g glUseProgram (_fun _uint -> _void))
(define-g glGetUniformLocation (_fun _uint _string -> _int))
(define-g inject_mvp (_fun _uint -> _void))

(struct vao (id array-id element-array-id))
(struct mesh (verts faces vao) #:transparent)

(define (%gen-mesh-vao flat-vertex-coords flat-face-indices)
  (define ARRAY-BUFFER #x8892)
  (define ELEMENT-ARRAY-BUFFER #x8893)
  (let* ((n (gen_vao))
         (r (gen_fvbo (f32vector->cpointer flat-vertex-coords)
                      ARRAY-BUFFER
                      (f32vector-length flat-vertex-coords)))
         (f (gen_uvbo (u32vector->cpointer flat-face-indices)
                      ELEMENT-ARRAY-BUFFER
                      (u32vector-length flat-face-indices))))
    (glBindVertexArray 0)
    (vao n r f)))


(define (read-mesh f)
  ; XXX assumes only one object
  ;
  ; Could get a speedup if using vectors rather than lists if needed?

  (let* ((o (cdr (with-input-from-file f (thunk (read)))))
         (verts (cadr (assoc 'vertices o)))
         (faces (cadr (assoc 'faces o)))
         (p (%gen-mesh-vao (list->f32vector (flatten verts))
                           (list->u32vector (flatten faces)))))
   (mesh verts faces p)))

(define (draw m)
  (draw_array (vao-id (mesh-vao m))
              (vao-array-id (mesh-vao m))
              (vao-element-array-id (mesh-vao m))
              (* 3 (length (mesh-faces m)))))

(define (make-shader vert-path frag-path)
  (define FRAGMENT-SHADER #x8B30)
  (define VERTEX-SHADER #x8B31)
  (let ((v (compile_shader (file->string vert-path) VERTEX-SHADER))
        (f (compile_shader (file->string frag-path) FRAGMENT-SHADER)))
    (link_program v f)))

