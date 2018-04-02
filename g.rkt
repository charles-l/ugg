#lang racket
(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         (only-in srfi/1 iota))

#;(provide init_screen
         make-shader
         read-mesh
         glUseProgram
         glGetUniformLocation
         main_loop
         clear_frame
         key_down
         draw)

(provide (all-defined-out))

(define-ffi-definer define-g (ffi-lib "./g"))

(define _win-ptr (_cpointer 'SDL_Window))

(define-cstruct _vec3
                ((x _float)
                 (y _float)
                 (z _float)))

(define-cstruct _mat4
                ((elements (_array _float 4 4))))

(define-cstruct _sdl-keysym
                ((scancode _int32)
                 (keycode (_enum '(right = 79 left down up)))
                 (mod _uint16)
                 (_ _uint32)))

(define _event-type (_enum '(keydown = #x300 keyup) _uint32 #:unknown identity))

(define-cstruct _sdl-keyboard-event
                ((type _event-type)
                 (timestamp _uint32)
                 (window-id _uint32)
                 (state _uint8)
                 (repeat _uint8)
                 (padding2 _uint8)
                 (padding3 _uint8)
                 (keysym _sdl-keysym)
                 (padding4 (_array _uint32 6))))

(define-cstruct _sdl-event
                ((type _event-type)
                 (_ (_array _uint32 13))))

(define-g get_win (_fun -> _win-ptr))
(define-g init_screen (_fun _string -> _void))
(define-g main_loop (_fun (_fun -> _void) (_fun _sdl-event -> _void) -> _void))
(define-g gen_vao (_fun -> _uint32))
(define-g clear_frame (_fun _float _float _float -> _void))
(define-g gen_fvbo (_fun _pointer _uint32 _size -> _uint32))
(define-g gen_uvbo (_fun _pointer _uint32 _size -> _uint32))
(define-g draw_array (_fun _uint32 _uint32 _uint32 _size -> _void))
(define-g glBindVertexArray (_fun _uint32 -> _void))
(define-g compile_shader (_fun _string _uint32 -> _uint32))
(define-g link_program (_fun _uint32 _uint32 -> _uint32))
(define-g glUseProgram (_fun _uint -> _void))
(define-g glUniformMatrix4fv (_fun _uint _size _bool _pointer -> _void))
(define-g glGetUniformLocation (_fun _uint _symbol -> _int))
(define-g is_key_down (_fun _int -> _uint8))
(define-g line_draw_mode (_fun -> _void))
(define-g fill_draw_mode (_fun -> _void))
(define-g calculate_mvp (_fun _vec3 -> _mat4))

(struct vao (id array-id element-array-id))
(struct mesh (verts faces vao) #:transparent)
(struct shader (id fields))

(define (key-down? key)
  (case key
    ((right) (not (zero? (is_key_down 79))))
    ((left) (not (zero? (is_key_down 80))))
    ((down) (not (zero? (is_key_down 81))))
    ((up) (not (zero? (is_key_down 82))))))

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


(define (make-shader vert-path frag-path fields)
  (define FRAGMENT-SHADER #x8B30)
  (define VERTEX-SHADER #x8B31)
  (let ((v (compile_shader (file->string vert-path) VERTEX-SHADER))
        (f (compile_shader (file->string frag-path) FRAGMENT-SHADER)))
    (shader (link_program v f) fields)))

(define (with-shader shader fields thunk)
  (glUseProgram (shader-id shader))
  (for ((f (shader-fields shader)))
    (cond
      ((assoc (car f) fields) => (λ (p)
                                    (let ((id (glGetUniformLocation (shader-id shader) (car f)))
                                          (type (cdr f))
                                          (val (cdr p)))
                                      (match type
                                        ('mat4 (glUniformMatrix4fv id 1 #f
                                                                   (array-ptr (mat4-elements val))))))))
      (else
        (error "required field not included" f))))
  (thunk)
  (glUseProgram 0))

