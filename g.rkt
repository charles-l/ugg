#lang racket
(require
  racket/contract
  (rename-in ffi/unsafe (-> c->))
  ffi/unsafe/define
  ffi/vector
  (only-in srfi/1 iota)
  (only-in rnrs/base-6 mod))

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

(define-cstruct _vec4
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

(define _event-type (_enum '(keydown = #x300
                             keyup
                             mouse-motion = #x400
                             mouse-button-down
                             mouse-button-up
                             mouse-wheel)
                           _uint32 #:unknown identity))

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

(define-cstruct _sdl-mouse-motion-event
                ((type _event-type)
                 (timestamp _uint32)
                 (window-id _uint32)
                 (which _uint32)
                 (state _uint32)
                 (x _int32)
                 (y _int32)
                 (xrel _int32)
                 (yrel _int32)))

(define _sdl-event (_union
                     _event-type
                     _sdl-keyboard-event
                     _sdl-mouse-motion-event
                     (_array _uint8 56) ; force size
                     ))

(define-g get_win (_fun c-> _win-ptr))
(define-g init_screen (_fun _string c-> _void))
(define-g main_loop (_fun (_fun c-> _void) (_fun _sdl-event c-> _void) c-> _void))
(define-g gen_vao (_fun c-> _uint32))
(define-g clear_frame (_fun _float _float _float c-> _void))
(define-g gen_fvbo (_fun _pointer _uint32 _size c-> _uint32))
(define-g gen_uvbo (_fun _pointer _uint32 _size c-> _uint32))
(define-g draw_elements (_fun _uint32 _uint32 _uint32 _size c-> _void))
(define-g draw_lines (_fun _uint32 _uint32 _size _bool c-> _void))
(define-g compile_shader (_fun _string _uint32 c-> _uint32))
(define-g link_program (_fun _uint32 _uint32 c-> _uint32))
(define-g is_key_down (_fun _int c-> _uint8))

(define-g mat4_relative_move (_fun _mat4 _vec3 _float _float c-> _vec3))
(define-g calculate_view (_fun _vec3 _float _float c-> _mat4))
(define-g calculate_mvp (_fun _mat4 c-> _mat4))

(define-g glBindVertexArray (_fun _uint32 c-> _void))
(define-g glUseProgram (_fun _uint c-> _void))
(define-g glUniformMatrix4fv (_fun _uint _size _bool _pointer c-> _void))
(define-g glUniform3f (_fun _int _float _float _float c-> _void))
(define-g glGetUniformLocation (_fun _uint _symbol c-> _int))
(define-g glPolygonMode (_fun _uint _uint c-> _void))
(define-g glBufferSubData (_fun _uint _intptr _ptrdiff _pointer c-> _void))

(define +gl-front-and-back+ #x0408)
(define +gl-point+ #x1B00)
(define +gl-line+ #x1B01)
(define +gl-fill+ #x1B02)

(define (warn f msg)
  (fprintf (current-error-port) "warning (~a): ~a\n" f msg))

(struct vao (id array-id element-array-id))

(struct mesh ((verts #:mutable) faces vao) #:transparent)

(struct shader (id fields))

(define (key-down? key)
  (case key
    ((right) (not (zero? (is_key_down 79))))
    ((left) (not (zero? (is_key_down 80))))
    ((down) (not (zero? (is_key_down 81))))
    ((up) (not (zero? (is_key_down 82))))))

; XXX currently no support for oversizing elements buffer - only for extra verts atm
(define (%gen-mesh-vao flat-vertex-coords flat-face-indices #:override-verts-n (override-verts-n #f))
  (define +gl-array-buffer+ #x8892)
  (define +gl-element-array-buffer+ #x8893)
  (let* ((n (gen_vao))
         (r (gen_fvbo (f32vector->cpointer flat-vertex-coords)
                      +gl-array-buffer+
                      (if override-verts-n
                        override-verts-n
                        (f32vector-length flat-vertex-coords))))
         (f (if flat-face-indices
              (gen_uvbo (u32vector->cpointer flat-face-indices)
                        +gl-element-array-buffer+
                        (u32vector-length flat-face-indices))
              #f)))
    (glBindVertexArray 0)
    (vao n r f)))

; FIXME add bounds check
(define (%append-verts! m verts)
  (let ((n (length (mesh-verts m))))
    (glBufferSubData
      (vao-id (mesh-vao m))
      n
      (length verts)
      (f32vector->cpointer (list->f32vector (flatten verts))))
    (set-mesh-verts! m (append (mesh-verts m) verts))))


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

(define/contract (make-line #:buffer-n (buffer-n #f) a b . rest)
  (->* (vec3? vec3?) (#:buffer-n positive-integer?) #:rest (listof vec3?) mesh?)
  (define verts (append
                  (list (vec3->list a) (vec3->list b))
                  (map vec3->list rest)))
  (define p (%gen-mesh-vao (list->f32vector (flatten verts))
                           #:override-verts-n (if buffer-n buffer-n #f)
                           #f))
  (mesh verts #f p))

(define (mesh-with-faces? m) (not (null? (mesh-faces m))))

(define/contract (draw m)
  (-> mesh-with-faces? void?)
  (draw_elements (vao-id (mesh-vao m))
                 (vao-array-id (mesh-vao m))
                 (vao-element-array-id (mesh-vao m))
                 (* 3 (length (mesh-faces m)))))

(define (draw-lines m #:connected? (connected? #t))
  (let ((n (length (mesh-verts m))))
    (when (and (not connected?) (not (zero? (mod n 2))))
      (warn 'draw-lines "extra point in line mesh will not be drawn - maybe you meant to draw connected lines with #:connected? #t"))
    (draw_lines (vao-id (mesh-vao m))
                (vao-array-id (mesh-vao m))
                n
                connected?)))


(define (make-shader vert-path frag-path fields)
  (define FRAGMENT-SHADER #x8B30)
  (define VERTEX-SHADER #x8B31)
  (let ((v (compile_shader (file->string vert-path) VERTEX-SHADER))
        (f (compile_shader (file->string frag-path) FRAGMENT-SHADER)))
    (shader (link_program v f) fields)))

(define (with-shader shader fields thunk #:draw-mode (draw-mode 'fill))
  (glUseProgram (shader-id shader))
  (case draw-mode
    ((fill) (glPolygonMode +gl-front-and-back+ +gl-fill+))
    ((line) (glPolygonMode +gl-front-and-back+ +gl-line+))
    ((point) (glPolygonMode +gl-front-and-back+ +gl-point+)))
  (for ((f (shader-fields shader)))
    (cond
      ((assoc (car f) fields) => (λ (p)
                                    (let ((id (glGetUniformLocation (shader-id shader) (car f)))
                                          (type (cdr f))
                                          (val (cdr p)))
                                      (match type
                                        ('mat4 (glUniformMatrix4fv id 1 #f
                                                                   (array-ptr (mat4-elements val))))
                                        ('vec3 (glUniform3f id (vec3-x val) (vec3-y val) (vec3-z val)))))))
      (else
        (error "required field not included" f))))
  (thunk)
  (glUseProgram 0))

