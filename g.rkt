#lang racket
(require
  "math.rkt"
  racket/contract
  (rename-in ffi/unsafe (-> c->))
  ffi/unsafe/define
  ffi/vector)

(provide (all-defined-out))

(define-ffi-definer define-g (ffi-lib "./g"))

(define _win-ptr (_cpointer 'SDL_Window))

(define _sdl-scancode (_enum '(
                               a = 4 b c d e f g h i j k l m n o p q r s t u v w x y z
                               right = 79 left down up)
                             _uint32 #:unknown identity))

(define _sdl-keycode (_enum '(right = 79 left down up
                                    a = 97 b c d e f g h i j k l m n o p q r s t u v w x y z)
                            _uint32 #:unknown identity))

(define-cstruct _sdl-keysym
                ((scancode _int32)
                 (keycode _sdl-keycode)
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
(define-g main_loop (_fun (_fun _float c-> _void) (_fun _sdl-event _float c-> _void) c-> _void))
(define-g gen_vao (_fun c-> _uint32))
(define-g clear_frame (_fun _float _float _float c-> _void))
(define-g gen_vert_vbo (_fun _pointer _size c-> _uint32))
(define-g gen_uv_vbo (_fun _pointer _size c-> _uint32))
(define-g gen_element_vbo (_fun _pointer _size c-> _uint32))
(define-g draw_elements (_fun _uint32 _uint32 _uint32 _int32 _size c-> _void))
(define-g draw_lines (_fun _uint32 _uint32 _size _bool c-> _void))
(define-g compile_shader (_fun _string _uint32 c-> _uint32))
(define-g link_program (_fun _uint32 _uint32 c-> _uint32))
(define-g is_key_down (_fun _sdl-scancode c-> _uint8))
(define-g load_texture (_fun _string c-> _uint32))

(define-g glBindVertexArray (_fun _uint32 c-> _void))
(define-g glUseProgram (_fun _uint c-> _void))
(define-g glUniformMatrix4fv (_fun _uint _size _bool _pointer c-> _void))
(define-g glUniform2f (_fun _int _float _float c-> _void))
(define-g glUniform3f (_fun _int _float _float _float c-> _void))
(define-g glUniform4f (_fun _int _float _float _float _float c-> _void))
(define-g glUniform1i (_fun _int _int c-> _void))
(define-g glGetUniformLocation (_fun _uint _symbol c-> _int))
(define-g glPolygonMode (_fun _uint _uint c-> _void))
(define-g glBufferSubData (_fun _uint _intptr _ptrdiff _pointer c-> _void))

(define +gl-front-and-back+ #x0408)
(define +gl-point+ #x1B00)
(define +gl-line+ #x1B01)
(define +gl-fill+ #x1B02)

(define (warn f msg)
  (fprintf (current-error-port) "warning (~a): ~a\n" f msg))

(define (key-down? key)
  (not (zero? (is_key_down key))))


#;(define (with-shader shader fields thunk #:draw-mode (draw-mode 'fill))
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

(define (%compile-shader vert frag)
  (define FRAGMENT-SHADER #x8B30)
  (define VERTEX-SHADER #x8B31)
  (let* ((v (compile_shader (file->string vert) VERTEX-SHADER))
         (f (compile_shader (file->string frag) FRAGMENT-SHADER))
         (l (link_program v f)))
    (when (eq? l -1)
      (error '%compile-shader "failed to link shader"))
    l))
