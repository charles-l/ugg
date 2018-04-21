#lang racket
(require
  ffi/unsafe
  ffi/vector
  ffi/unsafe/define)

(provide (all-defined-out))

(define-ffi-definer define-g (ffi-lib "./g"))

(define-cstruct _vec2
                ((x _float)
                 (y _float)))

(define-cstruct _vec3
                ((x _float)
                 (y _float)
                 (z _float)))

(define-cstruct _vec4
                ((x _float)
                 (y _float)
                 (z _float)
                 (w _float)))

(define-cstruct _mat4
                ((elements (_array _float 4 4))))

(define-g mat4_relative_move (_fun _mat4 _vec3 _float _float _float -> _vec3))
(define-g calculate_view (_fun _vec3 _float _float -> _mat4))
(define-g make_translate_matrix (_fun _vec3 -> _mat4))
(define-g make_projection (_fun -> _mat4))
(define-g make_id_mat (_fun -> _mat4))
(define-g make_zero_mat (_fun -> _mat4))
(define-g add_mat (_fun _mat4 _mat4 -> _mat4))
(define-g HMM_MultiplyMat4 (_fun _mat4 _mat4 -> _mat4))
