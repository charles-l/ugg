#lang typed/racket

(require/typed ffi/unsafe
               (#:opaque CArray array?))

(require/typed/provide "math-ffi.rkt"
                       (#:opaque Vec2 vec2?)
                       (#:opaque Vec3 vec3?)
                       (#:opaque Vec4 vec4?)
                       (#:opaque Mat4 mat4?)

                       (HMM_MultiplyMat4 (Mat4 Mat4 -> Mat4))
                       (add_mat (Mat4 Mat4 -> Mat4))
                       (make_id_mat (-> Mat4))
                       (make_zero_mat (-> Mat4))
                       (make_translate_matrix (Vec3 -> Mat4))
                       (make_projection (-> Mat4))
                       (calculate_view (Vec3 Real Real -> Mat4))
                       (mat4_relative_move (Mat4 Vec3 Real Real Real -> Vec3))
                       (vec2->list (Vec2 -> (Listof Real)))
                       (vec3->list (Vec3 -> (Listof Real)))
                       (vec4->list (Vec4 -> (Listof Real)))
                       (mat4-elements (Mat4 -> CArray))
                       (make-vec2 (Real Real -> Vec2))
                       (make-vec3 (Real Real Real -> Vec3))
                       (make-vec4 (Real Real Real Real -> Vec4))
                       (vec2-x (Vec2 -> Real))
                       (vec2-y (Vec2 -> Real))
                       (vec3-x (Vec3 -> Real))
                       (vec3-y (Vec3 -> Real))
                       (vec3-z (Vec3 -> Real))
                       (vec4-x (Vec4 -> Real))
                       (vec4-y (Vec4 -> Real))
                       (vec4-z (Vec4 -> Real))
                       (vec4-w (Vec4 -> Real)))

(provide (all-defined-out))

(define v
  (case-lambda
    (((x : Real) (y : Real)) (make-vec2 x y))
    (((x : Real) (y : Real) (z : Real)) (make-vec3 x y z))
    (((x : Real) (y : Real) (z : Real) (w : Real)) (make-vec4 x y z w))))

;;; Matrix math

(define mid (make_id_mat))
(define mzero (make_zero_mat))

(: m* (-> Mat4 * Mat4))
(define (m* . ms) (foldr HMM_MultiplyMat4 mid ms))

(: m+ (-> Mat4 * Mat4))
(define (m+ . ms) (foldr add_mat mzero ms))

(define translate-m
  (case-lambda
    (((x : Real) (y : Real) (z : Real))
     (make_translate_matrix (make-vec3 x y z)))
    ((v : Vec3)
     (make_translate_matrix v))))

;;; Util math functions

(: clamp (-> Real Real Real Real))
(define (clamp a b v)
  (max (min b v) a))

