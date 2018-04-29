#lang racket
(provide (all-defined-out))
(require ffi/vector)
(require "data.rkt")
(require "math.rkt")
(require "g.rkt")
(require (only-in rnrs/base-6 mod))

; XXX currently no support for oversizing elements buffer - only for extra verts atm
(define (%gen-mesh draw-type flat-vertex-coords (flat-face-indices #f) (flat-uv-coords #f) #:override-verts-n (override-verts-n #f))
  (let* ((vao-id (gen_vao))
         (vert-vbo-id (gen_vert_vbo (f32vector->cpointer flat-vertex-coords)
                                    (if override-verts-n
                                      override-verts-n
                                      (f32vector-length flat-vertex-coords))))
         (element-vbo-id (if flat-face-indices
                           (gen_element_vbo (u32vector->cpointer flat-face-indices)
                                            (u32vector-length flat-face-indices))
                           #f))
         (uv-vbo-id (if flat-uv-coords
                      (gen_uv_vbo (f32vector->cpointer flat-uv-coords)
                                  (f32vector-length flat-uv-coords))
                      #f)))
    (glBindVertexArray 0)
    (mesh
      draw-type
      vao-id vert-vbo-id element-vbo-id uv-vbo-id
      (/ (f32vector-length flat-vertex-coords) 3)
      (if flat-face-indices
        (/ (u32vector-length flat-face-indices) 3)
        0))))

; FIXME add bounds check
#;(define (%append-verts! m verts)
  (let ((n (mesh-verts-n m)))
    (glBufferSubData
      (vao-id (mesh-vao m))
      n
      verts
      (f32vector->cpointer (list->f32vector (flatten verts))))
    (set-mesh-verts! m (append (mesh-verts m) verts))))


(define (read-mesh f)
  ; XXX assumes only one object
  ;
  ; Could get a speedup if using vectors rather than lists if needed?
  (let* ((o (cdr (with-input-from-file f (thunk (read)))))
         (verts (cadr (assoc 'vertices o)))
         (faces (cadr (assoc 'faces o)))
         (uvs (cadr (assoc 'uvs o))))
    (%gen-mesh
      'fill
      (list->f32vector (flatten verts))
      (list->u32vector (flatten faces))
      (list->f32vector (flatten uvs)))))

(define (make-plane side-length)
  (define s (exact->inexact side-length))
  (define verts
    `((,(- s) 0.0 ,(+ s))
      (,(- s) 0.0 ,(- s))
      (,(+ s) 0.0 ,(- s))
      (,(+ s) 0.0 ,(+ s))))
  (define faces
    `((0 1 2)
      (0 2 3)))
  (define uvs
    '((0.0 1.0)
      (0.0 0.0)
      (1.0 0.0)
      (1.0 1.0)))
  (%gen-mesh
    'fill
    (list->f32vector (flatten verts))
    (list->u32vector (flatten faces))
    (list->f32vector (flatten uvs))))

(define/contract (make-line #:buffer-n (buffer-n #f) a b . rest)
                 (->* (vec3? vec3?) (#:buffer-n positive-integer?) #:rest (listof vec3?) mesh?)
                 (define verts (append
                                 (list (vec3->list a) (vec3->list b))
                                 (map vec3->list rest)))
                 (%gen-mesh
                   'line
                   (list->f32vector (flatten verts))
                   #:override-verts-n (if buffer-n buffer-n #f)
                   #f))


(define (fill-mesh? m) (eq? 'fill (mesh-c-draw-type m)))
(define (draw m)
  (match m
    ((? fill-mesh?) (draw_elements
                      (mesh-c-vao-id m)
                      (mesh-c-vert-vbo-id m)
                      (mesh-c-element-vbo-id m)
                      (or (mesh-c-uv-vbo-id m) -1)
                      (* 3 (mesh-c-faces-n m))))
    (else
      (let ((n (mesh-c-verts-n m)))
        (define connected? #f)
        (when (and (not connected?) (not (zero? (mod n 2))))
          (warn 'draw-lines "extra point in line mesh will not be drawn - maybe you meant to draw connected lines with #:connected? #t"))
        (draw_lines (mesh-c-vao-id m)
                    (mesh-c-vert-vbo-id m)
                    n
                    connected?)))))


