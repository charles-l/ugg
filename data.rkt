#lang typed/racket

(require/typed "g.rkt"
               (glUseProgram (Positive-Integer -> Void))
               (glGetUniformLocation (Nonnegative-Integer Symbol -> Nonnegative-Integer))
               (glUniformMatrix4fv (Nonnegative-Integer Nonnegative-Integer Boolean CPointer -> Void))
               (glUniform2f (Nonnegative-Integer Real Real -> Void))
               (glUniform3f (Nonnegative-Integer Real Real Real -> Void))
               (glUniform4f (Nonnegative-Integer Real Real Real Real -> Void))
               (glUniform1i (Nonnegative-Integer Nonnegative-Integer -> Void))
               (%compile-shader (String String -> Positive-Integer)))

(require "math.rkt")
(define-type Texture Nonnegative-Integer)
(require (for-syntax racket/syntax))

(provide (all-defined-out))

(define +max-entities+ 128)

(define-syntax (define-component stx)
  (syntax-case stx ()
    ((_ name fields ...)
     (let ((sname
             (string->symbol
               (string-append
                 (symbol->string (syntax->datum #'name))
                 "-c"))))
       (with-syntax ((tname
                       (format-id #'name "~a" sname))
                     (pname
                       (format-id #'name "~a?" (syntax->datum #'name)))
                     (constructor-name
                       (format-id #'name "%~a" sname))
                     (column-name
                       (format-id #'name "*~as*" sname)))
         #`(begin
             (struct tname
               (fields ...)
               #:constructor-name constructor-name)
             (: column-name (Vectorof tname))
             (define column-name #())
             (define-predicate pname tname)
             (define (name fields ...) : tname
               (let ((c (constructor-name #,@(map (compose (lambda (x) (datum->syntax stx x)) car) (syntax->datum #'(fields ...))))))
               (set! column-name (vector-append column-name (vector c)))
               c))))))))

(struct shader
  ((id : Positive-Integer)))

(define-syntax (define-shader stx)
  (syntax-case stx (:)
    ((_ name vert frag (fname : ftype) ...)
     (let ((sname
             (string->symbol
               (string-append
                 (symbol->string (syntax->datum #'name))
                 "-shader")))
           (pick-uniform-loader
             (lambda (ty)
               (case ty
                 ((Mat4) (lambda (id val)
                           (with-syntax ((id id) (val val))
                             #'(glUniformMatrix4fv id 1 #f (mat4->pointer val)))))
                 ((Vec2) (lambda (id val)
                           (with-syntax ((id id) (val val))
                             #'(glUniform2f id (vec2-x val) (vec2-y val)))))
                 ((Vec3) (lambda (id val)
                           (with-syntax ((id id) (val val))
                             #'(glUniform3f id (vec3-x val) (vec3-y val) (vec3-z val)))))
                 ((Vec4) (lambda (id val)
                           (with-syntax ((id id) (val val))
                             #'(glUniform4f id (vec4-x val) (vec4-y val) (vec4-z val) (vec4-w val)))))
                 ((Texture) (lambda (id val)
                              (with-syntax ((id id) (val val))
                                #'(glUniform1i id val))))
                 (else
                   (error "unsupported uniform type" ty))))))
       (with-syntax ((tname
                       (format-id #'name "~a" sname))
                     (lname
                       (format-id #'name "use-~a!" sname)))
         #`(begin
             (define tname (shader (%compile-shader vert frag)))
             (define (lname (fname : ftype) ...)
               (define id (shader-id tname))
               (glUseProgram id)
               #,@(for/list ((n (syntax->list #'(fname ...)))
                             (t (syntax->list #'(ftype ...))))
                    (with-syntax ((nn n))
                      (let ((u-id #'(glGetUniformLocation id (quote nn)))
                            (f (pick-uniform-loader (syntax->datum t))))
                        (f u-id n))))
               )))))))

(define-syntax (define-entity stx)
  (syntax-case stx ()
    ((_ name (cname cval) ...)
     (with-syntax
       ((c (datum->syntax stx (apply append (syntax->datum #'(((quote cname) cval) ...))))))
       #'(begin
           (define name
             (entity (%gen-id)
                     (hash )))
           (set-box! *entities* (vector-append (unbox *entities*) (vector name))))))))


(define-component transform
                  (position : Vec3))

(define-component mesh
                  (draw-type : (U 'line 'fill))
                  (vao-id : Positive-Integer)
                  (vert-vbo-id : Positive-Integer)
                  (element-vbo-id : (Option Positive-Integer))
                  (uv-vbo-id : (Option Positive-Integer))
                  (verts-n : Positive-Integer)
                  (faces-n : Nonnegative-Integer))

(define-type Component (U transform-c mesh-c))

(struct entity
  ((id : Nonnegative-Integer)
   (components : (Immutable-HashTable Symbol Component))))

(: *entities* (Boxof (Vectorof entity)))
(define *entities* (box #()))

(: %gen-id (-> Nonnegative-Integer))
(define (%gen-id)
  (vector-length (unbox *entities*)))

