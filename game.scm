(declare (unit game))
(module game *
        (import scheme chicken)
        (use soil (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-utils gl-math)

        (define *camera-pos* (make-point 1 0 2))

        (define *vertex*
#<<END
#version 400
in vec3 position;
in vec3 color;
in vec2 uv;
out vec2 tex_coord;
out vec3 c;
uniform mat4 MVP;

void main(){
gl_Position = MVP * vec4(position, 1.0);
tex_coord = uv;
c = color;
}
END
          )

        (define *fragment*
#<<END
#version 400
in vec2 tex_coord;
in vec3 c;
out vec4 fragColor;
uniform sampler2D tex;
uniform sampler2D tex2;
void main(){
fragColor = texture (tex, tex_coord) * texture (tex2, tex_coord) + 0.5 * vec4(c, 1.0);
}
END
          )

        (define rect (load-ply-mesh "test.ply" vertex: '((position x y z) (color nx ny nz) (uv s t))
                                    face: 'vertex_indices))

        (define texs '())

        (define projection-matrix
          (perspective 640 480 0.1 100 70))


        (define (cam-look-at t)
          (look-at *camera-pos*
                   t
                   (make-point 0 1 0)))

        (define model-matrix (mat4-identity))

        (define (compile-pipeline)
          (set! *vertex* (make-shader gl:+vertex-shader+ *vertex*))
          (set! *fragment* (make-shader gl:+fragment-shader+ *fragment*))
          (define prog (make-program (list *vertex* *fragment*)))

          (set! texs (cons (load-ogl-texture "test.png" force-channels/auto texture-id/create-new-id texture/repeats) texs))
          (print (last-result))
          (set! texs (cons (load-ogl-texture "test2.png" force-channels/auto texture-id/create-new-id texture/repeats) texs))
          (print (last-result))

          (mesh-make-vao! rect
                          `((position . ,(gl:get-attrib-location prog "position"))
                            (color . ,(gl:get-attrib-location prog "color"))
                            (uv . ,(gl:get-attrib-location prog "uv"))))
          prog)

        (define (render prog)
          (gl:enable gl:+depth-test+)
          (point-x-set! *camera-pos* (* 5 (sin (glfw:get-time))))
          (point-y-set! *camera-pos* (* 5 (sin (/ (glfw:get-time) 2))))
          (gl:use-program prog)
          (gl:uniform-matrix4fv (gl:get-uniform-location prog "MVP")
                                1 #f
                                (m* projection-matrix
                                    (m* (cam-look-at (make-point 0 0 0)) model-matrix)))

          (gl:active-texture gl:+texture0+)
          (gl:bind-texture gl:+texture-2d+ (car texs))
          (gl:active-texture gl:+texture1+)
          (gl:bind-texture gl:+texture-2d+ (cadr texs))

          (gl:uniform1i (gl:get-uniform-location prog "tex") 0)
          (gl:uniform1i (gl:get-uniform-location prog "tex2") 1)

          (gl:bind-vertex-array (mesh-vao rect))

          (gl:draw-elements-base-vertex (mode->gl (mesh-mode rect))
                                        (mesh-n-indices rect)
                                        (type->gl (mesh-index-type rect))
                                        #f 0)

          (check-error)
          (gl:bind-vertex-array 0)))
