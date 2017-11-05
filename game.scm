(module game *
        (import scheme chicken)
        (use soil (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-utils gl-math)

        (define *camera-pos* (make-point 1 0 2))

        (define *vertex*
#<<END
#version 400
in vec2 position;
in vec3 color;
in vec2 uv;
out vec2 tex_coord;
out vec3 c;
uniform mat4 MVP;

void main(){
gl_Position = MVP * vec4(position, 0.0, 1.0);
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
fragColor = texture (tex, tex_coord) * texture (tex2, tex_coord) * vec4(c, 1.0);
}
END
          )

        (define rect (make-mesh
                       vertices: '(attributes: ((position #:float 2)
                                                (color #:unsigned-byte 3 normalized: #t)
                                                (uv #:float 2))
                                               initial-elements: ((position . (-1 -1
                                                                                  1 -1
                                                                                  1  1
                                                                                  -1  1))
                                                                  (color . (255 0   0
                                                                            0   255 0
                                                                            0   0   255
                                                                            255 0   255))
                                                                  (uv . (-1 -1
                                                                         1 -1
                                                                         1 1
                                                                         -1 1))))
                       indices: '(type: #:ushort
                                        initial-elements: (0 1 2
                                                           0 2 3))))

        (define (test-fun)
          (print 'blah))

        (define projection-matrix
          (perspective 640 480 0.1 100 70))

        (define tex
          (load-ogl-texture "test.png" force-channels/auto texture-id/create-new-id texture/repeats))

        (define tex2
          (load-ogl-texture "test2.png" force-channels/auto texture-id/create-new-id texture/repeats))

        (define (cam-look-at t)
          (look-at *camera-pos*
                   t
                   (make-point 0 1 0)))

        (define model-matrix (mat4-identity))

        (define (compile-pipeline)
          (set! *vertex* (make-shader gl:+vertex-shader+ *vertex*))
          (set! *fragment* (make-shader gl:+fragment-shader+ *fragment*))
          (define prog (make-program (list *vertex* *fragment*)))

          (mesh-make-vao! rect
                          `((position . ,(gl:get-attrib-location prog "position"))
                            (color . ,(gl:get-attrib-location prog "color"))
                            (uv . ,(gl:get-attrib-location prog "uv"))))
          prog)

        (define (render prog)
          (point-x-set! *camera-pos* (* 5 (sin (glfw:get-time))))
          (gl:use-program prog)
          (gl:uniform-matrix4fv (gl:get-uniform-location prog "MVP")
                                1 #f
                                (m* projection-matrix
                                    (m* (cam-look-at (make-point 0 0 0)) model-matrix)))

          (gl:active-texture gl:+texture0+)
          (gl:bind-texture gl:+texture-2d+ tex)
          (gl:active-texture gl:+texture0+)
          (gl:bind-texture gl:+texture-2d+ tex)

          (gl:uniform1i (gl:get-uniform-location prog "tex") 0)
          (gl:uniform1i (gl:get-uniform-location prog "tex2") 1)

          (gl:bind-vertex-array (mesh-vao rect))

          (gl:draw-elements-base-vertex (mode->gl (mesh-mode rect))
                                        (mesh-n-indices rect)
                                        (type->gl (mesh-index-type rect))
                                        #f 0)

          (check-error)
          (gl:bind-vertex-array 0)))
