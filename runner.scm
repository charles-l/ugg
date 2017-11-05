(use (prefix glfw3 glfw:) (prefix opengl-glew gl:)
     gl-math
     gl-utils
     forcible
     debug
     nrepl)

;; main mutex for repl
(define with-main-mutex
  (let ((main-mutex (make-mutex)))
    (lambda (proc)
      (dynamic-wind (lambda () (mutex-lock! main-mutex))
                    proc
                    (lambda () (mutex-unlock! main-mutex))))))



(load "game.scm")
(import game)

(define *program* #f)

(define (r)
  (load "game.scm")
  (set! *program* (compile-pipeline))
  'ok)

(thread-start!
  (lambda ()
    (nrepl 1234
           (lambda (i o)
             (thread-start!
               (lambda ()
                 (nrepl-loop
                   i o (lambda (x) (with-main-mutex (lambda () (eval x)))))))))))

(glfw:with-window (640 480 "TEST" resizable: #f
                   context-version-major: 4
                   context-version-minor: 0)
                  (gl:init)

                  (set! *program* (compile-pipeline))
                  (let loop ()
                    (with-main-mutex
                      (lambda ()
                        (glfw:swap-buffers (glfw:window))
                        (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
                        (render *program*)
                        (glfw:poll-events))) ; Because of the context version, initializing GLEW results in a harmless invalid enum
                    (loop)))
