#lang racket
(require "g.rkt")

(init_screen "game")

(define shid (make-shader "vert.glsl" "frag.glsl"))

(define m (read-mesh "/tmp/x.scm"))

(main_loop (lambda ()
             (clear_frame)
             (inject_mvp shid)
             (draw m)))
