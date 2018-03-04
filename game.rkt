#lang racket

(require reloadable)
(require "g.rkt")

(provide run)

(define shid (make-shader "vert.glsl" "frag.glsl"))

(define m (read-mesh "/tmp/x.scm"))

(define (run)
  (clear_frame 0.1 0.2 0.3)
  (inject_mvp shid)
  (draw m))
