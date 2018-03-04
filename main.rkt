#lang racket
(require reloadable)
(require "g.rkt")

(init_screen "game")

(define run (reloadable-entry-point->procedure
              (make-reloadable-entry-point 'run "game.rkt")))

(reload!)

(main_loop run)

