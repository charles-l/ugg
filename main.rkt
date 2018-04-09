#lang racket
(require reloadable)
(require "g.rkt")

(init_screen "game")

(define run (reloadable-entry-point->procedure
              (make-reloadable-entry-point 'run "game.rkt")))


(define handler (reloadable-entry-point->procedure
                  (make-reloadable-entry-point 'handler "game.rkt")))

(reload!)

(main_loop run handler)

