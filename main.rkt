#lang racket
(require "g.rkt")
(require racket/include)

(init_screen "game")
(include "game.rkt")
(main_loop run handler)

