#lang racket
(require reloadable)
(require "g.rkt")

(require racket/tcp)
(define (serve port)
  (define listener (tcp-listen port 5 #t))
  (let l ()
    (accept-and-handle listener)
    (l)))

(define-namespace-anchor main-anchor)

(define (accept-and-handle l)
  (define-values (in out) (tcp-accept l))
  (parameterize ((current-namespace (namespace-anchor->namespace main-anchor))
                 (current-input-port in)
                 (current-error-port out)
                 (current-output-port out))
    (read-eval-print-loop))
  (close-input-port in)
  (close-output-port out))

(thread (thunk (serve 8080)))

(init_screen "game")

(define run (reloadable-entry-point->procedure
              (make-reloadable-entry-point 'run "game.rkt")))


(define handler (reloadable-entry-point->procedure
                  (make-reloadable-entry-point 'handler "game.rkt")))

(reload!)
(main_loop run handler)
