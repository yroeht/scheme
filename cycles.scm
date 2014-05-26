; Floyd's cycle detection algorithm.
(define (tortoise-and-hare f tortoise hare)
  (if (null? hare)
    '()
    (let ((half-step (f hare)))
      (cond ((or (eq? tortoise hare) (eq? tortoise half-step)) tortoise)
            ((tortoise-and-hare f (f tortoise) (f half-step)))))))

(define (run-floyd function start)
  (let ((init (function start)))
    (tortoise-and-hare function init (function init))))

(define (is-cyclic function start)
  (not (null? (run-floyd function start))))

; This function should return the cycle, starting from its entry point.
; An algorithm from this would be to first find that actual entry point (the
; one we currently have from tortoise-and-hare is a random point of the cycle,
; not its entrance), then do what follows. Finding this point requires
; "rewinding" the tortoise, slowing down the hare, and waiting for the two to
; meet again.
(define (find-cycle function start)
  (define tort (run-floyd function start))
  (let loop ((acc '())
             (current tort))
    (cond ((and (not (null? acc)) (eq? current tort)) acc)
          ((let ((next (function current)))
             (loop (cons next acc) next))))))

; Some tests.
(define (cyclic-function-1 x)
  (define li '((1 (2))
              (2 (3))
              (3 (4))
              (4 (1))))
  (caadr (assoc x li)))

(define (cyclic-function-2 x)
  (define li '((1 (2))
              (2 (3))
              (3 (4))
              (4 (5))
              (5 (6))
              (6 (2))))
  (caadr (assoc x li)))

(define (acyclic-function x)
  (cond ((< x 10) (+ 1 x)) ('())))

; Testing.
(display (is-cyclic acyclic-function 1))  ;#f
(display (is-cyclic cyclic-function-1 1)) ;#t
(display (is-cyclic cyclic-function-2 1)) ;#t
(newline)
(display (find-cycle cyclic-function-1 2));(1 4 3 2)
(display (find-cycle cyclic-function-2 2));(6 5 4 3 2)
