(load  "graph.scm")
(use srfi-1)

(define N 100)

(define knight-movement
  '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1)))

(define knight-graph
  (map
    (lambda (pos)
      (list
        pos
        (filter
          (lambda (candidate)
            (and (> (car candidate) 0)
                 (> (cdr candidate) 0)
                 (<= (car candidate) N)
                 (<= (cdr candidate) N)))
          (map
            (lambda (move)
              (cons (+ (car move) (car pos))
                    (+ (cadr move) (cdr pos))))
            knight-movement))))
    (apply append
           (map
             (lambda (y)
               (map
                 (lambda (x)
                   (cons x y))
                 (iota N 1)))
             (iota N 1)))))

;(dump-graph knight-graph)

(display (reverse (bfs knight-graph '(1 . 1) '(100 . 99))))
