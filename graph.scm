(load "queue.scm")

; http://www.cs.rpi.edu/~goldsd/images/graph02.png
(define graph-b '((A (C))
                  (B (A))
                  (C (B D))
                  (D (E F G))
                  (E (F))
                  (F ())
                  (G ())
                  (H (A G))))

(define (dfs graph)
  (let ((visited '()))
    (define (dfs-rec v)
        ; mark the current vertice as visited
        (set! visited (cons v visited))
        ; recursive descent on its neighbors
        (for-each
          (lambda (adj)
            (cond ((member adj visited)) ((dfs-rec adj))))
          (cadr (assoc v graph))))
    ; apply the DFS to other possible entry points.
    (for-each
      (lambda (n)
        (let ((vertice (car n)))
          (cond ((member vertice visited))((dfs-rec vertice)))))
      graph)
    ; return the order in which the nodes were visited
    visited))

;(display (reverse (dfs graph-b)))

(define (bfs graph start goal)
  (let ((visited (list (cons start '())))
        (q (make-queue)))
    (let loop  ((node start)
                (from '()))
      (for-each
        (lambda (adj)
          (cond ((not (assoc adj visited))
                 ; mark current vertice as visited. Associate it with its
                 ; parent, which was passed as an argument.
                 (set! visited (cons (cons adj node) visited))
                 ; enqueue adjacent vertices. Associate them with the current
                 ; node, their predecessor, for future reference.
                 (q 'push (cons adj node)))))
        (cadr (assoc node graph)))
      ; is the search over, or shall we iterate again?
      (cond
        ; the target as found. Backtrack using the predecessors associations.
        ((equal? node goal)
         (let backtrack ((node (car visited)))
           (cons (car node)
                 (if (null? (cdr node))
                   '()
                   (backtrack (assoc (cdr node) visited))))))
        ((q 'empty) '())
        ; this variable's car is the candidate, its cdr is its predecessor.
        ((define next-and-from (q 'pop))
         (loop (car next-and-from)
               (cdr next-and-from)))))))

(define (dump-graph graph)
  (for-each
    (lambda (p)
      (display (car p))
      (display " ---> ")
      (display (cdr p))
      (newline))
    graph))

;(display (reverse (bfs graph-b 'H 'E)))
