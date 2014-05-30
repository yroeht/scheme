(use format)
(use srfi-1)

(define N 8)

; The knight's tour: a classical chess problem, finding a hamiltonian path in a
; graph.
;
; Sample solution for a typical 8x8 chessboard:
;
;   4   1   6  21  42  27  16  19
;   7  22   3  26  17  20  41  28
;   2   5  24  59  40  43  18  15
;  23   8  63  50  25  52  29  44
;  64  49  60  39  58  45  14  33
;   9  38  57  62  51  32  53  30
;  48  61  36  11  46  55  34  13
;  37  10  47  56  35  12  31  54

; The relative moves a knight may make.
(define knight-movement
  '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1)))

; A list of all board coordinates, starting from '(1 . 1).
(define board
  (apply append
         (map
           (lambda (y)
             (map
               (lambda (x)
                 (cons x y))
               (iota N 1)))
           (iota N 1))))

; Return all legal successors of a position.
(define (knight-next position)
  (filter
    (lambda (candidate)
      (and (> (car candidate) 0)
           (> (cdr candidate) 0)
           (<= (car candidate) N)
           (<= (cdr candidate) N)))
    (map
      (lambda (move)
        (cons (+ (car move) (car position))
              (+ (cadr move) (cdr position))))
      knight-movement)))

; A graph that associates each position on the board to its successors.
(define (knight-graph)
  (map
    (lambda (position)
      (list position (knight-next position)))
    board))

; The outdegree of a position is the number of legal moves from that position.
(define (outdegree graph position)
  (length (cadr (assoc position graph))))

; Warnsdorff's rule is an heuristic that selects the vertice with the smallest
; outdegree
(define (warnsdorff moves graph)
  (if (null? (cdr moves))
    (car moves)
    (let ((current-best (warnsdorff (cdr moves) graph)))
      (if (> (outdegree graph current-best)
             (outdegree graph (car moves)))
        (car moves)
        current-best))))

; The knight's tour: return a path which visits each square exactly once.
(define (tour start)
  (let loop ((pos start)
             (graph (knight-graph))
             (path '()))
      (let ((adjacent (cadr (assoc pos graph))))
        ; Remove current node from (the neighboring) adjacency lists
        (for-each
          (lambda (p)
            (set-cdr!
              (assoc p graph)
              (list (filter
                      (lambda (m) (not (equal? pos m)))
                      (cadr (assoc p graph ))))))
          adjacent)
        ; Remove the vertice from the graph
        (set! graph (filter (lambda (p) (not (equal? (car p) pos))) graph))
        (if (null? graph)
          (cons pos path)
          (loop (warnsdorff adjacent graph) graph (cons pos path))))))

; Pretty-print a path.
(define (path-dump path)
  (for-each
    (lambda (square)
      (let ((order
              (list-index
                (lambda (pos)
                  (equal? pos square))
                path)))
        (cond ((equal?  (car square) 1)
               (newline)))
        (format #t "~4@A" (if (not order) "x" (- (* N N) order)))))
    board))

(path-dump (tour '(2 . 1)))
