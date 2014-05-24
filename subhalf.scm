; Given a linked list 5 -> 4 -> 3 -> 2 -> 1 produce a linked list 4 -> 2 -> 0
; -> 2 -> 1 by subtracting the last node of the list from the first, the
; next-to-last node from the second, and so on, stopping at the midpoint of the
; list.

; MIT Scheme has sublist. PLT has take-right. I can't decide on either, so 
; fuck that shit.
(define (discard-n n li)
  (if (zero? n)
    li
    (discard-n (- n 1) (cdr li))))

(define (discard-half li)
  (discard-n (quotient (length li) 2) li))

(define (sub-half li)
  (define (sub-half-rec half li)
    (if (null? half)
      li
      (cons (- (car li) (car half)) (sub-half-rec (cdr half) (cdr li)))))
  (sub-half-rec (reverse (discard-half li)) li))

(display (sub-half '(5 4 3 2 1)))
