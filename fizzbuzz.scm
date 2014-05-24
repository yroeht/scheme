(define (fizzbuzz)
  (do ((i 1 (+ i 1)))
    ((> i 100))
    (begin
      (display (cond ((zero? (modulo i 15)) "Fizz Buzz")
                     ((zero? (modulo i  5)) "Buzz")
                     ((zero? (modulo i  3)) "Fizz")
                     (else                   i)))
      (display ","))))

(fizzbuzz)
