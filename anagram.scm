; Necessary for string-downcase
(use srfi-13)
; Necessary for sort
(use data-structures)

(define (is-anagram word1 word2)
  (apply string=?
         (map (lambda (w)
                (list->string
                  (sort (string->list (string-delete #\ (string-downcase w)))
                        char<?)))
              (list word1 word2))))

(display (is-anagram "foo" "bar"))
(display (is-anagram "foo" "foo"))
(display (is-anagram "LISP" "lisp"))
(display (is-anagram "Desperation" "A rope ends it"))
