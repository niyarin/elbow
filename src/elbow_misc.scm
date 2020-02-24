(define-library (elbow misc)
   (import (scheme base)
           (scheme write)
           (srfi 42))
   (export elbow-misc/for-each-with-index)
   (begin
     (define (elbow-misc/for-each-with-index fn . ls)
         (apply for-each
                `(,fn ,(list-ec (: i 0 (apply min (map length ls))) i)
                      . ,ls)))))
