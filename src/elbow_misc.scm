(define-library (elbow misc)
   (import (scheme base)
           (scheme write)
           (scheme case-lambda)
           (srfi 42))
   (export elbow-misc/for-each-with-index elbow-misc/print-info
           elbow-misc/assq-with-default elbow-misc/assoc-with-default
           elbow-misc/map-with-index)
   (begin
     (define (elbow-misc/print-info info)
       (display info)(newline))

     (define (elbow-misc/assq-with-default key alist default)
       (cond
         ((assq key alist) => cadr)
         (else default)))

     (define elbow-misc/assoc-with-default
       (case-lambda
         ((key alist default)
          (elbow-misc/assoc-with-default key alist default equal? cadr))
         ((key alist default comparator)
          (elbow-misc/assoc-with-default key alist default comparator cadr))
         ((key alist default comparator value-accessor)
          (cond
            ((assoc key alist comparator) => value-accessor)
            (else default)))))

    (define (elbow-misc/map-with-index fn . ls)
         (apply map
                `(,fn ,(list-ec (: i 0 (apply min (map length ls))) i)
                      . ,ls)))

     (define (elbow-misc/for-each-with-index fn . ls)
         (apply for-each
                `(,fn ,(list-ec (: i 0 (apply min (map length ls))) i)
                      . ,ls)))))
