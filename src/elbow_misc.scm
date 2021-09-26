(define-library (elbow misc)
   (import (scheme base)
           (srfi 152);(scheme string)
           (scheme write)
           (scheme case-lambda)
           (srfi 42))
   (export elbow-misc/for-each-with-index elbow-misc/print-info
           elbow-misc/assq-with-default elbow-misc/assoc-with-default
           elbow-misc/map-with-index elbow-misc/concat-path
           elbow-misc/dot-file-name?
           elbow-misc/remove-head-slash
           elbow-misc/md-file-name?)
   (begin
     (define (elbow-misc/print-info info)
       (display info)(newline))

     (define (elbow-misc/dot-file-name? file-name)
         (char=? #\. (string-ref file-name 0)))

     (define (elbow-misc/md-file-name? file-name)
       (let ((find-right-index (string-contains-right file-name ".md")))
         (and find-right-index
               (= (string-length file-name)
                  (+ find-right-index 3)))))

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

    (define (elbow-misc/concat-path . path-list)
      (let loop ((input path-list)
                 (res ""))
        (cond
          ((null? input) res)
          ((string=? (car input) "./")
           (loop (cdr input) res))
          ((loop (cdr input)
                 (string-append res "/" (car input)))))))

    (define (elbow-misc/remove-head-slash input)
      (cond
        ((char=? (string-ref input 0) #\/) (substring input 1 (string-length input)))
        ((and (char=? (string-ref input 0) #\.)
              (char=? (string-ref input 1) #\/))
         (substring input 2 (string-length input)))
        (else input)))

    (define (elbow-misc/map-with-index fn . ls)
         (apply map
                `(,fn ,(list-ec (: i 0 (apply min (map length ls))) i)
                      . ,ls)))

     (define (elbow-misc/for-each-with-index fn . ls)
         (apply for-each
                `(,fn ,(list-ec (: i 0 (apply min (map length ls))) i)
                      . ,ls)))))
