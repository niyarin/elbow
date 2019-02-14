
;TODO
;support iso8601 format

(define-library (elbow utils date-tree)
   (import (scheme base)
           (scheme cxr)
           (scheme write)
           (srfi 152))

   (export elbow-date-tree-decompose-hyphen-date-string
           elbow-date-tree-date-list? 
           elbow-date-tree-less?
           )

   (begin

     (define (elbow-date-tree-decompose-hyphen-date-string hyphen-date-string)
       (if (zero? (string-length hyphen-date-string))
         (list 0 0 0 0)
         (let ((date-list (map string->number (string-split hyphen-date-string "-"))))
           (cond 
             ((eq? (car date-list)  #f) (error "Invalid date format."))
             ((= (length date-list)  3) (append date-list (list 0)))
             (else date-list)))))
      
      (define (elbow-date-tree-date-list? date-list)
        (not
          (not 
            (and (= (length date-list)  4)
                 (integer? (car date-list))
                 (integer? (cadr date-list))
                 (<= 1 (cadr date-list)) (<= (cadr date-list) 12)
                 (integer? (caddr date-list))
                 (<= 1 (caddr date-list)) (<= (caddr date-list) 31)
                 (integer? (cadddr date-list))
                 ))))
      (define (elbow-date-tree-less? date-list1 date-list2)
         (cond 
           ((< (car date-list1) (car date-list2)) #t)
           ((> (car date-list1) (car date-list2)) #f)
           ((< (cadr date-list1) (cadr date-list2)) #t)
           ((> (cadr date-list1) (cadr date-list2)) #f)
           ((< (caddr date-list1) (caddr date-list2)) #t)
           ((> (caddr date-list1) (caddr date-list2)) #f)
           ((< (cadddr date-list1) (cadddr date-list2)) #t)
           ((> (cadddr date-list1) (cadddr date-list2)) #f)
           (else #f)))
      ))



