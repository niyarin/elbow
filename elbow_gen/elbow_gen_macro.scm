(define-library (elbow gen macro)
   (import (scheme base))
   (export elbow-gen-embed)
   (begin
     (define-syntax elbow-gen-embed
         (syntax-rules 
           ()
           ((_ type object body)
            body)))
     ))


#|
;test
(import (elbow gen macro)
        (scheme base)
        (scheme write)
        (scheme read))


(define reduce
   (elbow-gen-embed
     elbow-document

     ((arg f function "target function")
      (arg init object "init")
      (arg ls list "list target")
      (return object "reduce result"))

     (lambda (f init ls)
       (if (null? ls)
         g
         (reduce f (f init (car ls)) (cdr ls))))))

(display 
   (reduce + 0 (list 1 2 3 4 5)))
(newline)
|#
