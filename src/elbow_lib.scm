(include "./elbow_markup.scm")
(include "./niyarin-rainbow-write/niyarin-rainbow-write.scm")

(define-library (elbow lib)
   (import (scheme base)
           (srfi 114)
           (scheme set)
           (scheme write)
           (elbow markup)
           (niyarin rainbow write)
           )
   (export elbow-lib-tag-escape elbow-lib-generate-short-text elbow-lib-warning)
   (begin 

     (define elbow-lib-elbow-commands
       (set 
         eq-comparator
         'elbow-load
         'elbow-for-each 
         'elbow-multiple-for-each
         'elbow-escape-html))


      (define (elbow-lib-tag-get-tag-symbol obj)
        (cond
          ((and (list? obj)  (list? (car obj)))
            (caar obj))
          ((list? obj)
            (car obj))
          (else obj)))


      (define (elbow-lib-tag-escape tag)
        (let loop ((index 0)(res ""))
          (if (= (string-length tag) index)
             res
             (let* ((c (string-ref tag index))
                    (escaped-c
                         (case c
                            ((#\space #\newline #\tab #\return) #\_)
                            (else c))))
                   (loop (+ index 1) (string-append res (string escaped-c)))))))

      (define (elbow-lib-warning text)
        (display-second-color  text (current-error-port)))

      (define (elbow-lib-generate-short-text env content len)
        (let* ((body (cadr (assv '*contents-body* content)))
               (body-string (elbow-markup-convert-html body env content #f)))
              (substring body-string 0 (min len (string-length body-string)))))
      ))
