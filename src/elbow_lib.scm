(include 
  "./niyarin-rainbow-write/niyarin-rainbow-write.scm"
  "./elbow_sxml.scm")

(define-library (elbow lib)
   (cond-expand 
      ((library (scheme set))
         (import (scheme base)
                 (srfi 114)
                 (scheme set)
                 (scheme write)
                 (niyarin rainbow write)
                 (elbow sxml)
                 ))
      ((library (srfi 113))
       (import (scheme base)
                 (srfi 114)
                 (srfi 113)
                 (scheme write)
                 (niyarin rainbow write)
                 (elbow sxml)
                 )))

   (export 
     elbow-lib-tag-escape 
     elbow-lib-generate-short-text 
     elbow-lib-warning 
     elbow-lib-error-msg 
     elbow-lib-error 
     elbow-lib-remove-tail-slashes )

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

      (define (elbow-lib-remove-tail-slashes dir-string)
         (let loop ((dir-string dir-string))
           (cond 
             ((zero? (string-length dir-string)) "")
             ((char=? (string-ref dir-string (- (string-length dir-string) 1)) #\/)
                (loop (substring dir-string 0 (- (string-length dir-string) 1))))
             (else dir-string)))
         )

      (define (elbow-lib-warning text)
        (display-second-color  (string-append "Warning: " text "\n") (current-error-port)))


      (define (elbow-lib-error-msg obj)
         (let ((string-port (open-output-string)))
              (write obj string-port)
              (display-first-color  (get-output-string string-port) (current-error-port))
              (close-output-port string-port)))


      (define (elbow-lib-error msg . obj)
        (elbow-lib-error-msg msg)
        (error ""))

      (define (elbow-lib-generate-short-text env content len)
        (with-exception-handler
          (lambda (x)
            (elbow-lib-error-msg x)
            (elbow-lib-error-msg env)
            (elbow-lib-error 
              (string-append
                "ERROR: generating short text in "
                (cond ((assv '*contents-title* content) => cadr) (else "???")))))

          (lambda ()
              (let* ((body (cadr (assv '*contents-body* content)))
                     (body-string (elbow-sxml-generate-html body env content #f)))
                    (substring body-string 0 (min len (string-length body-string)))))))
      ))
