(define-library (elbow markup)
   (import (scheme base)
           (scheme cxr)
           (scheme read)
           (scheme file)
           (scheme write)
           ;(only (srfi 13) string-contains string-replace)
           (srfi 152)

           )

   (export 
     elbow-markup-convert-html
     elbow-markup-string-html-escape 
     )

   (begin
      (define (elbow-markup-aux-convert-open-tag tag env env-contents . opt)
        (let ((close-tag (if (null? opt) ">" (car opt) )))
           (cond 
             ((symbol? tag) (string-append "<" (symbol->string tag) close-tag))
             ((list? tag) (string-append 
                 "<" 
                 (symbol->string (car tag))
                 (apply 
                    string-append
                    (map 
                      (lambda (attribute)
                        (let ((right-value 
                                (if (list? (cadr attribute)) 
                                  (elbow-markup-convert-html (cadr attribute) env env-contents)
                                  (cadr attribute)))
                              )
                           (string-append 
                             " "
                             (symbol->string (car attribute))
                             "=\""
                             right-value
                             "\"")))
                           (cdr tag)))
                  close-tag)))))

      (define (elbow-markup-string-html-escape str);TODO 未完成
         (let ((conv-patterns '(("<" . "&lt;")(">" . "&gt;"))))
           (let loop ((ls conv-patterns)(res str))
             (if (null? ls)
               res
               (let ((start-index (string-contains res (caar ls))))
                  (if start-index
                    (loop ls (string-replace res  (cdar ls) start-index (+ start-index (string-length (caar ls)))))
                    (loop (cdr ls) res)))))))


      (define (elbow-markup-aux-convert-close-tag tag)
        (cond 
          ((symbol? tag) (string-append "</" (symbol->string tag) ">"))
          ((list? tag) (string-append "</" (symbol->string (car tag)) ">"))))

   
      (define (elbow-markup-aux-elbow-load symbol env env-contents)
        (cond 
          ((assq symbol env) => cadr)
          ((assq symbol env-contents) => cadr)
          (else (error "error can't load:" symbol env))))


      (define (elbow-markup-convert-html elbow-markup-code env env-contents . opt)
        (let ((output-tag (cond ((assv 'output-tag opt) => cadr) (else #t))))
            (let loop ((code elbow-markup-code))
              (cond 
                ((null? code) '())
                ((string? code) code)

                 ((and (list? code) (eq? (car code) 'elbow-load))
                        (elbow-markup-aux-elbow-load (cadr code) env env-contents))

                 ((and (list? code) (eq? (car code) 'elbow-load*))
                        (loop (elbow-markup-aux-elbow-load (cadr code) env env-contents)))

                 ((and (list? code) (null? (cdr code)))
                              (if output-tag 
                                 (elbow-markup-aux-convert-open-tag (car code) env env-contents "/>")
                                 "" ))

                ((list? code)
                  (let ((open-tag
                          (if output-tag 
                            (elbow-markup-aux-convert-open-tag (car code) env env-contents)
                            ""))
                        (close-tag
                            (if output-tag
                              (elbow-markup-aux-convert-close-tag (car code))
                              ""))
                        (bodies (map loop (cdr code)))
                        )
                    (string-append open-tag 
                                   (apply string-append bodies)
                                   close-tag)))
        ))))))
