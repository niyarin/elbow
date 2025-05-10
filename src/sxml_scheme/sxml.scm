(define-library (niyarin sxml)
   (import
     (scheme base)
     (srfi 1);(scheme list)
     (scheme write);FOR DEBUG
     (scheme cxr)
     (scheme eval);
     )
   (export sxml->xml-string)

   (begin
     (define (%tag-checker tag-name)
       (lambda (sxml)
         (and (list? sxml)
              (eq? (car sxml) tag-name))))

     (define %attribute-list? (%tag-checker '@))
     (define %top? (%tag-checker '*TOP*))
     (define %comment? (%tag-checker '*COMMENT*))
     (define %PI? (%tag-checker '*PI*))

      (define (%element? sxml)
        (and (list? sxml)
             (list? (cdr sxml))
             (symbol? (car sxml))))

     (define (sxml->xml-string sxml . opt)
       (let* ((env (if (null? opt) '() (car opt)))
              (env-contains? (if (null? env) '() (cadr (assq 'contains? env))))
              (eval-env (if (null? env) '() (cadr (assq 'eval-env env))))
              (convert-env (if (null? env) '() (cadr (assq 'convert-env env)))))

          (let loop ((sxml sxml))
             (cond
               ((string? sxml) sxml)
               ((null? sxml) "")
               ((and (not (null? env))
                     (list? sxml)
                     (env-contains? convert-env (car sxml)))
                (let ((eval-res
                        (eval sxml eval-env)))
                  (loop eval-res)))
               ((%top? sxml)
                  (apply
                    string-append
                    (map loop (cdr sxml))))
               ((%PI? sxml)
                  (string-append
                    "<?"
                    (symbol->string (cadr sxml))
                    " "
                    (caddr sxml)
                    ">"))
               ((%element? sxml)
                (let* ((have-attribute
                       (%attribute-list? (if (null? (cdr sxml)) #f (cadr sxml))))
                       (children (if have-attribute (cddr sxml) (cdr sxml)))
                       (attribute
                           (if have-attribute
                              (let loop ((attribute (cdadr sxml))
                                         (res ""))
                                (if  (null? attribute)
                                  res
                                  (loop
                                    (cdr attribute)
                                    (string-append
                                        res
                                        " "
                                        (symbol->string (caar  attribute))
                                        "="
                                        "\""
                                        (if (null? (cdar attribute)) "" (cadar attribute))
                                        "\""))))
                              "")))

                      (if (null? children)
                        (string-append
                          "<"
                         (symbol->string  (car sxml))
                         attribute
                         "/>")

                       (string-append
                         "<"
                         (symbol->string  (car sxml))
                         attribute
                         ">"
                         (apply string-append (map loop (if have-attribute (cddr sxml) (cdr sxml))))
                         "</"
                         (symbol->string  (car sxml))
                         ">"
                         ))))
               (else
                 (error "ERROR:invalid sxml" sxml))
               ))))))
