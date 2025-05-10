(define-library (niyarin sxml)
   (import
     (scheme base)
     (srfi 1);(scheme list)
     (scheme cxr))
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

     (define (%atom->string atom)
       (cond
         ((string? atom) atom)
         ((symbol? atom) (symbol->string atom))
         ((number? atom) (number->string atom))
         (else (error "Invalid atom" atom))))

     (define (%attribute->string attribute)
       (let loop ((attribute attribute)
                  (res ""))
         (cond
           ((null? attribute) res)
           ((car attribute)
            => (lambda (key-val)
                 (loop
                   (cdr attribute)
                   (string-append
                       res
                       " "
                       (%atom->string (car key-val))
                       "="
                       "\""
                       (if (null? (cdr key-val)) "" (%atom->string (cadr key-val)))
                       "\"")))))))

     (define (make-self-closing-tag-string tag-name attribute)
       (string-append "<" (symbol->string tag-name) attribute "/>"))

     (define (sxml->additional-expander sxml additional-expanders)
        (let loop ((additional-expanders additional-expanders))
          (cond
            ((null? additional-expanders) #f)
            (((caar additional-expanders) sxml)
             (cdar additional-expanders))
            (else (loop (cdr additional-expanders))))))

     (define (sxml->xml-string sxml . opt)
       (let* ((additional-expanders (if (null? opt) '() (car opt))))
          (let loop ((sxml sxml))
             (cond
               ((string? sxml) sxml)
               ((number? sxml) (number->string sxml))
               ((null? sxml) "")
               ((and (list? sxml)
                     (sxml->additional-expander sxml additional-expanders))
                => (lambda (additional-expander)
                     (additional-expander sxml)))
               ((%top? sxml)
                  (apply string-append
                         (map loop (cdr sxml))))
               ((%PI? sxml)
                  (string-append
                    "<?" (symbol->string (cadr sxml)) " " (caddr sxml) ">"))
               ((%element? sxml)
                (let* ((attribute
                         (and (not (null? (cdr sxml)))
                              (%attribute-list? (cadr sxml))
                              (cdadr sxml)))
                       (children (if attribute (cddr sxml) (cdr sxml)))
                       (attribute-string (if attribute
                                             (%attribute->string attribute) "")))
                  (if (null? children)
                    (make-self-closing-tag-string (car sxml) attribute-string)
                     (string-append
                       "<" (symbol->string (car sxml)) attribute-string ">"
                       (apply string-append (map loop children))
                       "</" (symbol->string  (car sxml)) ">"))))
               (else (error "ERROR:invalid sxml" sxml))))))))
