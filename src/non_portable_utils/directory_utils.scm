(define-library (niyarin non-portable-utils directory-library-wrapper)
   (cond-expand
     (gauche
       (import (only (rename (file util) (copy-directory* org-copy-directory*)) current-directory create-directory* delete-directory* directory-list2  org-copy-directory* decompose-path)
               (scheme base))

       (begin 
         (define (directory-list2-add-path dir)
           (directory-list2 dir :add-path? #true))

         (define (copy-directory* from to)
           (org-copy-directory* from to :if-exists :supersede))
         )
       )

     (sagittarius
       (import (only (rename (util file) (copy-directory copy-directory*)) create-directory* delete-directory* path-for-each copy-directory* decompose-path)
               (scheme base))
        (begin 
           (define (directory-list2-add-path dir)
             (let ((dirs '())
                   (files '()))
               (path-for-each 
                 "./" 
                 (lambda (name type)
                   (if (eqv? name 'directory)
                     (set! dirs (cons name dirs))
                     (set! files (cons name files))))
                 :recursive #f)
               (values dirs files)))))


     (else 
       ("ERROR:This Scheme Imprementation is not supported.")))
   (export create-directory* delete-directory* directory-list2-add-path copy-directory* decompose-path)
   )
