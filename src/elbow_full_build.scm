(include "./non_portable_utils/directory_utils"
         "./elbow_markup.scm"
         "./elbow_contents.scm" 
         "./elbow_subcontents.scm")

(define-library (elbow full build)
   (import (scheme base)
           (scheme read)
           (scheme write)
           (scheme file)
           (scheme set)
           (srfi 114)
           (srfi 69)
           (elbow contents)
           (elbow lib)
           (elbow markup)
           (elbow subcontents)
           (niyarin non-portable-utils directory-library-wrapper))

   (export elbow-full-build-full-build)

   (begin
     ;末尾に/がいる
      (define (elbow-full-build-full-build contents-dir template-dir output-dir)
        (let ((contents-config 
                  (call-with-input-file (string-append contents-dir "config.elbow") (lambda (port) (read port))))
              (template 
                 (call-with-input-file (string-append template-dir "template.elbow") (lambda (port) (read port))))

              (tag-contents-env ;TODO:template直下にあるという前提(あとで、設定で変更できるようにする
                  (call-with-input-file (string-append template-dir "tag_contents.elbow") (lambda (port) (read port))))

              (contents-original
                (let-values (((dir files) ( directory-list2-add-path (string-append contents-dir "contents"))))
                   (map
                     (lambda (fname) (call-with-input-file fname (lambda (port) (read port))))
                     files)
                   ))
              (all-tags (set equal-comparator)))


          ;Add short text.
          (set! contents-original 
                (map 
                  (lambda (content)
                     (cons (list '*contents-short-text* (elbow-lib-generate-short-text '() content 100))
                            content))
                  contents-original))

         ;Generate tag list.
         (for-each 
           (lambda (content)
             (apply 
               set-adjoin! 
               (cons all-tags (cadr (assv '*contents-tags* content)))))
           contents-original)
         
         ;Create output-dir
         (elbow-full-build-create-output-dirs output-dir template-dir)

          (let-values 
            (((ids-contents tag-contents file-names) (elbow-contents-preprocess contents-original)))
             (let ((tag-pages
                     (map 
                       (lambda (tag)
                           (cons 
                             tag
                              (map (lambda (index) (vector-ref ids-contents index))
                                   (hash-table-ref tag-contents tag )) ))
                       (set->list all-tags))))

               (for-each 
                 (lambda (tag-contents-pair)
                    (let ((tag (car tag-contents-pair))
                          (sub-contents (cdr tag-contents-pair)))
                      (elbow-subcontents-create-sub-contents 
                        template 
                        sub-contents 
                        contents-config
                        (append 
                              (list 
                                (list '*contents-title* (string-append "TAGS:" tag))
                                (list '*contents-tag-name* tag)
                                (list '*contents-root-relative-path*  "../"))
                              tag-contents-env)
                        output-dir 
                        2)
                      ))
                 tag-pages)
        ))))


       (define (elbow-full-build-create-output-dirs output-dir template-dir)
         (create-directory* output-dir)
         (create-directory* (string-append output-dir "tags/"))
         (copy-directory* (string-append template-dir "resources") (string-append output-dir "resources"))
         )
      ))
