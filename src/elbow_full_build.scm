(include "./non_portable_utils/directory_utils"
         "./elbow_markup.scm"
         "./elbow_contents.scm" 
         "./elbow_sxml.scm"
         "./elbow_subcontents.scm")

(define-library (elbow full build)
   (cond-expand 
     ((library (scheme set))
      (import (scheme base)
              (scheme read)
              (scheme write)
              (scheme file)
              (scheme set)
              (only (srfi 1) filter)
              (srfi 114)
              (srfi 69)
              (elbow contents)
              (elbow lib)
              (elbow markup)
              (elbow subcontents)
              (elbow sxml)
              (niyarin non-portable-utils directory-library-wrapper)))
     ((library (srfi 113))
         (import (scheme base)
              (scheme read)
              (scheme write)
              (scheme file)
              (srfi 113)
              (only (srfi 1) filter)
              (srfi 114)
              (srfi 69)
              (elbow contents)
              (elbow lib)
              (elbow markup)
              (elbow subcontents)
              (elbow sxml)
              (niyarin non-portable-utils directory-library-wrapper))))

   (export elbow-full-build elbow-full-build-cmd-opt )

   (begin 

      (define *DEFAULT-CONTENTS-CONFIG*
         '((*site-author-page* ());絶対pathのサポートする?
           ))

      (define (elbow-full-build contents-dir template-dir output-dir)
        (let ((contents-config 
                  (append
                    (call-with-input-file (string-append contents-dir "/config.elbow") 
                      (lambda (port) (read port)))
                    *DEFAULT-CONTENTS-CONFIG*))
              (template 
                 (call-with-input-file (string-append template-dir "/template.elbow") (lambda (port) (read port))))

              (tag-contents-env ;TODO:template直下にあるという前提(あとで、設定で変更できるようにする
                  (call-with-input-file (string-append template-dir "/tag_contents.elbow") (lambda (port) (read port))))

              (contents-original
                (let-values (((dir files) ( directory-list2-add-path (string-append contents-dir "/contents"))))
                   (map
                     (lambda (fname) 
                       (call-with-input-file 
                         fname 
                         (lambda (port) 
                           (cons 
                             (list 
                               '*contents-file-name* 
                               (let-values (((_ name extension) (decompose-path fname))) 
                                           (string-append name "." "html")))
                             (read port)))))
                     (filter (lambda (fpath) (let-values (((_ fname __) (decompose-path fpath)))(not (char=? (string-ref fname 0) #\.)))) files))
                   ))
              (all-tags (set equal-comparator)))



          ;Add short-text & contents-sub-directory-name.
          (set! contents-original
                (map
                  (lambda (content)
                    (let ((contents-relative-root-path "../.."))

                       (append
                         (list
                           (list '*contents-short-text* (elbow-lib-generate-short-text '() content 100))
                           (list '*contents-sub-directory*  (elbow-contnts-create-sub-directory-name content))
                           (list '*contents-root-relative-path* contents-relative-root-path)
                           (list '*contents-tags-and-links*
                                 (map
                                   (lambda (tag)
                                     (list 
                                       tag 
                                       (string-append 
                                         (elbow-lib-remove-tail-slashes 
                                           contents-relative-root-path )
                                         "/tags/"
                                         tag 
                                         ".html")))
                                   (cadr (assv '*contents-tags* content)))))
                         content)))
                  contents-original))
         ;Generate tag list.
         (for-each 
           (lambda (content)
             (apply 
               set-adjoin! 
               (cons all-tags (cadr (assv '*contents-tags* content)))))
           contents-original)
 
         (unless (assq '*site-selected-tags* contents-config)
            (set! contents-config
               (cons 
                 (list '*site-selected-tags 
                       (let loop ((i 0) (tags (set->list all-tags)) (res '())) 
                         (cond 
                           ((null? tags) res)
                           ((= i 5) res);この定数あとでconfigにいれる。
                           (else 
                             (loop (+ i 1) (cdr tags) (cons (car tags) res))))))
                 contents-config))

             (set! contents-config
               (cons 
                 (list '*site-selected-tags-and-links* 
                       (let loop ((i 0) (tags (set->list all-tags)) (res '())) 
                         (cond 
                           ((null? tags) res)
                           ((= i 5) res);この定数あとでconfigにいれる。
                           (else 
                             (loop 
                               (+ i 1) 
                               (cdr tags) 
                               (cons 
                                 (list (car tags)  
                                       (string-append
                                          (elbow-subcontents-tag-file-base-name 
                                            (append (list 
                                                      (list '*contents-tag-name* (car tags)) 
                                                      (list '*contents-root-relative-path*  "/"))
                                                    tag-contents-env))
                                       ".html"))
                                 res))))))
                 contents-config)))

         ;Create output-dir
         (elbow-full-build-create-output-dirs output-dir template-dir contents-dir )


          (let-values 
            (((ids-contents tag-contents file-names) (elbow-contents-preprocess contents-original)))
            
             (set! contents-config
                   (cons
                     (list 
                       '*site-recent-entries*
                       (let ((end-index (max 0 (- (vector-length ids-contents) 5))))
                         (let loop ((i (- (vector-length ids-contents) 1))(res '()))
                           (if (< i end-index)
                               res
                               (loop (- i 1) (cons (vector-ref ids-contents i) res))))))
                     contents-config))
             ;Create tag parges
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
                        elbow-subcontents-tag-file-base-name
                        template 
                        sub-contents 
                        contents-config
                        (append 
                              (list 
                                (list '*contents-title* (string-append "TAGS:" tag))
                                (list '*contents-tag-name* tag)
                                (list '*contents-sub-directory* "tags")
                                (list '*contents-root-relative-path*  ".."))
                              tag-contents-env)
                        output-dir 
                        5)
                      ))
                 tag-pages))
  
            ;Create index
            (elbow-subcontents-create-sub-contents 
               (lambda (_) "index")
               template 
               (vector->list ids-contents )
               contents-config
               (append 
                     (list 
                       (list '*contents-title* (string-append "INDEX"))
                       (list '*contents-sub-directory* "./")
                       (list '*contents-root-relative-path*  "./"))
                     tag-contents-env)
               output-dir 
               10)
             )

         ;Create contents
         (for-each
            (lambda (content)
               (create-directory*  
                 (string-append output-dir "/contents/" (cadr (assq '*contents-sub-directory* content))));TODO:明らかに悪そう。あとで修正する 

              (call-with-output-file
                (string-append output-dir "/contents/" (cadr (assq '*contents-sub-directory* content)) "/" (cadr (assq '*contents-file-name* content)))
                (lambda (port)
                  (if 
                     (or 
                       (and
                          (assq '*contents-use-template* content)
                          (not (cadr (assq '*contents-use-template* content))))
                       (and 
                         (assq '*contents-use-template* contents-config)
                         (not (cadr (assq '*contents-use-template* contents-config)))))
                     (display
                       (elbow-sxml-generate-html 
                         '(begin *contents-body*)
                         contents-config
                         content)
                       port)

                   (display (elbow-sxml-generate-html template contents-config content) port)))))
            contents-original)
          
          )) 

       (define (elbow-full-build-create-output-dirs output-dir  template-dir contents-dir)
         (create-directory* output-dir)
         (create-directory* (string-append output-dir "/tags/"))
         (copy-directory* (string-append template-dir "/resources") (string-append output-dir "/resources"))
         (copy-directory* (string-append contents-dir "/resources") (string-append output-dir "/contents_resources"))
         (create-directory* (string-append output-dir "/contents"))
         (create-directory* (string-append output-dir "/contents/contents"))
         )

       (define NO-TEMPLATE-MESSAGE
         (string-append
            "ERROR:no template message \n\n"
            "elbow [command] [options] ... template-directory <template directory name>"
            ))


       (define (elbow-full-build-cmd-opt command-line-options)
          (let ((init-options 
                  '(("contents-directory" 1)("template-directory" 1)("output-directory" 1))))
            (let ((parsed-option
                  (let loop ((options (cond ((assoc "options" command-line-options) => cdr) (else '()))) (res '()))
                    (cond
                      ((null? options) res)
                      ((assoc (car options) init-options)
                           =>  (lambda (opt-size-pair)
                                 (cond 
                                   ((= (cadr opt-size-pair) 0)
                                    (loop (cdr options) (cons (list (car options) #t) res)))
                                   ((= (cadr opt-size-pair) 1)
                                    (loop (cddr options) (cons (list (car options) (cadr options)) res))))))
                      (else 
                        (error "undefined option " (car options))))))
                  )
              (let ((contents-directory (cond ((assoc "contents-directory" parsed-option) => cadr)(else ".")))
                    (template-directory (cond ((assoc "template-directory" parsed-option) => cadr)(else (elbow-lib-error NO-TEMPLATE-MESSAGE))))
                    (output-directory (cond ((assoc "output-directory" parsed-option) => cadr)(else "./build"))))
                  (elbow-full-build contents-directory template-directory output-directory)
              ))))

      ))
