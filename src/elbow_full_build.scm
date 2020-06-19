(include "./non_portable_utils/directory_utils"
         "./elbow_markup.scm"
         "./elbow_contents.scm"
         "./elbow_sxml.scm"
         "./elbow_misc.scm"
         "./elbow_subcontents.scm"
         "./lib/thread-syntax.scm")

(define-library (elbow full build)
   (cond-expand
     ((library (scheme set))
      (import (scheme base) (scheme read) (scheme write) (scheme file)
              (scheme set) (only (scheme list) filter take cons* remove concatenate)
              (scheme cxr)
              (srfi 114)
              (srfi 69)
              (elbow contents) (elbow lib) (elbow markup) (elbow subcontents) (elbow sxml)
              (elbow misc)
              (only (niyarin thread-syntax) ->> ->)
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

   (export elbow-full-build elbow-fuill-build/build-cmd-opt)

   (begin
      (define *DEFAULT-CONTENTS-CONFIG*
         '((*site-author-page* ());絶対pathのサポートする?
           ))

      (define (%only-file-name fpath)
        (let-values (((d name extension) (decompose-path fpath)))
            name))

      (define (%read-files-without-dotted contents-dir)
         (let-values (((dir files)
                        (directory-list2-add-path
                           (string-append contents-dir "/contents"))))
             (->> files
                  ;;decompose
                  (map (lambda (fpath)
                         (let* ((fname-without-extension
                                  (%only-file-name fpath))
                                (output-fname
                                  (string-append fname-without-extension
                                                 ".html")))
                           (list fpath fname-without-extension output-fname))))
                  ;;remove dot files
                  (remove (lambda (fpath-iname-oname)
                              (elbow-misc/dot-file-name?
                                (cadr fpath-iname-oname))))
                  ;;read files
                  (map (lambda (fpath-iname-oname)
                         (call-with-input-file
                            (car fpath-iname-oname)
                            (lambda (port)
                                 (cons `(*contents-file-name*
                                          ,(caddr fpath-iname-oname))
                                       (read port)))))))))

      (define (elbow-full-build contents-dir template-dir output-dir)
        (let* ((template
                  (call-with-input-file
                    (string-append template-dir "/template.elbow")
                    (lambda (port) (read port))))
               (tag-contents-env ;TODO:template直下にあるという前提(あとで、設定で変更できるようにする
                   (call-with-input-file (string-append template-dir
                                                        "/tag_contents.elbow")
                                         (lambda (port) (read port))))

               (contents-original
                 (->> (%read-files-without-dotted contents-dir)
                      ;remove draft contents
                      (filter elbow-contents-render-contents?)))
               (all-tag-names
                 (->> contents-original
                      (map (lambda (content)
                             (cadr (assq '*contents-tags* content))))
                      concatenate
                      (list->set equal-comparator)))
               (tag-root-name-alist
                 (->> (set->list all-tag-names)
                      (map (lambda (tag-name)
                             (list tag-name
                                   (string-append
                                     "/tags/"
                                     tag-name
                                     ".html"))))))
               (base-contents-fonfig
                 (->> *DEFAULT-CONTENTS-CONFIG*
                      (append (call-with-input-file
                                (string-append contents-dir "/config.elbow")
                                (lambda (port) (read port))))))
               (major-tags
                 (->> (or (assq '*site-selected-tags* base-contents-fonfig)
                           (list 'dummy
                                 (take (set->list all-tag-names)
                                       (min 5 (set-size all-tag-names)))))
                      cadr))
               (contents-config
                 (->> base-contents-fonfig
                     (cons* `(*site-selected-tags* ,major-tags)
                            `(*site-selected-tags-and-links*
                               ,(->> major-tags
                                     (map (lambda (tag)
                                            (list tag
                                                  (cadr (assoc tag tag-root-name-alist)))))))))))

         (begin
            ;Create output-dir
            (elbow-misc/print-info "Make output-directories.")
            (elbow-full-build-create-output-dirs output-dir template-dir contents-dir))

          (set! contents-original
            (->> contents-original
                 ;Add short-text & contents-sub-directory-name.
                 (map (lambda (content)
                        (let ((contents-relative-root-path "../.."))
                          `((*contents-short-text* ,(elbow-lib-generate-short-text '() content 100))
                            (*contents-sub-directory* ,(elbow-contnts-create-sub-directory-name content))
                            (*contents-root-relative-path* ,contents-relative-root-path)
                            (*contents-tags-and-links*
                                 ,(map (lambda (tag-name)
                                           (->> (assoc tag-name tag-root-name-alist string=?)
                                                cadr
                                                (string-append contents-relative-root-path)
                                                (list tag-name)))
                                       (cadr (assq '*contents-tags* content))))
                            .
                            ,content))))))



          (let-values
            (((ids-contents tag-contents)
                (elbow-contents-preprocess contents-original)))
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
             (->> (set->list all-tag-names)
                  (for-each
                    (lambda (tag-name)
                      (let ((sub-contents
                              (map (lambda (index) (vector-ref ids-contents index))
                                   (hash-table-ref tag-contents tag-name))))
                         (elbow-subcontents-create-sub-contents
                           elbow-subcontents-tag-file-base-name
                           template
                           sub-contents
                           contents-config
                           (cons* (list '*contents-title* (string-append "TAGS:" tag-name))
                                  (list '*contents-tag-name* tag-name)
                                  (list '*contents-sub-directory* "tags")
                                  (list '*contents-root-relative-path*  "..")
                                  tag-contents-env)
                           output-dir
                           6)))))

            ;Create index
            (elbow-subcontents-create-sub-contents
               (lambda (_) "index")
               template
               (vector->list ids-contents )
               contents-config
               (cons* (list '*contents-title* (string-append "INDEX"))
                      (list '*contents-sub-directory* "./")
                      (list '*contents-root-relative-path*  "./")
                     tag-contents-env)
               output-dir
               10))

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
            contents-original)))

       (define (elbow-full-build-create-output-dirs output-dir  template-dir contents-dir)
         (create-directory* output-dir)
         (create-directory* (string-append output-dir "/tags/"))
         (copy-directory* (string-append template-dir "/resources") (string-append output-dir "/resources"))
         (copy-directory* (string-append contents-dir "/resources") (string-append output-dir "/contents_resources"))
         (create-directory* (string-append output-dir "/contents"))
         (create-directory* (string-append output-dir "/contents/contents")))

       (define NO-TEMPLATE-MESSAGE
         (string-append
            "ERROR:no template message \n\n"
            "elbow full-build template-directory <template directory name> [options] ..."))

       (define (%output-full-build-option alist)
         (display "full-build-configs")(newline)
         (for-each
           (lambda (apair)
             (display "    ")
             (display (car apair))(display " : ")(write (cadr apair))(newline))
           alist))

       (define *INIT-FULLBUILD-OPTION*
         '(("contents-directory" 1)("template-directory" 1)("output-directory" 1)))

       (define (elbow-fuill-build/build-cmd-opt command-line-options)
         (let ((parsed-option
               (let loop ((options (elbow-misc/assoc-with-default
                                     "options" command-line-options '()
                                     equal?  cdr))
                          (res '()))
                 (cond
                   ((null? options) res)
                   ((assoc (car options) *INIT-FULLBUILD-OPTION*)
                        =>  (lambda (opt-size-pair)
                              (cond
                                ((zero? (cadr opt-size-pair))
                                 (loop (cdr options)
                                       (cons (list (car options) #t)
                                             res)))
                                ((= (cadr opt-size-pair) 1)
                                 (loop (cddr options)
                                       (cons (take options 2) res))))))
                   (else
                     (error "undefined option " (car options)))))))
           (let ((contents-directory (elbow-misc/assoc-with-default
                                       "contents-directory"
                                       parsed-option
                                       "."))
                 (template-directory (elbow-misc/assoc-with-default
                                       "template-directory"
                                       parsed-option
                                       #f))
                 (output-directory (elbow-misc/assoc-with-default
                                     "output-directory"
                                     parsed-option
                                     "./build")))
             (begin
               (unless template-directory (elbow-lib-error NO-TEMPLATE-MESSAGE))
               (elbow-full-build contents-directory template-directory output-directory)))))))
