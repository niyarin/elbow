(include "./non_portable_utils/directory_utils"
         "./elbow_markup.scm"
         "./elbow_contents.scm"
         "./elbow_sxml.scm"
         "./elbow_misc.scm"
         "./elbow_subcontents.scm"
         "./lib/thread-syntax.scm")
(include "./elbow/path.scm")
(include "./elbow/contents-middleware.scm")


(define-library (elbow full build)
   (cond-expand
     ((library (scheme set))
      (import (scheme base) (scheme read) (scheme write) (scheme file)
              (scheme set) (only (scheme list) filter take cons* remove concatenate)
              (scheme cxr)
              (srfi 114)
              (srfi 69)
              (prefix (elbow contents) econ/)
              (elbow lib) (elbow markup) (elbow subcontents) (elbow sxml)
              (elbow misc)
              (only (niyarin thread-syntax) ->> ->)
              (prefix (elbow contents-middleware) emware/)
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
              (prefix (elbow contents-middleware) emware/)
              (niyarin non-portable-utils directory-library-wrapper))))

   (export elbow-full-build elbow-fuill-build/build-cmd-opt)

   (begin
      (define *DEFAULT-CONTENTS-CONFIG*
         '((*site-author-page* ());絶対pathのサポートする?
           ))

      (define (%only-file-name fpath)
        (let-values (((d name extension) (decompose-path fpath)))
            name))

     (define (%make-filenames contents-dir)
       (let-values (((dir files)
                   (directory-list2-add-path
                      (string-append contents-dir))))
          (map (lambda (filename)
               `((*contents-filepath* ,filename)))
               files)))


      (define (%read-files-without-dotted contents-dir)
          (emware/elbow-pipeline>
            (emware/add-filename-key-middleware
                 (%make-filenames contents-dir) '())
             emware/remove-dotted-file-middleware
             emware/add-output-name-middleware
             emware/filter-elbow-file-middleware
             emware/read-file-middleware
             emware/remove-draft-middleware
             emware/add-tags-info-to-global-env-middleware
             emware/calc-major-tags-middleware
             emware/add-parsed-date-info-to-contents-middleware
             emware/add-date-as-list-to-contents-middleware
             emware/add-ids-to-contents-middleware
             emware/add-short-text-middleware
             cons))


      (define (read-template template-dir)
        (call-with-input-file (string-append template-dir "/template.elbow")
                              (lambda (port) (read port))))

      (define (take-recent-n-entries contents n)
        (let ((len (length contents)))
          (take (reverse contents)
                (min len n))))

      (define (content->output-path output-dir content)
        (string-append output-dir "/contents/"
                               (cadr (assq '*contents-sub-directory* content)) "/"
                               (cond ((assq '*contents-output-filename* content) => cadr)
                                     (else (cadr (assq '*contents-output-filename* content))))))

      (define (elbow-full-build contents-dir template-dir output-dir)
        (let* ((base-contents-fonfig
                 (->> *DEFAULT-CONTENTS-CONFIG*
                      (append (call-with-input-file
                                (string-append contents-dir "/config.elbow")
                                (lambda (port) (read port))))))
               (contents-original/env
                 (%read-files-without-dotted
                   (string-append contents-dir "/contents/")))

               (contents-env (cdr contents-original/env))
               (all-tag-names (set->list (cadr (assq '*tag-names* contents-env))))
               (tag->path-alist
                  (map (lambda (tag-name)
                         (list tag-name
                               (string-append
                                 "/tags/"
                                 tag-name
                                 ".html")))
                       all-tag-names))

               (tag->path
                 (lambda (tag)
                   (cadr (assoc tag tag->path-alist string=?))))


               (contents-relative-root-path "../..")
               (contents-original
                 (map (lambda (content)
                        (cons* `(*contents-sub-directory* ,(econ/make-sub-directory-name content))
                               `(*contents-root-relative-path* ,contents-relative-root-path)
                               `(*contents-tags-and-links*
                                  ,(map (lambda (tag-name)
                                          (list tag-name
                                                (string-append contents-relative-root-path
                                                               (tag->path tag-name))))
                                        (cadr (assq '*contents-tags* content))))
                               content))
                      (car contents-original/env)))



               (major-tags (cadr (assq '*major-tags* contents-env)))
               (contents-config
                 (cons* `(*site-selected-tags* ,major-tags)
                        `(*site-selected-tags-and-links*
                           ,(->> major-tags
                                 (map (lambda (tag)
                                        (list tag (tag->path tag))))))
                        `(*site-recent-entries*  ,(take-recent-n-entries contents-original 5))
                        base-contents-fonfig))
                (template (read-template template-dir)))
         (begin
            ;Create output-dir
            (elbow-misc/print-info "- Make output-directories.")
            (elbow-full-build-create-output-dirs output-dir template-dir contents-dir))

          (let ((ids-contents (list->vector contents-original)))
               ;Create tag pages
              (let ((tag-contents-env ;TODO:template直下にあるという前提(あとで、設定で変更できるようにする
                     (call-with-input-file (string-append template-dir
                                                          "/tag_contents.elbow")
                                           (lambda (port) (read port)))))

                ;Create index
                (elbow-subcontents-create-sub-contents
                   (lambda (_) "index")
                   template
                   (reverse contents-original)
                   contents-config
                   (cons* (list '*contents-title* (string-append "Index"))
                          (list '*contents-sub-directory* "./")
                          (list '*contents-root-relative-path*  "./")
                         tag-contents-env)
                   output-dir
                   10)

                  (elbow-misc/print-info "- Make tag pages.")
                  ;;create tags
                  (let* ((tag->ids (econ/make-tag->ids contents-original))
                         (tag->contents
                           (lambda (tag-name)
                             (map (lambda (index)
                                    (vector-ref ids-contents index))
                                  (tag->ids tag-name)))))

                      (for-each
                        (lambda (tag-name)
                          (elbow-misc/print-info (string-append "    - " tag-name))
                          (let ((sub-contents (tag->contents tag-name)))
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
                               6)))
                         all-tag-names))))

         ;Create contents
         (for-each
            (lambda (content)
               (create-directory*
                 (string-append output-dir "/contents/" (cadr (assq '*contents-sub-directory* content))));TODO:明らかに悪そう。あとで修正する

              (call-with-output-file
                (content->output-path output-dir content)
                (lambda (port)
                  (let ((sxml-scm-code
                          (cond ((or (and (assq '*contents-use-template* content)
                                         (not (cadr (assq '*contents-use-template*
                                                content))))
                                     (and (assq '*contents-use-template* contents-config)
                                          (not (cadr (assq '*contents-use-template*
                                               contents-config)))))
                                 '(begin *contents-body*))
                                ((equal? (assq '*contents-format* content)
                                         '('*contents-format* markdown))
                                 (begin (display "MARK DOWN!")(newline)'()))
                                (else template))))
                     (display
                       (elbow-sxml-generate-html
                         sxml-scm-code
                         contents-config
                         content)
                       port)))))
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

      (define (parse-option command-line-options)
          (let loop ((options (elbow-misc/assoc-with-default
                                     "options" command-line-options '()
                                     equal?  cdr))
                          (res '()))
                 (cond
                   ((null? options) res)
                   ((assoc (car options) *INIT-FULLBUILD-OPTION*)
                        =>  (lambda (opt-size-apair)
                              (case (cadr opt-size-apair)
                                ((0)
                                 (loop (cdr options)
                                       (cons (list (car options) #t)
                                             res)))
                                ((1)
                                 (loop (cddr options)
                                       (cons (take options 2) res))))))
                   (else
                     (error "undefined option " (car options))))))

       (define (elbow-fuill-build/build-cmd-opt command-line-options)
         (let* ((parsed-option (parse-option command-line-options))
                (contents-directory (elbow-misc/assoc-with-default
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
         (unless template-directory (elbow-lib-error NO-TEMPLATE-MESSAGE))
         (elbow-full-build contents-directory template-directory output-directory)))))
