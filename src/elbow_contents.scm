(include "./elbow_utils/elbow_date_tree.scm")
(include "./elbow_lib.scm")
(include "./elbow_misc.scm")
(include "./lib/thread-syntax.scm")

(define-library (elbow contents)
   (import (scheme base)
           (only (srfi 1) filter append-map delete-duplicates);scheme list
           (elbow utils date-tree)
           (elbow lib)
           (elbow misc)
           (only (srfi 69) make-hash-table hash-table-ref hash-table-set! hash-table->alist hash-table-exists? alist->hash-table)
           (only (srfi 113) set set->list);scheme set
           (only (srfi 114) equal-comparator)
           (only (niyarin thread-syntax) ->> ->)
           (scheme read)
           (scheme file)
           (scheme write))

   (export elbow-contents-preprocess
           elbow-contents-add-aux-data-to-content
           elbow-contnts-create-sub-directory-name)

   (begin

     (define (elbow-contents-add-aux-data-to-content content)
       (let ((root-dir (cadr (assq '*contents-root-relative-path*  content))))
         ;タグのリンクをつける
         (let loop ((tags (cadr (assq '*contents-tags* content)))
                    (tag-and-links '()))
           (if (null? tags)
             (set! content (cons (list '*contents-tags-and-links* tag-and-links) content))
             (loop (cdr tags)
                   (cons
                     (list (car tags)
                           (string-append
                             (elbow-lib-remove-tail-slashes root-dir )
                             "tags/"
                             (elbow-lib-tag-escape (car tags))))
                     tag-and-links)))))
       content)

     (define (elbow-contnts-create-sub-directory-name content)
       (cond
         ((assq '*contents-sub-directory* content) => (lambda (it) (cadr it)));TODO:ディレクトリがなければ自動作成するようにあとで変更
         ((assq '*contents-date* content) =>
            (lambda (date-string)
              (let ((date-tree (elbow-date-tree-decompose-hyphen-date-string (cadr date-string))))
                (string-append (number->string (car date-tree)) "-" (number->string (cadr date-tree))))))
         (else "contents")))

     (define (elbow-contents-render-contents? content)
        (not
          (cond
           ((assq '*contents-draft* content) => cadr)
           (else #f))))

     (define (elbow-contents-preprocess contents-list)
       (define (internal-elbow-contents-generate-name contents)
         (let ((date
                 (elbow-date-tree-decompose-hyphen-date-string (cadr (assq '*contents-date* contents)))))
           (string-append
             (string-append
                "contents/"
                (apply string-append
                    (map (lambda (d) (string-append (number->string d) "/" )) date)))
             "contents"
             (apply string-append
                    (map (lambda (d) (string-append "_" (number->string d))) date))
             ".html")))

       (let* ((with-id-contents
                (filter elbow-contents-render-contents? contents-list))
              (ids-contents (list->vector with-id-contents))
              (tag-names
                (delete-duplicates
                    (append-map (lambda (content)
                                        (cond ((assq '*contents-tags* content) => cadr)
                                              (else '())))
                                (reverse with-id-contents))
                    equal?))
              (content-has-tag?-fn (lambda (tag-name)
                                     (lambda (content)
                                      (cond ((assq '*contents-tags* content)
                                             => (lambda (key-tagnames)
                                                  (member tag-name (cadr key-tagnames))))
                                            (else #f)))))
              (tag-contents ;;make ((tag-name (contents-ids ... )) ... )
                (alist->hash-table
                  (map (lambda (tag-name)
                         (let ((has-tag? (content-has-tag?-fn tag-name)))
                           (cons tag-name
                                 (->> (reverse with-id-contents)
                                      (filter has-tag?)
                                      (map (lambda (content) (cadr (assq '*contents-id* content))))))))
                       tag-names)
                  equal-comparator)))
         (values ids-contents tag-contents)))))
