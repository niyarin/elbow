(include "./elbow_utils/elbow_date_tree.scm")
(include "./elbow_lib.scm")
(include "./elbow_misc.scm")
(include "./lib/thread-syntax.scm")

(define-library (elbow contents)
   (import (scheme base)
           (only (scheme list) append-map delete-duplicates filter-map)
           (elbow utils date-tree)
           (prefix (elbow lib) elib/)
           (only (srfi 69) hash-table-ref alist->hash-table)
           (only (srfi 114) equal-comparator)
           (only (niyarin thread-syntax) ->> ->)
           (scheme write))

   (export make-tag->ids
           elbow-contents-add-aux-data-to-content
           make-sub-directory-name)

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
                             (elib/elbow-lib-remove-tail-slashes root-dir )
                             "tags/"
                             (elib/elbow-lib-tag-escape (car tags))))
                     tag-and-links)))))
       content)

     (define (make-sub-directory-name content)
       (cond
         ((assq '*contents-sub-directory* content) => (lambda (it) (cadr it)));TODO:ディレクトリがなければ自動作成するようにあとで変更
         ((assq '*contents-date* content) =>
            (lambda (date-string)
              (let ((date-tree (elbow-date-tree-decompose-hyphen-date-string (cadr date-string))))
                (string-append (number->string (car date-tree)) "-" (number->string (cadr date-tree))))))
         (else "contents")))

     (define (make-tag->ids visible-contents-list)
       (let* ((id-tags (map (lambda (content)
                              (cons (cadr (assq '*contents-id* content))
                                    (cond ((assq '*contents-tags* content) => cadr) (else '()))))
                            visible-contents-list))
              (tag-names (delete-duplicates (append-map cdr id-tags) equal?))
              (tag->ids-alist
                (map (lambda (tag-name)
                       (cons tag-name
                             (filter-map (lambda (id-tag)
                                           (and (member tag-name (cdr id-tag))
                                                (car id-tag)))
                                         (reverse id-tags))))
                     tag-names))
              (tag->ids-hash
                (alist->hash-table tag->ids-alist equal-comparator)))
         (lambda (tag-name) (hash-table-ref tag->ids-hash tag-name))))))
