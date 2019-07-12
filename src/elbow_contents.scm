(include "./elbow_utils/elbow_date_tree.scm")
(include "./elbow_lib.scm")

(define-library (elbow contents)
   (import (scheme base)
           (elbow utils date-tree)
           (elbow lib)
           (only (srfi 95) sort!)
           (only (srfi 69) make-hash-table hash-table-ref hash-table-set! hash-table->alist hash-table-exists?)
           (scheme read)
           (scheme file)
           (scheme write))

   (export elbow-contents-preprocess 
           elbow-contents-add-aux-data-to-content 
           elbow-contents-read-from-file
           elbow-contnts-create-sub-directory-name)

   (begin

     (define (elbow-contents-add-aux-data-to-content content)
       (let ((root-dir (cadr (assv '*contents-root-relative-path*  content))))
         ;タグのリンクをつける
         (let loop ((tags (cadr (assv '*contents-tags* content)))
                    (tag-and-links '()))
           (if (null? tags)
             (set! content (cons (list '*contents-tags-and-links* tag-and-links) content) ) 
             (loop (cdr tags)
                   (cons 
                     (list (car tags) 
                           (string-append 
                             (elbow-lib-remove-tail-slashes root-dir )
                             "tags/"  
                             (elbow-lib-tag-escape (car tags))))
                     tag-and-links
                     ))))
         )
       content)

     (define (elbow-contnts-create-sub-directory-name content)
       (cond 
         ((assq '*contents-sub-directory* content) => (lambda (it) (cadr it)));TODO:ディレクトリがなければ自動作成するようにあとで変更
         ((assq '*contents-date* content) =>
            (lambda (date-string)
              (let ((date-tree (elbow-date-tree-decompose-hyphen-date-string (cadr date-string))))
                (string-append (number->string (car date-tree)) "-" (number->string (cadr date-tree))))))
         (else "contents")
       ))

     (define (elbow-contents-preprocess contents-list)
       (define (internal-elbow-contents-render-contents? contents)
         (not 
            (cond 
              ((assv '*contents-draft* contents) => cadr)
              (else #f))))

       (define (internal-elbow-contents-generate-name contents)
         (let ((date 
                 (elbow-date-tree-decompose-hyphen-date-string (cadr (assv '*contents-date* contents)))))
           (string-append 
             (string-append 
                "contents/"
                (apply string-append 
                    (map (lambda (d) (string-append (number->string d) "/" )) date)))
             "contents"
             (apply string-append 
                    (map (lambda (d) (string-append "_" (number->string d))) date))
             ".html")))

       (let ((ids-contents (make-vector (length contents-list)));index - コンテンツの関係
             (tag-contents (make-hash-table equal?));tag -> id-listのhashtable
             (file-names #f);id - ファイル名
             )
         

         (define (internal-elbow-contents-push-data! tag id)
            (when (not (hash-table-exists? tag-contents tag))
               (hash-table-set! tag-contents tag '()))
            (hash-table-set! tag-contents tag  (cons id (hash-table-ref tag-contents tag))))



         (let loop ((index 0)
                    (ls contents-list))
           (if (null? ls)
              (set! ids-contents (vector-copy ids-contents 0 index))
              (if (internal-elbow-contents-render-contents? (car ls))
                 (begin 
                    (vector-set! ids-contents index (car ls))
                    (loop (+ index 1) (cdr ls)) )
                 (loop index (cdr ls)))))

         ;日付順にsort!
         (sort! 
           ids-contents 
           (lambda (c1 c2)
             (let ((dl1 
                     (if (assv '*contents-date c1)
                       '(0 0 0 0)
                        (elbow-date-tree-decompose-hyphen-date-string (cadr (assv '*contents-date* c1)))))
                   (dl2 
                     (if (assv '*contents-date c2)
                       '(0 0 0 0)
                        (elbow-date-tree-decompose-hyphen-date-string (cadr (assv '*contents-date* c2))))))
                (elbow-date-tree-less? dl1 dl2))))

         (set! file-names
            (make-vector (vector-length ids-contents)))
           

         (let loop ((i 0))
           (when (< i (vector-length ids-contents))
               (vector-set! ids-contents i (cons (list '*contents-id* i) (vector-ref ids-contents i)))

               (vector-set! file-names i (internal-elbow-contents-generate-name (vector-ref ids-contents i)))

               (let ((tags
                       (cond 
                         ((assv '*contents-tags* 
                               (vector-ref ids-contents i)) => cadr )
                         (else '())))
                     )
                 (for-each
                   (lambda (tag)
                     (internal-elbow-contents-push-data! tag i))
                   tags)
               (loop (+ i 1)))))

            (values ids-contents tag-contents file-names)
         ))


     (define (elbow-contents-read-from-file filename)
       (call-with-input-file filename 
            (lambda (port)
              (let ((elbow-contents (read port)))
                elbow-contents))))
     
     ))

