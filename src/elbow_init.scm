(include "./non_portable_utils/directory_utils.scm")
(include "./elbow_lib.scm")

(define-library (elbow init)
   (import (scheme base)
           (scheme write)
           (scheme file)
           (elbow lib)
           (niyarin non-portable-utils directory-library-wrapper))
   (export elbow-init elbow-init-write)
   (begin 

     (define (elbow-init command-line-options)
       (let ((init-options 
               '(("contents-directory" 1)
                 ("site-title" 1)
                 ("site-author-name" 1)
                 ("site-author-explain" 1))))
         (elbow-init-write
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
                  (error "undefined option " (car options)))
              ))
         )))

     (define (elbow-init-write config)
        (let ((contents-directory (cond ((assoc  "contents-directory" config) => cadr )(else ".")))) ;末尾に/を許容する?
          ;TODO ディレクトリ存在確認(すでにあったら、エラーして終了)
          (unless (string=? contents-directory ".")
             (create-directory* contents-directory))
          (create-directory* (string-append contents-directory "/" "contents"))
          (create-directory* (string-append contents-directory "/" "resources"))

          (let ((site-title
                   (cond 
                     ((assoc 'site-title config) => cadr)
                     (else 
                        (elbow-lib-warning "No site-title.")
                        "site-title")))
                (site-author-name
                     (cond
                       ((assoc 'site-author-name config) => cadr)
                       (else "")))
                (site-author-explain 
                     (cond 
                       ((assoc 'site-author-explain config) => cadr)
                       (else ""))))
             (call-with-output-file 
               (string-append contents-directory "/" "config.elbow")
               (lambda (port)
                 (write 
                   (list 
                     (list '*site-title* site-title)
                     (list '*site-author-name* site-author-name)
                     (list '*site-author-explain* site-author-explain))
                   port ))))
       ))
     ))

