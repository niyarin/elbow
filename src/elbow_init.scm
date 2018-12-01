(include "./non_portable_utils/directory_utils.scm")

(define-library (elbow init)
   (import (scheme base)
           (scheme write)
           (scheme file)
           (niyarin non-portable-utils directory-library-wrapper))
   (export elbow-init elbow-init-write)
   (begin 

     (define (elbow-init command-line-options)
       (let ((init-options 
               '(("target-directory" 1))))
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
        (let ((target-directory (cond ((assoc  "target-directory" config) => cadr )(else (error "error")))));末尾に/を許容する?
          ;TODO ディレクトリ存在確認(すでにあったら、エラーして終了)
          (create-directory* target-directory)
          (create-directory* (string-append target-directory "/" "contents"))
          (create-directory* (string-append target-directory "/" "resources"))

          (call-with-output-file 
            (string-append target-directory "/" "config.elbow")
            (lambda (port)
              (write config port )))
          ))
     ))

