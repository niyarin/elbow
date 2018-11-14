(include "./non_portable_utils/directory_utils.scm")

(define-library (elbow init)
   (import (scheme base)
           (scheme write)
           (scheme file)
           (niyarin non-portable-utils directory-library-wrapper))
   (export elbow-init elbow-init-write)
   (begin 

     (define (elbow-init command-line-options)
         (display "init elbow project")(newline)
         (display command-line-options)(newline))

     (define (elbow-init-write config)
        (let ((target-directory (cond ((assv  'target-directory config) => cadr ))));末尾に/を許容する?
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

