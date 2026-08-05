(define-library (elbow load-markdown)
  (import (scheme base)
          (scheme eval)
          (scheme set)
          (md core)
          (md transform))

  (export install-load-markdown!)

  (begin
    (define (absolute-path? path)
      (and (< 0 (string-length path))
           (char=? (string-ref path 0) #\/)))

    (define (directory-name path)
      (let loop ((i (- (string-length path) 1)))
        (cond
          ((< i 0) ".")
          ((char=? (string-ref path i) #\/)
           (if (zero? i)
               "/"
               (substring path 0 i)))
          (else (loop (- i 1))))))

    (define (join-path directory filename)
      (cond
        ((absolute-path? filename) filename)
        ((or (string=? directory "")
             (string=? directory "."))
         filename)
        ((char=? (string-ref directory (- (string-length directory) 1)) #\/)
         (string-append directory filename))
        (else
         (string-append directory "/" filename))))

    (define (base-directory env-contents)
      (cond
        ((assq '*contents-filepath* env-contents)
         => (lambda (apair) (directory-name (cadr apair))))
        (else ".")))

    (define (make-load-markdown env-contents)
      (let ((base (base-directory env-contents)))
        (lambda (filename)
          (markdown-sxml-post-process (markdown-file->sxml (join-path base filename))))))

    (define (install-load-markdown! eval-env convert-env env-contents)
      (eval
        (list 'define 'load-markdown (make-load-markdown env-contents))
        eval-env)
      (set-adjoin! convert-env 'load-markdown))))
