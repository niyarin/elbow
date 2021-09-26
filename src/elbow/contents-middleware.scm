(define-library (elbow contents-middleware)
  (import (scheme base)
          (scheme list)
          (scheme file) (scheme read)
          (prefix (elbow path) epath/))
  (export add-filename-key-middleware remove-dotted-file-middleware
          filter-elbow-file-middleware
          read-file-middleware)
  (begin
    (define (contents-ref key contents default)
      (cond
        ((assq key contents) => cadr)
        (else default)))

    (define (contents-add contents key val)
      (cons (list key val)
            contents))

    (define (remove-dotted-file-middleware contents-list global-env)
      (values
        (remove (lambda (content)
                               (epath/dotted-filename?
                                    (contents-ref '*contents-filename* content "")))
                              contents-list)
                    global-env))

    (define (filter-elbow-file-middleware contents-list global-env)
      (values
         (filter (lambda (content)
                              (member
                               (epath/file-extension
                                    (contents-ref '*contents-filename* content ""))
                               '("scm" "elbow")))
                              contents-list)
                    global-env))

    (define (add-filename-key-middleware contents-list global-env)
      (values
           (map (lambda (content)
                  (contents-add
                    content
                    '*contents-filename*
                    (epath/filename-only
                      (contents-ref '*contents-filepath* content ""))))
                contents-list)
           global-env))

    (define (read-file-middleware contents-list global-env)
      (values
        (map (lambda (content)
               (call-with-input-file
                  (contents-ref '*contents-filepath* content #f)
                  (lambda (port)
                       (append content
                               (read port)))))
             contents-list)
        global-env))))
