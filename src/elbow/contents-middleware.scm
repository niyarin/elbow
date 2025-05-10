(define-library (elbow contents-middleware)
  (import (scheme base)
          (scheme list)
          (scheme set)
          (scheme comparator)
          (only (scheme sort) list-sort)
          (only (srfi 95) sort)
          (scheme write);;
          (scheme file) (scheme read)
          (srfi 19)
          (prefix (elbow lib) elib/)
          (prefix (elbow path) epath/)
          (prefix (elbow utils date-tree) dtree/))
  (export add-filename-key-middleware remove-dotted-file-middleware
          filter-elbow-file-middleware
          read-file-middleware
          remove-draft-middleware
          add-tags-info-to-global-env-middleware
          calc-major-tags-middleware
          add-date-as-list-to-contents-middleware
          add-parsed-date-info-to-contents-middleware
          add-ids-to-contents-middleware
          add-output-name-middleware
          add-short-text-middleware 
          elbow-pipeline>)
  (begin
    (define-syntax elbow-pipeline>
      (syntax-rules ()
        ((_ first-middleware) first-middleware)
        ((_ first-middleware rest1 rest2 ...)
         (let-values (((new-contents-list new-env) first-middleware))
          (elbow-pipeline> (rest1 new-contents-list new-env) rest2 ...)))))

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
        global-env))

    (define (add-output-name-middleware contents-list global-env)
      (values
        (map (lambda (content)
               (contents-add content
                             '*contents-output-filename*
                             (string-append (epath/core-filename
                                              (contents-ref '*contents-filename* content #f)) ".html")))
             contents-list)
        global-env))

    (define (remove-draft-middleware contents-list global-env)
      (values
        (remove (lambda (content)
                  (contents-ref '*contents-draft* content #f))
             contents-list)
        global-env))

    (define (%tag-list contents-list)
      (append-map (lambda (content)
                        (contents-ref '*contents-tags* content '()))
                      contents-list))

    (define (%freq-equal ls);;make ((tag . n-articls) ... )
      (map (lambda (name)
             (cons name (count (lambda (x) (equal? x name)) ls)))
           (delete-duplicates ls)))

    (define (add-tags-info-to-global-env-middleware contents-list global-env)
      (let ((tags (list->set (make-equal-comparator)
                             (%tag-list contents-list))))
      (values contents-list
              (cons (list '*tag-names* tags) global-env))))

    (define (calc-major-tags-middleware contents-list global-env)
      (if (contents-ref '*major-tags* global-env #f)
        (values contents-list global-env)
        (let ((freq (list-sort (lambda (x y) (> (cdr x) (cdr y)))
                               (%freq-equal (%tag-list contents-list)))))
          (write freq)(newline)
          (values contents-list
                  (cons  (list '*major-tags* (map car (take freq (min (length freq) 5))))
                         global-env)))))


    (define (add-parsed-date-info-to-contents-middleware contents-list global-env)
      (values
        (map (lambda (content)
               (let ((date-object
                       (string->date (contents-ref '*contents-date* content "") "~Y-~m-~d")))
                 (cons (list '*date-object* date-object) content)))
             contents-list)
        global-env))

    (define (add-date-as-list-to-contents-middleware contents-list global-env)
      (values
        (map (lambda (content)
              (cons (list '*formatted-contents-date*
                          (cond ((assv '*contents-date* content)
                                  => (lambda (key-datestr)
                                       (dtree/elbow-date-tree-decompose-hyphen-date-string (cadr key-datestr))))
                                 (else '(0 0 0 0))))
                       content))
             contents-list)
        global-env))

    (define (add-ids-to-contents-middleware contents-list global-env)
      (let ((sorted-contents-list
              (sort contents-list
                    (lambda (a b)
                      (dtree/elbow-date-tree-less?
                        (contents-ref '*formatted-contents-date* a #f)
                        (contents-ref '*formatted-contents-date* b #f))))))
        (values (map (lambda (id content)
                       (contents-add content '*contents-id* id))
                     (iota (length sorted-contents-list))
                     sorted-contents-list)
                global-env)))

    (define (add-short-text-middleware contents-list global-env)
      (values (map (lambda (content)
                     (cons (list '*contents-short-text*
                                 (elib/elbow-lib-generate-short-text '() content 100))
                           content))
                   contents-list)
              global-env))))
