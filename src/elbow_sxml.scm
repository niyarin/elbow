(include "./sxml_scheme/sxml.scm")

(define-library (elbow sxml)
   (import (scheme base)
           (scheme set)
           (scheme eval)
           (scheme write)
           (niyarin sxml))

   (export elbow-sxml-generate-html)

   (begin
      (define elbow-sxml-contents-vars
        '(*contents-tags-and-links* *contents-date* *contents-title* *contents-prev-page-link* *contents-next-page-link*))

      (define (elbow-sxml-make-default-environment)
        (let ((eval-env (environment '(scheme base) '(scheme cxr) '(scheme write)))
              (convert-env (set 'eq? 'begin)))

          (eval '(define *site-title "Title") eval-env)

          (list 
            (list 'eval-env eval-env)
            (list 'convert-env convert-env)
            (list 'contains? set-contains? ))))

      (define (elbow-sxml-generate-html template env env-contents . opt)
        (let* ((expand-env (elbow-sxml-make-default-environment))
               (contains? (cadr (assq 'contains? expand-env)))
               (eval-env (cadr (assq 'eval-env expand-env)))
               (convert-env (cadr (assq 'convert-env expand-env))))
          
           (for-each
             (lambda (name)
               (eval (list 'define name '()) eval-env))
             elbow-sxml-contents-vars)

           (for-each ;ここは毎回は不要
             (lambda (apair)
               (set-replace! convert-env (car apair))
               (eval 
                 (list 'define (car apair) (list 'quote (cadr apair)))
                 eval-env))
             env)

           (for-each 
             (lambda (apair)
               (set-replace! convert-env (car apair))
               (eval 
                 (list 'define (car apair) (list 'quote (cadr apair)))
                 eval-env))
             (reverse env-contents));TODO:あとからきたもので上書きされるのでてきとーに対処　跡で治す

          (sxml->xml-string 
               template
               expand-env)))))

