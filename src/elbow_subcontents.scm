(include "./elbow_markup.scm")
(include "./elbow_lib.scm")


(define-library (elbow subcontents)
   (import (scheme base)
           (scheme write)
            (scheme file)
           (elbow markup)
           (elbow lib))

   (export elbow-subcontents-create-sub-contents elbow-subcontents-tag-file-base-name)
   (begin 

     (define (elbow-subcontents-tag-file-base-name env-contents)
       (let ((root-dir (cadr (assv '*contents-root-relative-path*  env-contents))))
          (string-append root-dir "/tags/"  (elbow-lib-tag-escape (cadr (assv '*contents-tag-name* env-contents))))))

     ;コンテンツをまとめてページ(タグ、日付、など)を生成する　
     (define (elbow-subcontents-create-sub-contents base-name-generator template subcontents env env-contents output-dir contents-number-per-page)

       (define (elbow-subcontents-aux-title-text-thumbnail&tails ls n)
         (let loop ((i 0)(ls ls) (res '()))
           (cond 
             ((null? ls)
               (values (reverse res) ls))
             ((= i n)
               (values (reverse res) ls))
             (else 
               (loop
                  (+ i 1)
                  (cdr ls)
                  (cons
                    (list (cadr (assv '*contents-title* (car ls) ))
                          (cadr (assv '*contents-short-text* (car ls)))
                          "" ;thumbnail
                          (string-append 
                            (cadr (assq '*contents-root-relative-path* env-contents))
                            "/contents/"
                            (cadr (assq '*contents-sub-directory* (car ls)))
                            "/"
                            (cadr (assq '*contents-file-name* (car ls)))));link
                    res))))))

       (let* ((page-size (ceiling (/ (length subcontents) contents-number-per-page)))
              (root-dir (cadr (assv '*contents-root-relative-path*  env-contents)))
              (base-name (base-name-generator env-contents))
              (page-links
                (let loop ((i 0) (res-list '()))
                  (cond 
                    ((= i page-size)
                        (list->vector (reverse res-list)))
                    ((zero? i) 
                        (loop (+ i 1) (cons (string-append base-name ".html") res-list)))

                    (else
                      (loop (+ i 1) (cons (string-append base-name "_pages_" (number->string (+ i 1)) ".html") res-list))))))
              )
             (let loop ((i 0)(subcontents subcontents));ページに関するループ
               (let ((next-page (if (>= i (- page-size 1)) #f (vector-ref page-links (+ i 1))))
                     (prev-page (if (zero? i) #f (vector-ref page-links (- i 1))))
                     (env-contents env-contents)
                     (next-subcontents '() ))
                 (when (< i page-size)
                     (when next-page
                           (set! env-contents (cons (list '*contents-next-page-link* next-page) env-contents)))
                     (when prev-page
                           (set! env-contents (cons (list '*contents-prev-page-link* prev-page) env-contents)))


                     (let-values (((title-text-thumbnails-list tails) (elbow-subcontents-aux-title-text-thumbnail&tails subcontents contents-number-per-page)))
                        (set! next-subcontents tails)
                        (set! env-contents 
                              (cons
                                (list '*contents-subpages* title-text-thumbnails-list)
                                env-contents)))

                     (call-with-output-file 
                         (string-append output-dir "/" (cadr (assq '*contents-sub-directory* env-contents)) "/"(vector-ref page-links i) )
                         (lambda (port) 
                           (display 
                              (elbow-markup-convert-html template env env-contents)
                              port)))

                     (loop (+ i 1) next-subcontents))))
             ))))
