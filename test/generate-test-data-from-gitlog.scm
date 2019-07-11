(include "../src/elbow_init.scm")

(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme read)
        (srfi 152)
        (elbow init)
        )

;git log からテスト用データを生成するスクリプト

;usage
;git log --date=short|<scheme> <this script>

(define (get-first-word input)
  (let loop ((i 0))
    (cond 
      ((= (string-length input) i) input)
      ((char=? (string-ref input i) #\space)
       (substring input 0 i))
      (else (loop (+ i 1))))))

(define (gitgen-main)
  (let loop ((current-mode '())
             (current-tag '())
             (current-date "")
             (current-title "")
             (current-text ""))
    (let ((rline (read-line)))
      (cond 
        ((or (eof-object? rline) (and (zero? (string-length rline)) (eq? current-mode 'in-message)))

         (call-with-output-file
           (string-append "./tmp/gittest/contents/" current-title ".elbow")
           (lambda (port)
             (write
               (list
                 (list '*contents-tags* current-tag)
                 (list '*contents-date* current-date)
                 (list '*contents-title* current-title)
                 (list '*contents-body* current-text))
               port)))
         (unless (eof-object? rline)
            (loop '() '() "" "" "")))
        ((string=? (get-first-word rline) "commit")
         (loop '() '() "" (get-first-word (substring rline 7 (string-length rline))) ""))
        ((string=? (get-first-word rline) "Author:")
         (loop 
           current-mode 
           (cons  (get-first-word (substring rline 8 (string-length rline))) current-tag)
           current-date
           current-title
           current-text))
        ((string=? (get-first-word rline) "Date:")
         (loop current-mode current-tag (substring rline 8 (string-length rline)) current-title current-text))

        ((and (zero? (string-length rline)) (not (eq? current-mode 'in-message)))
         (loop 'in-message current-tag current-date current-title current-text))
        ((zero? (string-length current-text))
         (loop 'in-message (cons (get-first-word (substring rline 4 (string-length rline))) current-tag) current-date current-title (substring rline 4 (string-length rline))))
        (else 
          (loop 'in-message current-tag current-date current-title (string-append current-text (substring rline 4 (string-length rline)))))
        )
      )))

(elbow-init-write (list (list "contents-directory" "./tmp/gittest")))

(gitgen-main)
