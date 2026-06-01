(define-library (md core)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme file))

  (export markdown->sxml
          markdown-string->sxml
          markdown-file->sxml)

  (begin
    (define (char-space? c)
      (or (char=? c #\space)
          (char=? c #\tab)
          (char=? c #\newline)
          (char=? c #\return)))

    (define (string-prefix-at? s prefix index)
      (let ((s-len (string-length s))
            (prefix-len (string-length prefix)))
        (and (<= (+ index prefix-len) s-len)
             (let loop ((i 0))
               (cond
                 ((= i prefix-len) #t)
                 ((char=? (string-ref s (+ index i)) (string-ref prefix i))
                  (loop (+ i 1)))
                 (else #f))))))

    (define (string-index-from s ch start)
      (let loop ((i start))
        (cond
          ((= i (string-length s)) #f)
          ((char=? (string-ref s i) ch) i)
          (else (loop (+ i 1))))))

    (define (string-contains-from s needle start)
      (let ((needle-len (string-length needle)))
        (let loop ((i start))
          (cond
            ((> (+ i needle-len) (string-length s)) #f)
            ((string-prefix-at? s needle i) i)
            (else (loop (+ i 1)))))))

    (define (string-rstrip s)
      (let loop ((end (string-length s)))
        (cond
          ((zero? end) "")
          ((char-space? (string-ref s (- end 1)))
           (loop (- end 1)))
          (else (substring s 0 end)))))

    (define (string-lstrip s)
      (let loop ((start 0))
        (cond
          ((= start (string-length s)) "")
          ((char-space? (string-ref s start))
           (loop (+ start 1)))
          (else (substring s start (string-length s))))))

    (define (string-trim s)
      (string-rstrip (string-lstrip s)))

    (define (blank-line? s)
      (string=? (string-trim s) ""))

    (define (split-lines s)
      (let ((len (string-length s)))
        (let loop ((i 0)
                   (start 0)
                   (lines '()))
          (cond
            ((= i len)
             (reverse (cons (substring s start i) lines)))
            ((char=? (string-ref s i) #\newline)
             (let ((end (if (and (< start i)
                                 (char=? (string-ref s (- i 1)) #\return))
                            (- i 1)
                            i)))
               (loop (+ i 1) (+ i 1) (cons (substring s start end) lines))))
            (else (loop (+ i 1) start lines))))))

    (define (join-lines lines)
      (let loop ((lines lines)
                 (out ""))
        (cond
          ((null? lines) out)
          ((string=? out "") (loop (cdr lines) (car lines)))
          (else (loop (cdr lines) (string-append out "\n" (car lines)))))))

    (define (join-lines-with-space lines)
      (let loop ((lines lines)
                 (out ""))
        (cond
          ((null? lines) out)
          ((string=? out "") (loop (cdr lines) (string-trim (car lines))))
          (else
           (loop (cdr lines)
                 (string-append out " " (string-trim (car lines))))))))

    (define (read-port-string port)
      (let loop ((chars '()))
        (let ((c (read-char port)))
          (if (eof-object? c)
              (list->string (reverse chars))
              (loop (cons c chars))))))

    (define (count-leading s ch)
      (let loop ((i 0))
        (if (and (< i (string-length s))
                 (char=? (string-ref s i) ch))
            (loop (+ i 1))
            i)))

    (define (heading-line? line)
      (let ((level (count-leading line #\#)))
        (and (<= 1 level)
             (<= level 6)
             (or (= level (string-length line))
                 (char-space? (string-ref line level))))))

    (define (heading->sxml line)
      (let* ((level (count-leading line #\#))
             (body (string-trim (substring line level (string-length line))))
             (body-len (string-length body))
             (clean-body
              (let loop ((end body-len))
                (cond
                  ((zero? end) "")
                  ((char=? (string-ref body (- end 1)) #\#)
                   (loop (- end 1)))
                  (else (string-rstrip (substring body 0 end)))))))
        (cons (string->symbol (string-append "h" (number->string level)))
              (parse-inline clean-body))))

    (define (fence-line? line)
      (let ((trimmed (string-lstrip line)))
        (or (string-prefix-at? trimmed "```" 0)
            (string-prefix-at? trimmed "~~~" 0))))

    (define (fence-marker line)
      (let ((trimmed (string-lstrip line)))
        (substring trimmed 0 3)))

    (define (fence-info line)
      (let ((trimmed (string-lstrip line)))
        (string-trim (substring trimmed 3 (string-length trimmed)))))

    (define (blockquote-line? line)
      (let ((trimmed (string-lstrip line)))
        (and (< 0 (string-length trimmed))
             (char=? (string-ref trimmed 0) #\>))))

    (define (strip-blockquote-marker line)
      (let* ((trimmed (string-lstrip line))
             (rest (substring trimmed 1 (string-length trimmed))))
        (if (and (< 0 (string-length rest))
                 (char=? (string-ref rest 0) #\space))
            (substring rest 1 (string-length rest))
            rest)))

    (define (unordered-list-line? line)
      (let ((trimmed (string-lstrip line)))
        (and (<= 2 (string-length trimmed))
             (let ((c (string-ref trimmed 0)))
               (and (or (char=? c #\-)
                        (char=? c #\*)
                        (char=? c #\+))
                    (char-space? (string-ref trimmed 1)))))))

    (define (ordered-list-line? line)
      (let ((trimmed (string-lstrip line)))
        (let loop ((i 0))
          (cond
            ((= i (string-length trimmed)) #f)
            ((char-numeric? (string-ref trimmed i))
             (loop (+ i 1)))
            (else
                  (and (< 0 i)
                       (< (+ i 1) (string-length trimmed))
                       (char=? (string-ref trimmed i) #\.)
                  (char-space? (string-ref trimmed (+ i 1)))))))))

    (define (list-item-text line ordered?)
      (let ((trimmed (string-lstrip line)))
        (if ordered?
            (let loop ((i 0))
              (if (char=? (string-ref trimmed i) #\.)
                  (string-trim
                   (substring trimmed (+ i 1) (string-length trimmed)))
                  (loop (+ i 1))))
            (string-trim (substring trimmed 1 (string-length trimmed))))))

    (define (start-block? line)
      (or (blank-line? line)
          (heading-line? line)
          (fence-line? line)
          (blockquote-line? line)
          (unordered-list-line? line)
          (ordered-list-line? line)))

    (define (parse-fenced-code lines)
      (let* ((marker (fence-marker (car lines)))
             (info (fence-info (car lines))))
        (let loop ((rest (cdr lines))
                   (body '()))
          (cond
            ((null? rest)
             (values
              (make-code-block info (join-lines (reverse body)))
              '()))
            ((string-prefix-at? (string-lstrip (car rest)) marker 0)
             (values
              (make-code-block info (join-lines (reverse body)))
              (cdr rest)))
            (else (loop (cdr rest) (cons (car rest) body)))))))

    (define (make-code-block info body)
      (if (string=? info "")
          `(pre (code ,body))
          `(pre (code (@ (class ,(string-append "language-" info))) ,body))))

    (define (parse-blockquote lines)
      (let loop ((rest lines)
                 (quote-lines '()))
        (if (and (not (null? rest))
                 (blockquote-line? (car rest)))
            (loop (cdr rest)
                  (cons (strip-blockquote-marker (car rest)) quote-lines))
            (values
             (cons 'blockquote (parse-blocks (reverse quote-lines)))
             rest))))

    (define (parse-list lines ordered?)
      (let ((line? (if ordered? ordered-list-line? unordered-list-line?)))
        (let loop ((rest lines)
                   (items '()))
          (if (and (not (null? rest))
                   (line? (car rest)))
              (loop (cdr rest)
                    (cons (cons 'li
                                (parse-inline
                                 (list-item-text (car rest) ordered?)))
                          items))
              (values
               (cons (if ordered? 'ol 'ul) (reverse items))
               rest)))))

    (define (parse-paragraph lines)
      (let loop ((rest lines)
                 (paragraph-lines '()))
        (if (or (null? rest)
                (start-block? (car rest)))
            (values
             (cons 'p
                   (parse-inline
                    (join-lines-with-space (reverse paragraph-lines))))
             rest)
            (loop (cdr rest) (cons (car rest) paragraph-lines)))))

    (define (parse-blocks lines)
      (let loop ((rest lines)
                 (blocks '()))
        (cond
          ((null? rest) (reverse blocks))
          ((blank-line? (car rest)) (loop (cdr rest) blocks))
          ((heading-line? (car rest))
           (loop (cdr rest) (cons (heading->sxml (car rest)) blocks)))
          ((fence-line? (car rest))
           (let-values (((block next-rest) (parse-fenced-code rest)))
             (loop next-rest (cons block blocks))))
          ((blockquote-line? (car rest))
           (let-values (((block next-rest) (parse-blockquote rest)))
             (loop next-rest (cons block blocks))))
          ((unordered-list-line? (car rest))
           (let-values (((block next-rest) (parse-list rest #f)))
             (loop next-rest (cons block blocks))))
          ((ordered-list-line? (car rest))
           (let-values (((block next-rest) (parse-list rest #t)))
             (loop next-rest (cons block blocks))))
          (else
           (let-values (((block next-rest) (parse-paragraph rest)))
             (loop next-rest (cons block blocks)))))))

    (define (flush-text text out)
      (if (string=? text "")
          out
          (cons text out)))

    (define (parse-bracket-link s start)
      (let ((close-text (string-index-from s #\] (+ start 1))))
        (and close-text
             (< (+ close-text 1) (string-length s))
             (char=? (string-ref s (+ close-text 1)) #\()
             (let ((close-url (string-index-from s #\) (+ close-text 2))))
               (and close-url
                    (list
                     (substring s (+ start 1) close-text)
                     (substring s (+ close-text 2) close-url)
                     (+ close-url 1)))))))

    (define (parse-inline s)
      (let loop ((i 0)
                 (text "")
                 (out '()))
        (cond
          ((= i (string-length s))
           (reverse (flush-text text out)))

          ((char=? (string-ref s i) #\`)
           (let ((end (string-index-from s #\` (+ i 1))))
             (if end
                 (loop (+ end 1)
                       ""
                       (cons `(code ,(substring s (+ i 1) end))
                             (flush-text text out)))
                 (loop (+ i 1)
                       (string-append text "`")
                       out))))

          ((and (string-prefix-at? s "![" i)
                (parse-bracket-link s (+ i 1)))
           => (lambda (parsed)
                (let ((alt (car parsed))
                      (url (cadr parsed))
                      (next (caddr parsed)))
                  (loop next
                        ""
                        (cons `(img (@ (src ,url) (alt ,alt)))
                              (flush-text text out))))))

          ((and (char=? (string-ref s i) #\[)
                (parse-bracket-link s i))
           => (lambda (parsed)
                (let ((label (car parsed))
                      (url (cadr parsed))
                      (next (caddr parsed)))
                  (loop next
                        ""
                        (cons `(a (@ (href ,url)) ,@(parse-inline label))
                              (flush-text text out))))))

          ((and (string-prefix-at? s "**" i)
                (string-contains-from s "**" (+ i 2)))
           => (lambda (end)
                (loop (+ end 2)
                      ""
                      (cons `(strong ,@(parse-inline
                                        (substring s (+ i 2) end)))
                            (flush-text text out)))))

          ((and (string-prefix-at? s "__" i)
                (string-contains-from s "__" (+ i 2)))
           => (lambda (end)
                (loop (+ end 2)
                      ""
                      (cons `(strong ,@(parse-inline
                                        (substring s (+ i 2) end)))
                            (flush-text text out)))))

          ((and (char=? (string-ref s i) #\*)
                (string-index-from s #\* (+ i 1)))
           => (lambda (end)
                (loop (+ end 1)
                      ""
                      (cons `(em ,@(parse-inline
                                    (substring s (+ i 1) end)))
                            (flush-text text out)))))

          ((and (char=? (string-ref s i) #\_)
                (string-index-from s #\_ (+ i 1)))
           => (lambda (end)
                (loop (+ end 1)
                      ""
                      (cons `(em ,@(parse-inline
                                    (substring s (+ i 1) end)))
                            (flush-text text out)))))

          (else
           (loop (+ i 1)
                 (string-append text (string (string-ref s i)))
                 out)))))

    (define (markdown-string->sxml markdown)
      (cons '*TOP* (parse-blocks (split-lines markdown))))

    (define markdown->sxml markdown-string->sxml)

    (define (markdown-file->sxml path)
      (call-with-input-file path
        (lambda (port)
          (markdown-string->sxml (read-port-string port)))))))
