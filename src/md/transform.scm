(define-library (md transform)
  (import (scheme base)
          (md url))
  (export add-header-ids
          add-h2-anchor-links
          markdown-sxml-post-process)

  (begin
    (define header-tags '(h1 h2 h3))

    (define (header-tag? sym)
      (and (symbol? sym) (if (memq sym header-tags) #t #f)))

    (define (sxml-text node)
      (cond
        ((string? node) node)
        ((not (pair? node)) "")
        ((eq? (car node) '@) "")
        (else (apply string-append (map sxml-text (cdr node))))))

    (define (add-id header)
      (let* ((tag (car header))
             (rest (cdr header))
             (has-attrs? (and (pair? rest)
                              (pair? (car rest))
                              (eq? (caar rest) '@)))
             (existing-attrs (if has-attrs? (cdar rest) '()))
             (children (if has-attrs? (cdr rest) rest))
             (id (percent-encode (apply string-append (map sxml-text children)))))
        `(,tag (@ (id ,id) ,@existing-attrs) ,@children)))

    (define (h2-id sxml)
      (and (pair? sxml)
           (eq? (car sxml) 'h2)
           (pair? (cdr sxml))
           (pair? (cadr sxml))
           (eq? (car (cadr sxml)) '@)
           (let ((id-pair (assq 'id (cdr (cadr sxml)))))
             (and id-pair (cadr id-pair)))))

    (define (add-anchor-link h2)
      (let* ((id (h2-id h2))
             (attrs (cadr h2))
             (children (cddr h2)))
        `(h2 ,attrs (a (@ (class "header-anchor-link") (href ,(string-append "#" id)) (aria-hidden "true"))) ,@children)))

    (define (add-h2-anchor-links sxml)
      (cond
        ((not (pair? sxml)) sxml)
        ((eq? (car sxml) '@) sxml)
        ((h2-id sxml) (add-anchor-link sxml))
        (else (map add-h2-anchor-links sxml))))

    (define (add-header-ids sxml)
      (cond
        ((not (pair? sxml)) sxml)
        ((eq? (car sxml) '@) sxml)
        ((header-tag? (car sxml)) (add-id sxml))
        (else (map add-header-ids sxml))))

    (define (markdown-sxml-post-process sxml)
      (add-h2-anchor-links (add-header-ids sxml)))))
