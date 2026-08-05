(define-library (md transform)
  (import (scheme base)
          (md url))
  (export add-header-ids)

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

    (define (add-header-ids sxml)
      (cond
        ((not (pair? sxml)) sxml)
        ((eq? (car sxml) '@) sxml)
        ((header-tag? (car sxml)) (add-id sxml))
        (else (map add-header-ids sxml))))))
