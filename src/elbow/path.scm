(define-library (elbow path)
  (import (scheme base) (srfi 152))
  (export dotted-filename? filename-only file-extension core-filename)
  (begin
    (define (dotted-filename? file-name)
       (char=? #\. (string-ref file-name 0)))

    (define (filename-only file-path)
      ;;エスケープは考慮していない
      (let ((index (string-contains-right file-path "/")))
        (if index
          (substring file-path (+ index 1) (string-length file-path))
          file-path)))

    (define (file-extension file-name)
      (let ((index (string-contains-right file-name ".")))
        (if index
          (substring file-name (+ index 1) (string-length file-name))
          "")))

    (define (core-filename filepath)
      (let* ((filename-only (filename-only filepath))
             (index (string-contains filename-only ".")))
        (if index
          (substring filename-only 0 index)
          filename-only)))))

