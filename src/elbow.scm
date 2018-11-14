(include "niyarin_optparse/niyarin_optparse.scm")
(include "elbow_init.scm")

(import (scheme base)
        (scheme write)
        (niyarin optparse)
        (elbow init))

(define version 'beta)

(define arg-config
  '(("--help" "-h" (help "Display a help message and exit."))
    ("command" (nargs 1) (default "none") (help ""))
    ("options" (nargs *))))

(define (print-help)
  (display
    (niyarin-optparse-generate-help-text arg-config '(program-name "Elbow"))))

(define (print-version)
  (display version)(newline))

(define (elbow-main)
  (let ((parsed-option (niyarin-optparse-optparse arg-config)))
    (let ((command (cond ((assoc "command" parsed-option) => cadr))))
       (cond 
         ((assoc "--help" parsed-option string=?) (print-help))
         ((assoc "--version" parsed-option string=?) (print-version))
         ((string=? command "init") (elbow-init parsed-option))
         (else (error (string-append "Unknown command " command ".")))))))

(elbow-main)
