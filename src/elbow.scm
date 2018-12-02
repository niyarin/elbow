(include "niyarin_optparse/niyarin_optparse.scm")
(include "elbow_init.scm")
(include "elbow_full_build.scm")

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (niyarin optparse)
        (elbow init)
        (elbow full build))

(define version 'beta)

(define arg-config
  '(("--help" "-h" (help "Display a help message and exit."))
    ("command" (nargs 1) (default "none") (help ""))
    ("options" (nargs *))))

(define (print-help . port-opt)
  (let ((port (if (null? port-opt) (current-error-port) (car port-opt))))
     (display
       (niyarin-optparse-generate-help-text arg-config '(program-name "Elbow")))))

(define (print-version)
  (display version)(newline))

(define (elbow-main)
  (let ((parsed-option (niyarin-optparse-optparse arg-config)))
    (let ((command (cond ((assoc "command" parsed-option) => cadr))))
       (cond 
         ((assoc "--help" parsed-option string=?) (print-help))
         ((assoc "--version" parsed-option string=?) (print-version))
         ((string=? command "init") (elbow-init parsed-option))
         ((string=? command "full-build") (elbow-full-build-cmd-opt parsed-option))
         ((string=? command "none") 
             (display "Elbow error: no command.\n" (current-error-port)) 
             (newline (current-error-port))
             (print-help)
             (exit #f))
         (else (error (string-append "Unknown command " command ".")))))))

(elbow-main)
