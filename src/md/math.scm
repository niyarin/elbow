(define-library (md math)
  (import (scheme base) (scheme char))
  (export tex->mathml-sxml)

  (begin

    (define greek-letters
      '(("alpha" . "α") ("beta" . "β") ("gamma" . "γ") ("delta" . "δ")
        ("epsilon" . "ε") ("varepsilon" . "ε") ("zeta" . "ζ") ("eta" . "η")
        ("theta" . "θ") ("vartheta" . "ϑ") ("iota" . "ι") ("kappa" . "κ")
        ("lambda" . "λ") ("mu" . "μ") ("nu" . "ν") ("xi" . "ξ")
        ("pi" . "π") ("rho" . "ρ") ("sigma" . "σ") ("varsigma" . "ς")
        ("tau" . "τ") ("upsilon" . "υ") ("phi" . "φ") ("varphi" . "φ")
        ("chi" . "χ") ("psi" . "ψ") ("omega" . "ω")
        ("Gamma" . "Γ") ("Delta" . "Δ") ("Theta" . "Θ") ("Lambda" . "Λ")
        ("Xi" . "Ξ") ("Pi" . "Π") ("Sigma" . "Σ") ("Upsilon" . "Υ")
        ("Phi" . "Φ") ("Psi" . "Ψ") ("Omega" . "Ω")))

    (define trig-names
      '("sin" "cos" "tan" "sec" "csc" "cot"
        "arcsin" "arccos" "arctan" "sinh" "cosh" "tanh"))

    (define op-symbols
      '(("times" . "×") ("div" . "÷") ("pm" . "±") ("mp" . "∓") ("cdot" . "⋅")
        ("top" . "⊤") ("prime" . "′")
        ("to" . "→") ("rightarrow" . "→") ("leftarrow" . "←") ("gets" . "←")
        ("leftrightarrow" . "↔")
        ("Rightarrow" . "⇒") ("Leftarrow" . "⇐") ("Leftrightarrow" . "⇔")
        ("uparrow" . "↑") ("downarrow" . "↓")))

    (define limit-ops
      '(("sum" . "∑") ("prod" . "∏")))

    (define integral-ops
      '(("int" . "∫") ("oint" . "∮")))

    (define accents
      '(("hat" . "^") ("bar" . "‾") ("vec" . "⃗") ("tilde" . "˜")
        ("dot" . "˙") ("ddot" . "¨")))

    ;;; Tokenizer

    (define (read-alpha s i)
      (let loop ((j i))
        (if (and (< j (string-length s)) (char-alphabetic? (string-ref s j)))
            (loop (+ j 1))
            (values (substring s i j) j))))

    (define (read-digits s i)
      (let loop ((j i))
        (if (and (< j (string-length s))
                 (or (char-numeric? (string-ref s j)) (char=? (string-ref s j) #\.)))
            (loop (+ j 1))
            (values (substring s i j) j))))

    (define (tokenize s)
      (let loop ((i 0) (acc '()))
        (if (= i (string-length s))
            (reverse acc)
            (let ((c (string-ref s i)))
              (cond
                ((char-whitespace? c) (loop (+ i 1) acc))
                ((char=? c #\\)
                 (if (>= (+ i 1) (string-length s))
                     (reverse acc)
                     (let ((c2 (string-ref s (+ i 1))))
                       (if (char-alphabetic? c2)
                           (let-values (((name j) (read-alpha s (+ i 1))))
                             (loop j (cons (list 'command name) acc)))
                           (let ((sym (string c2)))
                             (loop (+ i 2)
                                   (cons (if (string=? sym "\\")
                                             '(newline "\\")
                                             (list 'command sym))
                                         acc)))))))
                ((char=? c #\{) (loop (+ i 1) (cons '(lbrace "{") acc)))
                ((char=? c #\}) (loop (+ i 1) (cons '(rbrace "}") acc)))
                ((char=? c #\^) (loop (+ i 1) (cons '(caret "^") acc)))
                ((char=? c #\_) (loop (+ i 1) (cons '(underscore "_") acc)))
                ((char=? c #\&) (loop (+ i 1) (cons '(ampersand "&") acc)))
                ((char-numeric? c)
                 (let-values (((num j) (read-digits s i)))
                   (loop j (cons (list 'number num) acc))))
                ((char-alphabetic? c)
                 (loop (+ i 1) (cons (list 'ident (string c)) acc)))
                (else
                 (loop (+ i 1) (cons (list 'op (string c)) acc))))))))

    (define (tok-type t) (car t))
    (define (tok-val t) (cadr t))

    ;;; SXML helpers

    (define (as-mrow nodes)
      (cond
        ((null? nodes) '(mrow))
        ((null? (cdr nodes)) (car nodes))
        (else `(mrow ,@nodes))))

    (define (fenced open nodes close)
      `(mrow (mo ,open) ,@nodes (mo ,close)))

    (define (sxml-text x)
      (cond
        ((string? x) x)
        ((pair? x) (apply string-append (map sxml-text (cdr x))))
        (else "")))

    (define (lookup key alist)
      (let ((r (assoc key alist))) (and r (cdr r))))

    ;;; Parser — each function returns (values sxml remaining-tokens)

    (define (parse-limits base tokens)
      (cond
        ((null? tokens) (values base tokens))
        ((eq? (tok-type (car tokens)) 'underscore)
         (let-values (((sub rem1) (parse-one-arg (cdr tokens))))
           (if (and (pair? rem1) (eq? (tok-type (car rem1)) 'caret))
               (let-values (((sup rem2) (parse-one-arg (cdr rem1))))
                 (values `(munderover ,base ,sub ,sup) rem2))
               (values `(munder ,base ,sub) rem1))))
        ((eq? (tok-type (car tokens)) 'caret)
         (let-values (((sup rem1) (parse-one-arg (cdr tokens))))
           (if (and (pair? rem1) (eq? (tok-type (car rem1)) 'underscore))
               (let-values (((sub rem2) (parse-one-arg (cdr rem1))))
                 (values `(munderover ,base ,sub ,sup) rem2))
               (values `(mover ,base ,sup) rem1))))
        (else (values base tokens))))

    (define (parse-expr tokens)
      (let loop ((rest tokens) (nodes '()))
        (if (or (null? rest) (eq? (tok-type (car rest)) 'rbrace))
            (values (reverse nodes) rest)
            (let-values (((node rem) (parse-atom rest)))
              (loop rem (cons node nodes))))))

    (define (parse-group tokens)
      (let-values (((nodes rem) (parse-expr tokens)))
        (if (and (pair? rem) (eq? (tok-type (car rem)) 'rbrace))
            (values nodes (cdr rem))
            (values nodes rem))))

    (define (parse-group-mrow tokens)
      (let-values (((nodes rem) (parse-group tokens)))
        (values (as-mrow nodes) rem)))

    (define (parse-one-arg tokens)
      (if (and (pair? tokens) (eq? (tok-type (car tokens)) 'lbrace))
          (parse-group-mrow (cdr tokens))
          (parse-atom tokens)))

    (define (parse-scripts base tokens)
      (cond
        ((null? tokens) (values base tokens))
        ((eq? (tok-type (car tokens)) 'caret)
         (let-values (((sup rem1) (parse-one-arg (cdr tokens))))
           (if (and (pair? rem1) (eq? (tok-type (car rem1)) 'underscore))
               (let-values (((sub rem2) (parse-one-arg (cdr rem1))))
                 (values `(msubsup ,base ,sub ,sup) rem2))
               (values `(msup ,base ,sup) rem1))))
        ((eq? (tok-type (car tokens)) 'underscore)
         (let-values (((sub rem1) (parse-one-arg (cdr tokens))))
           (if (and (pair? rem1) (eq? (tok-type (car rem1)) 'caret))
               (let-values (((sup rem2) (parse-one-arg (cdr rem1))))
                 (values `(msubsup ,base ,sub ,sup) rem2))
               (values `(msub ,base ,sub) rem1))))
        (else (values base tokens))))

    (define (parse-atom tokens)
      (if (null? tokens)
          (values '(mrow) '())
          (let ((t (car tokens)) (rest (cdr tokens)))
            (let-values (((base rem)
                          (case (tok-type t)
                            ((number)  (values `(mn ,(tok-val t)) rest))
                            ((ident)   (values `(mi ,(tok-val t)) rest))
                            ((op)      (values `(mo ,(tok-val t)) rest))
                            ((command) (parse-command (tok-val t) rest))
                            ((lbrace)  (parse-group-mrow rest))
                            (else      (values '(mrow) rest)))))
              (parse-scripts base rem)))))

    (define (parse-frac tokens)
      (let-values (((num rem1) (parse-one-arg tokens)))
        (let-values (((den rem2) (parse-one-arg rem1)))
          (values `(mfrac ,num ,den) rem2))))

    (define (parse-sqrt tokens)
      (if (and (pair? tokens)
               (eq? (tok-type (car tokens)) 'op)
               (string=? (tok-val (car tokens)) "["))
          (let loop ((rest (cdr tokens)) (idx '()))
            (cond
              ((null? rest) (values '(msqrt (mrow)) '()))
              ((and (eq? (tok-type (car rest)) 'op)
                    (string=? (tok-val (car rest)) "]"))
               (let-values (((idx-nodes _) (parse-expr (reverse idx))))
                 (let-values (((arg rem) (parse-one-arg (cdr rest))))
                   (values `(mroot ,arg ,(as-mrow idx-nodes)) rem))))
              (else (loop (cdr rest) (cons (car rest) idx)))))
          (let-values (((arg rem) (parse-one-arg tokens)))
            (values `(msqrt ,arg) rem))))

    (define (parse-accent name tokens)
      (let-values (((arg rem) (parse-one-arg tokens)))
        (values `(mover ,arg (mo ,(lookup name accents))) rem)))

    (define (bracket-char token)
      (case (tok-type token)
        ((op) (tok-val token))
        ((command)
         (cond ((string=? (tok-val token) ".") "")
               ((string=? (tok-val token) "|") "‖")
               (else (tok-val token))))
        (else "(")))

    (define (parse-norm tokens)
      (let loop ((rest tokens) (nodes '()))
        (cond
          ((null? rest)
           (values `(mo "‖") tokens))
          ((and (eq? (tok-type (car rest)) 'command)
                (string=? (tok-val (car rest)) "|"))
           (values (fenced "‖" (reverse nodes) "‖") (cdr rest)))
          (else
           (let-values (((node rem) (parse-atom rest)))
             (loop rem (cons node nodes)))))))

    (define (parse-left tokens)
      (if (null? tokens)
          (values '(mo "(") '())
          (let ((open (bracket-char (car tokens))))
            (let loop ((rest (cdr tokens)) (nodes '()))
              (cond
                ((null? rest)
                 (values (fenced open (reverse nodes) ")") '()))
                ((and (eq? (tok-type (car rest)) 'command)
                      (string=? (tok-val (car rest)) "right"))
                 (if (null? (cdr rest))
                     (values (fenced open (reverse nodes) ")") '())
                     (values (fenced open (reverse nodes) (bracket-char (cadr rest)))
                             (cddr rest))))
                (else
                 (let-values (((node rem) (parse-atom rest)))
                   (loop rem (cons node nodes)))))))))

    (define (parse-begin tokens)
      (let-values (((env-node rem) (parse-one-arg tokens)))
        (parse-matrix-env (sxml-text env-node) rem)))

    (define (parse-matrix-env name tokens)
      (let loop ((rest tokens) (rows '()) (cur-row '()) (cur-cell '()))
        (cond
          ((null? rest)
           (let* ((last-cell (as-mrow (reverse cur-cell)))
                  (last-row  (reverse (cons last-cell cur-row)))
                  (all-rows  (reverse (cons last-row rows))))
             (values (matrix-sxml name all-rows) '())))
          ((and (eq? (tok-type (car rest)) 'command)
                (string=? (tok-val (car rest)) "end"))
           (let-values (((env-node rem) (parse-one-arg (cdr rest))))
             (let* ((last-cell (as-mrow (reverse cur-cell)))
                    (last-row  (reverse (cons last-cell cur-row)))
                    (all-rows  (reverse (cons last-row rows))))
               (values (matrix-sxml name all-rows) rem))))
          ((eq? (tok-type (car rest)) 'ampersand)
           (loop (cdr rest) rows (cons (as-mrow (reverse cur-cell)) cur-row) '()))
          ((eq? (tok-type (car rest)) 'newline)
           (let* ((last-cell    (as-mrow (reverse cur-cell)))
                  (finished-row (reverse (cons last-cell cur-row))))
             (loop (cdr rest) (cons finished-row rows) '() '())))
          (else
           (let-values (((node rem) (parse-atom rest)))
             (loop rem rows cur-row (cons node cur-cell)))))))

    (define (matrix-sxml name rows)
      (let ((table `(mtable ,@(map (lambda (row)
                                     `(mtr ,@(map (lambda (c) `(mtd ,c)) row)))
                                   rows))))
        (cond
          ((string=? name "pmatrix") (fenced "(" (list table) ")"))
          ((string=? name "bmatrix") (fenced "[" (list table) "]"))
          (else table))))

    (define (parse-command name tokens)
      (cond
        ((lookup name greek-letters) =>
         (lambda (ch) (values `(mi ,ch) tokens)))
        ((member name trig-names)
         (values `(mi ,name) tokens))
        ((lookup name op-symbols) =>
         (lambda (sym) (values `(mo ,sym) tokens)))
        ((lookup name limit-ops) =>
         (lambda (sym) (parse-limits `(mo ,sym) tokens)))
        ((lookup name integral-ops) =>
         (lambda (sym) (values `(mo ,sym) tokens)))
        ((lookup name accents)
         (parse-accent name tokens))
        ((string=? name "|")     (parse-norm tokens))
        ((string=? name "frac")  (parse-frac tokens))
        ((string=? name "sqrt")  (parse-sqrt tokens))
        ((string=? name "left")  (parse-left tokens))
        ((string=? name "right") (values '(mo ")") tokens))
        ((string=? name "begin") (parse-begin tokens))
        ((string=? name ",")     (values '(mspace (@ (width "0.167em"))) tokens))
        ((string=? name ";")     (values '(mspace (@ (width "0.278em"))) tokens))
        ((string=? name "quad")  (values '(mspace (@ (width "1em"))) tokens))
        ((string=? name "qquad") (values '(mspace (@ (width "2em"))) tokens))
        (else                    (values `(mi ,name) tokens))))

    (define (tex->mathml-sxml tex display?)
      (let-values (((nodes _) (parse-expr (tokenize tex))))
        `(math (@ (xmlns "http://www.w3.org/1998/Math/MathML")
                  (display ,(if display? "block" "inline")))
               ,@nodes)))))
