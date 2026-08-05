(define-library (md url)
  (import (scheme base)
          (scheme char))
  (export percent-encode)

  (begin
    (define hex-chars "0123456789ABCDEF")

    (define (byte->percent-string b)
      (string #\%
              (string-ref hex-chars (quotient b 16))
              (string-ref hex-chars (remainder b 16))))

    (define (encode-bytes bv)
      (let loop ((i 0) (acc ""))
        (if (= i (bytevector-length bv))
            acc
            (loop (+ i 1)
                  (string-append acc (byte->percent-string (bytevector-u8-ref bv i)))))))

    (define (url-safe-char? c)
      (or (char-alphabetic? c)
          (char-numeric? c)
          (char=? c #\-)
          (char=? c #\_)
          (char=? c #\.)
          (char=? c #\~)))

    (define (percent-encode s)
      (let loop ((i 0) (acc ""))
        (if (= i (string-length s))
            acc
            (let ((c (string-ref s i)))
              (loop (+ i 1)
                    (if (url-safe-char? c)
                        (string-append acc (string c))
                        (string-append acc (encode-bytes (string->utf8 (string c))))))))))))
