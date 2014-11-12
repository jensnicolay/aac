(define (↓ m s)
  (let loop ((s s) (r (hash)))
    (if (set-empty? s)
        r
        (let ((key (set-first s)))
          (if (hash-has-key? m key)
              (loop (set-rest s) (hash-set r key (hash-ref m key)))
              (loop (set-rest s) r))))))

(define (value->file value file)
  (let ((out (open-output-file file #:exists 'replace)))
    (write value out)
    (close-output-port out)))
