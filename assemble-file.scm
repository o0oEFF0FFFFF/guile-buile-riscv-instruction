(define-module (assemble-file)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:export (assemble-file))

(define (assemble-file in-path out-path)
  (call-with-input-file in-path
    (lambda (in)
      (call-with-output-file out-path
        (lambda (out)
          (let loop ()
            (let ((line (read-line in)))
              (unless (eof-object? line)
                (let ((code (string-trim line)))
                  (unless (string-null? code)
                    (let* ((s-expr (read (open-input-string (string-append "(" code ")"))))
                           (value   (eval s-expr (current-module))))
                      ;; 写小端 32 位整数：手动拆 4 字节
                      (put-u8 out (logand value #xff))
                      (put-u8 out (logand (ash value -8) #xff))
                      (put-u8 out (logand (ash value -16) #xff))
                      (put-u8 out (logand (ash value -24) #xff)))))
                (loop)))))))))