(define-module (riscv build-instruction-module)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:export (build-module))

;; 工具：获取路径的 basename（最后一个 / 之后的部分）
(define (basename path)
  (let ((last-slash (string-rindex path #\/)))
    (if last-slash
        (substring path (+ last-slash 1))
        path)))

;; 读取器：按行读取 RISC-V 指令文件，跳过注释和空行
(define (read-rv-instruction-file filename)
  "返回一个闭包，每次调用返回下一条有效指令行，EOF 时返回 #f"
  (if (not (file-exists? filename))
      (error "文件不存在:" filename)
      (let ((port (open-input-file filename)))
        (lambda ()
          (let loop ()
            (let ((line (read-line port)))
              (cond
               ((eof-object? line)
                (close-input-port port)
                #f)
               ((or (string-null? (string-trim-both line))
                    (string-prefix? "#" line)
                    (string-prefix? ";" line)
                    (string-prefix? "$pseudo_op" line))
                (loop))
               (else line))))))))

;; 判断字符串是否以数字开头
(define (starts-with-digit? str)
  (and (> (string-length str) 0)
       (char-numeric? (string-ref str 0))))

;; 判断是否是 range-fixed 表达式
(define (range-fixed? x)
  (and (list? x)
       (>= (length x) 1)
       (eq? (car x) 'range-fixed)))

;; 将指令行转换为 define 表达式，并返回 (opcode . s-expr)
(define (parse-rv-instruction-as-definition instr-str)
  (let* ((words (string-tokenize instr-str))
         (opcode (car words))
         (parts (cdr words)))
    (define (process-part part)
      (if (starts-with-digit? part)
          (let ((match (regexp-exec (make-regexp "^([0-9]+)(\\.\\.([0-9]+))?=(.+)$") part)))
            (if match
                (let* ((start (string->number (match:substring match 1)))
                       (end   (if (match:substring match 2)
                                  (string->number (match:substring match 3))
                                  start))
                       (value-str (match:substring match 4))
                       (value (if (string-prefix? "0x" value-str)
                                  (string->number (substring value-str 2) 16)
                                  (string->number value-str))))
                  `(range-fixed ,start ,end ,value))
                (error "无法解析位域定义:" part)))
          (cond
            ((string=? part "imm12hi") 'simm12)
            ((string=? part "imm12lo") #f)
            ((and (string-suffix? "lo" part)
                  (> (string-length part) 2))
             (string->symbol (substring part 0 (- (string-length part) 2))))
            ((string-suffix? "hi" part)
             #f)
            (else (string->symbol part)))))
    (let ((processed-parts (filter (lambda (x) x) (map process-part parts))))
      ;; 分离字段名和 range-fixed 表达式
      (let* ((non-fixed (filter (lambda (x) (not (range-fixed? x))) processed-parts))
             (fixed     (filter range-fixed? processed-parts))
             (sorted-parts (append non-fixed fixed))
             (def `(define ,(string->symbol opcode)
                     (make-instruction ,@sorted-parts))))
        (cons (string->symbol opcode) def)))))

;; 主函数：生成完整的模块文件（覆盖写）
(define (build-module input-file output-file)
  "从 input-file 读取 RISC-V 指令定义，生成完整的 (define-module ...) 文件并写入 output-file（覆盖模式）"
  (let* ((next-instruction (read-rv-instruction-file input-file))
         (instructions
           (let loop ((acc '()))
             (let ((instr (next-instruction)))
               (if instr
                   (loop (cons (parse-rv-instruction-as-definition instr) acc))
                   (reverse acc)))))
         (opcodes (map car instructions))
         (definitions (map cdr instructions)) ; 提取定义部分
         (basename (basename input-file)))

    (let ((out-port (open-output-file output-file)))
      ;; 正确的模块声明：开始 define-module，不立即闭合
      (display "(define-module (riscv instruction " out-port)
      (display basename out-port)
      (display ")\n" out-port) ; 模块名后换行，开始子句

      (display "  #:use-module (riscv build-base)\n" out-port)
      (display "  #:use-module (riscv range-base)\n" out-port)

      ;; 导出列表
      (if (null? opcodes)
          (display "  #:export ()\n" out-port)
          (begin
            (display "  #:export (" out-port)
            (let loop ((ops opcodes))
              (display (symbol->string (car ops)) out-port)
              (if (null? (cdr ops))
                  (display ")\n" out-port)
                  (begin
                    (display " " out-port)
                    (loop (cdr ops))))) ; 使用 display 而非 write 以避免引号
            ))

      ;; 闭合 define-module 表达式
      (display ")\n\n" out-port)

      ;; 指令定义
      (for-each
        (lambda (def)
          (write def out-port)
          (newline out-port))
        definitions)

      (close-output-port out-port))))