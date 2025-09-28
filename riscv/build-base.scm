(define-module (riscv build-base)
  #:export (range range-fixed range-split range-fixed-split make-instruction)
)

;; (range a b c d) — 从[a,b]移到[c,d]；(range x y) — 从位x移到位y
(define-syntax range
  (syntax-rules ()
    ((_ a b c d)
     (let* ((low-src (min a b))
            (high-src (max a b))
            (low-dst (min c d))
            (high-dst (max c d))
            (width (+ 1 (- high-src low-src)))
            (mask (1- (ash 1 width)))
            (src-mask (ash mask low-src)))
       (lambda (v)
         (let* ((field (logand v src-mask))
                (shifted-to-zero (ash field (- low-src)))
                (final (ash shifted-to-zero low-dst)))
           final))))
    ((_ x y)
     (lambda (v)
       (ash (logand (ash v (- x)) 1) y)))))

;; (range-fixed a b val) — 将val低(b-a+1)位写入[a,b]，无论a和b顺序如何
(define-syntax range-fixed
  (syntax-rules ()
    ((_ a b val)
     (let* ((small (min a b))    ; 确保small <= big
            (big (max a b))
            (h (+ (- big small) 1)))  ; 位域宽度
       (lambda (_)
         (ash (logand val (1- (ash 1 h))) small))))))

;; (range-split f1 f2 ...) — 合并多个动态位域（同输入变量）
(define-syntax range-split
  (syntax-rules ()
    ((_ f) (lambda (v) (f v)))
    ((_ f g ...)
     (lambda (v)
       (logior (f v)
               ((range-split g ...) v))))))

;; (range-fixed-split f1 f2 ... val) — 用val作为输入，合并多个(range ...)函数结果
(define-syntax range-fixed-split
  (syntax-rules ()
    ((_ f val) (lambda (_) (f val)))
    ((_ f g ... val)
     (lambda (_)
       (logior (f val)
               ((range-fixed-split g ... val) 0))))))

;; (make-instruction dyn1 dyn2 ... (range-fixed-split ... val))
;; 示例: (make-instruction (range 0 3 4 7) (range-fixed-split (range 0 1 2 3) #b11))
(define-syntax make-instruction
  (syntax-rules ()
    ((_ f)
     (lambda args
       (if (null? args)
           (f 0)
           (f (car args)))))
    ((_ f g ...)
     (lambda args
       (if (null? args)
           (logior (f 0)
                   (apply (make-instruction g ...) '()))
           (logior (f (car args))
                   (apply (make-instruction g ...) (cdr args))))))))