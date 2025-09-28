(define-module (riscv range-base)
  #:use-module (riscv build-base)
  #:export (rs1 rs2 rd imm12 simm12 bimm12 imm20 jimm20 fm pred succ)
)

;; 测试定义
(define rs1 (range 0 4 15 19))
(define rs2 (range 0 4 20 24))
(define rd (range 0 4 7 11))
(define imm12 (range 0 11 20 31))
(define simm12 (range-split 
               (range 5 11 25 31) 
               (range 0 4 7 11)))
(define bimm12
  (range-split (range 11 7)
               (range 1 4 8 11)
               (range 5 10 25 30)
               (range 12 31)))
(define imm20 (range 12 31 12 31))
(define jimm20 
  (range-split (range 20 31) 
               (range 1 10 21 30) 
               (range 11 20) 
               (range 12 19 12 19)))

(define fm (range 0 3 28 31))
(define pred (range 0 3 24 27))
(define succ (range 0 3 20 23))

(define PI #b1000)
(define PO #b0100)
(define PR #b0010)
(define PW #b0001)
(define SI #b1000)
(define SO #b0100)
(define SR #b0010)
(define SW #b0001)