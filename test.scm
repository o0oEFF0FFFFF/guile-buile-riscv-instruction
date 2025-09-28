(add-to-load-path (getcwd))
(use-modules (riscv instruction rv_i)
 (riscv variable)
 (riscv tool)
)

(print16 (lui x1 #x64000))

