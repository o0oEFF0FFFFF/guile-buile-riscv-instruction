#!/usr/bin/guile -s
!#

(add-to-load-path (getcwd))
(use-modules (riscv build-instruction-module))

(build-module "riscv-opcodes/extensions/rv_i" "riscv/instruction/rv_i.scm")
(build-module "riscv-opcodes/extensions/rv64_i" "riscv/instruction/rv64_i.scm")
(build-module "riscv-opcodes/extensions/rv_zicsr" "riscv/instruction/rv_zicsr.scm")
(build-module "riscv-opcodes/extensions/rv_m" "riscv/instruction/rv_m.scm")
(build-module "riscv-opcodes/extensions/rv64_m" "riscv/instruction/rv64_m.scm")
(build-module "riscv-opcodes/extensions/rv_a" "riscv/instruction/rv_a.scm")
(build-module "riscv-opcodes/extensions/rv64_a" "riscv/instruction/rv64_a.scm")
(build-module "riscv-opcodes/extensions/rv_f" "riscv/instruction/rv_f.scm")
(build-module "riscv-opcodes/extensions/rv64_f" "riscv/instruction/rv64_f.scm")
(build-module "riscv-opcodes/extensions/rv_d" "riscv/instruction/rv_d.scm")
(build-module "riscv-opcodes/extensions/rv64_d" "riscv/instruction/rv64_d.scm")
(build-module "riscv-opcodes/extensions/rv_c" "riscv/instruction/rv_c.scm")
(build-module "riscv-opcodes/extensions/rv64_c" "riscv/instruction/rv64_c.scm")