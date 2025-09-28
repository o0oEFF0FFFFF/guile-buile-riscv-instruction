运行需要guile-3.0

first_build.scm从riscv opcode读取指令定义并生成指令生成模块

这只是一个基础，你可以从这里开始拓展。

就像test.scm里的示例，导入模块后就可以使用
(lui x0,#x64000) 将64移动到对应寄存器的高位，立即数低12位会被截断，在这里实现里是这样的，
和gcc的lui x0,64不同，这里不对立即数做任何处理。

已经将对应riscv_acdfimsf zicsr模块生成好放在riscv路径下

如果要自己生成，请
在项目根目录运行
git clone https://github.com/riscv/riscv-opcodes.git

然后运行./first_build.scm

就能从新生成模块

导入assemble-file后就可以在文件中使用汇编指令并输出
就像这样：
(add-to-load-path (getcwd))
(use-modules (riscv instruction rv_i)
 (riscv variable)
 (riscv tool)
 (assemble-file)
)

(assemble-file "a.s" "a.bin")

a.bin就是生成的二进制文件。
