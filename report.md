target triple = "x86_64-pc-linux-gnu"
@fmt = private constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)

define i32 @fibonacci(i32 %n) {
    %1 = add i32 0, %n
    %2 = add i32 0, 0
    %3 = icmp eq i32 %1, %2
    br i1 %3, label %true19, label %false19
true19:
    %4 = add i32 0, 1
    br label %merge19
false19:
    %5 = add i32 0, %n
    %6 = add i32 0, 1
    %7 = icmp eq i32 %5, %6
    br i1 %7, label %true18, label %false18
true18:
    %8 = add i32 0, 1
    br label %merge18
false18:
    %9 = add i32 0, %n
    %10 = add i32 0, 1
    %11 = sub i32 %9, %10
    %12 = call i32 @fibonacci(i32 %11)
    %13 = add i32 0, %n
    %14 = add i32 0, 2
    %15 = sub i32 %13, %14
    %16 = call i32 @fibonacci(i32 %15)
    %17 = add i32 %12, %16
    br label %merge18
merge18:
    %18 = phi i32 [%8, %true18], [%17, %false18]
    br label %merge19
merge19:
    %19 = phi i32 [%4, %true19], [%18, %false19]
    ret i32 %19
}

define i32 @compute() {
    %1 = add i32 0, 5
    %2 = call i32 @fibonacci(i32 %1)
    ret i32 %2
}

define i32 @main() {
    %r = call i32 @compute()
    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0
    call i32 (i8*, ...) @printf(i8* %fmt, i32 %r)
    ret i32 0
}

$ clang test.ll -o test && ./test
PLEASE submit a bug report to https://github.com/llvm/llvm-project/issues/ and include the crash backtrace, preprocessed source, and associated run script.
Stack dump:
0.	Program arguments: /usr/lib/llvm-18/bin/clang -cc1 -triple x86_64-pc-linux-gnu -emit-obj -mrelax-all -dumpdir test- -disable-free -clear-ast-before-backend -disable-llvm-verifier -discard-value-names -main-file-name test.ll -mrelocation-model pic -pic-level 2 -pic-is-pie -mframe-pointer=all -fmath-errno -ffp-contract=on -fno-rounding-math -mconstructor-aliases -funwind-tables=2 -target-cpu x86-64 -tune-cpu generic -debugger-tuning=gdb -fdebug-compilation-dir=/home/bogosorter/Documents/bogosorter/bogocode/bsl -fcoverage-compilation-dir=/home/bogosorter/Documents/bogosorter/bogocode/bsl -resource-dir /usr/lib/llvm-18/lib/clang/18 -ferror-limit 19 -fgnuc-version=4.2.1 -fskip-odr-check-in-gmf -fcolor-diagnostics -faddrsig -D__GCC_HAVE_DWARF2_CFI_ASM=1 -o /tmp/test-5128d3.o -x ir test.ll
1.	Code generation
2.	Running pass 'Function Pass Manager' on module 'test.ll'.
3.	Running pass 'X86 DAG->DAG Instruction Selection' on function '@fibonacci'
