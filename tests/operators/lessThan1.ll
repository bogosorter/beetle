target triple = "x86_64-pc-linux-gnu"
@fmt = private constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)
declare ptr @malloc(i64)
%closure_type = type { ptr, ptr }
define i32 @main() {
    %1 = add i32 0, 1
    %2 = add i32 0, 2
    %3 = icmp sge i32 %1, %2
    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0
    call i32 (i8*, ...) @printf(i8* %fmt, i1 %3)
    ret i32 0
}
