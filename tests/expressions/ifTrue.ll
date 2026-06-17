target triple = "x86_64-pc-linux-gnu"
@fmt = private constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)
declare ptr @malloc(i64)
%closure_type = type { ptr, ptr }
define i32 @main() {
    %1 = add i32 0, 1
    %2 = add i32 0, 1
    %3 = icmp eq i32 %1, %2
    br i1 %3, label %then3, label %else3
then3:
    %4 = add i32 0, 1
    br label %merge3
else3:
    %5 = add i32 0, 2
    br label %merge3
merge3:
    %6 = phi i32 [%4, %then3], [%5, %else3]
    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0
    call i32 (i8*, ...) @printf(i8* %fmt, i32 %6)
    ret i32 0
}
