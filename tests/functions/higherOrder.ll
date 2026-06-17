target triple = "x86_64-pc-linux-gnu"
@fmt = private constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)
declare ptr @malloc(i64)
%closure_type = type { ptr, ptr }
%lambda0_env = type {  }
%lambda1_env = type {  }
define i1 @lambda0(ptr %env, ptr %argument) {
    %1 = add i32 0, %argument
    %2 = add i32 0, 0
    %3 = getelementptr %closure_type, ptr %1, i32 0, i32 0
    %4 = load ptr, ptr %3
    %5 = getelementptr %closure_type, ptr %1, i32 0, i32 1
    %6 = load ptr, ptr %5
    %7 = call i1 %4(ptr %6, i32 %2)
    br i1 %7, label %then7, label %else7
then7:
    %8 = add i1 0, 1
    br label %merge7
else7:
    %9 = add i1 0, 0
    br label %merge7
merge7:
    %10 = phi i1 [%8, %then7], [%9, %else7]
    ret i1 %10
}
define i1 @lambda1(ptr %env, i32 %argument) {
    %1 = add i32 0, %argument
    %2 = add i32 0, 0
    %3 = icmp sgt i32 %1, %2
    ret i1 %3
}
define i32 @main() {
    %1 = getelementptr %lambda0_env, ptr null, i32 1
    %2 = ptrtoint ptr %1 to i64
    %3 = call ptr @malloc(i64 %2)
    %4 = getelementptr %closure_type, ptr null, i32 1
    %5 = ptrtoint ptr %4 to i64
    %6 = call ptr @malloc(i64 %5)
    %fn_6 = getelementptr %closure_type, ptr %6, i32 0, i32 0
    store ptr @lambda0, ptr %fn_6
    %env_6 = getelementptr %closure_type, ptr %6, i32 0, i32 1
    store ptr %3, ptr %env_6
    %7 = bitcast ptr %6 to ptr
    %test = bitcast ptr %7 to ptr
    %8 = getelementptr %lambda1_env, ptr null, i32 1
    %9 = ptrtoint ptr %8 to i64
    %10 = call ptr @malloc(i64 %9)
    %11 = getelementptr %closure_type, ptr null, i32 1
    %12 = ptrtoint ptr %11 to i64
    %13 = call ptr @malloc(i64 %12)
    %fn_13 = getelementptr %closure_type, ptr %13, i32 0, i32 0
    store ptr @lambda1, ptr %fn_13
    %env_13 = getelementptr %closure_type, ptr %13, i32 0, i32 1
    store ptr %10, ptr %env_13
    %14 = bitcast ptr %13 to ptr
    %positive = bitcast ptr %14 to ptr
    %15 = bitcast ptr %test to ptr
    %16 = bitcast ptr %positive to ptr
    %17 = getelementptr %closure_type, ptr %15, i32 0, i32 0
    %18 = load ptr, ptr %17
    %19 = getelementptr %closure_type, ptr %15, i32 0, i32 1
    %20 = load ptr, ptr %19
    %21 = call i1 %18(ptr %20, ptr %16)
    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0
    call i32 (i8*, ...) @printf(i8* %fmt, i1 %21)
    ret i32 0
}
