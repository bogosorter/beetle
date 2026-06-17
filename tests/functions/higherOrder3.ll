target triple = "x86_64-pc-linux-gnu"
@fmt = private constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)
declare ptr @malloc(i64)
%closure_type = type { ptr, ptr }
%lambda1_env = type { i32, ptr, i32 }
%lambda0_env = type { ptr }
define i32 @lambda1(ptr %env, i32 %argument) {
    %ptr_1 = getelementptr %lambda1_env, ptr %env, i32 0, i32 0
    %1 = load i32, ptr %ptr_1
    %2 = add i32 0, 0
    %3 = icmp eq i32 %1, %2
    br i1 %3, label %then3, label %else3
then3:
    %4 = add i32 0, 0
    br label %merge3
else3:
    %ptr_5 = getelementptr %lambda1_env, ptr %env, i32 0, i32 1
    %5 = load ptr, ptr %ptr_5
    %ptr_6 = getelementptr %lambda1_env, ptr %env, i32 0, i32 0
    %6 = load i32, ptr %ptr_6
    %7 = add i32 0, 1
    %8 = sub i32 %6, %7
    %9 = getelementptr %closure_type, ptr %5, i32 0, i32 0
    %10 = load ptr, ptr %9
    %11 = getelementptr %closure_type, ptr %5, i32 0, i32 1
    %12 = load ptr, ptr %11
    %13 = call ptr %10(ptr %12, i32 %8)
    %14 = bitcast i32 %argument to i32
    %15 = getelementptr %closure_type, ptr %13, i32 0, i32 0
    %16 = load ptr, ptr %15
    %17 = getelementptr %closure_type, ptr %13, i32 0, i32 1
    %18 = load ptr, ptr %17
    %19 = call i32 %16(ptr %18, i32 %14)
    br label %merge3
merge3:
    %20 = phi i32 [%4, %then3], [%19, %else3]
    ret i32 %20
}
define ptr @lambda0(ptr %env, i32 %argument) {
    %1 = getelementptr %lambda1_env, ptr null, i32 1
    %2 = ptrtoint ptr %1 to i64
    %3 = call ptr @malloc(i64 %2)
    %4 = getelementptr %closure_type, ptr null, i32 1
    %5 = ptrtoint ptr %4 to i64
    %6 = call ptr @malloc(i64 %5)
    %fn_6 = getelementptr %closure_type, ptr %6, i32 0, i32 0
    store ptr @lambda1, ptr %fn_6
    %env_6 = getelementptr %closure_type, ptr %6, i32 0, i32 1
    store ptr %3, ptr %env_6
    %7 = bitcast i32 %argument to i32
    %8 = getelementptr %lambda1_env, ptr %3, i32 0, i32 0
    store i32 %7, ptr %8
    %ptr_9 = getelementptr %lambda0_env, ptr %env, i32 0, i32 0
    %9 = load ptr, ptr %ptr_9
    %10 = getelementptr %lambda1_env, ptr %3, i32 0, i32 1
    store ptr %9, ptr %10
    %11 = bitcast i32 %argument to i32
    %12 = getelementptr %lambda1_env, ptr %3, i32 0, i32 2
    store i32 %11, ptr %12
    %13 = bitcast ptr %6 to ptr
    ret ptr %13
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
    %8 = getelementptr %lambda0_env, ptr %3, i32 0, i32 0
    store ptr %7, ptr %8
    %9 = bitcast ptr %6 to ptr
    %accumulate = bitcast ptr %9 to ptr
    %10 = bitcast ptr %accumulate to ptr
    %11 = add i32 0, 1
    %12 = getelementptr %closure_type, ptr %10, i32 0, i32 0
    %13 = load ptr, ptr %12
    %14 = getelementptr %closure_type, ptr %10, i32 0, i32 1
    %15 = load ptr, ptr %14
    %16 = call ptr %13(ptr %15, i32 %11)
    %17 = add i32 0, 0
    %18 = getelementptr %closure_type, ptr %16, i32 0, i32 0
    %19 = load ptr, ptr %18
    %20 = getelementptr %closure_type, ptr %16, i32 0, i32 1
    %21 = load ptr, ptr %20
    %22 = call i32 %19(ptr %21, i32 %17)
    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0
    call i32 (i8*, ...) @printf(i8* %fmt, i32 %22)
    ret i32 0
}
