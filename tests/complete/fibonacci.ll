target triple = "x86_64-pc-linux-gnu"
@fmt = private constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)
declare ptr @malloc(i64)
%closure_type = type { ptr, ptr }
%lambda0_env = type { ptr, ptr }
define i32 @lambda0(ptr %env, i32 %argument) {
    %1 = bitcast i32 %argument to i32
    %2 = add i32 0, 0
    %3 = icmp eq i32 %1, %2
    br i1 %3, label %then3, label %else3
then3:
    %4 = add i32 0, 1
    br label %merge3
else3:
    %5 = bitcast i32 %argument to i32
    %6 = add i32 0, 1
    %7 = icmp eq i32 %5, %6
    br i1 %7, label %then7, label %else7
then7:
    %8 = add i32 0, 1
    br label %merge7
else7:
    %ptr_9 = getelementptr %lambda0_env, ptr %env, i32 0, i32 0
    %9 = load ptr, ptr %ptr_9
    %10 = bitcast i32 %argument to i32
    %11 = add i32 0, 1
    %12 = sub i32 %10, %11
    %13 = getelementptr %closure_type, ptr %9, i32 0, i32 0
    %14 = load ptr, ptr %13
    %15 = getelementptr %closure_type, ptr %9, i32 0, i32 1
    %16 = load ptr, ptr %15
    %17 = call i32 %14(ptr %16, i32 %12)
    %ptr_18 = getelementptr %lambda0_env, ptr %env, i32 0, i32 0
    %18 = load ptr, ptr %ptr_18
    %19 = bitcast i32 %argument to i32
    %20 = add i32 0, 2
    %21 = sub i32 %19, %20
    %22 = getelementptr %closure_type, ptr %18, i32 0, i32 0
    %23 = load ptr, ptr %22
    %24 = getelementptr %closure_type, ptr %18, i32 0, i32 1
    %25 = load ptr, ptr %24
    %26 = call i32 %23(ptr %25, i32 %21)
    %27 = add i32 %17, %26
    br label %merge7
merge7:
    %28 = phi i32 [%8, %then7], [%27, %else7]
    br label %merge3
merge3:
    %29 = phi i32 [%4, %then3], [%28, %merge7]
    ret i32 %29
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
    %10 = getelementptr %lambda0_env, ptr %3, i32 0, i32 1
    store ptr %9, ptr %10
    %11 = bitcast ptr %6 to ptr
    %fibonacci = bitcast ptr %11 to ptr
    %12 = bitcast ptr %fibonacci to ptr
    %13 = add i32 0, 6
    %14 = getelementptr %closure_type, ptr %12, i32 0, i32 0
    %15 = load ptr, ptr %14
    %16 = getelementptr %closure_type, ptr %12, i32 0, i32 1
    %17 = load ptr, ptr %16
    %18 = call i32 %15(ptr %17, i32 %13)
    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0
    call i32 (i8*, ...) @printf(i8* %fmt, i32 %18)
    ret i32 0
}
