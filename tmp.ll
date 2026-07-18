target triple = "x86_64-pc-linux-gnu"
@fmt = private constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)
declare ptr @malloc(i64)



define i32 @main() {
    ; Allocating space for tuple
    %1 = getelementptr {}, ptr null, i32 1
    %2 = ptrtoint ptr %1 to i64
    %3 = call ptr @malloc(i64 %2)
    ; Calculating and inserting members
    ; Lifting to sum type
    %4 = getelementptr {i32, ptr}, ptr null, i32 1
    %5 = ptrtoint ptr %4 to i64
    %6 = call ptr @malloc(i64 %5)
    %7 = getelementptr {i32, ptr}, ptr %6, i32 0, i32 0
    store i32 1, ptr %7
    %8 = getelementptr {i32, ptr}, ptr %6, i32 0, i32 1
    store ptr %3, ptr %8
    %result_1 = bitcast ptr %6 to ptr
    ; Extracting the constructor of a sum type
    %9 = getelementptr {i32, ptr}, ptr %result_1, i32 0, i32 0
    %10 = load i32, ptr %9
    switch i32 %10, label %default_1 [
        i32 0, label %case_1_1
    ]
    
    case_1_1:
    ; Loading and casting the value of the new variable
    %11 = getelementptr {i32, ptr}, ptr %result_1, i32 0, i32 1
    %n_1 = load i32, ptr %11
    ; Case branch body
    br label %merge_1
    
    default_1:
    %12 = add i32 0, 0
    %13 = add i32 0, 1
    %14 = sub i32 %12, %13
    br label %merge_1
    
    merge_1:
    %15 = phi i32 [%n_1, %case_1_1], [%14, %default_1]

    %_fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0
    call i32 (i8*, ...) @printf(i8* %_fmt, i32 %15)

    ret i32 0
}
