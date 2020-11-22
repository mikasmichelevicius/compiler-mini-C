; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @While(i32 %n) {
entry:
  %result = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 0, i32* %result
  store i32 0, i32* %result
  %calltmp = call i32 @print_int(i32 0)
  %result2 = load i32, i32* %result
  %cmptmp = icmp ult i32 %result2, 10
  %booltmp = uitofp i1 %cmptmp to double
  %isloopcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %isloopcond, label %loop, label %afterloop

loop:                                             ; preds = %loop, %entry
  %result3 = load i32, i32* %result
  %addtmp = add i32 %result3, 1
  store i32 %addtmp, i32* %result
  %result4 = load i32, i32* %result
  %cmptmp5 = icmp ult i32 %result4, 10
  %booltmp6 = uitofp i1 %cmptmp5 to double
  %loopcond = fcmp one double %booltmp6, 0.000000e+00
  br i1 %loopcond, label %loop, label %afterloop

afterloop:                                        ; preds = %loop, %entry
  %result7 = load i32, i32* %result
  ret i32 %result7
}
