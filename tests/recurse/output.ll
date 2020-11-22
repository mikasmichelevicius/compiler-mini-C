; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @addNumbers(i32 %n) {
entry:
  %result = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 0, i32* %result
  store i32 0, i32* %result
  %n2 = load i32, i32* %n1
  %0 = sitofp i32 %n2 to float
  %cmptmp = fcmp une float %0, 0.000000e+00
  %booltmp = uitofp i1 %cmptmp to double
  %ifcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %n3 = load i32, i32* %n1
  %n4 = load i32, i32* %n1
  %subtmp = sub i32 %n4, 1
  %calltmp = call i32 @addNumbers(i32 %subtmp)
  %addtmp = add i32 %n3, %calltmp
  store i32 %addtmp, i32* %result
  br label %ifcont

else:                                             ; preds = %entry
  %n5 = load i32, i32* %n1
  store i32 %n5, i32* %result
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %result6 = load i32, i32* %result
  %calltmp7 = call i32 @print_int(i32 %result6)
  %result8 = load i32, i32* %result
  ret i32 %result8
}

define i32 @recursion_driver(i32 %num) {
entry:
  %num1 = alloca i32
  store i32 %num, i32* %num1
  %num2 = load i32, i32* %num1
  %calltmp = call i32 @addNumbers(i32 %num2)
  ret i32 %calltmp
}
