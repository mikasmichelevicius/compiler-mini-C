; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @addition(i32 %n, i32 %m) {
entry:
  %result = alloca i32
  %m2 = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 %m, i32* %m2
  store i32 0, i32* %result
  %n3 = load i32, i32* %n1
  %m4 = load i32, i32* %m2
  %addtmp = add i32 %n3, %m4
  store i32 %addtmp, i32* %result
  %n5 = load i32, i32* %n1
  %0 = sitofp i32 %n5 to float
  %cmptmp = fcmp ueq float %0, 4.000000e+00
  %booltmp = uitofp i1 %cmptmp to double
  %ifcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %n6 = load i32, i32* %n1
  %m7 = load i32, i32* %m2
  %addtmp8 = add i32 %n6, %m7
  %calltmp = call i32 @print_int(i32 %addtmp8)
  br label %ifcont

else:                                             ; preds = %entry
  %n9 = load i32, i32* %n1
  %m10 = load i32, i32* %m2
  %addtmp11 = mul i32 %n9, %m10
  %calltmp12 = call i32 @print_int(i32 %addtmp11)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %result13 = load i32, i32* %result
  ret i32 %result13
}
