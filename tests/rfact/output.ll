; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @multiplyNumbers(i32 %n) {
entry:
  %result = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 0, i32* %result
  store i32 0, i32* %result
  %n2 = load i32, i32* %n1
  %cmptmp = icmp uge i32 %n2, 1
  %booltmp = uitofp i1 %cmptmp to double
  %ifcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %n3 = load i32, i32* %n1
  %n4 = load i32, i32* %n1
  %subtmp = sub i32 %n4, 1
  %calltmp = call i32 @multiplyNumbers(i32 %subtmp)
  %addtmp = mul i32 %n3, %calltmp
  store i32 %addtmp, i32* %result
  br label %ifcont

else:                                             ; preds = %entry
  store i32 1, i32* %result
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %result5 = load i32, i32* %result
  ret i32 %result5
}

define i32 @rfact(i32 %n) {
entry:
  %n1 = alloca i32
  store i32 %n, i32* %n1
  %n2 = load i32, i32* %n1
  %calltmp = call i32 @multiplyNumbers(i32 %n2)
  ret i32 %calltmp
}
