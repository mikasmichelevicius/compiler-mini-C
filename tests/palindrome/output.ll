; ModuleID = 'mini-c'
source_filename = "mini-c"

define i1 @palindrome(i32 %number) {
entry:
  %result = alloca i1
  %rmndr = alloca i32
  %rev = alloca i32
  %t = alloca i32
  %number1 = alloca i32
  store i32 %number, i32* %number1
  store i32 0, i32* %t
  store i32 0, i32* %rev
  store i32 0, i32* %rmndr
  store i1 false, i1* %result
  store i32 0, i32* %rev
  store i1 false, i1* %result
  %number2 = load i32, i32* %number1
  store i32 %number2, i32* %t
  %number3 = load i32, i32* %number1
  %cmptmp = icmp ugt i32 %number3, 0
  %booltmp = uitofp i1 %cmptmp to double
  %isloopcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %isloopcond, label %loop, label %afterloop

loop:                                             ; preds = %loop, %entry
  %number4 = load i32, i32* %number1
  %remtmp = srem i32 %number4, 10
  store i32 %remtmp, i32* %rmndr
  %rev5 = load i32, i32* %rev
  %addtmp = mul i32 %rev5, 10
  %rmndr6 = load i32, i32* %rmndr
  %addtmp7 = add i32 %addtmp, %rmndr6
  store i32 %addtmp7, i32* %rev
  %number8 = load i32, i32* %number1
  %divtmp = sdiv i32 %number8, 10
  store i32 %divtmp, i32* %number1
  %number9 = load i32, i32* %number1
  %cmptmp10 = icmp ugt i32 %number9, 0
  %booltmp11 = uitofp i1 %cmptmp10 to double
  %loopcond = fcmp one double %booltmp11, 0.000000e+00
  br i1 %loopcond, label %loop, label %afterloop

afterloop:                                        ; preds = %loop, %entry
  %t12 = load i32, i32* %t
  %rev13 = load i32, i32* %rev
  %0 = sitofp i32 %t12 to float
  %1 = sitofp i32 %rev13 to float
  %cmptmp14 = fcmp ueq float %0, %1
  %booltmp15 = uitofp i1 %cmptmp14 to double
  %ifcond = fcmp one double %booltmp15, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %afterloop
  store i1 true, i1* %result
  br label %ifcont

else:                                             ; preds = %afterloop
  store i1 false, i1* %result
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %result16 = load i1, i1* %result
  ret i1 %result16
}
