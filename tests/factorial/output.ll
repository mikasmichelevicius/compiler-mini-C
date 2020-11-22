; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @factorial(i32 %n) {
entry:
  %factorial = alloca i32
  %i = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 0, i32* %i
  store i32 0, i32* %factorial
  store i32 1, i32* %factorial
  store i32 1, i32* %i
  %i2 = load i32, i32* %i
  %n3 = load i32, i32* %n1
  %cmptmp = icmp ule i32 %i2, %n3
  %booltmp = uitofp i1 %cmptmp to double
  %isloopcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %isloopcond, label %loop, label %afterloop

loop:                                             ; preds = %loop, %entry
  %factorial4 = load i32, i32* %factorial
  %i5 = load i32, i32* %i
  %addtmp = mul i32 %factorial4, %i5
  store i32 %addtmp, i32* %factorial
  %i6 = load i32, i32* %i
  %addtmp7 = add i32 %i6, 1
  store i32 %addtmp7, i32* %i
  %i8 = load i32, i32* %i
  %n9 = load i32, i32* %n1
  %cmptmp10 = icmp ule i32 %i8, %n9
  %booltmp11 = uitofp i1 %cmptmp10 to double
  %loopcond = fcmp one double %booltmp11, 0.000000e+00
  br i1 %loopcond, label %loop, label %afterloop

afterloop:                                        ; preds = %loop, %entry
  %factorial12 = load i32, i32* %factorial
  ret i32 %factorial12
}
