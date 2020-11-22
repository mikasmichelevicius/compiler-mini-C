; ModuleID = 'mini-c'
source_filename = "mini-c"

define float @pi() {
entry:
  %i = alloca i32
  %PI = alloca float
  %flag = alloca i1
  store i1 false, i1* %flag
  store float 0.000000e+00, float* %PI
  store i32 0, i32* %i
  store i1 true, i1* %flag
  store float 3.000000e+00, float* %PI
  store i32 2, i32* %i
  %i1 = load i32, i32* %i
  %cmptmp = icmp ult i32 %i1, 100
  %booltmp = uitofp i1 %cmptmp to double
  %isloopcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %isloopcond, label %loop, label %afterloop

loop:                                             ; preds = %ifcont, %entry
  %flag2 = load i1, i1* %flag
  %ifcond = icmp ne i1 %flag2, false
  br i1 %ifcond, label %then, label %else

afterloop:                                        ; preds = %ifcont, %entry
  %PI26 = load float, float* %PI
  ret float %PI26

then:                                             ; preds = %loop
  %PI3 = load float, float* %PI
  %i4 = load i32, i32* %i
  %i5 = load i32, i32* %i
  %addtmp = add i32 %i5, 1
  %i6 = load i32, i32* %i
  %addtmp7 = add i32 %i6, 2
  %addtmp8 = mul i32 %addtmp, %addtmp7
  %addtmp9 = mul i32 %i4, %addtmp8
  %0 = sitofp i32 %addtmp9 to float
  %divtmp = fdiv float 4.000000e+00, %0
  %addtmp10 = fadd float %PI3, %divtmp
  store float %addtmp10, float* %PI
  br label %ifcont

else:                                             ; preds = %loop
  %PI11 = load float, float* %PI
  %i12 = load i32, i32* %i
  %i13 = load i32, i32* %i
  %addtmp14 = add i32 %i13, 1
  %i15 = load i32, i32* %i
  %addtmp16 = add i32 %i15, 2
  %addtmp17 = mul i32 %addtmp14, %addtmp16
  %addtmp18 = mul i32 %i12, %addtmp17
  %1 = sitofp i32 %addtmp18 to float
  %divtmp19 = fdiv float 4.000000e+00, %1
  %subtmp = fsub float %PI11, %divtmp19
  store float %subtmp, float* %PI
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %flag20 = load i1, i1* %flag
  %2 = xor i1 %flag20, true
  store i1 %2, i1* %flag
  %i21 = load i32, i32* %i
  %addtmp22 = add i32 %i21, 2
  store i32 %addtmp22, i32* %i
  %i23 = load i32, i32* %i
  %cmptmp24 = icmp ult i32 %i23, 100
  %booltmp25 = uitofp i1 %cmptmp24 to double
  %loopcond = fcmp one double %booltmp25, 0.000000e+00
  br i1 %loopcond, label %loop, label %afterloop
}
