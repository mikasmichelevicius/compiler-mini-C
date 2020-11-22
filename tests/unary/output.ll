; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

declare float @print_float(float)

define float @unary(i32 %n, float %m) {
entry:
  %sum = alloca float
  %result = alloca float
  %m2 = alloca float
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store float %m, float* %m2
  store float 0.000000e+00, float* %result
  store float 0.000000e+00, float* %sum
  store float 0.000000e+00, float* %sum
  %n3 = load i32, i32* %n1
  %m4 = load float, float* %m2
  %0 = sitofp i32 %n3 to float
  %addtmp = fadd float %0, %m4
  store float %addtmp, float* %result
  %result5 = load float, float* %result
  %calltmp = call float @print_float(float %result5)
  %sum6 = load float, float* %sum
  %result7 = load float, float* %result
  %addtmp8 = fadd float %sum6, %result7
  store float %addtmp8, float* %sum
  %n9 = load i32, i32* %n1
  %m10 = load float, float* %m2
  %1 = fneg float %m10
  %2 = sitofp i32 %n9 to float
  %addtmp11 = fadd float %2, %1
  store float %addtmp11, float* %result
  %result12 = load float, float* %result
  %calltmp13 = call float @print_float(float %result12)
  %sum14 = load float, float* %sum
  %result15 = load float, float* %result
  %addtmp16 = fadd float %sum14, %result15
  store float %addtmp16, float* %sum
  %n17 = load i32, i32* %n1
  %m18 = load float, float* %m2
  %3 = fneg float %m18
  %4 = fneg float %3
  %5 = sitofp i32 %n17 to float
  %addtmp19 = fadd float %5, %4
  store float %addtmp19, float* %result
  %result20 = load float, float* %result
  %calltmp21 = call float @print_float(float %result20)
  %sum22 = load float, float* %sum
  %result23 = load float, float* %result
  %addtmp24 = fadd float %sum22, %result23
  store float %addtmp24, float* %sum
  %n25 = load i32, i32* %n1
  %6 = sub i32 0, %n25
  %m26 = load float, float* %m2
  %7 = fneg float %m26
  %8 = sitofp i32 %6 to float
  %addtmp27 = fadd float %8, %7
  store float %addtmp27, float* %result
  %result28 = load float, float* %result
  %calltmp29 = call float @print_float(float %result28)
  %sum30 = load float, float* %sum
  %result31 = load float, float* %result
  %addtmp32 = fadd float %sum30, %result31
  store float %addtmp32, float* %sum
  %sum33 = load float, float* %sum
  ret float %sum33
}
