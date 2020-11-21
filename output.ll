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
  %1 = fadd float %0, %m4
  store float %1, float* %result
  %result5 = load float, float* %result
  %calltmp = call float @print_float(float %result5)
  %sum6 = load float, float* %sum
  %result7 = load float, float* %result
  %2 = fadd float %sum6, %result7
  store float %2, float* %sum
  %n8 = load i32, i32* %n1
  %m9 = load float, float* %m2
  %3 = fneg float %m9
  %4 = sitofp i32 %n8 to float
  %5 = fadd float %4, %3
  store float %5, float* %result
  %result10 = load float, float* %result
  %calltmp11 = call float @print_float(float %result10)
  %sum12 = load float, float* %sum
  %result13 = load float, float* %result
  %6 = fadd float %sum12, %result13
  store float %6, float* %sum
  %n14 = load i32, i32* %n1
  %m15 = load float, float* %m2
  %7 = fneg float %m15
  %8 = fneg float %7
  %9 = sitofp i32 %n14 to float
  %10 = fadd float %9, %8
  store float %10, float* %result
  %result16 = load float, float* %result
  %calltmp17 = call float @print_float(float %result16)
  %sum18 = load float, float* %sum
  %result19 = load float, float* %result
  %11 = fadd float %sum18, %result19
  store float %11, float* %sum
  %n20 = load i32, i32* %n1
  %12 = sub i32 0, %n20
  %m21 = load float, float* %m2
  %13 = fneg float %m21
  %14 = sitofp i32 %12 to float
  %15 = fadd float %14, %13
  store float %15, float* %result
  %result22 = load float, float* %result
  %calltmp23 = call float @print_float(float %result22)
  %sum24 = load float, float* %sum
  %result25 = load float, float* %result
  %16 = fadd float %sum24, %result25
  store float %16, float* %sum
  %sum26 = load float, float* %sum
  ret float %sum26
}
