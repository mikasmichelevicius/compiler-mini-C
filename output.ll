; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define float @addition(i32 %n, i32 %m) {
entry:
  %a = alloca float
  %m2 = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 %m, i32* %m2
  store float 0.000000e+00, float* %a
  store i32 -1, float* %a
  %a3 = load float, float* %a
  %0 = fneg float %a3
  store float %0, float* %a
  %a4 = load float, float* %a
  ret float %a4
}
