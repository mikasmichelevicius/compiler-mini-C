; ModuleID = 'mini-c'
source_filename = "mini-c"

declare float @print_float(float)

define float @cosine(float %x) {
entry:
  %i = alloca float
  %alt = alloca float
  %eps = alloca float
  %term = alloca float
  %n = alloca float
  %cos = alloca float
  %x1 = alloca float
  store float %x, float* %x1
  store float 0.000000e+00, float* %cos
  store float 0.000000e+00, float* %n
  store float 0.000000e+00, float* %term
  store float 0.000000e+00, float* %eps
  store float 0.000000e+00, float* %alt
  store float 0.000000e+00, float* %i
  store float 1.000000e+00, float* %i
  store float 0x3EB0C6F7A0000000, float* %eps
  store float 1.000000e+00, float* %n
  store float 1.000000e+00, float* %cos
  store float 1.000000e+00, float* %term
  store float -1.000000e+00, float* %alt
  %term2 = load float, float* %term
  %eps3 = load float, float* %eps
  %cmptmp = fcmp ugt float %term2, %eps3
  %booltmp = uitofp i1 %cmptmp to double
  %isloopcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %isloopcond, label %loop, label %afterloop

loop:                                             ; preds = %loop, %entry
  %term4 = load float, float* %term
  %x5 = load float, float* %x1
  %x6 = load float, float* %x1
  %n7 = load float, float* %n
  %n8 = load float, float* %n
  %addtmp = fadd float %n8, 1.000000e+00
  %divtmp = fdiv float %n7, %addtmp
  %divtmp9 = fdiv float %x6, %divtmp
  %addtmp10 = fmul float %x5, %divtmp9
  %addtmp11 = fmul float %term4, %addtmp10
  store float %addtmp11, float* %term
  %cos12 = load float, float* %cos
  %alt13 = load float, float* %alt
  %term14 = load float, float* %term
  %addtmp15 = fmul float %alt13, %term14
  %addtmp16 = fadd float %cos12, %addtmp15
  store float %addtmp16, float* %cos
  %alt17 = load float, float* %alt
  %0 = fneg float %alt17
  store float %0, float* %alt
  %n18 = load float, float* %n
  %addtmp19 = fadd float %n18, 2.000000e+00
  store float %addtmp19, float* %n
  %term20 = load float, float* %term
  %eps21 = load float, float* %eps
  %cmptmp22 = fcmp ugt float %term20, %eps21
  %booltmp23 = uitofp i1 %cmptmp22 to double
  %loopcond = fcmp one double %booltmp23, 0.000000e+00
  br i1 %loopcond, label %loop, label %afterloop

afterloop:                                        ; preds = %loop, %entry
  %term24 = load float, float* %term
  %calltmp = call float @print_float(float %term24)
  %alt25 = load float, float* %alt
  ret float %alt25
}
