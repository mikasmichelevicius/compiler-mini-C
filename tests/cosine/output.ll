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
  %x2 = load float, float* %x1
  %n3 = load float, float* %n
  %n4 = load float, float* %n
  %addtmp = fadd float %n4, 1.000000e+00
  %divtmp = fdiv float %n3, %addtmp
  %divtmp5 = fdiv float %x2, %divtmp
  store float %divtmp5, float* %term
  %cos6 = load float, float* %cos
  %alt7 = load float, float* %alt
  %term8 = load float, float* %term
  %addtmp9 = fmul float %alt7, %term8
  %addtmp10 = fadd float %cos6, %addtmp9
  store float %addtmp10, float* %cos
  %term11 = load float, float* %term
  %calltmp = call float @print_float(float %term11)
  %term12 = load float, float* %term
  %calltmp13 = call float @print_float(float %term12)
  %alt14 = load float, float* %alt
  ret float %alt14
}
