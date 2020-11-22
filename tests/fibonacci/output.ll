; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @fibonacci(i32 %n) {
entry:
  %total = alloca i32
  %c = alloca i32
  %next = alloca i32
  %second = alloca i32
  %first = alloca i32
  %n1 = alloca i32
  store i32 %n, i32* %n1
  store i32 0, i32* %first
  store i32 0, i32* %second
  store i32 0, i32* %next
  store i32 0, i32* %c
  store i32 0, i32* %total
  %n2 = load i32, i32* %n1
  %calltmp = call i32 @print_int(i32 %n2)
  store i32 0, i32* %first
  store i32 1, i32* %second
  store i32 1, i32* %c
  store i32 0, i32* %total
  %c3 = load i32, i32* %c
  %n4 = load i32, i32* %n1
  %cmptmp = icmp ult i32 %c3, %n4
  %booltmp = uitofp i1 %cmptmp to double
  %isloopcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %isloopcond, label %loop, label %afterloop

loop:                                             ; preds = %ifcont, %entry
  %c5 = load i32, i32* %c
  %cmptmp6 = icmp ule i32 %c5, 1
  %booltmp7 = uitofp i1 %cmptmp6 to double
  %ifcond = fcmp one double %booltmp7, 0.000000e+00
  br i1 %ifcond, label %then, label %else

afterloop:                                        ; preds = %ifcont, %entry
  %total24 = load i32, i32* %total
  %calltmp25 = call i32 @print_int(i32 %total24)
  %total26 = load i32, i32* %total
  ret i32 %total26

then:                                             ; preds = %loop
  %c8 = load i32, i32* %c
  store i32 %c8, i32* %next
  br label %ifcont

else:                                             ; preds = %loop
  %first9 = load i32, i32* %first
  %second10 = load i32, i32* %second
  %addtmp = add i32 %first9, %second10
  store i32 %addtmp, i32* %next
  %second11 = load i32, i32* %second
  store i32 %second11, i32* %first
  %next12 = load i32, i32* %next
  store i32 %next12, i32* %second
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %next13 = load i32, i32* %next
  %calltmp14 = call i32 @print_int(i32 %next13)
  %c15 = load i32, i32* %c
  %addtmp16 = add i32 %c15, 1
  store i32 %addtmp16, i32* %c
  %total17 = load i32, i32* %total
  %next18 = load i32, i32* %next
  %addtmp19 = add i32 %total17, %next18
  store i32 %addtmp19, i32* %total
  %c20 = load i32, i32* %c
  %n21 = load i32, i32* %n1
  %cmptmp22 = icmp ult i32 %c20, %n21
  %booltmp23 = uitofp i1 %cmptmp22 to double
  %loopcond = fcmp one double %booltmp23, 0.000000e+00
  br i1 %loopcond, label %loop, label %afterloop
}
