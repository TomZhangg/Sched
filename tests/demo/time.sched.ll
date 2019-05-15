; ModuleID = 'Schedch'

%struct.Array = type { i32, %struct.Array_element*, %struct.Array_element*, i32 }
%struct.Array_element = type { %struct.Array_element*, %struct.Array_element*, i8* }
%struct.time = type { i32, i32, i32, i32, i32, i32 }

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%g\0A\00"

declare i32 @printf(i8*, ...)

declare %struct.Array* @arr_init()

declare %struct.Array* @arr_set_contains_struct(%struct.Array*)

declare %struct.Array_element* @arr_append(%struct.Array*, i8*)

declare %struct.Array_element* @arr_get(%struct.Array*, i32)

declare %struct.Array_element* @arr_set(%struct.Array*, i8*, i32)

declare i32 @arr_length(%struct.Array*)

declare i32 @arr_contains(%struct.Array*, i8*)

declare %struct.time* @time_init(i32, i32, i32, i32, i32, i32)

declare void @print_time(%struct.time*)

define i32 @main() {
entry:
  %a = alloca %struct.time*
  %time_init = call %struct.time* @time_init(i32 2000, i32 9, i32 10, i32 0, i32 0, i32 0)
  store %struct.time* %time_init, %struct.time** %a
  %b = alloca %struct.time*
  %time_init1 = call %struct.time* @time_init(i32 2000, i32 9, i32 10, i32 10, i32 10, i32 10)
  store %struct.time* %time_init1, %struct.time** %b
  %d = alloca i1
  %a2 = alloca i32
  %c = alloca %struct.time*
  %d3 = alloca %struct.time*
  %time_init4 = call %struct.time* @time_init(i32 2000, i32 9, i32 1, i32 0, i32 0, i32 0)
  store %struct.time* %time_init4, %struct.time** %d3
  %z = alloca %struct.time*
  %myfunc_result = call %struct.time* @myfunc(i32 1, i32 2, i32 3, i32 4)
  %time_init5 = call %struct.time* @time_init(i32 2000, i32 0, i32 1, i32 0, i32 0, i32 0)
  store %struct.time* %time_init5, %struct.time** %z
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), i32 2000)
  %e = alloca %struct.time*
  %time_init6 = call %struct.time* @time_init(i32 0, i32 0, i32 0, i32 0, i32 0, i32 0)
  store %struct.time* %time_init6, %struct.time** %e
  %d7 = alloca i1
  %f = alloca %struct.time*
  %myfunc_result8 = call %struct.time* @myfunc.4(i32 1)
  store %struct.time* %myfunc_result8, %struct.time** %b
  %b9 = load %struct.time*, %struct.time** %b
  call void @print_time(%struct.time* %b9)
  %t = alloca %struct.time*
  %time_init10 = call %struct.time* @time_init(i32 2000, i32 0, i32 0, i32 0, i32 0, i32 0)
  %myfunc_result11 = call %struct.time* @myfunc.4(i32 3)
  store %struct.time* %myfunc_result11, %struct.time** %t
  ret i32 0
}

define %struct.time* @myfunc(i32, i32, i32, i32) {
entry:
  %t = alloca %struct.time*
  %time_init = call %struct.time* @time_init(i32 2019, i32 5, i32 15, i32 0, i32 0, i32 0)
  store %struct.time* %time_init, %struct.time** %t
  %t1 = load %struct.time*, %struct.time** %t
  ret %struct.time* %t1
}

define %struct.time* @myfunc.3(i32, i32, i32) {
entry:
  %t = alloca %struct.time*
  %time_init = call %struct.time* @time_init(i32 2019, i32 5, i32 15, i32 0, i32 0, i32 0)
  store %struct.time* %time_init, %struct.time** %t
  %t1 = load %struct.time*, %struct.time** %t
  ret %struct.time* %t1
}

define %struct.time* @myfunc.4(i32) {
entry:
  %time_init = call %struct.time* @time_init(i32 0, i32 0, i32 0, i32 0, i32 0, i32 0)
  ret %struct.time* %time_init
}
