module OrderOperation


let inline (^<|) f x = f x
let StringToInt s = 123
let IntToDouble i = 3.14
let SimpleFunction (s:string) (i:int) = "HELLO"

let example1 = 123 |> SimpleFunction "ASDF"
let example2 = IntToDouble ^<| StringToInt ^<| 123 
let example3 = IntToDouble <| (StringToInt <| 123) 