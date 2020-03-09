module A = Alcotest
open Runtime

let core () =
  check "core"
    (Ok (Common.obj_of_str "PASSED"))
    (Common.execute_test_script ~core:true "core.jly")

let tests = [A.test_case "core" `Quick core]
