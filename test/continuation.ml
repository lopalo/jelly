module A = Alcotest
open Runtime

let non_local_return () =
  check "pair of result"
    (Ok (Common.obj_of_str "[7 (114 123 132 141)]"))
    (Common.execute_test_script ~core:true "non-local-return.jly")

let cooperative_multitasking () =
  check "output"
    (Ok (Common.obj_of_test_script "cooperative-multitasking-output.jly"))
    (Common.execute_test_script ~core:true "cooperative-multitasking.jly")

let preemptive_multitasking () =
  check "output"
    (Ok (Common.obj_of_test_script "preemptive-multitasking-output.jly"))
    (Common.execute_test_script ~core:true "preemptive-multitasking.jly")

let generator () =
  check "iteration output"
    (Ok (Common.obj_of_test_script "generator-output.jly"))
    (Common.execute_test_script ~core:true "generator.jly")

let tests =
  [ A.test_case "non-local-return" `Quick non_local_return;
    A.test_case "cooperative-multitasking" `Quick cooperative_multitasking;
    A.test_case "preemptive-multitasking" `Quick preemptive_multitasking;
    A.test_case "generator" `Quick generator ]
