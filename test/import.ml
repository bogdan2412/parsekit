open! Base
include Parsekit.With_let_syntax

let show_raise = Expect_test_helpers_base.show_raise

let run t buf require_input_entirely_consumed sexp_of =
  let value = run t buf ~require_input_entirely_consumed in
  Stdio.print_s (sexp_of value)
;;
