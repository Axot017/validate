type test_record = {
  a : string option list; [@email] [@min_length 5]
  b : test_record2 list;
}
[@@deriving validator]

and test_record2 = { c : string; d : string } [@@deriving validator]
