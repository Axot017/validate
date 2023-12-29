open Err

type test_record = { a : string; b : string; c : string }

let test_validate_record_error_simple () =
  let r = { a = "111"; b = "333"; c = "" } in
  let validate =
    Validator.(
      record
        [
          field "a"
            (fun x -> x.a)
            [ validate_length_between String.length ~min:1 ~max:2 ];
          field "b" (fun x -> x.b) [ validate_min_length String.length 1 ];
          field "c" (fun x -> x.c) [ validate_min_length String.length 1 ];
        ])
  in
  let result = validate r in

  Alcotest.(check (result unit validation_error_testable))
    "validate record"
    (Error
       (Validator.RecordError
          [
            ( "c",
              [
                Validator.BaseError
                  { code = "min_length"; params = [ ("threshold", "1") ] };
              ] );
            ( "a",
              [
                Validator.BaseError
                  { code = "max_length"; params = [ ("threshold", "2") ] };
              ] );
          ]))
    result

let test_validate_record_ok_simple () =
  let r = { a = "111"; b = "333"; c = "444" } in
  let validate =
    Validator.(
      record
        [
          field "a"
            (fun x -> x.a)
            [ validate_length_between String.length ~min:1 ~max:3 ];
          field "b" (fun x -> x.b) [ validate_min_length String.length 1 ];
          field "c" (fun x -> x.c) [ validate_min_length String.length 1 ];
        ])
  in

  let result = validate r in

  Alcotest.(check (result unit validation_error_testable))
    "validate record" (Ok ()) result

type complex_record = {
  x1 : string;
  x2 : complex_record_nested;
  x3 : complex_record_nested list;
}

and complex_record_nested = { y1 : string; y2 : string }

let test_validate_record_error_complex () =
  let r =
    {
      x1 = "111";
      x2 = { y1 = "111"; y2 = "222" };
      x3 = [ { y1 = "1"; y2 = "222" }; { y1 = "111"; y2 = "22222" } ];
    }
  in
  let validate =
    Validator.(
      record
        [
          field "x1"
            (fun x -> x.x1)
            [ validate_length_between String.length ~min:1 ~max:2 ];
          field "x2"
            (fun x -> x.x2)
            [
              record
                [
                  field "y1"
                    (fun x -> x.y1)
                    [ validate_length_between String.length ~min:1 ~max:2 ];
                  field "y2"
                    (fun x -> x.y2)
                    [ validate_length_between String.length ~min:1 ~max:2 ];
                ];
            ];
          field "x3"
            (fun x -> x.x3)
            [
              list
                [
                  record
                    [
                      field "y1"
                        (fun x -> x.y1)
                        [ validate_length_between String.length ~min:1 ~max:2 ];
                      field "y2"
                        (fun x -> x.y2)
                        [ validate_length_between String.length ~min:1 ~max:2 ];
                    ];
                ];
            ];
        ])
  in
  let result = validate r in

  Alcotest.(check (result unit validation_error_testable))
    "validate record"
    (Error
       (Validator.RecordError
          [
            ( "x3",
              [
                Validator.IterableError
                  [
                    ( 1,
                      [
                        Validator.RecordError
                          [
                            ( "y2",
                              [
                                Validator.BaseError
                                  {
                                    code = "max_length";
                                    params = [ ("threshold", "2") ];
                                  };
                              ] );
                            ( "y1",
                              [
                                Validator.BaseError
                                  {
                                    code = "max_length";
                                    params = [ ("threshold", "2") ];
                                  };
                              ] );
                          ];
                      ] );
                    ( 0,
                      [
                        Validator.RecordError
                          [
                            ( "y2",
                              [
                                Validator.BaseError
                                  {
                                    code = "max_length";
                                    params = [ ("threshold", "2") ];
                                  };
                              ] );
                          ];
                      ] );
                  ];
              ] );
            ( "x2",
              [
                Validator.RecordError
                  [
                    ( "y2",
                      [
                        Validator.BaseError
                          {
                            code = "max_length";
                            params = [ ("threshold", "2") ];
                          };
                      ] );
                    ( "y1",
                      [
                        Validator.BaseError
                          {
                            code = "max_length";
                            params = [ ("threshold", "2") ];
                          };
                      ] );
                  ];
              ] );
            ( "x1",
              [
                Validator.BaseError
                  { code = "max_length"; params = [ ("threshold", "2") ] };
              ] );
          ]))
    result

let test_validate_record_ok_complex () =
  let r =
    {
      x1 = "111";
      x2 = { y1 = "111"; y2 = "222" };
      x3 = [ { y1 = "1"; y2 = "2" }; { y1 = "11"; y2 = "22" } ];
    }
  in
  let validate =
    Validator.(
      record
        [
          field "x1"
            (fun x -> x.x1)
            [ validate_length_between String.length ~min:1 ~max:3 ];
          field "x2"
            (fun x -> x.x2)
            [
              record
                [
                  field "y1"
                    (fun x -> x.y1)
                    [ validate_length_between String.length ~min:1 ~max:3 ];
                  field "y2"
                    (fun x -> x.y2)
                    [ validate_length_between String.length ~min:1 ~max:3 ];
                ];
            ];
          field "x3"
            (fun x -> x.x3)
            [
              list
                [
                  record
                    [
                      field "y1"
                        (fun x -> x.y1)
                        [ validate_length_between String.length ~min:1 ~max:3 ];
                      field "y2"
                        (fun x -> x.y2)
                        [ validate_length_between String.length ~min:1 ~max:3 ];
                    ];
                ];
            ];
        ])
  in
  let result = validate r in

  Alcotest.(check (result unit validation_error_testable))
    "validate record" (Ok ()) result

let test_validate_list_error () =
  let r = [ "111"; "3"; "" ] in
  let validate =
    Validator.(list [ validate_length_between String.length ~min:1 ~max:2 ])
  in
  let result = validate r in

  Alcotest.(check (result unit validation_error_testable))
    "validate list"
    (Error
       (Validator.IterableError
          [
            ( 2,
              [
                Validator.BaseError
                  { code = "min_length"; params = [ ("threshold", "1") ] };
              ] );
            ( 0,
              [
                Validator.BaseError
                  { code = "max_length"; params = [ ("threshold", "2") ] };
              ] );
          ]))
    result

let test_validate_list_ok () =
  let r = [ "111"; "333"; "444" ] in
  let validate =
    Validator.(
      list
        [
          validate_length_between String.length ~min:1 ~max:3;
          validate_min_length String.length 1;
        ])
  in
  let result = validate r in

  Alcotest.(check (result unit validation_error_testable))
    "validate list" (Ok ()) result

let validate_record =
  let open Alcotest in
  ( "validate_record",
    [
      test_case "Error in simple record" `Quick
        test_validate_record_error_simple;
      test_case "Ok in simple record" `Quick test_validate_record_ok_simple;
      test_case "Error in complex record" `Quick
        test_validate_record_error_complex;
      test_case "Ok in complex record" `Quick test_validate_record_ok_complex;
    ] )

let validate_list =
  let open Alcotest in
  ( "validate_list",
    [
      test_case "Error in list" `Quick test_validate_list_error;
      test_case "Ok in list" `Quick test_validate_list_ok;
    ] )

let t = [ validate_record; validate_list ]
