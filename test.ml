open Doca
open Format


let d = document [
  emph ~~"Hello";
  verb ~~"HDFSF";
  section (verb ~~"Section") [
    ~~"Body1";
    ~~"Body2";
    ~~"Body3";
  ]
]

let en = language `en
let de = language `de

let _ =
  print (ansi_printer Format.std_formatter) d;
  print (text_printer Format.std_formatter) d;
  ()
