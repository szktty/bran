let prog = "../../src/bran"
let flags = ["-I"; "../../libsrc/stdlib"]
let command = List.concat [[prog]; flags]

let beam_path path =
  Sealing.replace_extension path ".beam"
