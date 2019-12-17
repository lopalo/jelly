let read_file file_name =
  let ch = open_in file_name in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch; str
