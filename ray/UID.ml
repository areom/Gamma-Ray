(** A global UID generator *)

(** The number of digits in a UID [error after rollover] *)
let uid_digits = 8

(**
    A function to return the a fresh UID. Note that UIDs are copies,
    so they need not be copied on their own
  *)
let uid_counter =
    let counter = String.make uid_digits '0' in
    let inc () =
        let i = ref (uid_digits - 1) in
        while (!i >= 0) && (String.get counter (!i) = 'z') do
            String.set counter (!i) '0' ;
            i := !i - 1
        done ;
        String.set counter (!i) (match String.get counter (!i) with
            | '9' -> 'A'
            | 'Z' -> 'a'
            | c -> char_of_int (int_of_char c + 1));
        String.copy counter in
    inc
