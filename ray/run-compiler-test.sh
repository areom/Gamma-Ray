#!/bin/bash

function errwith {
  echo "$1" >&2
  exit 1
}

test "$#" -lt 1 && errwith "Please give a file to test"
file=$1
main=$2

test -e "$file" || errwith "File $file does not exist."
test -f "$file" || errwith "File $file is not a file."

args=()
test -n "$main" && args+=( "$main" )
./bin/ray "$file" > ctest/test.c && cd ctest && ./compile && ./a.out "${args[@]}"

