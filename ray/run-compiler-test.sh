#!/bin/bash

function errwith {
  echo "$1" >&2
  exit 1
}

test "$#" -lt 1 && errwith "Please give a file to test"
file=$1

test -e "$file" || errwith "File $file does not exist."
test -f "$file" || errwith "File $file is not a file."

echo "=========================="
echo "$file"
cat "$file"
echo "=========================="
./bin/ray "$file" > ctest/test.c && cd ctest && ./compile && ./a.out Test

