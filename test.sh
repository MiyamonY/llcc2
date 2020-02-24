#! /usr/bin/env sh

try() {
  expected="$1"
  input="$2"
  exe="_build/default/llcc.exe"

  "$exe" "$input" > tmp.s
  gcc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

dune build

try 0 0
try 42 42
try 5 "3+2"
try 10 "7-2+5"
try 21 "5+20-4"
try 22 "23+23+1-23-3-4+5"

echo OK
