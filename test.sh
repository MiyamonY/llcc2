#! /usr/bin/env sh

exe="_build/default/llcc.exe"

try() {
  expected="$1"
  input="$2"

  "$exe" "$input" > tmp.s
  status="$?"

  if [ "$status" = 1 ]; then
      echo "error occured"
      exit 1
  fi

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

try_error() {
  input="$1"

  "$exe" "$input" > tmp.s
  status="$?"
  if [ "$status" = 0 ]; then
      echo "$input. error not occured"
      exit 1
  fi
}

dune build

try 0 0
try 42 42
try 5 "3+2"
try 10 "7-2+5"
try 21 "5+20-4"
try 3 "(3)"
try 0 "1-(4-3)"
try 2 "(1+3)-2"

try_error "234+24-a"
try_error "-23+3"
try_error "23++23"
try_error "1-(4-3"
try_error ")+1+2 "

echo OK
