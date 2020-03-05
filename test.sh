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

  "$exe" "$input" > tmp.s 2> /dev/null
  status="$?"
  if [ "$status" = 0 ]; then
      echo "$input. error not occured"
      exit 1
  fi
  echo "$input => error"
}

dune build

try 0 "0;"
try 42 "42;"
try 5 "3+2;"
try 10 "7-2+5;"
try 21 "5+20-4;"
try 2 "3+2-3;"
try 3 "(3);"
try 0 "1-(4-3);"
try 2 "(1+3)-2;"
try 3 "1*3;"
try 7 "1+2*3;"
try 1 "3*3/3/3;"
try 2 "3/3*2;"
try 16 "(2+2)*(3/3+3*1);"
try 9 "+3*+3*(+2-+1);"
try 12 "10--2;"
try 30 "-23+53;"
try 9 "-3++5--2++5;"
try 1 "1==1;"
try 1 "2+3*3== 3*3+2;"
try 0 "1==0;"
try 0 "(1==0+2) * (1==1);"
try 0 "3*2==4+3;"
try 0 "1!=1;"
try 1 "1!=0;"
try 0 "1<0;"
try 0 "1<1;"
try 1 "1<2;"
try 0 "1<=0;"
try 1 "1<=1;"
try 1 "1<= 2;"
try 1 "2<=1*3;"
try 0 "2>3;"
try 0 "3>3;"
try 1 "4>3;"
try 0 "1==1>=1+2;"
try 1 "12 >= 3*4*1==1;"
try 1 "12 >= 2*5;"
try 3 "a=3;"
try 10 "b=b=10;"
try 6 "z=2*3;z;"
try 8 "z=3;z=4;z=5;z+3;"
try 8 "abc=3;abc+5;"
try 15 "abc=3;abc*5;"
try 3 "a=b=c=d=e=f=g=h=i=j=k=l=m=3; g;"

try_error "0"
try_error "2 3;"
try_error "23 323 2;"
try_error "1-(4-3;"
try_error ")+1+2 ;"
try_error "2**3;"
try_error "3*2*;"
try_error "23+++23;"
try_error "32*++23;"
try_error "++32*2;"
try_error "--2+3;"
try_error "2+--3;"
try_error "2===4;"
try_error "==2-4;"
try_error "2*==3;"
try_error "2!==3;"
try_error "!=2;"
try_error "2!=;"
try_error "1<;"
try_error "<3;"
try_error "()<(;"
try_error "< <;"
try_error "<=<=;"
try_error "<3<2;"
try_error ">3;"
try_error "><;"
try_error ">=3;"
try_error "3>==4;"
try_error "@=3;"
try_error "#==3;"
try_error "z=3;z=;"
try_error "z=3;z*3"
try_error "23a=3;23a"

echo OK
