#! /usr/bin/env sh

exe="_build/default/llcc.exe"

try() {
  expected="$1"
  input="$2"

  "$exe" "$input" > tmp.s
  status="$?"

  if [ "$status" = 1 ]; then
      echo "$input => error occured"
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

try 0  "main(){0;}"
try 42 "main(){42;}"
try 5 "main(){3+2;}"
try 10 "main(){7-2+5;}"
try 21 "main(){5+20-4;}"
try 2 "main(){3+2-3;}"
try 3 "main(){(3);}"
try 0 "main(){1-(4-3);}"
try 2 "main(){(1+3)-2;}"
try 3 "main(){1*3;}"
try 7 "main(){1+2*3;}"
try 1 "main(){3*3/3/3;}"
try 2 "main(){3/3*2;}"
try 16 "main(){(2+2)*(3/3+3*1);}"
try 9 "main(){+3*+3*(+2-+1);}"
try 12 "main(){10--2;}"
try 30 "main(){-23+53;}"
try 9 "main(){-3++5--2++5;}"
try 1 "main(){1==1;}"
try 1 "main(){2+3*3== 3*3+2;}"
try 0 "main(){1==0;}"
try 0 "main(){(1==0+2) * (1==1);}"
try 0 "main(){3*2==4+3;}"
try 0 "main(){1!=1;}"
try 1 "main(){1!=0;}"
try 0 "main(){1<0;}"
try 0 "main(){1<1;}"
try 1 "main(){1<2;}"
try 0 "main(){1<=0;}"
try 1 "main(){1<=1;}"
try 1 "main(){1<= 2;}"
try 1 "main(){2<=1*3;}"
try 0 "main(){2>3;}"
try 0 "main(){3>3;}"
try 1 "main(){4>3;}"
try 0 "main(){1==1>=1+2;}"
try 1 "main(){12 >= 3*4*1==1;}"
try 1 "main(){12 >= 2*5;}"
try 3 "main(){a=3;}"
try 10 "main(){b=b=10;}"
try 6 "main(){z=2*3;z;}"
try 8 "main(){z=3;z=4;z=5;z+3;}"
try 8 "main(){abc=3;abc+5;}"
try 15 "main(){abc=3;abc*5;}"
try 3 "main(){a=b=c=d=e=f=g=h=i=j=k=l=m=3; g;}"
try 5 "main(){a=5; return a; a+3;}"
try 10 "main(){return 10; return 5; return 0;}"
try 3 "main(){a=1; return a*3;}"
try 3 "main(){a=0; if(3)a=3; a;}"
try 8 "main(){a=0; if(0)a=2; else a=3;a+5;}"
try 2 "main(){a=2; if(1)a=2;}"
try 0 "main(){n=10; while(n) n=n-1;n;}"
try 10 "main(){n=10; while(n < 10) n=n+1;n;}"
try 0 "main(){n=0; while(n) n=n+1; n;}"
try 0 "main(){n=10; while(n = 0) n=n+1; n;}"
try 15 "main(){x = 0;for(n=0; n < 5; n = n+1) x = x + n;}"
try 55 "main(){x = 0;n = 0; for(;n<10; n = n +1) x = x + n;}"
try 13 "main(){for(n=0; n<10; ) n = n +1; n+3;}"
try 3 "main(){for(a=0;a<3;) a=3; a;}"
try 2 "main(){for(;;)return 2;}"
try 4 "main(){for(a=10;a;a = 1+a) a = 0; a+4;}"
try 126 "main(){b= 0; for(a=0;a<5; a = a + 1) { b = b + 1; b = b * 2;} b;}"
try 3 "foo(){} main(){foo(); 3;}"
try 17 "foo3(a,b,c){ return a+b*3;} main(){x=foo3(2, 4, 3 ); return x+ 3;}"
try 33 "foo6(a,b,c,d,e,f){a+ b+c+d+e+f;} main(){foo6(3, 4,5,6,7,  8);}"
try 4 "foo(){return 4;} main(){return foo();}"
try 12 "bar(){return 5;}foo(){return 4;} main(){return foo()+bar()+3;}"
try 3 "bar(a){return a;} main(){3;}"
try 6 "bar (a,b,c) {return a+b+c;} main(){bar(1,2,3);}"
try 67 "foo ( ) { 3; }
bar(a,b,c,d,e,f){return a*b-c*d+e*f;}
main(){x=3; return bar(2*5,x*3,8,7,6,5) + foo();}"

try_error "main(){0}"
try_error "main(){2 3;}"
try_error "main(){23 323 2;}"
try_error "main(){1-(4-3;}"
try_error "main(){)+1+2 ;}"
try_error "main(){2**3;}"
try_error "main(){3*2*;}"
try_error "main(){23+++23;}"
try_error "main(){32*++23;}"
try_error "main(){++32*2;}"
try_error "main(){--2+3;}"
try_error "main(){2+--3;}"
try_error "main(){2===4;}"
try_error "main(){==2-4;}"
try_error "main(){2*==3;}"
try_error "main(){2!==3;}"
try_error "main(){!=2;}"
try_error "main(){2!=;}"
try_error "main(){1<;}"
try_error "main(){<3;}"
try_error "main(){()<(;}"
try_error "main(){< <;}"
try_error "main(){<=<=;}"
try_error "main(){<3<2;}"
try_error "main(){>3;}"
try_error "main(){><;}"
try_error "main(){>=3;}"
try_error "main(){3>==4;}"
try_error "main(){@=3;}"
try_error "main(){#==3;}"
try_error "main(){z+1=3;}"
try_error "main(){z*2=3;}"
try_error "main(){return z*2=3;}"
try_error "main(){z=3;z=;}"
try_error "main(){z=3;z*3}"
try_error "main(){23a=3;23a}"
try_error "main(){retur 3;}"
try_error "main(){return return;}"
try_error "main(){if(3 a=2;}"
try_error "main(){if 3) a=2;}"
try_error "main(){if (3) a=2; el}"
try_error "main(){if (3) a=2 else a=3;}"
try_error "main(){if (3) a=2; else a=3}"
try_error "main(){n=10; whi(n < 10) n=n+1;n;}"
try_error "main(){n=10; while n < 10) n=n+1;n;}"
try_error "main(){n=10; while(n < 10 n=n+1;n;}"
try_error "main(){n=10; while(n < 10) n=n+1 n;}"
try_error "main(){fo(a=0;a<10;a=1) return 3;}"
try_error "main(){for(a=0 a<10;a=1) return 3;}"
try_error "main(){for(a=0; a<10 a=1) return 3;}"
try_error "main(){for(a=0; a<10) return 3;}"
try_error "main(){for(a=0; a<10 ;a) return 3}"
try_error "main(){return=3;return;}"
try_error "main(){for=3;}"
try_error "main(){if=3;}"
try_error "main(){while=3;}"
try_error "main(){a=3;b=4;}}"
try_error "main(){{a=3;b=4;}"
try_error "main(){foo(;}"
try_error "main(){foooo(3 2);}"
try_error "main(){foooo(3,2,);}"
try_error "main(){fooo 2,3);}"
try_error "main({1+2;}"
try_error "main){1+2;}"
try_error "main()1+2;}"
try_error "main(){1+2;"
try_error "foo(a b){1+2;}"
try_error "foo(a,b c){1+2;}"
try_error "foo(a,b){1+2;};"
try_error "foo(a+1, b){1+2;}"
try_error "foo(a){1+2;3+;4+5;return 3;}main(){foo();}"

echo OK
