
: gcd ( n1 n2 -- n )
  begin
    2dup mod rot drop 
    dup 0= until drop ;
  
: fizzbuzz ( u -- )
  cr
  1 do
    i 15 mod
    0= if ." fizzbuzz"
       else i 5 mod 0=
	    if ." buzz"
	    else i 3 mod 0=
		 if ." fizz"
		 else i .
		 then
	    then
       then
    cr
  loop ;

: test-loop ( n1 n2 -- )
  cr do 2 i u. i' 2 u.r cr loop ;


: expt ( nB uN -- uB^N )
  dup 0=
  if 2drop 1
  else
    over swap
    0 do
      2dup * nip
    loop
    swap /
    then ;
