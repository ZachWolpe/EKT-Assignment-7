quit ;
dm 'odsresults;clear';

options ls=72 nodate pageno=1 nocenter;

libname ekt "c:\departement\ekt720\logit" ;


proc logistic data=ekt.model outest=ekt.emodel outdesign=ekt.des out=aa;
 class  d3sc d8s gmesc nd5s q50b_7s q55as ;
  model rq34_1s = d3sc d8s gmesc nd5s q50b_7s q55as  / outroc=roc1 lackfit;
run ;

data ekt.des (drop=rq34_1s);
 set ekt.des ;
rq34 = rq34_1s + 0 ;
run ;

proc contents data=ekt.des varnum out=bb;
run ;

data bb (keep=name);
 set bb;
run ;


proc iml ;

use ekt.des ;
read all into xy ;

use bb ;
read all var _char_ into nnn ;

  print "Binary outcome logistic regression" ;

  n=nrow(xy) ;
  print n ;
  
   x =  xy[,1:ncol(xy)-1] ;

   y = 1- xy[,ncol(xy)] ;

*  print x y ;

  bhi = J(ncol(x),1,0) ; 
  bhold = bhi ;


diff = 9999999 ;

do i = 1 to 20  until (diff < 0.000000001);
  prg1 =exp(x*bhold)/(1+exp(x*bhold)) ;

  ppp = x*bhold ;
  pppp = ppp[101:120,] ;
  print pppp ;

  *print i prg1 ;

  w = diag(prg1#(1-prg1)) ;

  *print w1 ;
  
  	  bhnew = bhold  + inv(x`*w*x)*x`*(y-prg1) ;

	  *print bhi  pr1 pr2 prg1 w d1 d2 z bhold bhnew;

	  print "X-X-X-X-X-X" ;
	  print i bhold bhnew;

	  ll = y`*log(prg1)+(J(n,1,1)-y)`*log(J(n,1,1)-prg1) ;
	  print ll   (-2*ll) ;
	  
	  diff = abs(max(bhnew - bhold)) ;
	  bhold = bhnew ;
  
 end; 

 ind = exp(bhnew) ; 
 print nnn bhnew ind; *prg1 ;
quit ;
