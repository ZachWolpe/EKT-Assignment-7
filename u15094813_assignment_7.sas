* --- - --- - --- - --- - --- - --- - --- - --- Assignment 7 --- - --- - --- - --- - --- - --- - --- - ---;




* -------------------------- Logistic Regression --------------------------;


data a (keep = age chd )  ;
input nr age chd @@;
cards ;
1 20 0 2  23 0 3  24 0 4  25 0
5 25 1 6  26 0 7  26 0 8  28 0
9 28 0 10 29 0 11 30 0 12 30 0
13 30 0 14 30 0 15 30 0 16 30 1
17 32 0 18 32 0 19 33 0 20 33 0
21 34 0 22 34 0 23 34 1 24 34 0
25 34 0 26 35 0 27 35 0 28 36 0
29 36 1 30 36 0 31 37 0 32 37 1
33 37 0 34 38 0 35 38 0 36 39 0
37 39 1 38 40 0 39 40 1 40 41 0
41 41 0 42 42 0 43 42 0 44 42 0
45 42 1 46 43 0 47 43 0 48 43 1
49 44 0 50 44 0 51 44 1 52 44 1
53 45 0 54 45 1 55 46 0 56 46 1
57 47 0 58 47 0 59 47 1 60 48 0
61 48 1 62 48 1 63 49 0 64 49 0
65 49 1 66 50 0 67 50 1 68 51 0
69 52 0 70 52 1 71 53 1 72 53 1
73 54 1 74 55 0 75 55 1 76 55 1
77 56 1 78 56 1 79 56 1 80 57 0
81 57 0 82 57 1 83 57 1 84 57 1
85 57 1 86 58 0 87 58 1 88 58 1
89 59 1 90 59 1 91 60 0 92 60 1
93 61 1 94 62 1 95 62 1 96 63 1
97 64 0 98 64 1 99 65 1 100 69 1
;
run ;

proc means data=a;
	var chd age;
run;

proc sgplot data=a;
	scatter y=chd x=age;
run;

proc logistic data=a;
	model chd(event='1') = age / outroc=rocl lackfit;
	output out=b p=pred;
run;



proc iml;
use a;
read all into data;
y = data[,2];
n = nrow(y);
x = J(n,1,1)||data[,1];

* randomly initialize Beta=0 ==> p(x,b)=0.5 ;
bo = {0,0};
diff = 100;


do i=1 to 15 while (diff > 0.0001);
	lo = x*bo;
	o = exp(lo);
	p = o/(1+o);
	w = diag(p#(1-p));

	bn = bo + inv(X`*W*X)*X`*(y-p);
	diff = max(abs(bn-bo));
	
	print i bn bo;
	bo = bn;
	
	if diff <= 0.0001 then; print 'covergence at iteration' i; 
end;
quit;

proc logistic data=a;
	model chd(event='1') = age / outroc=roc1 lackfit;
	* model chd2(event='1') = age / outroc=roc1 lackfit;
	output out=b p=pred;
run;


proc means data=b 
	min p10 p20 p30 p40 p50 p60 p70 p80 p90 max;
var pred;
output out=cc 
	min= p10= p20= p30= p40= p50= p60= p70= p80= p90= max = / autoname;
run;


 

data dd (drop = _TYPE_ _FREQ_);
	set b;
	if _N_=1 then set cc;
	if pred <= pred_max then pred_c = "p80 - max";
	if pred <= pred_p80 then pred_c = "p60 - p80";
	if pred <= pred_p60 then pred_c = "p40 - p60";
	if pred <= pred_p40 then pred_c = "p20 - p40";
	if pred <= pred_p20 then pred_c = "min - p20";
run ; 

proc freq data=dd;
	tables pred_c*chd;
run;

proc freq data=dd;
	tables pred_c*chd / out=eel;
	where chd=0;
run;

data ee1 (keep=pred_c freq0 perc0 cumper0);
	set eel;
	freq0 = count;
	perc0 = percent;
	retain cumper0;
	cumper0 + perc0;
run;

proc freq data=dd;
	tables pred_c*chd / out=ee2;
	where chd=1;
run;

data ee2 (keep = pred_c freq1 perc1 cumper1);
	set ee2;
	freq1 = count;
	perc1 = percent;
	retain cumper1;
	cumper1 + perc1;
run;

data ff;
	pred_c = 'min';
	cumper0 = 0;
	cumper1 = 0;
run;

data gg;
	merge ff ee1 ee2;
	by pred_c;
run;

data gg;
set gg;
	ks = abs(cumper1 - cumper0);	
	ksr = ks/100;
run;

proc means data=gg max;
	var ks ksr;
run;

proc print data=gg;
run;

data b;
set b;
chd_p = 0;
if pred > 0.5 then chd_p = 1;
run;


proc freq data=b;
	tables chd*chd_p;
run;

symbol1 interpol=join width=4 color=blue value=dot height=1;
symbol2 interpol=join width=3 color=red value=dot height=1;


proc sgplot data=gg;
	series y=cumper1 x=cumper1; 
	series y=cumper0 x=cumper1; 
run;

proc sgplot data=gg;
	series y=cumper1 x=pred_c;
	series y=cumper0 x=pred_c;
run;
ods pdf close ; 










* -------------------------- Additional Data --------------------------;

title 'Assignment 7';
proc import
	datafile='/folders/myfolders/sasuser.v94/EKT 720/Assignment 7/description.xls'
	dbms=xls 
	out=desc
	replace;
run;

proc import 
	datafile='/folders/myfolders/sasuser.v94/EKT 720/Assignment 7/model2016.xlsx'
	dbms=xlsx
	out=model2016
	replace;
run;







	