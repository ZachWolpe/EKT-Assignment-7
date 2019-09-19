* --- - --- - --- - --- - --- - --- - --- - --- Assignment 7 --- - --- - --- - --- - --- - --- - --- - ---;

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

* -------------------------- Question 1 --------------------------;

proc print data=model2016 (obs=10);
run;

* 2.A Using proc logistic;

proc logistic data=model2016;
	*class gmesc;
	model d8s = q55as q50b_7s nd5s rq34_1s;
run;	




proc print data=model2016 (obs=10);
run;


* 2.B Using IML - first principles;
proc iml;
use model2016;
read all into xy;
y = xy[,1];
n = nrow(y);
x = J(n,1,1)||xy[,2:ncol(xy)];
print x;




















	