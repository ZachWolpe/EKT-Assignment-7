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

proc print data=desc (obs=10);
run;

proc print data=model2016 (obs=10);
run;