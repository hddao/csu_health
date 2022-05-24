*** Simulation comparing augmented backward elimination (ABE), backward elimination (BE) and various pre-specified models;
*** for linear, logistic and Cox-proportional hazards regression;
***;
*** Daniela Dunkler & Georg Heinze, Sept. 2014, Version 1.0;
***;
*************************************************************************************************************************;

libname abe "C:\ ... \abe";                                        * adapt path accordingly;

%include "C:\ ... \ABE.sas";                                       * %ABE SAS macro, adapt path accordingly;
%include "C:\ ... \Simulation\Macros_for_simulation.sas";          * some SAS macros for the simulation, adapt path accordingly;  

options linesize=180 nocenter;

*************************************************************************************************************************;
*************************************************************************************************************************;
*************************************************************************************************************************;
*** Cox model;
%let name      = ABE_COX_V2_B1_4;           * name of scenario;
%let nsim      = 1000;                      * number of simulated data sets;
%let n         = 120;                       * sample size;
%let beta1     = 1;                         * regression coefficient of variable of main interest X1, either 1 or 0;
%let seed      = 712348;                    * random seed;
%let procedure = phreg;                     * type of regression;
%let tau       = 3.35;                      * for censoring;


data simdesign;
do isim=1 to &nsim;
	do i=1 to &n;
		xroot = rannor(&seed);
		x2 = xroot + rannor(&seed);
		x3 = xroot - rannor(&seed);
		x4 = xroot + rannor(&seed);
		x5 = xroot + rannor(&seed);
		x6= rannor(&seed);
		x7= rannor(&seed);

		*** x1 confounded by x2, x4, x6;
        *** scenario with VIF = 2 or;
        x1 = (0.3*x2+0.3*x3+0.3*x6+rannor(&seed)*0.8) / 1.1265776  / 2;   
		*** scenario with VIF = 4;
*		x1 = (0.6*x2+0.6*x3+0.6*x6+rannor(&seed)*0.8) / 1.7810749 / 2;    

        x2 = x2/sqrt(2) / 2;
		x3 = x3/sqrt(2) / 2;
		x4 = x4/sqrt(2) / 2;
		x5 = x5/sqrt(2) / 2;
		x6 = x6 / 2;
		x7 = x7 / 2;
		lp = &beta1 * x1 + x2 + x4 + x7;   
	    y = (-log(ranuni(&seed))/(1/8*exp(lp)))**(1/3);
		cens = 1;
		futime = 0.001 + ranuni(&seed) * &tau;
		if y > futime then do;
			cens = 0;
			y = futime;
		end;	
		output;
	end;
end;
run;

proc corr data=simdesign;
var x1-x7 lp y cens;
run;

proc freq data=simdesign; 
 tables cens; 
run;

proc lifetest data=simdesign notable;
 time y * Cens(0);
run;


*** investigate bias of 'mismodeling';
data allmodels;
run;

options nonotes;
ods graphics off;
proc phreg data=simdesign outest=models noprint; 
by isim;
A_CORRECT: model y*cens(0)=x1 x2 x4 x7; *correct model;
%nameit(A_CORRECT);

proc phreg data=simdesign outest=models noprint; 
by isim;
A_CONFOMIT: model y*cens(0)=x1; * correct confounders omitted;
%nameit(A_CONFOMIT);

proc phreg data=simdesign outest=models noprint; 
by isim;
A_WRONGCONF_FULL: model y*cens(0)=x1 x2 x4  x3 x5 x6 x7; *wrong confounders included;
%nameit(A_WRONGCONF_FULL);

proc phreg data=simdesign outest=models noprint; 
by isim;
A_OMITWRONG: model y*cens(0)=x1 x3 x5 x6; *correct omitted&wrong included;
%nameit(A_OMITWRONG);

proc phreg data=simdesign outest=models noprint; 
by isim;
B_DISJUNCTIVE: model y*cens(0)=x1 x2 x3 x4 x6 x7;
%nameit(B_DISJUNCTIVE);

proc phreg data=simdesign outest=models noprint; 
by isim;
B_COMMONCAUSE: model y*cens(0)=x1 x2;
%nameit(B_COMMONCAUSE);

proc phreg data=simdesign outest=models noprint; 
by isim;
C_BW_0_25: model y*cens(0)=x1 x2 x3 x4 x5 x6 x7/selection=backward include=1 slstay=0.25;
%nameit(C_BW_0_25);

proc phreg data=simdesign outest=models noprint; 
by isim;
C_BW_0_20: model y*cens(0)=x1 x2 x3 x4 x5 x6 x7/selection=backward include=1 slstay=0.20;
%nameit(C_BW_0_20);

proc phreg data=simdesign outest=models noprint; 
by isim;
C_BW_0_157: model y*cens(0)=x1 x2 x3 x4 x5 x6 x7/selection=backward include=1 slstay=0.157;
%nameit(C_BW_0_157);

proc phreg data=simdesign outest=models noprint; 
by isim;
C_BW_0_10: model y*cens(0)=x1 x2 x3 x4 x5 x6 x7/selection=backward include=1 slstay=0.10;
%nameit(C_BW_0_10);

proc phreg data=simdesign outest=models noprint; 
by isim;
C_BW_0_05: model y*cens(0)=x1 x2 x3 x4 x5 x6 x7/selection=backward include=1 slstay=0.05;
%nameit(C_BW_0_05);


proc phreg data=simdesign outest=models noprint; 
by isim;
D_BW_DISJ_0_25: model y*cens(0)=x1 x2 x3 x4 x6 x7/selection=backward include=1 slstay=0.25;
%nameit(D_BW_DISJ_0_25);

proc phreg data=simdesign outest=models noprint; 
by isim;
D_BW_DISJ_0_20: model y*cens(0)=x1 x2 x3 x4 x6 x7/selection=backward include=1 slstay=0.20;
%nameit(D_BW_DISJ_0_20);


proc phreg data=simdesign outest=models noprint; 
by isim;
D_BW_DISJ_0_157: model y*cens(0)=x1 x2 x3 x4 x6 x7/selection=backward include=1 slstay=0.157;
%nameit(D_BW_DISJ_0_157);

proc phreg data=simdesign outest=models noprint; 
by isim;
D_BW_DISJ_0_10: model y*cens(0)=x1 x2 x3 x4 x6 x7/selection=backward include=1 slstay=0.10;
%nameit(D_BW_DISJ_0_10);

proc phreg data=simdesign outest=models noprint; 
by isim;
D_BW_DISJ_0_05: model y*cens(0)=x1 x2 x3 x4 x6 x7/selection=backward include=1 slstay=0.05;
%nameit(D_BW_DISJ_0_05);

quit;
data models;
set allmodels;
run;


%abesim2(nsim=&nsim, proc=&procedure);
options notes;

data abe.stacked_&name;
 set models allabe;
 drop _TYPE_ _TIES_ _NAME_ _STATUS_; 
 if isim=. then delete;

 converged = _STATUS_;

 if x2=0 then x2=.;
 if x3=0 then x3=.;
 if x4=0 then x4=.;
 if x5=0 then x5=.;
 if x6=0 then x6=.;
 if x7=0 then x7=.;

 selected_x2=0;
 selected_x3=0;
 selected_x4=0;
 selected_x5=0;
 selected_x6=0;
 selected_x7=0;
 
 if x2^=. then selected_x2=1;
 if x3^=. then selected_x3=1;
 if x4^=. then selected_x4=1;
 if x5^=. then selected_x5=1;
 if x6^=. then selected_x6=1;
 if x7^=. then selected_x7=1;

 no_vars=1+selected_x2+selected_x3+selected_x4+selected_x5+selected_x6+selected_x7;

 if method="ABE-DISJ-NONE-p0.25-tau0.05"  or method="ABE-DISJ-NONE-p0.25-tau0.05-active" or method="ABE-DISJ-NONE-p0.25-tau0.1" or 
    method="ABE-DISJ-NONE-p0.25-tau0.1-active" or method="ABE-DISJ-NONE-p0.25-tau0.2" or method="ABE-DISJ-NONE-p0.25-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.20-tau0.05"  or method="ABE-DISJ-NONE-p0.20-tau0.05-active" or method="ABE-DISJ-NONE-p0.20-tau0.1" or
    method="ABE-DISJ-NONE-p0.20-tau0.1-active" or method="ABE-DISJ-NONE-p0.20-tau0.2" or method="ABE-DISJ-NONE-p0.20-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.157-tau0.05" or method="ABE-DISJ-NONE-p0.157-tau0.05-active" or method="ABE-DISJ-NONE-p0.157-tau0.1" or 
    method="ABE-DISJ-NONE-p0.157-tau0.1-active"  or method="ABE-DISJ-NONE-p0.157-tau0.2" or method="ABE-DISJ-NONE-p0.157-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.10-tau0.05"  or method="ABE-DISJ-NONE-p0.10-tau0.05-active"  or method="ABE-DISJ-NONE-p0.10-tau0.1" or 
    method="ABE-DISJ-NONE-p0.10-tau0.1-active"   or method="ABE-DISJ-NONE-p0.10-tau0.2" or method="ABE-DISJ-NONE-p0.10-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.05-tau0.05"  or method="ABE-DISJ-NONE-p0.05-tau0.05-active"  or method="ABE-DISJ-NONE-p0.05-tau0.1" or 
    method="ABE-DISJ-NONE-p0.05-tau0.1-active" or method="ABE-DISJ-NONE-p0.05-tau0.2" or method="ABE-DISJ-NONE-p0.05-tau0.2-active" or 
    method="D_BW_DISJ_0_05" or method="D_BW_DISJ_0_10" or method="D_BW_DISJ_0_157" or method="D_BW_DISJ_0_20" or method="D_BW_DISJ_0_25" 
    then selected_x5=.; 
run;
proc sort data=abe.stacked_&name; by method isim; run;

proc means mean std p50 p25 p75 data=abe.stacked_&name noprint;
var x1;
class method;
output out=abe.&name mean= std= n= p50= p25= p75= / autoname;
run;

data abe.&name; set abe.&name;
 drop _type_ _freq_;
 if _type_=0 then delete;

 bias = x1_mean - &beta1;
 mse = (bias*bias) + (x1_StdDev*x1_StdDev);
 rmse = sqrt(mse);
run;
proc print data=abe.&name; run;

proc freq data=abe.stacked_&name;
  tables method*no_vars / nocol nopercent out=freq1; 
run;
proc freq data=abe.stacked_&name noprint;
 tables method*selected_x2 / nocol nopercent out=freq2;
 tables method*selected_x3 / nocol nopercent out=freq3;
 tables method*selected_x4 / nocol nopercent out=freq4;
 tables method*selected_x5 / nocol nopercent out=freq5;
 tables method*selected_x6 / nocol nopercent out=freq6;
 tables method*selected_x7 / nocol nopercent out=freq7;
run;

data freq1; set freq1; keep method no_vars no_vars_count; no_vars_count=count; run;
data freq2; set freq2; keep method selected_x2 selected_x2_count; selected_x2_count=count; run;
data freq3; set freq3; keep method selected_x3 selected_x3_count; selected_x3_count=count; run;
data freq4; set freq4; keep method selected_x4 selected_x4_count; selected_x4_count=count; run;
data freq5; set freq5; keep method selected_x5 selected_x5_count; selected_x5_count=count; run;
data freq6; set freq6; keep method selected_x6 selected_x6_count; selected_x6_count=count; run;
data freq7; set freq7; keep method selected_x7 selected_x7_count; selected_x7_count=count; run;
data abe.freq_&name; merge freq1-freq7; 
 by method;
run; 

*************************************************************************************************************************;
*************************************************************************************************************************;
*************************************************************************************************************************;
*** Linear model;

%let name      = abe_LIN_V2_B1_4;        * name of scenario;
%let nsim      = 1000;                   * number of simulated data sets;
%let n         = 120;                    * sample size;
%let beta1     = 1;                      * regression coefficient of variable of main interest X1, either 1 or 0;
%let seed      = 712348;                 * random seed;
%let procedure = reg;                    * type of regression;

data simdesign;
do isim=1 to &nsim;
	do i=1 to &n;
		xroot = rannor(&seed);
		x2 = xroot + rannor(&seed);
		x3 = xroot - rannor(&seed);
		x4 = xroot + rannor(&seed);
		x5 = xroot + rannor(&seed);
		x6 = rannor(&seed);
		x7 = rannor(&seed);

		*** x1 confounded by x2, x4, x6;
        *** scenario with VIF = 2 or;
        x1=(0.3*x2+0.3*x3+0.3*x6+rannor(&seed)*0.8) / 1.1265776;      
		*** scenario with VIF = 4;
*		x1=(0.6*x2+0.6*x3+0.6*x6+rannor(&seed)*0.8) / 1.7810749;      

        x2 = x2 / sqrt(2);
		x3 = x3 / sqrt(2);
		x4 = x4 / sqrt(2);
		x5 = x5 / sqrt(2);
		y = &beta1 * x1 + x2 + x4 + x7 + rannor(&seed) * 3.6;
		output;
	end;
end;
run;


proc corr data=simdesign;
var x1-x7;
run;

*** investigate bias of 'mismodeling';
ods graphics off;
proc reg data=simdesign outest=models noprint; 
by isim;
A_CORRECT: model y=x1 x2 x4 x7;                   * correct model;
A_CONFOMIT: model y=x1;                           * correct confounders omitted;
A_WRONGCONF_FULL: model y=x1 x2 x4 x3 x5 x6 x7;   * wrong confounders included;
A_OMITWRONG: model y=x1 x3 x5 x6;                 * correct omitted&wrong included;
B_DISJUNCTIVE: model y=x1 x2 x3 x4 x6 x7;
B_COMMONCAUSE: model y=x1 x2;
C_BW_0_25: model y=x1 x2 x3 x4 x5 x6 x7 / selection=backward include=1 slstay=0.25;
C_BW_0_20: model y=x1 x2 x3 x4 x5 x6 x7 / selection=backward include=1 slstay=0.20;
C_BW_0_157: model y=x1 x2 x3 x4 x5 x6 x7 / selection=backward include=1 slstay=0.157;
C_BW_0_10: model y=x1 x2 x3 x4 x5 x6 x7 / selection=backward include=1 slstay=0.10;
C_BW_0_05: model y=x1 x2 x3 x4 x5 x6 x7 / selection=backward include=1 slstay=0.05;
D_BW_DISJ_0_25: model y=x1 x2 x3 x4 x6 x7 / selection=backward include=1 slstay=0.25;
D_BW_DISJ_0_20: model y=x1 x2 x3 x4 x6 x7 / selection=backward include=1 slstay=0.20;
D_BW_DISJ_0_157: model y=x1 x2 x3 x4 x6 x7 / selection=backward include=1 slstay=0.157;
D_BW_DISJ_0_10: model y=x1 x2 x3 x4 x6 x7 / selection=backward include=1 slstay=0.10;
D_BW_DISJ_0_05: model y=x1 x2 x3 x4 x6 x7 / selection=backward include=1 slstay=0.05;
run;
quit;
data models;
set models;
length method $ 50;
method=_model_;
run;

options nonotes;
%abesim1(nsim=&nsim, proc=&procedure);
options notes;

data abe.stacked_&name;
 set models allabe;
 drop _MODEL_ _TYPE_ _DEPVAR_ y; 
 if isim=. then delete;

 if x2=0 then x2=.;
 if x3=0 then x3=.;
 if x4=0 then x4=.;
 if x5=0 then x5=.;
 if x6=0 then x6=.;
 if x7=0 then x7=.;

 selected_x2=0;
 selected_x3=0;
 selected_x4=0;
 selected_x5=0;
 selected_x6=0;
 selected_x7=0;
 
 if x2^=. then selected_x2=1;
 if x3^=. then selected_x3=1;
 if x4^=. then selected_x4=1;
 if x5^=. then selected_x5=1;
 if x6^=. then selected_x6=1;
 if x7^=. then selected_x7=1;

 no_vars=1+selected_x2+selected_x3+selected_x4+selected_x5+selected_x6+selected_x7;

 if method="ABE-DISJ-NONE-p0.25-tau0.05" or method="ABE-DISJ-NONE-p0.25-tau0.05-active" or 
    method="ABE-DISJ-NONE-p0.25-tau0.1" or method="ABE-DISJ-NONE-p0.25-tau0.1-active" or method="ABE-DISJ-NONE-p0.25-tau0.2" or method="ABE-DISJ-NONE-p0.25-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.20-tau0.05" or method="ABE-DISJ-NONE-p0.20-tau0.05-active" or 
    method="ABE-DISJ-NONE-p0.20-tau0.1" or method="ABE-DISJ-NONE-p0.20-tau0.1-active" or method="ABE-DISJ-NONE-p0.20-tau0.2" or method="ABE-DISJ-NONE-p0.20-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.157-tau0.05" or method="ABE-DISJ-NONE-p0.157-tau0.05-active" or
    method="ABE-DISJ-NONE-p0.157-tau0.1" or method="ABE-DISJ-NONE-p0.157-tau0.1-active" or method="ABE-DISJ-NONE-p0.157-tau0.2" or method="ABE-DISJ-NONE-p0.157-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.10-tau0.05" or method="ABE-DISJ-NONE-p0.10-tau0.05-active" or 
    method="ABE-DISJ-NONE-p0.10-tau0.1" or method="ABE-DISJ-NONE-p0.10-tau0.1-active" or method="ABE-DISJ-NONE-p0.10-tau0.2" or method="ABE-DISJ-NONE-p0.10-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.05-tau0.05" or method="ABE-DISJ-NONE-p0.05-tau0.05-active" or 
    method="ABE-DISJ-NONE-p0.05-tau0.1" or method="ABE-DISJ-NONE-p0.05-tau0.1-active" or method="ABE-DISJ-NONE-p0.05-tau0.2" or method="ABE-DISJ-NONE-p0.05-tau0.2-active" or 
    method="D_BW_DISJ_0_05" or method="D_BW_DISJ_0_10" or method="D_BW_DISJ_0_157" or method="D_BW_DISJ_0_20" or method="D_BW_DISJ_0_25" 
 then selected_x5=.; 
run;
proc sort data=abe.stacked_&name; by method isim; run;

proc means mean std p50 p25 p75 data=abe.stacked_&name noprint;
var x1;
class method;
output out=abe.&name mean= std= n= p50= p25= p75= / autoname;
run;

data abe.&name; set abe.&name;
 drop _type_ _freq_;
 if _type_=0 then delete;

 bias = x1_mean - &beta1;
 mse = (bias*bias) + (x1_StdDev*x1_StdDev);
 rmse = sqrt(mse);
run;
proc print data=abe.&name; run;

proc freq data=abe.stacked_&name;
  tables method*no_vars / nocol nopercent out=freq1; 
run;
proc freq data=abe.stacked_&name noprint;
 tables method*selected_x2 / nocol nopercent out=freq2;
 tables method*selected_x3 / nocol nopercent out=freq3;
 tables method*selected_x4 / nocol nopercent out=freq4;
 tables method*selected_x5 / nocol nopercent out=freq5;
 tables method*selected_x6 / nocol nopercent out=freq6;
 tables method*selected_x7 / nocol nopercent out=freq7;
run;

data freq1; set freq1; keep method no_vars no_vars_count; no_vars_count=count; run;
data freq2; set freq2; keep method selected_x2 selected_x2_count; selected_x2_count=count; run;
data freq3; set freq3; keep method selected_x3 selected_x3_count; selected_x3_count=count; run;
data freq4; set freq4; keep method selected_x4 selected_x4_count; selected_x4_count=count; run;
data freq5; set freq5; keep method selected_x5 selected_x5_count; selected_x5_count=count; run;
data freq6; set freq6; keep method selected_x6 selected_x6_count; selected_x6_count=count; run;
data freq7; set freq7; keep method selected_x7 selected_x7_count; selected_x7_count=count; run;
data abe.freq_&name; merge freq1-freq7; 
 by method;
run; 

*************************************************************************************************************************;
*************************************************************************************************************************;
*************************************************************************************************************************;
*** logistic model;

libname abe "C:\Users\dani\Desktop\abe";
%include "C:\Users\dani\Desktop\abe\Simulation macros.sas";
%include "C:\Users\dani\Desktop\abe\ABE.sas";

options linesize=180 nocenter;

%let name      = abe_LOG_V2_B1_4;             * name of scenario;
%let nsim      = 1000;                        * number of simulated data sets;
%let n         = 120;                         * sample size;
%let beta1     = 1;                           * regression coefficient of variable of main interest X1, either 1 or 0;
%let seed      = 712348;                      * random seed;
%let procedure = logistic;                    * type of regression;

data simdesign;
do isim=1 to &nsim;
	do i=1 to &n;
		xroot=rannor(&seed);
		x2=xroot+rannor(&seed);
		x3=xroot-rannor(&seed);
		x4=xroot+rannor(&seed);
		x5=xroot+rannor(&seed);
		x6=rannor(&seed);
		x7=rannor(&seed);

		*** x1 confounded by x2, x4, x6;
        *** scenario with VIF = 2 or;
		x1=(0.3*x2+0.3*x3+0.3*x6+rannor(&seed)*0.8) / 1.1265776;      
        *** scenario with VIF = 4;
*		x1=(0.6*x2+0.6*x3+0.6*x6+rannor(&seed)*0.8) / 1.7810749;      

        x2 = x2 / sqrt(2);
		x3 = x3 / sqrt(2);
		x4 = x4 / sqrt(2);
		x5 = x5 / sqrt(2);

		lp = &beta1 * x1 + x2 + x4 + x7;
		py = 1 /(1 + exp(-lp));
		y = ranbin(&seed, 1, py);
		output;
	end;
end;
run;


proc corr data=simdesign;
var x1-x7 lp y;
run;


*** investigate bias of 'mismodeling';
data allmodels;
run;
options nonotes;
ods graphics off;

proc logistic descending data=simdesign outest=models noprint; 
by isim;
A_CORRECT: model y=x1 x2 x4 x7; *correct model;
%nameit(A_CORRECT);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
A_CONFOMIT: model y=x1; * correct confounders omitted;
%nameit(A_CONFOMIT);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
A_WRONGCONF_FULL: model y=x1 x2 x4  x3 x5 x6 x7; *wrong confounders included / pretreatment;
%nameit(A_WRONGCONF_FULL);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
A_OMITWRONG: model y=x1 x3 x5 x6; *correct omitted & wrong included;
%nameit(A_OMITWRONG);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
B_DISJUNCTIVE: model y=x1 x2 x3 x4 x6 x7; *disjunctive cause;
%nameit(B_DISJUNCTIVE);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
B_COMMONCAUSE: model y=x1 x2; *common cause;
%nameit(B_COMMONCAUSE);


proc logistic descending data=simdesign outest=models noprint; 
by isim;
C_BW_0_25: model y=x1 x2 x3 x4 x5 x6 x7 / selection=backward include=1 slstay=0.25;
%nameit(C_BW_0_25);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
C_BW_0_20: model y=x1 x2 x3 x4 x5 x6 x7 / selection=backward include=1 slstay=0.20;
%nameit(C_BW_0_20);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
C_BW_0_157: model y=x1 x2 x3 x4 x5 x6 x7 / selection=backward include=1 slstay=0.157;
%nameit(C_BW_0_157);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
C_BW_0_10: model y=x1 x2 x3 x4 x5 x6 x7 / selection=backward include=1 slstay=0.10;
%nameit(C_BW_0_10);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
C_BW_0_05: model y=x1 x2 x3 x4 x5 x6 x7 / selection=backward include=1 slstay=0.05;
run;
%nameit(C_BW_0_05);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
D_BW_DISJ_0_25: model y=x1 x2 x3 x4 x6 x7 / selection=backward include=1 slstay=0.25;
%nameit(D_BW_DISJ_0_25);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
D_BW_DISJ_0_20: model y=x1 x2 x3 x4 x6 x7 / selection=backward include=1 slstay=0.20;
%nameit(D_BW_DISJ_0_20);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
D_BW_DISJ_0_157: model y=x1 x2 x3 x4 x6 x7 / selection=backward include=1 slstay=0.157;
%nameit(D_BW_DISJ_0_157);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
D_BW_DISJ_0_10: model y=x1 x2 x3 x4 x6 x7 / selection=backward include=1 slstay=0.10;
%nameit(D_BW_DISJ_0_10);

proc logistic descending data=simdesign outest=models noprint; 
by isim;
D_BW_DISJ_0_05: model y=x1 x2 x3 x4 x6 x7 / selection=backward include=1 slstay=0.05;
run;
%nameit(D_BW_DISJ_0_05);

quit;
data models;
set allmodels;
run;

options nonotes;
%abesim1(nsim=&nsim, proc=&procedure);
options notes;

data abe.stacked_&name;
 set models allabe;
 drop _LINK_ _TYPE_ _STATUS_ _NAME_;
 if isim=. then delete;

 if x2=0 then x2=.;
 if x3=0 then x3=.;
 if x4=0 then x4=.;
 if x5=0 then x5=.;
 if x6=0 then x6=.;
 if x7=0 then x7=.;

 selected_x2=0;
 selected_x3=0;
 selected_x4=0;
 selected_x5=0;
 selected_x6=0;
 selected_x7=0;
 
 if x2^=. then selected_x2=1;
 if x3^=. then selected_x3=1;
 if x4^=. then selected_x4=1;
 if x5^=. then selected_x5=1;
 if x6^=. then selected_x6=1;
 if x7^=. then selected_x7=1;

 no_vars=1+selected_x2+selected_x3+selected_x4+selected_x5+selected_x6+selected_x7;

 if method="ABE-DISJ-NONE-p0.25-tau0.05" or method="ABE-DISJ-NONE-p0.25-tau0.05-active" or 
    method="ABE-DISJ-NONE-p0.25-tau0.1" or method="ABE-DISJ-NONE-p0.25-tau0.1-active" or method="ABE-DISJ-NONE-p0.25-tau0.2" or method="ABE-DISJ-NONE-p0.25-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.20-tau0.05" or method="ABE-DISJ-NONE-p0.20-tau0.05-active" or 
    method="ABE-DISJ-NONE-p0.20-tau0.1" or method="ABE-DISJ-NONE-p0.20-tau0.1-active" or method="ABE-DISJ-NONE-p0.20-tau0.2" or method="ABE-DISJ-NONE-p0.20-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.157-tau0.05" or method="ABE-DISJ-NONE-p0.157-tau0.05-active" or
    method="ABE-DISJ-NONE-p0.157-tau0.1" or method="ABE-DISJ-NONE-p0.157-tau0.1-active" or method="ABE-DISJ-NONE-p0.157-tau0.2" or method="ABE-DISJ-NONE-p0.157-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.10-tau0.05" or method="ABE-DISJ-NONE-p0.10-tau0.05-active" or 
    method="ABE-DISJ-NONE-p0.10-tau0.1" or method="ABE-DISJ-NONE-p0.10-tau0.1-active" or method="ABE-DISJ-NONE-p0.10-tau0.2" or method="ABE-DISJ-NONE-p0.10-tau0.2-active" or
    method="ABE-DISJ-NONE-p0.05-tau0.05" or method="ABE-DISJ-NONE-p0.05-tau0.05-active" or 
    method="ABE-DISJ-NONE-p0.05-tau0.1" or method="ABE-DISJ-NONE-p0.05-tau0.1-active" or method="ABE-DISJ-NONE-p0.05-tau0.2" or method="ABE-DISJ-NONE-p0.05-tau0.2-active" or 
    method="D_BW_DISJ_0_05" or method="D_BW_DISJ_0_10" or method="D_BW_DISJ_0_157" or method="D_BW_DISJ_0_20" or method="D_BW_DISJ_0_25" 
 then selected_x5=.; 
run;
proc sort data=abe.stacked_&name; by method isim; run;

proc means mean std p50 p25 p75 data=abe.stacked_&name noprint;
var x1;
class method;
output out=abe.&name mean= std= n= p50= p25= p75= / autoname;
run;

data abe.&name; set abe.&name;
 drop _type_ _freq_;
 if _type_=0 then delete;

 bias = x1_mean - &beta1;
 mse = (bias*bias) + (x1_StdDev*x1_StdDev);
 rmse = sqrt(mse);
run;
proc print data=abe.&name; run;

proc freq data=abe.stacked_&name;
  tables method*no_vars / nocol nopercent out=freq1; 
run;
proc freq data=abe.stacked_&name noprint;
 tables method*selected_x2 / nocol nopercent out=freq2;
 tables method*selected_x3 / nocol nopercent out=freq3;
 tables method*selected_x4 / nocol nopercent out=freq4;
 tables method*selected_x5 / nocol nopercent out=freq5;
 tables method*selected_x6 / nocol nopercent out=freq6;
 tables method*selected_x7 / nocol nopercent out=freq7;
run;

data freq1; set freq1; keep method no_vars no_vars_count; no_vars_count=count; run;
data freq2; set freq2; keep method selected_x2 selected_x2_count; selected_x2_count=count; run;
data freq3; set freq3; keep method selected_x3 selected_x3_count; selected_x3_count=count; run;
data freq4; set freq4; keep method selected_x4 selected_x4_count; selected_x4_count=count; run;
data freq5; set freq5; keep method selected_x5 selected_x5_count; selected_x5_count=count; run;
data freq6; set freq6; keep method selected_x6 selected_x6_count; selected_x6_count=count; run;
data freq7; set freq7; keep method selected_x7 selected_x7_count; selected_x7_count=count; run;
data abe.freq_&name; merge freq1-freq7; 
 by method;
run; 
