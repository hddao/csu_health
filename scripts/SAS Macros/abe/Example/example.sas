*************************************************************************************************************************;
*** Analysis of urine osmolarity data with the %ABE algorithm                                                         ***;
***                                                                                                                   ***;
*** Version 2.0, Sept. 2014                                                                                           ***; 
*************************************************************************************************************************;

options nocenter nodate linesize = 120;

*** Load %abe and the %abe_bootstrap SAS macros;
%include "U:\CSU EPA Health Study\csu_health\scripts\SAS Macros\abe\ABE.sas";                            * %ABE SAS macro, adapt path accordingly;
%include "U:\CSU EPA Health Study\csu_health\scripts\SAS Macros\abe\ABE_BOOTSTRAP.sas";                  * %ABE_BOOTSTRAP SAS macro, adapt path accordingly;


*** load data;
PROC IMPORT OUT = urineosmolarity DBMS = EXCEL REPLACE
            DATAFILE="U:\CSU EPA Health Study\csu_health\scripts\SAS Macros\abe\Example\Urine osmolarity data.XLS";            * adapt to your system accordingly;
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN; 


*** some descriptive measures;
data urineosmolarity; set urineosmolarity;
 age = age10 * 10;
run;

proc means data = urineosmolarity fw = 4 median p25 p75 mean std;
 var log2CCL log2Prot log2Uosm map age;
run;

proc freq data = urineosmolarity;
 tables dialysis diur gender pkd acei bblock;
run;


*** Backward elimination with alpha = 0.2;
proc phreg data = urineosmolarity;
 model time * dialysis(0) = log2Uosm log2CCL log2Prot bblock pkd diur age10 acei map gender
                            / selection = backward slstay = 0.2 rl include = 1;
run;

*** Estimating the exposure-outcome relationship using ABE: 
*** The exposure of interest log2Uosm will be forced into every working model (option include). 
*** The other nine explanatory variables will only be included into the final main effects model, 
*** if they are significant at a significance threshold pmulti of 0.20 or if their inclusion into 
*** the model changes at least one other standardized hazard ratio by at least 5% (option tau). 
*** Only the final model (option print=0 and final=1) will be printed. As requested in the options 
*** risk limits will be added to the final model.
*** ;

%abe(data = urineosmolarity, time = time, cens = dialysis, include = log2Uosm,
     varlist = log2CCL log2Prot bblock pkd diur age10 acei map gender,
     first = none, pmulti = 0.20, tau = 0.05, proc = phreg,
     options = %str(rl), logfile = _logfile, confounders = _confounders,
     print = 0, notes = 1, final = 1);  


*** Apply bootstrap:
*** B = the number of bootstrap resamples, should be set e.g. to 1000.
*** ;

%abe_bootstrap(seed = 12345, B = 10,                     
     bootstrap_variables = _bootstrap_variables, 
     bootstrap_regcoeffs = _bootstrap_regcoeffs,
     data = urineosmolarity, time = time, cens = dialysis, include = log2Uosm,
     varlist = log2CCL log2Prot bblock pkd diur age10 acei map gender,
     first = none, pmulti = 0.20, tau = 0.05, proc = phreg);  


*** show which variables were selected in each bootstrap resample;
proc print data = _bootstrap_variables; 
run;

data _bootstrap_variables; set _bootstrap_variables; 
 if acei     = . then acei     = 0;
 if age10    = . then age10    = 0;
 if bblock   = . then bblock   = 0;
 if diur     = . then diur     = 0;
 if gender   = . then gender   = 0;
 if log2CCL  = . then log2CCL  = 0;
 if log2Prot = . then log2Prot = 0;
 if log2Uosm = . then log2Uosm = 0;
 if map      = . then map      = 0;
 if pkd      = . then pkd      = 0;
run;

proc means data = _bootstrap_variables mean fw = 4; 
 var acei age10 bblock diur gender log2CCL log2Prot log2Uosm map pkd;
run;


*** request estimated regression coefficients in each bootstrap resample;
proc print data = _bootstrap_regcoeffs;
run;


*** bootstrap SE for log2Uosm;
proc means data = _bootstrap_regcoeffs std;
 var log2Uosm;
run;

*** for comparison robust SE for log2Uosm;
proc phreg data = urineosmolarity covs; 
 model time * dialysis(0) = log2Uosm log2CCL log2Prot bblock pkd diur acei / rl;
run;


