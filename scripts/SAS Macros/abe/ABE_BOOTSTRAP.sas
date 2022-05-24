%Macro abe_bootstrap(seed = 12345, B =, bootstrap_variables = _bootstrap_variables, bootstrap_regcoeffs = _bootstrap_regcoeffs,
                     data = , time = , cens = , censval = 0, y = , varlist = , active = , include = , initial = , proc = PHREG, 
                     first = NONE, puni = 0.2, pmulti = 0.2, pequiv = 0.2, tau = 0.05, equiv = 0, cycles = 10, options = , 
                     confounders = _confounders, print = 0, notes = 0, final = 0, logfile = _logfile);

*** %abe analysis for bootstrap samples;

*** 20140526:
    seed = random number for generation of bootstrap resamples
    B = number of bootstrap resamples (all bootstrap resamples are saved as data set bootstrapped)
	bootstrap_variables & bootstrap_regcoeffs = 2 data sets with variables selected in each bootstrap resample and estimates of the final models in the bootstrap resamples

    all other arguments are from %abe;

%let version = 2014.05;
%let build = 201405261633;



%let proc=%upcase(&proc);

proc surveyselect data = &data out = bootstrapped seed = &seed method = urs samprate = 1 outhits rep = &B; 
run;  

options nonotes;     

data &bootstrap_variables; LENGTH temp 5; STOP; RUN;
data &bootstrap_regcoeffs; LENGTH temp 5; STOP; RUN;

%do bootB=1 %to &B;
	data booti; set bootstrapped; if replicate = &bootB; run;

    %abe(data = booti, time = &time, cens = &cens, y = &y, censval = &censval, varlist = &varlist, active = &active, include = &include, puni = &puni, pmulti = &pmulti, pequiv = &pequiv, tau = &tau, 
         equiv = &equiv, cycles = &cycles, options = &options, confounders = &confounders, proc = &proc, print = &print, notes = &notes, initial = &initial, final = &final, 
        first = &first, logfile = &logfile); 

    data &confounders; set &confounders; bootsample = &bootB; run;
    data &bootstrap_variables; set &bootstrap_variables &confounders; run;
	data _matrix_; set _matrix_; b = &bootB; run;
    data &bootstrap_regcoeffs; set &bootstrap_regcoeffs _matrix_; run;
%end;

* preprocessing of results;
data &bootstrap_variables; set &bootstrap_variables; drop temp; run;
proc sort data = &bootstrap_variables; by variables;
data bootstrap_variables_temp1; set &bootstrap_variables; boot1=1; if bootsample=1; run;
%do bootB=2 %to &B;
	data bootstrap_variables_temp2; set &bootstrap_variables; boot&&bootB=1; if bootsample=&bootB; run;
	data bootstrap_variables_temp1; merge bootstrap_variables_temp1 bootstrap_variables_temp2; by variables; run;
%end;
data &bootstrap_variables; set bootstrap_variables_temp1; drop bootsample; run;
proc transpose data=&bootstrap_variables out=&bootstrap_variables; id variables; run;

%if &final EQ 1 %then %do;
	%if &proc EQ PHREG %then %do;
		data &bootstrap_regcoeffs; set &bootstrap_regcoeffs; drop temp _ties_; run;
	%end;
	%if &proc EQ REG %then %do;
		data &bootstrap_regcoeffs; set &bootstrap_regcoeffs; drop temp _model_ _DEPVAR_; run;
	%end;
	%if &proc EQ LOGISTIC %then %do;
		data &bootstrap_regcoeffs; set &bootstrap_regcoeffs; drop temp _LINK_ _model_; run;
	%end;
%end;
%if &final EQ 0 %then %do;
	data &bootstrap_regcoeffs; set &bootstrap_regcoeffs; drop temp; n = _n_; run;
	proc sort data=&bootstrap_regcoeffs; by b n;
	data &bootstrap_regcoeffs; set &bootstrap_regcoeffs; 
	 drop n;
	 by b n;
	 if first.b;
	run;
%end;

options notes;
%MEND;
