%macro abe(data = , time = , cens = , censval = 0, y = , varlist = , active = , include = , initial = , proc = , 
           first = NONE, puni = 0.2, pmulti = 0.2, pequiv = 0.2, tau = 0.05, equiv = 0, cycles = 10, options = , 
           confounders = _confounders, print = 0, notes = 1, final = 1, logfile = _logfile);

*** augmented backward elimination;
*** 'fast' backward elimination combined with a standardized change-in-estimate criterion on the quantity of interest for linear, 
	logistic and Cox proportional hazards regression

%let version = 2014.05;
%let build = 201405261633;


** 20140526: 

   changed name of macro to abe, 
   changed chbeta to tau, 
   added censval argument;

** 20140506: augmented backward elimination

	interpretation of &chbeta:
	for linear models: 				|delta| * SDXpassive / SDY   > chbeta -> include active variable
	for logistic and Cox models: 	exp(|delta| * SDXpassive)   > 1+chbeta -> include active variable
	
	rationale: SDXpassive makes exp(|delta|) (logistic, Cox models) and |delta| (linear) criterion comparable between continuous and binary variables,
               in linear models: SDY allows a general definition of chbeta as the change in the standardized effect on Y
			   in logistic and Cox models: it is more relevant to include a variable if the change in the HR or OR is more than 10% 
			   (e.g. if beta is 0.10 and 0.09, the HR is very similar, but according to the old definition  this would justify inclusion of a variable)

    equiv: = 1 a test for equivalence is conducted
	pequiv: significance level of equivalence test;

** 20140502: 

	stddelta: required standardized change in beta (e.g., 0.1 per standard deviation of X) for using the change-in-estimate criterion.
			  if the standardized change in beta of a variable is less than stddelta, then even for a large relative change, the change-in-estimate criterion is ignored.
			  for linear models, this is redefined as standardized change in beta in standard deviations of Y;
*   purposeful step (backward elimination) is now carried out eliminating 1 term per cycle;

** 20131120: 

   changed alwayskeep to include, clinvar to initial and lowprior to active

   new logfile generation: the macro now just logs all computed models

   old-style logfile still available as _logfile_old

   new option active: "low priority" variables mentioned here will not be included in evaluating of confounding, i.e., it does not matter if their beta changes by more than &chbeta

   puni: inclusion criterion for initial multivariable model (from univariate model)

   pmulti: retention criterion for multivariable model

   chbeta: a variable is considered a confounder if dropping it from the model changes the beta of another variable by that proportion
    * the relative change in beta is approximated using the estimated parameter vector and covariance matrix
    * therefore, it needs to estimate only one model per cycle

   first: strategy to build first model:
		UNI: based on univariate significance (puni)
		BACKWARD (B): based on backward elimination (pmulti)
 		FORWARD (F): based on forward selection (puni)
		NONE: based on full model

   variables in varlist are evaluated for:
       * being significant in multivariable model (pmulti)
       * being a confounder for any other variable (chbeta);
       * a variable is dropped from the model if it is none of both.

   variables named in "include" will not be dropped but used to rate other variables as confounders

   variables in "initial" will be included, irrespective of univariate significance, in first multivariable model, but may be eliminated afterwards

   print=1 will show all intermediate models

   notes=0 will suppress notes in saslog

   final=1 (default) shows last model

   confounders = output data set containing a variable confounders which contains as string the list of confounders

   if in a cycle all variables have been rated (either significant or confounder or none of both) the macro adds all variables which were significant in univariate models and 
      currently not contained in the model to be evaluated in the next cycle.

   the macro performs several cycles (option cycles specifies maximum number of cycles) until it converges to a unique variable list;


%let first=%upcase(&first);

%let varlist=&include &initial &varlist;
*** first create dynamic master table of all variables ***;
%let nvar=0;
%do %while(%scan(&varlist,&nvar+1)~=);
 %let nvar=%eval(&nvar+1);
 %let var&nvar=%scan(&varlist,&nvar);
 %let double&nvar=0;
%end;

** check double entries, possibly caused by ranking as first variables those in include and initial;
%do j=1 %to %eval(&nvar-1);
	%do jj=%eval(&j+1) %to &nvar;
		%if %upcase(&&var&j) = %upcase(&&var&jj) %then %let double&jj=1;
	%end;
%end;
%let jj=0;
%let newvarlist=;
%do j=1 %to &nvar;
	%if &&double&j = 0 %then %do;
		%let jj=%eval(&jj+1);
		%let newvarlist=&newvarlist &&var&j;
	%end;
%end;
%let varlist=&newvarlist;
%let nvar=&jj;

* re-read variable names from new list;
%let nvar=0;
%do %while(%scan(&varlist,&nvar+1)~=);
 %let nvar=%eval(&nvar+1);
 %let var&nvar=%scan(&varlist,&nvar);
%end;

%do j=1 %to &nvar;
 %let cur&j = 0;
 %let ini&j =0;
 %let keepuni&j=0;
 %let clin&j =0;
 %let alw&j=0;
 %let low&j=0;
 %let jj=0;
 %if &notes=1 %then %put Variable &&var&j ;
 %do %while(%scan(&include,&jj+1)~=);
  %let jj=%eval(&jj+1);
  %let work=%scan(&include,&jj);
  %if %upcase(&work)=%upcase(&&var&j) %then %do; 
   %if &notes=1 %then %put  is INCLUDE.;
   %let alw&j=1;
  %end;
 %end;
 %let n_alw=&jj;
 %let jj=0;
 %do %while(%scan(&initial,&jj+1)~=);
  %let jj=%eval(&jj+1);
  %let work=%scan(&initial,&jj);
  %if %upcase(&work)=%upcase(&&var&j) %then %do; 
   %if &notes=1 %then %put  is INITIAL.;
   %let clin&j=1;
  %end;
 %end;
 %let n_clin=&jj;
 %let n_include=%eval(&n_clin+&n_alw);
 %let jj=0;
 %do %while(%scan(&active,&jj+1)~=);
  %let jj=%eval(&jj+1);
  %let work=%scan(&active,&jj);
  %if %upcase(&work)=%upcase(&&var&j) %then %do; 
   %if &notes=1 %then %put  is ACTIVE.;
   %let low&j=1;
  %end;
 %end;
 %put ;
%end;
run;

*******;
*** macro for logging;

%let vlen=%length(&varlist);
* initialize logfile;
data &logfile;
length cycle $ 6 stage $ 12 vars $ %eval(&vlen+5);
run;
	
%macro logmodel(cycle, stage, vars);
	data _line;
	length cycle $ 6 stage $ 12 vars $ %eval(&vlen+5);
	cycle="&cycle";
	stage="&stage";
	vars="&vars";
	output;
	run;
	data &logfile;
	set &logfile _line;
	run;
%mend;


*****;
%let varln=%length(&varlist);
data _logfile_old;
length type $35 variables $&varln;
%if &first = UNI %then %do; 
    length selection $100;
	type="Initial set competing for selection";
	variables="&varlist";
	selection="Univariate screening (p<&puni)                                   ";
	output;
%end;
%else %do;
 variables="";
 type="";
* output;
%end;
*drop &first uni;
if type ne "";
run;


  %if &print=0 %then %do;
    ods select none;
  %end;

%let opt_used=0;
%if &options ne %then %do;
 %let options=/ &options;
 %let opt_used=1;
%end;




%let origvarlist=&varlist;
%let orignvar=&nvar;
quit;
title3 "Univariate Models";

%let proc=%upcase(&proc);
%if &print=0 %then %let nop= NOPRINT ;
%else %let nop=;
%if &proc=PHREG %then %let procsub=%str(proc phreg outest=_matrix_ covout data=_work; model &time * &cens(&censval) );
%if &proc=LOGISTIC %THEN %let procsub=%str(proc logistic   outest=_matrix_ covout data=_work descending; model &y );
%if &proc=REG %then %let procsub=%str(proc reg  outest=_matrix_ covout data=_work; model &y );


	proc means noprint data= &data;
	var &varlist %if &proc=REG %then %do; &y %end;;
	output out=_PFSSD std=sd1-sd&nvar %if &proc=REG %then %do; sdy %end; ;
	run;
	data _PFSSD;
	set _PFSSD;
	%if &proc=REG %then %do;
		%do j=1 %to &nvar;
			sd&j = sd&j/sdy;
		%end;
	%end;
	keep sd1-sd&nvar;
	run;

data _work;
set &data;
%do j=1 %to &nvar;
 __save&j=&&var&j;
%end;
run;

%if &first =UNI %then %do;
	%do j=1 %to &nvar;
	  %if &&alw&j=0 & &&clin&j=0 %then %do;
		  ods output parameterestimates=_uniest&j;
		  &procsub = &&var&j &options;
			  run;
		  %logmodel(0, Stage 1 (UNI), &&var&j);

		  data _uniest&j;
		  set _uniest&j;
		  %if &proc=REG %then %do;
		   		probchi=probt;
		  %end;
		  %else %if &proc=LOGISTIC %then %do;
			   probchi=1-probchi(Waldchisq,1);
		  %end;
		  %else %do;
			   probchi=1-probchi(chisq,1);
		  %end;
		  if probchi<&puni then keep=1;
		  else keep=0;
		  call symput("ini&j", keep);
		  call symput("keepuni&j", keep);
		  call symput("P_uni_&j", probchi);
		  run;
	 %end;
	 %else %do;
	  %let ini&j = 1;
	  %let p_uni_&j=-1;
	 %end;

	%end;


	* prepare first circle;
	data _work;
	set _work;
	%do j=1 %to &nvar;
	 %if &&ini&j = 0 %then %do;
	 	&&var&j = 0;
	 %end;
	 %else %do;
	    &&var&j = __save&j;
	 %end;
	%end;
	run;


	%let sigvars=;
	 %do j=1 %to &nvar;
	  %let add=0;
	  %if &&keepuni&j = 1 %then %let add=1;
	  %if &&clin&j=1 %then %let add=1;
      %if &&alw&j=1 %then %let add=1;
      %if &add=1 %then %do;
	   %let sigvars=&sigvars &&var&j;
	  %end;
	 %end; ;

	 data _x;
	 file print;
	 title3 "Before Cycle 1";
	 length parameter $ 20;
	 %if &print=1 %then %do; 
	 	if _n_=1 then put " NOTE: Variable list before 1st cycle: &sigvars &include"; 
	 %end;
	 %do j=1 %to &nvar;
	  Parameter="&&var&j";
	  p_value_uni=&&p_uni_&j;
	  if p_value_uni<&puni then mark="... selected for first model.";
	  else                      mark="                             ";
	  output;
	 %end;
	 label p_value_uni="P-value (univariate)";
	 run;

	 %if &print=1 %then %do;
		 proc print label;
		 format p_value_uni pvalue6.;
		 run;
	%end;
%end;
%else %do;
	%do j=1 %to &nvar;
		 %let keepuni&j = 1;
	%end;
%end;

%let k=0;
%let stop=0;
%if &first ne UNI %then %let sigvars=&initial &varlist;

%do %while(&k<=&cycles & &stop=0);

%let k=%eval(&k+1);
%put NOTE: ************************************************;
%put NOTE: ***************** Stage 2 CYCLE &K *******************;
%put NOTE: ************************************************;

data _logc;
length selection $ 100;
type="Cycle &k";
variables="&sigvars";
if &k=1 & "&first" ne "UNI" & "&first" ne "NONE" then selection="&first (entry: p<&puni, stay: p<&pmulti)                           ";
else selection="Purposeful (stay: p<&pmulti or relative change in any other beta > &tau)";
output;
run;
data _logfile_old;
set _logfile_old _logc;
run;


 title3 "Cycle &k";
 %let doit=0;
 %if &first=UNI %then %let doit=1;
 %if &k ne 1 %then %let doit=1;
 %if &doit=1 %then %do;
	 &procsub = &varlist &options ;
	 run;
	  %logmodel(&k, Stage 2, &sigvars);

 %end;
 %else %do; * if not UNI and 1st cycle;
	 %let sigvars=&varlist;
	 &procsub =  &varlist  %if &opt_used=1 %then %do; &options selection=&first include=&n_include slentry=&puni slstay=&pmulti %end; 
       %else %do;
	   	/ selection=&first include=&n_include slentry=&puni slstay=&pmulti 
       %end;
      ;
	 run;
	 %logmodel(&k,Stage 2 full, &varlist);
	 *** clean _matrix_;
	 %let revisit=0;
	 %if %substr(&first,1,1)=F %then %let revisit=1;
	 %if %substr(&first,1,1)=B %then %let revisit=1;
	 %if &revisit=1 %then %do;
		data _for;
		set _matrix_;
		if _n_=1;
		run;
		data _for;
		set _for;
		%do j=1 %to &nvar;
		 if &&var&j = . then do;
			call symput("keepuni&j", 0); *** revisit variables not selected in initial selection later;
			call symput("keep&j", 0); *** not in current model;
		 end;
		 else do;
		 	call symput("keep&j",1); *** in current model;
		 end;
		%end;
		run;

		*** run again with ALL variables to have the full covariance matrix;
		data _work;
		set _work;
		%do j=1 %to &nvar;
			%if &&keep&j=1 %then %do;
				&&var&j=__save&j;
			%end;
			%else %do;
				&&var&j=0;
			%end;
		%end;
		run;
		&procsub = &varlist %if &opt_used=1 %then %do; &options %end; ;
		run;
     %end; 

 %end;

 data _matrix_ ;
 set _matrix_;
 keep &varlist;
 if _name_ ne "Intercept";
 run;

 data _matrix_A&k;
 set _matrix_;
 run;
 
 proc iml;
 file log;
 use _matrix_;
 read all into _matrix_;
 close _matrix_;
 use _pfssd;
 read all into varsd;
 close _pfssd;

 %if &proc=REG %then %do;
		criterion=&tau;
 %end;
 %else %do;
 		criterion=log(1+&tau);
 %end;

 nvar=&nvar;
 b=_matrix_[1,];
 if any(b=.)=1 then b[loc(b = .)]=0;
 cov=_matrix_[2:nrow(_matrix_),];
 if any(cov=.)=1 then cov[loc(cov=.)]=1;
 if any(diag(cov=0)) then do; 
	locdiagcov=loc(diag(cov=0));
 	cov[locdiagcov]=1;
 end;
 
 *print b cov;
 keep=repeat(0,1,nvar);
 conf=repeat(0,1,nvar);
 do j=1 to nvar;                      
   thisalw=0;
   %do j=1 %to &nvar;
    if &j = j then do;
	 if &notes=1 then do;
		put " ";
		put "NOTE: Processing Variable &j, &&var&j";
	 end;
	 if &&alw&j=1 then do;
		if &notes=1 then put "NOTE: is include.";
		thisalw=1;
	 end;
	end;
   %end;
   t=b[j]/sqrt(cov[j,j]);
   pvalue=1-probchi(t**2,1);
   if thisalw=1 then pvalue=-1;
   if pvalue=. | (b[j]=0 & cov[j,j]=1) then pvalue=2;
   if pvalue=2 then do;
		keep[j]=-1;
		if &notes=1 then put "NOTE: not in model.";
   end;
   else do;
    if thisalw=0 then do;
		if &notes=1 then do;
			put "NOTE: p-value = " pvalue @;
	    	if pvalue < &pmulti then put " < &pmulti ";
 	    	else put " ";
		end;
	end;
   end;
  if pvalue < 2 then do;
   if pvalue<&pmulti then do;
    keep[j]=1;
	if &notes=1 then put "NOTE: Variable " j " will be kept.";
   end;
   else do;
    vnconf="";
	do jj=1 to nrow(cov);
	 if jj ^= j then do;
	  %let noteval=0;
	  noteval=&noteval;
	  %do jj=1 %to &nvar;
	   if jj=&jj then do;
        noteval=&&low&jj;
		vnconf="&&var&jj";
	   end;
	  %end;
	  if noteval=0 then do;
	  	  if b[jj] ^= 0 then do;
		  	*print j, jj, b, cov;
			delta=-b[j]*cov[j,jj]/cov[j,j] * varsd[jj];
            if &notes=1 then put "NOTE: ...Passive variable " vnconf ": stand. delta= |" delta "|"@;
			if &equiv=1 then do;
				delta=abs(delta)+probit(1-&pequiv /2)*sqrt(cov[j,j])*varsd[jj];
				if &notes=1 then put " equiv.delta= " delta @;
			end;
			if abs(delta)>=criterion then do;
		        keep[j]=1;
				conf[j]=1;
				if &notes=1 then put " >= " criterion;
			end;
			else if &notes=1 then put " ";
		  end;
       end;
	 end;
	end;
    if keep[j]=0 then keep[j]=1-pvalue;                 *** add GH 140502, keep temporarily holds a code for keeping: 1 if kept, -1 if not in model, 1-pvalue if "on elimination list";
	if &notes=1 then do;
	    if keep[j]=1 then put "NOTE: Variable " j " is a confounder and will be kept.";
		else put "NOTE: Variable " j " is neither a confounder nor significant and might be dropped.";
	end;
   end;
  end;
end;
minkeep=(keep[loc(keep>=0)])[><];		*** add GH 140502 begin, eliminates only variable with highest p-value (and which is not confounder of others);
if minkeep<1 then do;
	dd=loc(keep<=minkeep);
	keep[dd]=0;
	keep[loc(keep>minkeep)]=1;
end;	
*keep[loc(keep=-1)]=0;
*** add GH 140502 end;
create _keeps from keep;
append from keep;
close _keeps;
quit;

data _keeps;
set _keeps;
%do j=1 %to &nvar;
 call symput("keep&j", col&j);
%end;
run;

 %let lastlist=&sigvars;
 %let sigvars=;
 %do j=1 %to &nvar;
  %if &&keep&j=1 %then %do;
   %let sigvars=&sigvars &&var&j;
  %end;
 %end;
 
 data _work;
 set _work;
 %do j=1 %to &nvar;
  &&var&j = 0;
  if &&keep&j=1 then &&var&j=__save&j;
  if &&alw&j=1 then &&var&j=__save&j;
 %end;
 run;

 data _x;
 file log;
 if &notes=1 then do;
	 put "NOTE: Variable list after cycle &k :";
	 put " ";

	 put "      &sigvars ";
	 *put "      &include"; ** include variables are contained in sigvars;
 end;


 *** here, re-evaluate excluded variables ;

  *** cycle through all excluded variables (except of the one just excluded) j;
	  *** fit model including the variable j;
	  *** memorize p-value and max reltau and keep indicator: 1 if pvalue<alpha2 OR max reltau>tau;
  *** among all variables with keep=1, keep the one with lowest p-value if that p-value < alpha2;
  *** among all variables with keep=1, keep the one with highest reltau if that reltau >tau;



 *** re-evaluation stop;



 %if &lastlist=&sigvars %then %do;
  %let stop=1;
 if &notes=1 then do;
	  put " ";
	  put "NOTE: Variable list did not change, iteration stops.";
 end;
 %end;
 %else %let stop=0;

 %end;

 run;

 * adding variables originally not selected to the variable list ;

   data _x;
   file log;
   if &notes=1 then put "NOTE: Adding variables originally not selected to the variable list";
  run;
   %let firstsig=&sigvars;

 * repeat algorithm until convergence;
   %do j=1 %to &nvar;
    %put NOTE: Variable &j keep= &&keep&j keepuni=&&keepuni&j;
    %if &&keep&j <= 0 %then %do; *** reenter only variables currently not in the model;
	    %if &&keepuni&j=0 %then %do;
		    %if &&clin&j = 0 %then %do;    *** initial variables had their chance...;
			    %put NOTE: re-entering variable &&var&j.;
			    %let sigvars=&sigvars &&var&j;
			%end;
    	%end;
	%end;
   %end;

  data _work;
  set _work;
  %do j=1 %to &nvar;
   if &&keepuni&j = 0 and &&clin&j = 0 then &&var&j = __save&j;
  %end;
  run;



****let k=0;
   data _x;
   file log;

%if &firstsig = &sigvars %then %do;
 %let stop=1;
 if &notes=1 then put "NOTE: No nonsignificant candidates. Macro stops.";
%end;
%else %do;
 %let stop=0;
 %let kp1=%eval(&k+1);
 if &notes=1 then put "NOTE: Variable list before cycle: &kp1";
 if &notes=1 then put " ";
 if &notes=1 then put "      &sigvars";
 if &notes=1 then put "      &include";
%end;
%do %while(&k<=&cycles & &stop=0);
 %let k=%eval(&k+1);

 %put NOTE: ************************************************;
 %put NOTE: ***************** Stage 3 CYCLE &K *******************;
 %put NOTE: ************************************************;

data _logc;
length selection $ 100;
type="Cycle &k";
variables="&sigvars";
if &k=&kp1 then selection="Re-entered nonsignificant candidate variables, purposeful selection";
else selection="Purposeful (stay: p<&pmulti or relative change in any other beta > &tau)";
output;
run;
data _logfile_old;
set _logfile_old _logc;
run;


title3 "Cycle &k";
&procsub = &varlist &options;
 run;
 %logmodel(&k, Stage 3, &sigvars);

 data _matrix_ ;
 set _matrix_;
 keep &varlist;
  if _name_ ne "Intercept";
 run;

 data _matrix_B&k;
 set _matrix_;
 run;

 proc iml;
 file log;
 use _matrix_;
 read all into _matrix_;
 close _matrix_;
 use _pfssd;
 read all into varsd;
 close _pfssd;

 %if &proc=REG %then %do;
		criterion=&tau;
 %end;
 %else %do;
 		criterion=log(1+&tau);
 %end;

 nvar=&nvar;
 b=_matrix_[1,];
 if any(b=.)=1 then b[loc(b = .)]=0;
 cov=_matrix_[2:nrow(_matrix_),];
 if any(cov=.)=1 then cov[loc(cov=.)]=1;
 if any(diag(cov=0)) then do; 
	locdiagcov=loc(diag(cov=0));
 	cov[locdiagcov]=1;
 end;
 
 *print b cov;
 keep=repeat(0,1,nvar);
 conf=repeat(0,1,nvar);
 do j=1 to nvar;                      
   thisalw=0;
   %do j=1 %to &nvar;
    if &j = j then do;
	 if &notes=1 then do;
		put " ";
		put "NOTE: Processing Variable &j, &&var&j";
	 end;
	 if &&alw&j=1 then do;
		if &notes=1 then put "NOTE: is include.";
		thisalw=1;
	 end;
	end;
   %end;
   t=b[j]/sqrt(cov[j,j]);
   pvalue=1-probchi(t**2,1);
   if thisalw=1 then pvalue=-1;
   if pvalue=. | (b[j]=0 & cov[j,j]=1) then pvalue=2;
   if pvalue=2 then do;
		keep[j]=-1;
		if &notes=1 then put "NOTE: not in model.";
   end;
   else do;
    if thisalw=0 then do;
		if &notes=1 then do;
			put "NOTE: p-value = " pvalue @;
	    	if pvalue < &pmulti then put " < &pmulti ";
 	    	else put " ";
		end;
	end;
   end;
  if pvalue < 2 then do;
   if pvalue<&pmulti then do;
    keep[j]=1;
	if &notes=1 then put "NOTE: Variable " j " will be kept.";
   end;
   else do;
    vnconf="";
	do jj=1 to nrow(cov);
	 if jj ^= j then do;
	  %let noteval=0;
	  noteval=&noteval;
	  %do jj=1 %to &nvar;
	   if jj=&jj then do;
        noteval=&&low&jj;
		vnconf="&&var&jj";
	   end;
	  %end;
	  if noteval=0 then do;
	  	  if b[jj] ^= 0 then do;
		  	*print j, jj, b, cov;
			delta=-b[j]*cov[j,jj]/cov[j,j] * varsd[jj];
            if &notes=1 then put "NOTE: ...Passive variable " vnconf ": stand. delta= |" delta "|"@;
			if &equiv=1 then do;
				delta=abs(delta)+probit(1-&pequiv /2)*sqrt(cov[j,j])*varsd[jj];
				if &notes=1 then put " equiv.delta= " delta @;
			end;
			if abs(delta)>=criterion then do;
		        keep[j]=1;
				conf[j]=1;
				if &notes=1 then put " >= " criterion;
			end;
			else if &notes=1 then put " ";
		  end;
       end;
	 end;
	end;
    if keep[j]=0 then keep[j]=1-pvalue;                 *** add GH 140502, keep temporarily holds a code for keeping: 1 if kept, -1 if not in model, 1-pvalue if "on elimination list";
	if &notes=1 then do;
	    if keep[j]=1 then put "NOTE: Variable " j " is a confounder and will be kept.";
		else put "NOTE: Variable " j " is neither a confounder nor significant and might be dropped.";
	end;
   end;
  end;
end;
minkeep=(keep[loc(keep>=0)])[><];		*** add GH 140502 begin, eliminates only variable with highest p-value (and which is not confounder of others);
if minkeep<1 then do;
	dd=loc(keep<=minkeep);
	keep[dd]=0;
	keep[loc(keep>minkeep)]=1;
end;	
*keep[loc(keep=-1)]=0;
*** add GH 140502 end;
create _keeps from keep;
append from keep;
close _keeps;
quit;
data _keeps;
set _keeps;
%do j=1 %to &nvar;
 call symput("keep&j", col&j);
%end;
run;

 %let lastlist=&sigvars;
 %let sigvars=;
 %do j=1 %to &nvar;
  %if &&keep&j=1 %then %do;
   %let sigvars=&sigvars &&var&j;
  %end;
 %end;

  data _work;
 set _work;
 %do j=1 %to &nvar;
  &&var&j = 0;
  if &&keep&j=1 then &&var&j=__save&j;
  if &&alw&j=1 then &&var&j=__save&j;
 %end;
 run;


 data _x;
 file log;
 if &notes=1 then put "NOTE: After Cycle &k";
 if &notes=1 then do;
	 put "NOTE: Variable list after cycle &k :";
	 put " ";
	 put "      &sigvars";
	 *put "      &include"; *contained in sigvars;
 end;
 %if &lastlist=&sigvars %then %do;
  %let stop=1;
  if &notes=1 then put " ";
  if &notes=1 then put "NOTE: Variable list did not change, macro stops.";
 %end;
 %else %let stop=0;
 run;
%end;


 %let nsig=0;
 %let maxl=0;
%do %while(%scan(&sigvars,&nsig+1)~=);
 %let nsig=%eval(&nsig+1);
 %let con&nsig=%scan(&sigvars,&nsig);
 %let thisl=%length(&&con&nsig);
 %if &thisl>&maxl %then %let maxl=&thisl;
%end;

data &confounders;
length variables $ &maxl;
%do j=1 %to &nsig;
	variables="&&con&j";
	output;
%end;
run;


  %if &print=0 %then %do;
    ods select all;
  %end;
  %if &final=1 %then %do;
      title3 "Final model selected by ABE with first=&first, puni=&puni, pmulti=&pmulti, tau=&tau";
	  &procsub = &sigvars &options ;
	  run;
	  title3;
	  %logmodel(&k,Final model,&sigvars);
  %end;
%mend;
