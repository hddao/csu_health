*** SAS macros for simulation study: %nameit, %abesim1, %abesim2;
***;
*** Daniela Dunkler and Georg Heinze, Sept. 2014, Version 1.0;
***; 
*************************************************************************************************************************************************************;

%macro nameit(name);
	data models_&name;
	set models;
	length method $ 50;
	method="&name";
	run;
	data allmodels;
	set allmodels models_&name;
	run;
%mend;

%macro abesim1(nsim=1, proc=logistic); 
data allabe;
run;
%do isim=1 %to &nsim;
	%put simulated data set &isim ;

    data sim1;
	set simdesign;
	if &isim=isim;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.1, final=0, notes=0);
	data abeestim1;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.25-tau0.1";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.1, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim2;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.25-tau0.1-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.2, final=0, notes=0);
	data abeestim3;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.25-tau0.2";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.2, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim4;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.25-tau0.2-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.1, final=0, notes=0);
	data abeestim5;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.157-tau0.1";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.1, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim6;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.157-tau0.1-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.2, final=0, notes=0);
	data abeestim7;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.157-tau0.2";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.2, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim8;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.157-tau0.2-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.1, final=0, notes=0);
	data abeestim9;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.10-tau0.1";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.1, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim10;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.10-tau0.1-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.2, final=0, notes=0);
	data abeestim11;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.10-tau0.2";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.2, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim12;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.10-tau0.2-active";
	keep isim x1-x7 method;
	run;


    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.1, final=0, notes=0);
	data abeestim13;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.05-tau0.1";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.1, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim14;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.05-tau0.1-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.2, final=0, notes=0);
	data abeestim15;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.05-tau0.2";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.2, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim16;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.05-tau0.2-active";
	keep isim x1-x7 method;
	run;


	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.1, final=0, notes=0);
	data abeestim101;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.25-tau0.1";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.1, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim102;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.25-tau0.1-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.2, final=0, notes=0);
	data abeestim103;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.25-tau0.2";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.2, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim104;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.25-tau0.2-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.1, final=0, notes=0);
	data abeestim105;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.157-tau0.1";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.1, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim106;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.157-tau0.1-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.2, final=0, notes=0);
	data abeestim107;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.157-tau0.2";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.2, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim108;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.157-tau0.2-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.1, final=0, notes=0);
	data abeestim109;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.10-tau0.1";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.1, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim110;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.10-tau0.1-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.2, final=0, notes=0);
	data abeestim111;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.10-tau0.2";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.2, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim112;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.10-tau0.2-active";
	keep isim x1-x4 x6 x7 method;
	run;


    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.1, final=0, notes=0);
	data abeestim113;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.05-tau0.1";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.1, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim114;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.05-tau0.1-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.2, final=0, notes=0);
	data abeestim115;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.05-tau0.2";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.2, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim116;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.05-tau0.2-active";
	keep isim x1-x4 x6 x7 method;
	run;

		%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.1, final=0, notes=0);
	data abeestim17;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.20-tau0.1";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.1, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim18;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.20-tau0.1-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.2, final=0, notes=0);
	data abeestim19;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.20-tau0.2";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.2, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim20;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.20-tau0.2-active";
	keep isim x1-x7 method;
	run;


	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.1, final=0, notes=0);
	data abeestim117;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.20-tau0.1";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.1, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim118;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.20-tau0.1-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.2, final=0, notes=0);
	data abeestim119;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.20-tau0.2";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.2, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim120;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.20-tau0.2-active";
	keep isim x1-x4 x6 x7 method;
	run;



    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.05, final=0, notes=0);
	data abeestimA1;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.25-tau0.05";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.05, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestimA2;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.25-tau0.05-active";
	keep isim x1-x7 method;
	run;


	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.05, final=0, notes=0);
	data abeestimA5;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.157-tau0.05";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.05, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestimA6;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.157-tau0.05-active";
	keep isim x1-x7 method;
	run;


	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.05, final=0, notes=0);
	data abeestimA9;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.10-tau0.05";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.05, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestimA10;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.10-tau0.05-active";
	keep isim x1-x7 method;
	run;



    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.05, final=0, notes=0);
	data abeestimA13;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.05-tau0.05";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.05, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestimA14;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.05-tau0.05-active";
	keep isim x1-x7 method;
	run;



	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.05, final=0, notes=0);
	data abeestimA101;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.25-tau0.05";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.05, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestimA102;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.25-tau0.05-active";
	keep isim x1-x4 x6 x7 method;
	run;


	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.05, final=0, notes=0);
	data abeestimA105;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.157-tau0.05";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.05, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestimA106;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.157-tau0.05-active";
	keep isim x1-x4 x6 x7 method;
	run;


	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.05, final=0, notes=0);
	data abeestimA109;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.10-tau0.05";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.05, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestimA110;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.10-tau0.05-active";
	keep isim x1-x4 x6 x7 method;
	run;


    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.05, final=0, notes=0);
	data abeestimA113;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.05-tau0.05";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.05, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestimA114;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.05-tau0.05-active";
	keep isim x1-x4 x6 x7 method;
	run;


		%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.05, final=0, notes=0);
	data abeestimA17;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.20-tau0.05";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.05, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestimA18;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-NONE-p0.20-tau0.05-active";
	keep isim x1-x7 method;
	run;


	%abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.05, final=0, notes=0);
	data abeestimA117;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.20-tau0.05";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, y=y, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.05, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestimA118;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="abe-DISJ-NONE-p0.20-tau0.05-active";
	keep isim x1-x4 x6 x7 method;
	run;

	data allabe;
	set allabe 
    abeestim1 abeestim2 abeestim3 abeestim4 abeestim5 abeestim6 abeestim7 abeestim8 abeestim9 abeestim10 abeestim11 abeestim12 abeestim13 abeestim14 abeestim15 abeestim16
	abeestim17 abeestim18 abeestim19 abeestim20
	abeestim101 abeestim102 abeestim103 abeestim104 abeestim105 abeestim106 abeestim107 abeestim108 abeestim109 abeestim110 abeestim111 abeestim112 abeestim113 abeestim114 abeestim115 abeestim116
    abeestim117 abeestim118 abeestim119 abeestim120
    abeestimA1 abeestimA2 abeestimA5 abeestimA6 abeestimA9 abeestimA10 abeestimA13 abeestimA14 abeestimA101	abeestimA102 abeestimA105 abeestimA106 abeestimA109	abeestimA110 abeestimA113 abeestimA114 	abeestimA17 abeestimA18 abeestimA117 abeestimA118;
	run;
%end;
%mend;

%macro abesim2(nsim=1, proc=phreg); 
* for cox;
data allabe;
run;
%do isim=1 %to &nsim;
	%put simulated data set &isim ;

    data sim1;
	set simdesign;
	if &isim=isim;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.1, final=0, notes=0);
	data abeestim1;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.25-tau0.1";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.1, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim2;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.25-tau0.1-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.2, final=0, notes=0);
	data abeestim3;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.25-tau0.2";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.2, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim4;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.25-tau0.2-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.1, final=0, notes=0);
	data abeestim5;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.157-tau0.1";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.1, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim6;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.157-tau0.1-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.2, final=0, notes=0);
	data abeestim7;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.157-tau0.2";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.2, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim8;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.157-tau0.2-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.1, final=0, notes=0);
	data abeestim9;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.10-tau0.1";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.1, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim10;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.10-tau0.1-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.2, final=0, notes=0);
	data abeestim11;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.10-tau0.2";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.2, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim12;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.10-tau0.2-active";
	keep isim x1-x7 method;
	run;


    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.1, final=0, notes=0);
	data abeestim13;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.05-tau0.1";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.1, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim14;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.05-tau0.1-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.2, final=0, notes=0);
	data abeestim15;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.05-tau0.2";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.2, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim16;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.05-tau0.2-active";
	keep isim x1-x7 method;
	run;



	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.1, final=0, notes=0);
	data abeestim101;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.25-tau0.1";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.1, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim102;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.25-tau0.1-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.2, final=0, notes=0);
	data abeestim103;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.25-tau0.2";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.2, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim104;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.25-tau0.2-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.1, final=0, notes=0);
	data abeestim105;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.157-tau0.1";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.1, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim106;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.157-tau0.1-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.2, final=0, notes=0);
	data abeestim107;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.157-tau0.2";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.2, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim108;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.157-tau0.2-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.1, final=0, notes=0);
	data abeestim109;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.10-tau0.1";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.1, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim110;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.10-tau0.1-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.2, final=0, notes=0);
	data abeestim111;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.10-tau0.2";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.2, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim112;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.10-tau0.2-active";
	keep isim x1-x4 x6 x7 method;
	run;


    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.1, final=0, notes=0);
	data abeestim113;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.05-tau0.1";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.1, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim114;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.05-tau0.1-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.2, final=0, notes=0);
	data abeestim115;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.05-tau0.2";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.2, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim116;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.05-tau0.2-active";
	keep isim x1-x4 x6 x7 method;
	run;


	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.1, final=0, notes=0);
	data abeestim17;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.20-tau0.1";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.1, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim18;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.20-tau0.1-active";
	keep isim x1-x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.2, final=0, notes=0);
	data abeestim19;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.20-tau0.2";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.2, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestim20;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.20-tau0.2-active";
	keep isim x1-x7 method;
	run;


    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.1, final=0, notes=0);
	data abeestim117;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.20-tau0.1";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.1, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim118;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.20-tau0.1-active";
	keep isim x1-x4 x6 x7 method;
	run;

	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.2, final=0, notes=0);
	data abeestim119;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.20-tau0.2";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.2, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestim120;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.20-tau0.2-active";
	keep isim x1-x4 x6 x7 method;
	run;


	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.05, final=0, notes=0);
	data abeestimA1;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.25-tau0.05";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.05, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestimA2;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.25-tau0.05-active";
	keep isim x1-x7 method;
	run;



	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.05, final=0, notes=0);
	data abeestimA5;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.157-tau0.05";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.05, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestimA6;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.157-tau0.05-active";
	keep isim x1-x7 method;
	run;



	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.05, final=0, notes=0);
	data abeestimA9;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.10-tau0.05";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.05, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestimA10;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.10-tau0.05-active";
	keep isim x1-x7 method;
	run;



    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.05, final=0, notes=0);
	data abeestimA13;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.05-tau0.05";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.05, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestimA14;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.05-tau0.05-active";
	keep isim x1-x7 method;
	run;



	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.05, final=0, notes=0);
	data abeestimA101;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.25-tau0.05";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.25, tau=0.05, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestimA102;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.25-tau0.05-active";
	keep isim x1-x4 x6 x7 method;
	run;



	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.05, final=0, notes=0);
	data abeestimA105;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.157-tau0.05";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.157, tau=0.05, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestimA106;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.157-tau0.05-active";
	keep isim x1-x4 x6 x7 method;
	run;


	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.05, final=0, notes=0);
	data abeestimA109;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.10-tau0.05";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.1, tau=0.05, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestimA110;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.10-tau0.05-active";
	keep isim x1-x4 x6 x7 method;
	run;




    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.05, final=0, notes=0);
	data abeestimA113;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.05-tau0.05";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.05, tau=0.05, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestimA114;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.05-tau0.05-active";
	keep isim x1-x4 x6 x7 method;
	run;


	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.05, final=0, notes=0);
	data abeestimA17;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.20-tau0.05";
	keep isim x1-x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x5 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.05, active=x2 x3 x4 x5 x6 x7, final=0, notes=0);
	data abeestimA18;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-NONE-p0.20-tau0.05-active";
	keep isim x1-x7 method;
	run;


	%abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.05, final=0, notes=0);
	data abeestimA117;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.20-tau0.05";
	keep isim x1-x4 x6 x7 method;
	run;

    %abe(data=sim1, time=y, cens=cens, varlist=x1 x2 x3 x4 x6 x7, first=none, include=x1, proc=&proc, pmulti=0.2, tau=0.05, active=x2 x3 x4 x6 x7, final=0, notes=0);
	data abeestimA118;
	set _matrix_;
	length method $ 50;
	if _n_=1;
	isim=&isim;
	method="ABE-DISJ-NONE-p0.20-tau0.05-active";
	keep isim x1-x4 x6 x7 method;
	run;


	data allabe;
	set allabe abeestim1 abeestim2 abeestim3 abeestim4 abeestim5 abeestim6 abeestim7 abeestim8 abeestim9 abeestim10 abeestim11 abeestim12 abeestim13 abeestim14 abeestim15 abeestim16
	abeestim17 abeestim18 abeestim19 abeestim20
    abeestim101 abeestim102 abeestim103 abeestim104 abeestim105 abeestim106 abeestim107 abeestim108 abeestim109 abeestim110 abeestim111 abeestim112 abeestim113 abeestim114 abeestim115 abeestim116
    abeestim117 abeestim118 abeestim119 abeestim120
	abeestimA1 abeestimA2 abeestimA5 abeestimA6 abeestimA9 abeestimA10 abeestimA13 abeestimA14 abeestimA101	abeestimA102 abeestimA105 abeestimA106 abeestimA109	abeestimA110 abeestimA113 abeestimA114 	abeestimA17 abeestimA18 abeestimA117 abeestimA118;
run;
%end;
%mend;

