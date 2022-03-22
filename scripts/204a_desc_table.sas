/*libname csu "\\romans\departments\Campbell Projects\June\CSU EPA Health Study\csu_health\DATA\Processed";*/
%Let folder = \\romans\departments\Campbell Projects\June\CSU EPA Health Study\csu_health;

%include "\\romans\departments\Campbell Projects\June\CSU EPA Health Study\csu_health\scripts\SAS Macros\UNI_CAT V29.sas";

/*************************************************************************************************************/
PROC IMPORT OUT= WORK.aim2_desc 
            DATAFILE= "U:\CSU EPA Health Study\csu_health\DATA\Processed\Aim2\Agreement\aim2_desc.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="'Sheet 1$'"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
/*************************************************************************************************************/
/*CLEAN DATASET ANALYSIS*/
DATA analysis;
Set aim2_desc;
Label 	gs_0025 = "25m"
		gs_0050 = "50m"
		gs_0100 = "100m"
		gs_0250 = "250m"
		gs_0500 = "500m"
		gs_1000 = "1000m"
		month = "Month"
		raster = "Data source"
		id_dao = "ID";
run;
PROC CONTENTS; run;
/*************************************************************************************************************/




/*************************************************************************************************************/
%let dataset = analysis;
%let cat_var = ;
%let num_var = gs_0025 gs_0050 gs_0100 gs_0250 gs_0500 gs_1000;
%let outcome = raster;
%let outpath_value = &folder.\outputs\tables\Aim2\;
%let fname = table1;
/*************************************************************************************************************/
/*CREATE TABLE 1*/
TITLE 'Table 1 Univariate Association with Progression';
%UNI_CAT(dataset = &dataset, 
	outcome = &outcome, 
	clist = &cat_var, 
	nlist = &num_var, 
	spread = T,
	nonpar = F,
	rowpercent = F,
	orientation = portrait,
	doc = T,
	outpath = &outpath_value , 
	fname = &fname);
quit;
TITLE;
/*************************************************************************************************************/
