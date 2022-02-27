libname csu "\\romans\departments\Campbell Projects\June\CSU EPA Health Study\csu_health\DATA\Processed\Aim1";
%Let folder = \\romans\departments\Campbell Projects\June\CSU EPA Health Study\csu_health;

%include "\\romans\departments\Campbell Projects\June\CSU EPA Health Study\csu_health\scripts\SAS Macros\UNI_CAT V29.sas";

/*************************************************************************************************************/
/* READ SAS FILES FROM R*/
* Written by R;
*  foreign::write.foreign(dataset.name, datafile = paste0(file.location,  ;

DATA  aim1_analysis ;
LENGTH
 id_dao $ 11
 cdenumber $ 4
 studentkey $ 6
 grade $ 3
 birth_date $ 16
 testscore_gender $ 1
 testscore_ethnicity $ 16
 testscore_gifted $ 33
 testscore_special_ed $ 6
 GEOID $ 11
 ses_crowding_cat $ 1
 ses_poverty_all_cat $ 1
 ses_renter_all_cat $ 1
 ses_unemployed_cat $ 1
 testscore_totalunexcuseddays_cat $ 1
 school_student_enrollment_avg_ct $ 3
 ieq_indoor_cat $ 1
 ieq_visual_cat $ 1
;

INFILE  "&folder.\DATA\Processed\Aim1\aim1_analysis.txt" 
     DSD 
     LRECL= 422 ;
INPUT
 id_dao
 cdenumber
 elascalescore
 mathscalescore
 studentkey
 grade
 endyear
 birth_date
 testscore_gender
 testscore_totaldaysmissed
 testscore_totalunexcuseddays
 testscore_instructionday
 testscore_ethnicity
 testscore_gifted
 testscore_special_ed
 ieq_thermal
 ieq_acoustics
 ieq_visual
 ieq_indoor
 school_pct_frl_avg
 school_student_enrollment_avg
 GEOID
 ses_married_6to17
 ses_edu_highschoolmore
 ses_edu_bachelormore
 ses_poverty_all
 ses_medianhhincome
 ses_unemployed
 ses_renter_all
 ses_crowding
 ses_uninsured_6to18
 i1
 i2
 i3
 i4
 ses_medianhhincome_log10
 ses_crowding_cat
 ses_poverty_all_cat
 ses_renter_all_cat
 ses_unemployed_cat
 testscore_totalunexcuseddays_cat
 school_student_enrollment_avg_ct
 ieq_indoor_cat
 ieq_visual_cat $ 
;
LABEL  school_student_enrollment_avg_ct = "school_student_enrollment_avg_cat" ;
RUN;
/*************************************************************************************************************/



/*************************************************************************************************************/
/*CLEAN DATASET ANALYSIS*/
DATA analysis;
Set aim1_analysis;
where grade in ('50', '80', '90');
run;




/*************************************************************************************************************/

%let cat_var = testscore_gender testscore_ethnicity testscore_gifted testscore_special_ed ;
%let num_var = 
 			testscore_totaldaysmissed testscore_totalunexcuseddays testscore_instructionday
 			school_pct_frl_avg school_student_enrollment_avg			
 			ses_married_6to17 ses_edu_highschoolmore ses_edu_bachelormore ses_poverty_all 
			ses_medianhhincome ses_medianhhincome_log10 ses_unemployed ses_renter_all ses_crowding ses_uninsured_6to18
			ieq_thermal ieq_acoustics ieq_visual ieq_indoor
			elascalescore mathscalescore;
/*%let outpath_value = U:\CSU EPA Health Study\csu_health\outputs\tables\Aim1\;*/
%let outpath_value = &folder.\outputs\tables\Aim1\;

/*************************************************************************************************************/
/*CREATE TABLE 1*/
TITLE 'Table 1 Univariate Association with Progression';
%UNI_CAT(dataset = analysis, 
	outcome = grade, 
	clist = &cat_var, 
	nlist = &num_var, 
	spread = T,
	nonpar = F,
	rowpercent = F,
	orientation = landscape,
	doc = T,
	outpath = &outpath_value , 
	fname = table1);
quit;
TITLE;

/*************************************************************************************************************/



/*	clist = */
/*/*			id_dao cdenumber studentkey grade birth_date GEOID*/*/
/*			testscore_gender testscore_ethnicity testscore_gifted testscore_special_ed */
/*/*			testscore_totalunexcuseddays_cat*/*/
/*/*			ses_crowding_cat ses_poverty_all_cat ses_renter_all_cat ses_unemployed_cat*/*/
/*/*			school_student_enrollment_avg_ct*/*/
/*/*			ieq_indoor_cat ieq_visual_cat, */*/
/*	nlist = */
/*			elascalescore mathscalescore*/
/*/*			endyear*/*/
/* 			testscore_totaldaysmissed testscore_totalunexcuseddays testscore_instructionday*/
/* 			ieq_thermal ieq_acoustics ieq_visual ieq_indoor*/
/*			school_pct_frl_avg school_student_enrollment_avg*/
/* 			ses_married_6to17 ses_edu_highschoolmore ses_edu_bachelormore ses_poverty_all */
/*			ses_medianhhincome ses_medianhhincome_log10 ses_unemployed ses_renter_all ses_crowding ses_uninsured_6to18, */
