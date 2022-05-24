libname csu "\\isi-okc-business\HCOPH\Research\Campbell Projects\June\CSU EPA Health Study\csu_health\DATA\Processed\Aim1";
%Let folder = \\isi-okc-business\HCOPH\Research\Campbell Projects\June\CSU EPA Health Study\csu_health;


/*************************************************************************************************************/
PROC IMPORT OUT= WORK.analysis 
            DATAFILE= "U:\CSU EPA Health Study\csu_health\DATA\Processed\Aim1\aim1_analysis.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="'Sheet 1$'"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
PROC CONTENTS; run;
/*************************************************************************************************************/


proc mixed data=analysis ;
   class ieq_visual_cat ieq_indoor_cat
		testscore_ethnicity testscore_gender testscore_gifted testscore_special_ed testscore_totalunexcuseddays_cat
		school_student_enrollment_avg_ca
		cdenumber GEOID id_dao;
   model elascalescore = 
/*   						ieq_thermal*/
/*						ieq_acoustics*/
/*						ieq_visual_cat*/
/*						ieq_indoor_cat*/

						ses_medianhhincome_log10 
						testscore_ethnicity testscore_gender testscore_gifted testscore_special_ed testscore_totalunexcuseddays_cat testscore_totaldaysmissed 
						school_pct_frl_avg school_student_enrollment_avg_ca/ s;
   random cdenumber GEOID / type=un sub=id_dao s;
run;


