PROC IMPORT OUT= WORK.aim2_desc 
            DATAFILE= "U:\CSU EPA Health Study\csu_health\DATA\Processed
\Aim2\Agreement\aim2_desc.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="'Sheet 1$'"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
