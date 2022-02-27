/**********************************************************************************************
***********************************************************************************************
Macro Name: DESCRIPTIVE
Created Date/Author: Feb. 2012/Yuan Liu
Last Update Date/Person: July 9, 2014/Dana Nickleach
Current Version: V14
Working Environment: SAS 9.3 English version

Contact: Dr. Yuan Liu Yliu31@emory.edu 

Purpose:  To produce a descriptive statistics summary table for each variable in the dataset. 
The frequency, including # of missing value will be generated for categorical variables; and 
summary statistics (n, mean, median, min, max, standard deviation, # of missing) for numerical 
variables.  

Notes: The order of variables in the summary table is the same as the input order. For the best
results, you may want to put the demographic variables together and also clinical 
characteristics variables together.     

Parameters: 

DATASET        The name of the data set to be analyzed.
CLIST          List of categorical variables, separated by empty space.
NLIST          List of numerical variables, separated by empty space.
CHI            Set to T to calculate the chi-square goodness of fit p-value for categorical 
               variables (optional).  The default value is F.
TTEST          Set to T to calculate the t-test p-value for mean=0 for numeric variables 
               (optional).  The default value is F. 
NONPAR         Set to T to calculate the sign test p-value for median=0 for numeric 
               variables (optional).  The default value is F.  
DOC            Set to T to create a RTF file containing the output or F to suppress creation of 
               the RTF file (optional).  The default value is T.
OUTPATH        Path for output table to be stored.
FNAME          File name for output table.
FOOTNOTE       Text of footnote to include in the table (optional).  It should be in quotes.  
               Leave this field blank if not including a footnote.
DEBUG          Set to T if running in debug mode (optional).  Work datasets will not be deleted 
               in debug mode.  This is useful if you are editing the code or want to further 
               manipulate the resulting data sets.  The default value is F.

***********************************************************************************************
For more details, please see the related documentation
**********************************************************************************************/

%MACRO DESCRIPTIVE(DATASET=, CLIST=, NLIST=, CHI=F, TTEST=F, NONPAR=F, DOC=T, OUTPATH=, FNAME=, 
   FOOTNOTE=, DEBUG=F); 

   %let debug = %UPCASE(&debug);
   %let chi = %UPCASE(&chi);
   %let doc = %UPCASE(&doc);
   %let ttest = %UPCASE(&ttest);
   %let nonpar = %UPCASE(&nonpar);

   /* If there are no categorical variables then can't produce chi-square test */
   %if &clist = %STR() %then %do;
      %let chi = F;
   %end;
   /* If there are no numeric variables then can't produce t- test */
   %if &nlist = %STR() %then %do;
      %let ttest = F;
   %end;

   /* Get list of data sets in work library to avoid deletion later */
   ODS EXCLUDE members Directory;
   ODS OUTPUT Members(nowarn)=_DataSetList;
   PROC DATASETS lib=work memtype=data;
   QUIT;

   /* If there are data sets in the work library */
   %if %sysfunc(exist(_DataSetList)) %then %do;
      PROC SQL noprint;
         select Name
         into :work_sets separated by ' '
         from _DataSetList;
      quit;
   %end;
   %else %do;
      %let work_sets =;
   %end;

   /* Save current options */
   PROC OPTSAVE out=_options;
   RUN;

   /* Format missing values consistently */
   OPTIONS MISSING = " ";

   * Character Variables;

   %IF &CLIST NE  %THEN %DO; 

      %LET N = 1; 

      %DO %UNTIL (%SCAN(&CLIST, &N) =   ); 
         %LET CVAR = %SCAN(&CLIST, &N); 

         ODS EXCLUDE "One-Way Frequencies" OneWayChiSq;
         ods output "One-Way Frequencies" = _temp %if &chi = T %then %do; OneWayChiSq=_chi %end;;
         proc freq data= &DATASET;
            table &CVAR/missprint %if &chi = T %then %do; chisq %end; PLOTS=NONE;
         run;
          
         %if &chi = T %then %do;
            /* Keep p-value */
            DATA _chi;
               set _chi;
               where Name1 = 'P_PCHI';
               RENAME nValue1=pval;
            RUN;
         %end;

         data _temp2; 
            length variable $256. levels $200.;
            set _temp;
               Variable = label(&CVAR);
               levels = strip(vvalue(&CVAR));
               keep variable levels Frequency Percent table;
           run;

           data _temp3 _temp4;
            set _temp2;
            if levels in  ("." " ") then output _temp3;
            if levels not in  ("." " ") then output _temp4 ;
         run;

         data _freq&N;
            set _temp4 _temp3; 
            if levels in  ("." " ") then levels = "Missing";
            %if &chi ~= T %then %do;
               DROP table;
            %end;
         run;

         %if &chi = T %then %do;
            /* Merge on p-values */
            DATA _freq&N;
               merge _freq&N _chi (keep=table pval);
               by table;
               DROP table;
            RUN;
         %end;
         

         %LET N = %EVAL(&N+1);

      %END; 

      %LET N = %EVAL(&N-1);
      DATA _freq_all; 
         SET _freq1-_freq&N; 
      RUN; 
      
   %END;

   *NUMERIC VARIABLES ;
   %IF &NLIST NE  %THEN %DO; 

      %LET N = 1; 

      %DO %UNTIL (%SCAN(&NLIST, &N) =   ); 
         %LET NVAR = %SCAN(&NLIST, &N); 

         PROC MEANS DATA=&DATASET noprint; 
            var &NVAR; 
            output out=_summary (drop=_TYPE_ _FREQ_) mean=Mean median=Median min=Minimum 
                    max=Maximum std=Std nmiss=Nmiss %if &ttest = T %then %do; probt=pval %end;/autoname;
         RUN;
          
         DATA _summary;
            set _summary;
            LABEL mean = 'Mean'
               median = 'Median'
               Minimum = 'Minimum'
               Maximum = 'Maximum'
               std = 'Std Dev'
               nmiss = 'Missing';
            %if &ttest = T %then %do;
               LABEL pval='P-value';
            %end;
         RUN;
     
         proc transpose data=_summary out=_summaryt;
            /* If t-test requested keep p-value in a separate column */
            %if &ttest = T %then %do;
               by pval;
            %end;
         run;
   
         data _NULL_; 
            set &dataset; 
            call symput('vv', put(label(&NVAR),$256.));
         run;

         %if &nonpar = T %then %do;
            /* Calculate signed rank test */
            ODS OUTPUT TestsForLocation=_nonpar;
            PROC UNIVARIATE DATA = &DATASET;
               var &nvar;
            RUN;
         %end;

         data _summary&N;
            length variable $256. levels $50. ;
            set _summaryt;
            /* Merge on non-parametric p-value */
            %if &nonpar = T %then %do;
               if _n_ = 1 then set _nonpar (keep=test pValue RENAME=(pvalue=np_pval) where=(Test='Sign'));
            %end;
            variable = "&vv";
            levels = _LABEL_;
            /* This is named frequency just to be consistent with the categorical data */
            /* It is NOT a frequency */
            Frequency =  COL1;
            if levels = "N Miss" then levels = "Missing";
            DROP _NAME_ _LABEL_ col1;
         run;

         %LET N = %EVAL(&N+1);

      %END; 

      %LET N = %EVAL(&N-1);
      DATA _summary_all; 
         SET _summary1 - _summary&N; 
      RUN; 

   %END;

   /* Combine categorical and numerical results */
   DATA _report;
      set 
      %if &clist ~= %STR() %then %do;
          _freq_all
      %end;
      %if &nlist ~= %STR() %then %do;
          _summary_all
      %end;;
      /* Create order variable retaining original order */
      length hold $256;
      RETAIN hold ' ';
      IF _n_ = 1 THEN DO;
         hold = variable;
         order=1;
      END;
      IF hold ~= variable THEN DO;
         hold = variable;
         order + 1;
      END;
      DROP hold;
   RUN;
          
   /*Extract total number of observations in the dataset*/
   proc sql noprint; 
      select count(*) into :totalN from &dataset;
   quit;
 
   *---- table template -----;  

   ODS PATH WORK.TEMPLAT(UPDATE)
   SASUSR.TEMPLAT(UPDATE) SASHELP.TMPLMST(READ);

   PROC TEMPLATE;
   DEFINE STYLE STYLES.TABLES;
   NOTES "MY TABLE STYLE"; 
   PARENT=STYLES.MINIMAL;

     STYLE SYSTEMTITLE /FONT_SIZE = 12pt     FONT_FACE = "TIMES NEW ROMAN";

     STYLE HEADER /
           FONT_FACE = "TIMES NEW ROMAN"
            CELLPADDING=8
            JUST=C
            VJUST=C
            FONT_SIZE = 10pt
           FONT_WEIGHT = BOLD; 

     STYLE TABLE /
            FRAME=HSIDES            /* outside borders: void, box, above/below, vsides/hsides, lhs/rhs */
            RULES=GROUP              /* internal borders: none, all, cols, rows, groups */
            CELLPADDING=6            /* the space between table cell contents and the cell border */
            CELLSPACING=6           /* the space between table cells, allows background to show */
            JUST=C
            FONT_SIZE = 10pt
            BORDERWIDTH = 0.5pt;  /* the width of the borders and rules */

     STYLE DATAEMPHASIS /
           FONT_FACE = "TIMES NEW ROMAN"
           FONT_SIZE = 10pt
           FONT_WEIGHT = BOLD;

     STYLE DATA /
           FONT_FACE = "TIMES NEW ROMAN" 
           FONT_SIZE = 10pt;

     STYLE SYSTEMFOOTER /FONT_SIZE = 9pt FONT_FACE = "TIMES NEW ROMAN" JUST=C;
   END;

   RUN; 

   *------- build the table -----;

   OPTIONS ORIENTATION=PORTRAIT MISSING = "-" NODATE;

   %if &doc = T %then %do;
      ODS RTF STYLE=tables FILE= "&OUTPATH.&FNAME &SYSDATE..DOC"; 
   %end;

   PROC REPORT DATA=_report HEADLINE HEADSKIP CENTER STYLE(REPORT)={JUST=CENTER} SPLIT='~' nowd 
          SPANROWS LS=256;
      COLUMNS order variable levels Frequency 
         %if &clist ~= %STR() %then %do; Percent %end;
         %if &chi = T OR &ttest = T %then %do; pval %end;
         %if &nonpar = T %then %do; np_pval %end;; 
      DEFINE order/order order=internal noprint;
      DEFINE variable/ Order order=data  "Variable"  STYLE(COLUMN) = {JUST = L };
      DEFINE levels/ DISPLAY   "Level"   STYLE(COLUMN) = {JUST = R CellWidth=20%};

      DEFINE Frequency/DISPLAY "N = %trim(&totalN)" STYLE(COLUMN) = {JUST = C CellWidth=8%} format = BESTD6.2;
      /* Only print % column if there are categorical variables */
      %if &clist ~= %STR() %then %do;
         DEFINE percent/DISPLAY "%" STYLE(COLUMN) = {JUST = C CellWidth=8%} format=8.1;
      %end;

      /* Only print chi-square if requested */
      %if &chi = T OR &ttest = T %then %do; 
         DEFINE pval/ORDER MISSING "Parametric P-value*" STYLE(COLUMN)={JUST = C CellWidth=10%} FORMAT=PVALUE5.3;

         COMPUTE pval; 
              IF . < pval <0.05 THEN 
               CALL DEFINE("pval", "STYLE", "STYLE=[FONT_WEIGHT=BOLD]");
         ENDCOMP; 
      %end;

      %if &nonpar = T %then %do;
         DEFINE np_pval/ORDER MISSING "Non-Parametric P-value**" STYLE(COLUMN)={JUST = C CellWidth=13%} FORMAT=PVALUE5.3;

         COMPUTE np_pval; 
            IF . < np_pval <0.05 THEN 
               CALL DEFINE("np_pval", "STYLE", "STYLE=[FONT_WEIGHT=BOLD]");
         ENDCOMP; 
      %end;

      compute after _page_;
         %if &chi = T %then %do;
            line @0 "*  The parametric p-value is calculated by chi-square goodness-of-fit test for categorical variables.";
         %end;
         %if &ttest = T %then %do;
            line @0 "*  The parametric p-value is calculated using a t-test for (mean=0) for numeric variables.";
         %end;
         %if &nonpar = T %then %do;
            line @0 "*  The non-parametric p-value is calculated using a sign test (median=0) for numeric variables.";
         %end;
         %if &footnote ~= %STR() %then %do;
            line @0 &footnote;
         %end;
      ENDCOMP;
       
      compute after variable; line ''; endcomp; 
       
   RUN; 

   %if &doc = T %then %do;
      ODS RTF CLOSE; 
   %end;

   /* Reload original options that were in use before running the macro */
   PROC OPTLOAD data=_options;
   RUN;

   /* Only delete files if not in debug mode */
   %if &debug ~= T %then %do;

      /* If there are work data sets that should not be deleted */
      %if %sysevalf(%superq(work_sets)~=,boolean) %then %do;
         /* DELETE ALL TEMPORARY DATASETS that were created */
         proc datasets lib=work memtype=data noprint;  
            save &work_sets;
         quit;  
      %end;
      %else %do;
         proc datasets lib=work kill memtype=data noprint;  
         quit; 
      %end;
   %end;

%mend; 



