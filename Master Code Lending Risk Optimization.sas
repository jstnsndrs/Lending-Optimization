
*************************************************************************************************************************;
******************************************************Sorting Data Set and Cleaning Variables****************************;
*************************************************************************************************************************;
LIBNAME class "/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid";

*'Sorting' data by matchkey and 'delqid';
proc sort data=CD4330.perf out=testperf;
by Matchkey delqid;
run;
proc sort data=CD4330.cpr out=testcpr;
by Matchkey;
run;

*Won't merge correctly;
Data badtest;
merge testcpr testperf;
run;
Proc print data = badtest (obs = 100);
var matchkey delqid trades age;
run;

*Let's try a different approach!;
proc sql;
create table test1 as
select matchkey,count(distinct delqid) as COUNT 
from badtest 
group by matchkey having count>1;
quit;
Proc print data = test1 (obs=20);
run;


*Must be sorted before merging;
data merged;
merge testcpr testperf;
by Matchkey;
	if last.Matchkey;
	if delqid ne .; *create new dataset without missing delqid values;
	if age ne .; 
run;
*Final dataset has 1255429 observations;
proc print data=merged (obs=20);
run;
proc contents data=merged;
run;

*////////////////////////////////////////////////;
*/Establish binary Dependent variable////////////;
*/if there is no DELQID,what are you going to do?;  
*QUESTION - If age is missing, I can tell you//// 
that the whole observation is missing////////////;
*////////////////////////////////////////////////;

Data merged1;
set class.merged;
if delqid <3 then goodbad = 0;
else goodbad =1;
run;

Proc freq data = merged1;
tables goodbad;
run;


* Drop variables;
data creditrisk;
  set merged1;
  drop age beacon cashamt cashbal chgoffid cycledt 
  	latefee numcash numpay numpur opendate
	ovlimfee payamt paydue puramt purbal tbal;
run;

*THIS LOOKS GOOD.  LETS EXPORT THE DATASET TO OUR PERMANENT LIBRARY IN SAS UNDER 'STAT 4330 SAS Grid' FOLDER;
proc export data=creditrisk
    outfile='/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid/creditrisk.csv'
    dbms=csv replace;
run;


*************************************************************************************************************************;
******************************************************Manual Imputation**************************************************;
*************************************************************************************************************************;
FILENAME REFFILE '/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid/creditrisk.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=csv REPLACE
	OUT=data;
	GETNAMES=YES;
RUN;

%let d1 = data;
Proc univariate data=&d1 noprint;
Var PRDEROG;
histogram;
title "Figure X: Histogram of PRDEROG Variable";
Run;
Proc means data = &d1 min max mean median std N;
var PRDEROG;
where PRDEROG;
title "Table X: Descriptive Statistics of PRDEROG Variable";
run;
Proc freq data=&d1;
Tables PRDEROG;
where PRDEROG > 92;
title "Table X: Frequency Table of PRDEROG Variable For Coded Values";
Run;
Proc means data = &d1 min max median mean std N;
var PRDEROG;
where PRDEROG <= 92;
title "Table X: Descriptive Statistics of PRDEROG Variable for Non-Coded Values";
run;

Data &d1;
set &d1;
if PRDEROG >92 then PRDEROG=1;
Proc univariate data=&d1 noprint;
Var PRDEROG;
histogram;
title "Figure X: Histogram of PRDEROG Variable After Imputatation of Coded Values";
Run;
Proc means data = &d1 min max mean median std N;
var PRDEROG;
where PRDEROG;
title "Table X: Descriptive Statistics of PRDEROG Variable After Imputatation of Coded Values";
run;



*************************************************************************************************************************;
***************************************************************MACRO CALL************************************************;
*************************************************************************************************************************;
%MACRO IMPV3(DSN=,VARS=,EXCLUDE=,PCTREM=.6,MSTD=4)/MINOPERATOR;
%PUT IMPUTE 3.0 IS NOW RUNNING YOU ARE THE GREATEST;

*DETERMING LOG STUFF;
FILENAME LOG1 DUMMY;
PROC PRINTTO LOG=LOG1;
RUN;

/*FILE AND DATA SET REFERENCES*/
%IF %INDEX(&DSN,.) %THEN %DO;
        %LET LIB=%UPCASE(%SCAN(&DSN,1,.));
        %LET DATA=%UPCASE(%SCAN(&DSN,2,.));
%END;
%ELSE %DO;
        %LET LIB=WORK;
        %LET DATA=%UPCASE(&DSN);
%END;

%LET DSID=%SYSFUNC(OPEN(&LIB..&DATA));
%LET NOBS=%SYSFUNC(ATTRN(&DSID,NOBS));
%LET CLOSE=%SYSFUNC(CLOSE(&DSID));

%PUT &NOBS;

DATA TEMP;
        SET &LIB..&DATA;
RUN;

/*MODULE IF _ALL_ KEYWORD IS PRESENT*/
%IF %UPCASE(&VARS)=_ALL_ AND &EXCLUDE= %THEN %DO;
PROC PRINTTO;
RUN;

%PUT ============================================;
%PUT XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX;
%PUT ;
%PUT XXX: EXCLUDE PARAMETER IS NULL;
%PUT XXX: IMPUTE MACRO IS TERMINATING PREMATURELY;
%PUT;
%PUT XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX;
%PUT ============================================;

%RETURN;
%END;

%ELSE %IF %UPCASE(&VARS)=_ALL_ AND &EXCLUDE^= %THEN %DO;

%LET NEXC=%SYSFUNC(COUNTW(&EXCLUDE,%STR( )));
/*SELECTING ALL THE VARIABLES TO BE IMPUTED*/

PROC SQL NOPRINT;
        SELECT NAME INTO: VARNAME SEPARATED BY ' '
                FROM DICTIONARY.COLUMNS
                WHERE UPCASE(LIBNAME)="&LIB" AND UPCASE(MEMNAME)="&DATA"
                                AND NAME NOT IN("%SCAN(&EXCLUDE,1,%STR( ))" %DO A=2 %TO &NEXC;
                                                                                                                 ,"%SCAN(&EXCLUDE,&A,%STR( ))"
                                                                                                                %END;);
QUIT;

%DO B=1 %TO %SYSFUNC(COUNTW(&VARNAME,%STR( )));
                
%LET CURR=%SCAN(&VARNAME,&B,%STR( ));
/*FINDING OUT IF VAR CONTAINS CODES*/
PROC MEANS DATA=TEMP(KEEP=&CURR) NOPRINT MAX;
        VAR &CURR;
        OUTPUT OUT=MAX MAX=MAX;
RUN;

DATA _NULL_;
        SET MAX;
        CALL SYMPUTX('MAX',MAX);
RUN;
/*IF NO CODED VALUES ARE DETECTED THEN IMPUTATION OF MISSING VALUES OCCURS*/
/*I KNOW THERE ARE MSSING VALUES WITHIN THE DATA SET BUT JUST IN CASE*/
%IF %EVAL(%SYSFUNC(INDEXW(%STR(9999999 9999 999 99 9.9999),&MAX))<1) %THEN %DO;
PROC SQL NOPRINT;
        SELECT MISSING(&CURR) INTO: MISS
        FROM TEMP;
QUIT;
/*THIS DROPS VARS IF PROBALITY OF FINDING A MISSING VALUES IS GREATER THEN PCTREM*/
%IF %SYSEVALF((&MISS/&NOBS)>&PCTREM) %THEN %DO;
PROC SQL NOPRINT;
        ALTER TABLE TEMP
        DROP &CURR;
QUIT;

PROC PRINTTO;
RUN;
%PUT &CURR HAS BEEN REMOVED BECAUSE IT DOES NOT MEET &PCTREM CRITERION;
PROC PRINTTO LOG=LOG1
RUN;
%END;

%ELSE %DO;
/*FINDING MEDIAN*/
PROC MEANS DATA=TEMP(KEEP=&CURR) NOPRINT MEDIAN;
        VAR &CURR;
        OUTPUT OUT=MEDI MEDIAN=MEDIAN;
RUN;

DATA _NULL_;
        SET MEDI;
        CALL SYMPUTX('MEDIAN',MEDIAN);
RUN;
/*MISSING IMPUTATION*/
DATA TEMP;
        SET TEMP;
        IF &CURR=. THEN &CURR=&MEDIAN;
RUN;
%END;
%END;
/*THIS NEXT PART IS SAME AS ABOVE WITH THE EXCEPTION THIS HANDLES CODED VALUES*/
%ELSE %DO;
DATA _NULL_;
        IF &MAX=99 THEN CALL SYMPUTX('LOW',92);
                ELSE IF &MAX=999 THEN CALL SYMPUTX('LOW',992);
                        ELSE IF &MAX=9999 THEN CALL SYMPUTX('LOW',9992);
                ELSE IF &MAX=9.9999 THEN CALL SYMPUTX('LOW',9.9992);
        ELSE CALL SYMPUTX('LOW',9999992);
RUN;

PROC SQL NOPRINT;
        SELECT COUNT(&CURR) INTO: CCODE
                        FROM TEMP
                        WHERE &CURR BETWEEN &LOW AND &MAX;
        SELECT MISSING(&CURR) INTO: MISS
                        FROM TEMP;
QUIT;

%IF %SYSEVALF(((&CCODE+&MISS)/&NOBS)>&PCTREM) %THEN %DO;

PROC SQL;
        ALTER TABLE TEMP
        DROP &CURR;
QUIT;

PROC PRINTTO;
RUN;
%PUT &CURR HAS BEEN REMOVED, TOO MUCH DATA IS CODED        OR MISSING;
PROC PRINTTO LOG=LOG1;
RUN;
%END;
                        
%ELSE %DO;
PROC MEANS DATA=TEMP(KEEP=&CURR) NOPRINT MEDIAN STD MEAN;
        VAR &CURR;
        OUTPUT OUT=NUM MEDIAN=MEDIAN STD=STD MEAN=MEAN;
        WHERE &CURR<&LOW;
RUN;

DATA _NULL_;
        SET NUM;
        CALL SYMPUTX('MEDIAN',MEDIAN);
        CALL SYMPUTX('STD',STD);
        CALL SYMPUTX('MEAN',MEAN);
RUN;

DATA TEMP;
        SET TEMP;
        IF &LOW<=&CURR<=&MAX | &CURR>&MEAN+&MSTD*&STD | &CURR<&MEAN-&MSTD*&STD THEN &CURR=&MEDIAN;
RUN;
%END;
%END;
%END;
%END;

/*THIS NEXT PART HANDLES A LIST OF VARIABLES PROVIDED IN THE VAR STATEMENT*/
%ELSE %DO;
%LET NVAR=%SYSFUNC(COUNTW(&VARS,%STR( )));

%DO C=1 %TO &NVAR;
%LET CURR=%SCAN(&VARS,&C,%STR( ));

PROC MEANS DATA=TEMP(KEEP=&CURR) NOPRINT MAX;
VAR &CURR;
OUTPUT OUT=MAX MAX=MAX;
RUN;

DATA _NULL_;
        SET MAX;
        CALL SYMPUTX('MAX',MAX);
RUN;
                
%IF %EVAL(%SYSFUNC(INDEXW(%STR(9999999 9999 999 99 9.9999),&MAX))<1) %THEN %DO;
PROC SQL NOPRINT;
        SELECT MISSING(&CURR) INTO: MISS
                FROM TEMP;
QUIT;

%IF %SYSEVALF((&MISS/&NOBS)>&PCTREM) %THEN %DO;
PROC SQL NOPRINT;
        ALTER TABLE TEMP
        DROP &CURR;
QUIT;
%END;

%ELSE %DO;
PROC MEANS DATA=TEMP(KEEP=&CURR) NOPRINT MEDIAN;
        VAR &CURR;
        OUTPUT OUT=MEDI MEDIAN=MEDIAN;
RUN;

DATA _NULL_;
        SET MEDI;
        CALL SYMPUTX('MEDIAN',MEDIAN);
RUN;

DATA TEMP;
        SET TEMP;
        IF &CURR=. THEN &CURR=&MEDIAN;
RUN;
%END;
%END;

%ELSE %DO;
DATA _NULL_;
        IF &MAX=99 THEN CALL SYMPUTX('LOW',92);
                ELSE IF &MAX=999 THEN CALL SYMPUTX('LOW',992);
                        ELSE IF &MAX=9999 THEN CALL SYMPUTX('LOW',9992);
                ELSE IF &MAX=9.9999 THEN CALL SYMPUTX('LOW',9.9992);
        ELSE CALL SYMPUTX('LOW',9999992);
RUN;

PROC SQL NOPRINT;
        SELECT COUNT(&CURR) INTO: CCODE
                FROM TEMP
                WHERE &CURR BETWEEN &LOW AND &MAX;
        SELECT MISSING(&CURR) INTO: MISS
                FROM TEMP;
QUIT;

%IF %SYSEVALF(((&CCODE+&MISS)/&NOBS)>&PCTREM) %THEN %DO;

PROC SQL;
        ALTER TABLE TEMP
        DROP &CURR;
QUIT;

PROC PRINTTO;
RUN;
%PUT &CURR HAS BEEN REMOVED, TOO MUCH DATA IS CODED        OR MISSING;
PROC PRINTTO LOG=LOG1;
RUN;
%END;
                        
%ELSE %DO;
PROC MEANS DATA=TEMP(KEEP=&CURR) NOPRINT MEDIAN STD MEAN;
        VAR &CURR;
        OUTPUT OUT=NUM MEDIAN=MEDIAN STD=STD MEAN=MEAN;
        WHERE &CURR<&LOW;
RUN;

DATA _NULL_;
        SET NUM;
        CALL SYMPUTX('MEDIAN',MEDIAN);
        CALL SYMPUTX('STD',STD);
        CALL SYMPUTX('MEAN',MEAN);
RUN;

DATA TEMP;
        SET TEMP;
        IF &LOW<=&CURR<=&MAX | &CURR>(&MEAN + &MSTD*&STD) | &CURR<(&MEAN -&MSTD*&STD) THEN &CURR=&MEDIAN;
RUN;
%END;
%END;
%END;
%END;

/*CREATING NEW DATASET*/
DATA &LIB..&DATA.OUT;
        SET TEMP;
RUN;

PROC DATASETS NOLIST;
        DELETE NUM TEMP MEDI MAX;
QUIT;

PROC PRINTTO;
RUN;

%PUT THIS MACRO HAS FINISHED RUNNING HAVE A NICE DAY;
%MEND IMPV3;




*************************************************************************************************************************;
****************************************************MSTD MACRO***********************************************************;
*************************************************************************************************************************;
FILENAME REFFILE '/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid/creditrisk.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=csv REPLACE
	OUT=data;
	GETNAMES=YES;
RUN;
%IMPV3(DSN=data, VARS=_ALL_, 
	EXCLUDE= goodbad matchkey delqid crelim, 
	PCTREM=0.379, MSTD=5.0);
proc export data=dataout
    outfile='/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid/imputed credit risk.csv'
    dbms=csv replace;
run;
Proc means data=data n min max mean median nmiss maxdec=2 t kurtosis ;
run;
Proc means data=dataout n min max mean median nmiss maxdec=2 t kurtosis ;
run;



*************************************************************************************************************************;
************************************************************************Variable Clustering******************************;
*************************************************************************************************************************;
FILENAME REFFILE '/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid/imputed credit risk.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=csv REPLACE
	OUT=dataout;
	GETNAMES=YES;
RUN;

data clusterdata;
set dataout;
drop delqid;
run;

ods listing close;
ods output clusterquality = summary rsquare=clusters;
proc varclus data = clusterdata outtree = tree maxc=35;  *1-r^2 ratio wants to be low. R-square is the percent of
determination of the dependent variable as explained by the independent variables. PLOTS(MAXPOINTS=5);
*var &inputs;
run;

ods listing;
data _null_;
set summary;
call symput ('nvar',compress(NumberofClusters));
run;
proc print data = clusters noobs;
where NumberofClusters = &nvar;
var cluster variable rsquareratio;
run;
proc contents data=summary varnum;
run;
symbol v=square color=black i=join;
proc gplot data=summary;
plot propvarexplained*numberofclusters;
run;
quit;

data clusters35;
set clusters;
if numberofclusters = 35;
run;
proc contents data=clusters35;
run;
/* *Example of cluster correlation for Clusters 10 & 11;
%let inputs = TADB BRADB RADB6
			  TRATE2 TR224 BRRATE2 BRR224;  
proc corr data=dataout;
var &inputs;
run;

%let inputs = BRCRATE7 BRCRATE1 TRCR49 TOPEN12 DCCR49
			  BRRATE3 TCR1BAL TR4524 RADB6 TROPENEX BRRATE2
			  RBAL BADPR1 LOCINQS OT3PTOT PRMINQS BRHIC BRAGE
			  TPOPEN CRATE3 BRR29P24 BRMINB LAAGE OT24PTOT BRCRATE2 COLLS
			  DCWCRATE BKPOP TOPEN OBRPTAT CPAF29 TOPENB75 BNKINQ2 DCLAAGE BRPOPEN;
proc corr data=dataout;
var &inputs;
run;
proc reg data=dataout;
	model goodbad = &inputs / vif p;
run;
%let inputs = BRCRATE1 TROPENEX TOPEN;
proc corr data=dataout;
var &inputs;
run;
*TOPEN is removed;

%let inputs = BRCRATE7 BRCRATE1 TRCR49 TOPEN12 DCCR49
			  BRRATE3 TCR1BAL TR4524 RADB6 BRRATE2 TROPENEX
			  RBAL BADPR1 LOCINQS OT3PTOT PRMINQS BRHIC BRAGE
			  TPOPEN BRCRATE3 BRR29P24 BRMINB LAAGE OT24PTOT BRCRATE2 COLLS
			  DCWCRATE BKPOP OBRPTAT CPAF29 TOPENB75 BNKINQ2 DCLAAGE BRPOPEN;
proc corr data=dataout;
var &inputs;
run;
proc reg data=dataout;
	model goodbad = &inputs / vif p;
run;
%let inputs = TRCR49 TCR1BAL BADPR1;
proc corr data=dataout;
var &inputs;
run;
*BRCRATE1 is removed;

%let inputs = BRCRATE7 TRCR49 TOPEN12 DCCR49
			  BRRATE3 TCR1BAL TR4524 RADB6 BRRATE2 TROPENEX
			  RBAL BADPR1 LOCINQS OT3PTOT PRMINQS BRHIC BRAGE
			  TPOPEN BRCRATE3 BRR29P24 BRMINB LAAGE OT24PTOT BRCRATE2 COLLS
			  DCWCRATE BKPOP OBRPTAT CPAF29 TOPENB75 BNKINQ2 DCLAAGE BRPOPEN;
proc corr data=dataout;
var &inputs;
run;
proc reg data=dataout;
	model goodbad = &inputs / vif p;
run;
%let inputs = TRCR49 TCR1BAL BADPR1;
proc corr data=dataout;
var &inputs;
run; */


* Keep variables;
%let inputs = BRCRATE7 TRCR49 TOPEN12 DCCR49
			  BRRATE3 TCR1BAL TR4524 RADB6 BRRATE2 TROPENEX
			  RBAL BADPR1 LOCINQS OT3PTOT PRMINQS BRHIC BRAGE
			  TPOPEN BRCRATE3 BRR29P24 BRMINB LAAGE OT24PTOT BRCRATE2 COLLS
			  DCWCRATE BKPOP OBRPTAT CPAF29 TOPENB75 BNKINQ2 DCLAAGE BRPOPEN goodbad matchkey crelim;
data creditrisk;
  set clusterdata;
  keep &inputs;
run;
proc export data=creditrisk
    outfile='/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid/imputed credit risk after clustering VIF.csv'
    dbms=csv replace;
run;



*************************************************************************************************************************;
**********************************************DISCRETIZATION OF VARIABLES************************************************;
*************************************************************************************************************************;
*Importing and LIBNAME statement;
LIBNAME class "/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid";
FILENAME REFFILE '/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid/imputed credit risk after clustering VIF.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=csv REPLACE
	OUT=dataout;
	GETNAMES=YES;
RUN;
************************************************************************************;
************************Variables Already in Discrete Format************************;
************************************************************************************;
*1. Histogram and descriptive statistics of COLLS variable (no);
Proc univariate data=dataout;
var COLLS;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var COLLS;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class COLLS;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*COLLS;
run;
quit;
************;
************;
*2. Histogram and descriptive statistics of BKPOP variable (no);
Proc univariate data=dataout;
var BKPOP;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BKPOP;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BKPOP;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BKPOP;
run;
quit;
************;
************;
*3. Histogram and descriptive statistics of LOCINQS variable (no);
Proc univariate data=dataout;
var LOCINQS;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var LOCINQS;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class LOCINQS;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*LOCINQS;
run;
quit;
************;
************;
*7. Histogram and descriptive statistics of TR4524 variable (no);
Proc univariate data=dataout;
var TR4524;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var TR4524;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class TR4524;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*TR4524;
run;
quit;
************;
************;
*8. Histogram and descriptive statistics of TOPENB75 variable (no);
Proc univariate data=dataout;
var TOPENB75;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var TOPENB75;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class TOPENB75;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*TOPENB75;
run;
quit;
************;
************;
*10. Histogram and descriptive statistics of BRCRATE2 variable (no);
Proc univariate data=dataout;
var BRCRATE2;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BRCRATE2;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BRCRATE2;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BRCRATE2;
run;
quit;
************;
************;
*11. Histogram and descriptive statistics of BRCRATE3 variable (no);
Proc univariate data=dataout;
var BRCRATE3;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BRCRATE3;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BRCRATE3;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BRCRATE3;
run;
quit;
************;
************;
*12. Histogram and descriptive statistics of BRRATE2 variable (no);
Proc univariate data=dataout;
var BRRATE2;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BRRATE2;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BRRATE2;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BRRATE2;
run;
quit;
************;
************;
*13. Histogram and descriptive statistics of BRRATE3 variable (no);
Proc univariate data=dataout;
var BRRATE3;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BRRATE3;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BRRATE3;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BRRATE3;
run;
quit;
************;
************;
*14. Histogram and descriptive statistics of BRCRATE7 variable (no);
Proc univariate data=dataout;
var BRCRATE7;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BRCRATE7;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BRCRATE7;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BRCRATE7;
run;
quit;
************;
************;
*19. Histogram and descriptive statistics of BADPR1 variable (no);
Proc univariate data=dataout;
var BADPR1;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BADPR1;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BADPR1;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BADPR1;
run;
quit;
************;
************;
*20. Histogram and descriptive statistics of CPAF29 variable (no);
Proc univariate data=dataout;
var CPAF29;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var CPAF29;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class CPAF29;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*CPAF29;
run;
quit;
************;
************;
*21. Histogram and descriptive statistics of BRR29P24 variable (no);
Proc univariate data=dataout;
var BRR29P24;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BRR29P24;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BRR29P24;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BRR29P24;
run;
quit;
**************************************;
**************************************;
*25. Histogram and descriptive statistics of DCCR49 variable (NO);
Proc univariate data=dataout;
var DCCR49;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var DCCR49;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class DCCR49;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*DCCR49;
run;
quit;
************;
************;
*28. Histogram and descriptive statistics of OT24PTOT variable (YES);
Proc univariate data=dataout;
var OT24PTOT;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var OT24PTOT;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class OT24PTOT;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*OT24PTOT;
run;
quit;




************************************************************************************;
************************DISC 1 For Non-Discretized Variables***************************;
************************************************************************************;


**************************************;
**************************************;
*4. Histogram and descriptive statistics of PRMINQS variable (YES);
Proc univariate data=dataout;
var PRMINQS;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var PRMINQS;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class PRMINQS;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*PRMINQS;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set dataout;
if PRMINQS <= 11 then ORDPRMINQS = 1;
else ORDPRMINQS = 2;
Run;
Proc univariate data=disc1;
var ORDPRMINQS;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDPRMINQS;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDPRMINQS;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDPRMINQS;
Var PRMINQS GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsPRMINQS = (avg_dep/(1-avg_dep));
loddsPRMINQS = log(avg_dep/(1-avg_dep));
testPRMINQS = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDPRMINQS;
Run;
Proc sort data=&d1;
by ORDPRMINQS;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDPRMINQS;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*5. Histogram and descriptive statistics of TOPEN12 variable (yes);
Proc univariate data=dataout;
var TOPEN12;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var TOPEN12;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class TOPEN12;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*TOPEN12;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if TOPEN12 <= 5 then ORDTOPEN12 = 1;
else ORDTOPEN12 = 2;
Run;
Proc univariate data=disc1;
var ORDTOPEN12;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDTOPEN12;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDTOPEN12;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDTOPEN12;
Var TOPEN12 GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsTOPEN12 = (avg_dep/(1-avg_dep));
loddsTOPEN12 = log(avg_dep/(1-avg_dep));
testTOPEN12 = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDTOPEN12;
Run;
Proc sort data=&d1;
by ORDTOPEN12;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDTOPEN12;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*6. Histogram and descriptive statistics of LAAGE variable (YES);
Proc univariate data=dataout;
var LAAGE;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var LAAGE;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class LAAGE;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*LAAGE;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if LAAGE <= 7 then ORDLAAGE = 1;
else ORDLAAGE = 2;
Run;
Proc univariate data=disc1;
var ORDLAAGE;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDLAAGE;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDLAAGE;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDLAAGE;
Var LAAGE GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsLAAGE = (avg_dep/(1-avg_dep));
loddsLAAGE = log(avg_dep/(1-avg_dep));
testLAAGE = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDLAAGE;
Run;
Proc sort data=&d1;
by ORDLAAGE;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDLAAGE;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*9. Histogram and descriptive statistics of BRAGE variable (yes);
Proc univariate data=dataout;
var BRAGE;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BRAGE;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BRAGE;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BRAGE;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if BRAGE <= 24 then ORDBRAGE = 1;
else ORDBRAGE = 2;
Run;
Proc univariate data=disc1;
var ORDBRAGE;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDBRAGE;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDBRAGE;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDBRAGE;
Var BRAGE GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsBRAGE = (avg_dep/(1-avg_dep));
loddsBRAGE = log(avg_dep/(1-avg_dep));
testBRAGE = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDBRAGE;
Run;
Proc sort data=&d1;
by ORDBRAGE;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDBRAGE;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*15. Histogram and descriptive statistics of DCLAAGE variable (YES);
Proc univariate data=dataout;
var DCLAAGE;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var DCLAAGE;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class DCLAAGE;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*DCLAAGE;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if DCLAAGE = 0 then ORDDCLAAGE = 1;
else ORDDCLAAGE = 2;
Run;
Proc univariate data=disc1;
var ORDDCLAAGE;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDDCLAAGE;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDDCLAAGE;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDDCLAAGE;
Var DCLAAGE GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsDCLAAGE = (avg_dep/(1-avg_dep));
loddsDCLAAGE = log(avg_dep/(1-avg_dep));
testDCLAAGE = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDDCLAAGE;
Run;
Proc sort data=&d1;
by ORDDCLAAGE;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDDCLAAGE;
if matchkey = . then delete;
Run;

**************************************;
**************************************;
*17. Histogram and descriptive statistics of TPOPEN variable (YES);
Proc univariate data=dataout;
var TPOPEN;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var TPOPEN;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class TPOPEN;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*TPOPEN;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if TPOPEN <= .4 then ORDTPOPEN = 1;
else ORDTPOPEN = 2;
Run;
Proc univariate data=disc1;
var ORDTPOPEN;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDTPOPEN;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDTPOPEN;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDTPOPEN;
Var TPOPEN GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsTPOPEN = (avg_dep/(1-avg_dep));
loddsTPOPEN = log(avg_dep/(1-avg_dep));
testTPOPEN = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDTPOPEN;
Run;
Proc sort data=&d1;
by ORDTPOPEN;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDTPOPEN;
if matchkey = . then delete;
Run;

**************************************;
**************************************;
*18. Histogram and descriptive statistics of BRPOPEN variable (YES);
Proc univariate data=dataout;
var BRPOPEN;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BRPOPEN;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BRPOPEN;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BRPOPEN;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if BRPOPEN <= .45 then ORDBRPOPEN = 1;
else ORDBRPOPEN = 2;
Run;
Proc univariate data=disc1;
var ORDBRPOPEN;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDBRPOPEN;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDBRPOPEN;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDBRPOPEN;
Var BRPOPEN GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsBRPOPEN = (avg_dep/(1-avg_dep));
loddsBRPOPEN = log(avg_dep/(1-avg_dep));
testBRPOPEN = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDBRPOPEN;
Run;
Proc sort data=&d1;
by ORDBRPOPEN;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDBRPOPEN;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*22. Histogram and descriptive statistics of BNKINQ2 variable (YES);
Proc univariate data=dataout;
var BNKINQ2;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BNKINQ2;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BNKINQ2;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BNKINQ2;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if BNKINQ2 <= 4 then ORDBNKINQ2 = 1;
else ORDBNKINQ2 = 2;
Run;
Proc univariate data=disc1;
var ORDBNKINQ2;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDBNKINQ2;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDBNKINQ2;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDBNKINQ2;
Var BNKINQ2 GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsBNKINQ2 = (avg_dep/(1-avg_dep));
loddsBNKINQ2 = log(avg_dep/(1-avg_dep));
testBNKINQ2 = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDBNKINQ2;
Run;
Proc sort data=&d1;
by ORDBNKINQ2;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDBNKINQ2;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*23. Histogram and descriptive statistics of TCR1BAL variable (YES);
Proc univariate data=dataout;
var TCR1BAL;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var TCR1BAL;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class TCR1BAL;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*TCR1BAL;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if TCR1BAL <= 2 then ORDTCR1BAL = 1;
else ORDTCR1BAL = 2;
Run;
Proc univariate data=disc1;
var ORDTCR1BAL;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDTCR1BAL;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDTCR1BAL;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDTCR1BAL;
Var TCR1BAL GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsTCR1BAL = (avg_dep/(1-avg_dep));
loddsTCR1BAL = log(avg_dep/(1-avg_dep));
testTCR1BAL = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDTCR1BAL;
Run;
Proc sort data=&d1;
by ORDTCR1BAL;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDTCR1BAL;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*24. Histogram and descriptive statistics of TRCR49 variable (YES);
Proc univariate data=dataout;
var TRCR49;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var TRCR49;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class TRCR49;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*TRCR49;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if TRCR49 <= 1 then ORDTRCR49 = 1;
else ORDTRCR49 = 2;
Run;
Proc univariate data=disc1;
var ORDTRCR49;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDTRCR49;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDTRCR49;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDTRCR49;
Var TRCR49 GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsTRCR49 = (avg_dep/(1-avg_dep));
loddsTRCR49 = log(avg_dep/(1-avg_dep));
testTRCR49 = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDTRCR49;
Run;
Proc sort data=&d1;
by ORDTRCR49;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDTRCR49;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*26. Histogram and descriptive statistics of OBRPTAT variable (YES);
Proc univariate data=dataout;
var OBRPTAT;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var OBRPTAT;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class OBRPTAT;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*OBRPTAT;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if OBRPTAT <= .34 then ORDOBRPTAT = 1;
else ORDOBRPTAT = 2;
Run;
Proc univariate data=disc1;
var ORDOBRPTAT;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDOBRPTAT;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDOBRPTAT;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDOBRPTAT;
Var OBRPTAT GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsOBRPTAT = (avg_dep/(1-avg_dep));
loddsOBRPTAT = log(avg_dep/(1-avg_dep));
testOBRPTAT = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDOBRPTAT;
Run;
Proc sort data=&d1;
by ORDOBRPTAT;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDOBRPTAT;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*27. Histogram and descriptive statistics of OT3PTOT variable (YES);
Proc univariate data=dataout;
var OT3PTOT;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var OT3PTOT;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class OT3PTOT;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*OT3PTOT;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if OT3PTOT <= 0.3103 then ORDOT3PTOT = 1;
else ORDOT3PTOT = 2;
Run;
Proc univariate data=disc1;
var ORDOT3PTOT;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDOT3PTOT;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDOT3PTOT;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDOT3PTOT;
Var OT3PTOT GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsOT3PTOT = (avg_dep/(1-avg_dep));
loddsOT3PTOT = log(avg_dep/(1-avg_dep));
testOT3PTOT = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDOT3PTOT;
Run;
Proc sort data=&d1;
by ORDOT3PTOT;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDOT3PTOT;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*28. Histogram and descriptive statistics of OT24PTOT variable (YES);
Proc univariate data=dataout;
var OT24PTOT;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var OT24PTOT;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class OT24PTOT;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*OT24PTOT;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if OT24PTOT <= 1 then ORDOT24PTOT = 1;
else ORDOT24PTOT = 2;
Run;
Proc univariate data=disc1;
var ORDOT24PTOT;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDOT24PTOT;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDOT24PTOT;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDOT24PTOT;
Var OT24PTOT GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsOT24PTOT = (avg_dep/(1-avg_dep));
loddsOT24PTOT = log(avg_dep/(1-avg_dep));
testOT24PTOT = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDOT24PTOT;
Run;
Proc sort data=&d1;
by ORDOT24PTOT;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDOT24PTOT;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*29. Histogram and descriptive statistics of RBAL variable (YES);
Proc univariate data=dataout;
var RBAL;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var RBAL;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class RBAL;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*RBAL;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if RBAL <= 1500 then ORDRBAL = 1;
else ORDRBAL = 2;
Run;
Proc univariate data=disc1;
var ORDRBAL;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDRBAL;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDRBAL;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDRBAL;
Var RBAL GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsRBAL = (avg_dep/(1-avg_dep));
loddsRBAL = log(avg_dep/(1-avg_dep));
testRBAL = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDRBAL;
Run;
Proc sort data=&d1;
by ORDRBAL;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDRBAL;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*30. Histogram and descriptive statistics of RADB6 variable (YES);
Proc univariate data=dataout;
var RADB6;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var RADB6;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class RADB6;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*RADB6;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if RADB6 <= .45 then ORDRADB6 = 1;
else ORDRADB6 = 2;
Run;
Proc univariate data=disc1;
var ORDRADB6;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDRADB6;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDRADB6;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDRADB6;
Var RADB6 GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsRADB6 = (avg_dep/(1-avg_dep));
loddsRADB6 = log(avg_dep/(1-avg_dep));
testRADB6 = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDRADB6;
Run;
Proc sort data=&d1;
by ORDRADB6;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDRADB6;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
*32. Histogram and descriptive statistics of BRMINB variable (YES);
Proc univariate data=dataout;
var BRMINB;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BRMINB;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BRMINB;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BRMINB;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if BRMINB <= 2750 then ORDBRMINB = 1;
else ORDBRMINB = 2;
Run;
Proc univariate data=disc1;
var ORDBRMINB;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDBRMINB;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDBRMINB;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDBRMINB;
Var BRMINB GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsBRMINB = (avg_dep/(1-avg_dep));
loddsBRMINB = log(avg_dep/(1-avg_dep));
testBRMINB = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDBRMINB;
Run;
Proc sort data=&d1;
by ORDBRMINB;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDBRMINB;
if matchkey = . then delete;
Run;


**************************************;
**************************************;
************;
*33. Histogram and descriptive statistics of BRHIC variable (YES);
Proc univariate data=dataout;
var BRHIC;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var BRHIC;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class BRHIC;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*BRHIC;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if BRHIC <= 1750 then ORDBRHIC = 1;
else ORDBRHIC = 2;
Run;
Proc univariate data=disc1;
var ORDBRHIC;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDBRHIC;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDBRHIC;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDBRHIC;
Var BRHIC GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsBRHIC = (avg_dep/(1-avg_dep));
loddsBRHIC = log(avg_dep/(1-avg_dep));
testBRHIC = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDBRHIC;
Run;
Proc sort data=&d1;
by ORDBRHIC;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDBRHIC;
if matchkey = . then delete;
Run;


************;
************;
*16. Histogram and descriptive statistics of DCWCRATE variable (YES);
Proc univariate data=dataout;
var DCWCRATE;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var DCWCRATE;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class DCWCRATE;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*DCWCRATE;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if DCWCRATE <= 1 then ORDDCWCRATE = 1;
else ORDDCWCRATE = 2;
Run;
Proc univariate data=disc1;
var ORDDCWCRATE;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDDCWCRATE;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDDCWCRATE;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDDCWCRATE;
Var DCWCRATE GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsDCWCRATE = (avg_dep/(1-avg_dep));
loddsDCWCRATE = log(avg_dep/(1-avg_dep));
testDCWCRATE = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDDCWCRATE;
Run;
Proc sort data=&d1;
by ORDDCWCRATE;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDDCWCRATE;
if matchkey = . then delete;
Run;




************;
************;
*21. Histogram and descriptive statistics of TROPENEX variable (no);
Proc univariate data=dataout;
var TROPENEX;
histogram;
run;
Proc means data = dataout n min max median mean stddev nmiss maxdec=2 t kurtosis ;
var TROPENEX;
run;
Proc contents data = dataout;
run;
Proc means data=dataout;
Var goodbad;
class TROPENEX;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*TROPENEX;
run;
quit;
*Creating manual discretization of ranks with a histogram and 
 descriptive statistics of the newly discretized variable;
Data disc1;
set modelingfile;
if TROPENEX <= 10 then ORDTROPENEX = 1;
else ORDTROPENEX = 2;
Run;
Proc univariate data=disc1;
var ORDTROPENEX;
histogram;
run;
Proc means data=disc1;
Var goodbad;
class ORDTROPENEX;
output out=test mean=meangoodbad;
Run;
*Line Plot of the goodbad variable in relation to the 
 changes to the discretized independent variable;
symbol v=square color = black i=join;
Proc gplot data = test;
plot meangoodbad*ORDTROPENEX;
run;
quit;
*Frequncy table of average probability of default at each (discretized) rank;
%let d1 = disc1;
Proc means data = &d1 nmiss;
run;
Proc Summary data=&d1 mean missing std;
Class ORDTROPENEX;
Var TROPENEX GOODBAD;
Output out=summary mean=avg_ind avg_dep;
Run;
Proc print data = summary;
run;
*Calculating the odds / log of odds ratios of independent variable;
Data summary;
Set summary;
oddsTROPENEX = (avg_dep/(1-avg_dep));
loddsTROPENEX = log(avg_dep/(1-avg_dep));
testTROPENEX = ((1-avg_dep)/(avg_dep));
Run;
Proc Print data=summary;
Run;
*Code to merge the summary file values back into the original dataset;
Proc sort data=summary;
by ORDTROPENEX;
Run;
Proc sort data=&d1;
by ORDTROPENEX;
Run;
Data modelingfile (drop = _TYPE_  _FREQ_ avg_ind avg_dep);
Merge summary &d1;
by ORDTROPENEX;
if matchkey = . then delete;
Run;



************************************************************************************;
************************DISC 2 Macro Call***************************;
************************************************************************************;
************************************************************************************;
%MACRO disc2(pval_col=,var1=,d1=);

Proc Rank data=&d1 out=ranked groups = 10 ties = high;
ranks rank;
var &var1;
Run;

Proc Freq data=ranked;
Tables RANK;
Run;

*Here we are just checking to make sure that everything worked as expected;
*Again, no output created;
Proc Summary data=ranked missing mean std min max;
class rank;
Var &var1 GOODBAD;
Output out = summaryrank mean = avg_indp avg_dep std= std_indp std_dep min=min_indp min_dep 
max=max_indp max_dep;
Run;

Proc print data=summaryrank;
run;

*Note that the file reference here will be whatever was output from the Proc Summary;
**DO NOT CHANGE...EVER!;
proc sort data=summaryrank; by rank; run;
 

Data summaryrank;
Set summaryrank;
v1="&var1";
If _TYPE_ = 1;
rename _freq_ = numobs;
Run;

data temp;
   set summaryrank;
   by v1;
   retain _finit _rnk1 _rnk2 _freq1 _freq2 _aindp1 _aindp2
          _mindp1 _mindp2 _mxindp1 _mxindp2 _adep1 _adep2 
          _sdep1 _sdep2 _sdenom1 _sdenom2;

   *use _finit to flag when first initialization occurs;
   *First initialization is when the first non-missing level is;
   *encountered.  it is from this point forward that the t-tests;
   *take place, since missing levels are automatically outputted;
   *as their own level;

   if first.v1 then _finit=0;

   if rank<=.Z then output;
   else do;
      if _finit=0 then do;
         _finit=1;
		 _rnk1=rank;
		 _freq1=numobs;
		 _aindp1=numobs*avg_indp;
         _mindp1=min_indp;
         _mxindp1=max_indp;
         _adep1=numobs*avg_dep;
         if std_dep=. then _sdep1=0;
         else _sdep1=(numobs-1)*std_dep*std_dep;
         _sdenom1=numobs-1;
      end;
      else do;
         _w1=(_sdep1/_sdenom1)/_freq1;
         _w2=(std_dep**2)/numobs;
         _df=((_w1+_w2)**2)/((_w1**2)/(_freq1-1)+(_w2**2)/(numobs-1));
         _t=abs(_adep1/_freq1-avg_dep)/sqrt(_w1+_w2);
         pvalue=(1-probt(_t,round(_df,1)))*2;
      
	     * If t-test is significant, then output cumulated variables;
         * Otherwise continue cumulating;
         if pvalue<=&pval_col then do;
            * Store current row;
            _rnk2=rank;
            _freq2=numobs;
		    _aindp2=numobs*avg_indp;
            _mindp2=min_indp;
            _mxindp2=max_indp;
            _adep2=numobs*avg_dep;
            if std_dep=. then _sdep2=0;
            else _sdep2=(numobs-1)*std_dep*std_dep;
            _sdenom2=numobs-1;

		    * Switch cumulated variables with current row and output;
            rank=_rnk1;
            numobs=_freq1;
		    avg_indp=_aindp1/_freq1;
            min_indp=_mindp1;
            max_indp=_mxindp1;
            avg_dep=_adep1/_freq1;
            std_dep=sqrt(_sdep1/_sdenom1);
            output;

            * Set first variables to current row;
            _rnk1=_rnk2;
            _freq1=_freq2;
		    _aindp1=_aindp2;
            _mindp1=_mindp2;
            _mxindp1=_mxindp2;
            _adep1=_adep2;
            _sdep1=_sdep2;
            _sdenom1=_sdenom2;
         end;
        	else do; 
       		_rnk1=rank;
      		_freq1=_freq1+numobs;
       		_aindp1=_aindp1+numobs*avg_indp;
      		_mxindp1=max_indp;
      		_adep1=_adep1+numobs*avg_dep;
     		if std_dep ne . then _sdep1=_sdep1+(numobs-1)*std_dep*std_dep;
     		_sdenom1=_sdenom1+numobs-1;
  		end;


         *end;

         * If last row for current variable, then output;
         if last.v1 then do;
           rank=_rnk1;
           numobs=_freq1;
           avg_indp=_aindp1/_freq1;
           min_indp=_mindp1;
           max_indp=_mxindp1;
           avg_dep=_adep1/_freq1;
           std_dep=sqrt(_sdep1/_sdenom1);
           output;
         end;
      end;
   end;
   drop _finit _rnk1 _rnk2 _freq1 _freq2 _aindp1 _aindp2
        _mindp1 _mindp2 _mxindp1 _mxindp2 _adep1 _adep2
        _sdep1 _sdep2 _sdenom1 _sdenom2 _w1 _w2 _df _t /*pvalue*/;
run;

Proc Print data=temp;
Run;

%MEND;



************************************************************************************;
************************DISC 2 For Non-Discretized Variables***************************;
************************************************************************************;
************************************************************************************;
*Disc 2. Use the final dataset from the end of the disc1 step as the beginning datafile 
to begin disc2. Also, do disc1 on all variables used in the final VIF stage in clustering;

***************************************************************;
%disc2(pval_col = 0.10,var1=PRMINQS,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2PRMINQS;
Merge ranked1 temp;
by rank;
Run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data disc2PRMINQS;
set disc2PRMINQS;
if rank = 0 then ordeqPRMINQS = 1;
else if rank = 1 then ordeqPRMINQS = 2;
else if rank = 2 then ordeqPRMINQS = 3;
else if rank = 3 then ordeqPRMINQS = 4;
else if rank = 4 then ordeqPRMINQS = 5;
else if rank = 5 then ordeqPRMINQS = 6;
else if rank = 6 then ordeqPRMINQS = 7;
else if rank = 7 then ordeqPRMINQS = 8;
else if rank = 8 then ordeqPRMINQS = 9;
else if rank = 9 then ordeqPRMINQS = 10;
else rank = rank;
oddseqPRMINQS  = (avg_dep)/(1-avg_dep);
loddseqPRMINQS = log ((avg_dep)/(1-avg_dep));
testeqPRMINQS = (1-avg_dep)/(avg_dep);
run;

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set disc2PRMINQS;
Run;
Proc freq data=modelingfile;
table  ordeqPRMINQS oddseqPRMINQS loddseqPRMINQS;
Run;
Proc Means data = modelingfile ;
class ordeqPRMINQS;
var PRMINQS;
run;


***************************************************************;
%disc2(pval_col = 0.10,var1=TOPEN12,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 2;
else if rank = 1 then rank = 2;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 4;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 6;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2TOPEN12;
Merge ranked1 temp;
by rank;
Run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2TOPEN12;
if rank = 2 then ordeqTOPEN12 = 1;
if rank = 4 then ordeqTOPEN12 = 2;
if rank = 6 then ordeqTOPEN12 = 3;
if rank = 7 then ordeqTOPEN12 = 4;
if rank = 8 then ordeqTOPEN12 = 5;
if rank = 9 then ordeqTOPEN12 = 6;
else rank = rank;
oddseqTOPEN12  = (avg_dep)/(1-avg_dep);
loddseqTOPEN12 = log ((avg_dep)/(1-avg_dep));
testeqTOPEN12 = (1-avg_dep)/(avg_dep);
run;

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqTOPEN12 oddseqTOPEN12 loddseqTOPEN12;
Run;
Proc Means data = modelingfile ;
class ordeqTOPEN12;
var TOPEN12;
run;


***************************************************************;
%disc2(pval_col = 0.10,var1=LAAGE,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;


*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 7;
else if rank = 1 then rank = 7;
else if rank = 2 then rank = 7;
else if rank = 3 then rank = 7;
else if rank = 4 then rank = 7;
else if rank = 5 then rank = 7;
else if rank = 6 then rank = 7;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 9;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2LAAGE;
Merge ranked1 temp;
by rank;
Run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2LAAGE;
if rank = 7 then ordeqLAAGE = 1;
if rank = 9 then ordeqLAAGE = 2;
else rank = rank;
oddseqLAAGE  = (avg_dep)/(1-avg_dep);
loddseqLAAGE = log ((avg_dep)/(1-avg_dep));
testeqLAAGE = (1-avg_dep)/(avg_dep);
run;

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqLAAGE oddseqLAAGE loddseqLAAGE;
Run;
Proc Means data = modelingfile ;
class ordeqLAAGE;
var LAAGE;
run;



***************************************************************;
%disc2(pval_col = 0.10,var1=BRAGE,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2BRAGE;
Merge ranked1 temp;
by rank;
Run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2BRAGE;
if rank = 0 then ordeqBRAGE = 1;
if rank = 1 then ordeqBRAGE = 2;
if rank = 2 then ordeqBRAGE = 3;
if rank = 3 then ordeqBRAGE = 4;
if rank = 4 then ordeqBRAGE = 5;
if rank = 5 then ordeqBRAGE = 6;
if rank = 6 then ordeqBRAGE = 7;
if rank = 7 then ordeqBRAGE = 8;
if rank = 8 then ordeqBRAGE = 9;
if rank = 9 then ordeqBRAGE = 10;
else rank = rank;
oddseqBRAGE  = (avg_dep)/(1-avg_dep);
loddseqBRAGE = log ((avg_dep)/(1-avg_dep));
testeqBRAGE = (1-avg_dep)/(avg_dep);
run;

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqBRAGE oddseqBRAGE loddseqBRAGE;
Run;
Proc Means data = modelingfile ;
class ordeqBRAGE;
var BRAGE;
run;



***************************************************************;
%disc2(pval_col = 0.10,var1=DCLAAGE,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 1;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 9;
else if rank = 3 then rank = 9;
else if rank = 4 then rank = 9;
else if rank = 5 then rank = 9;
else if rank = 6 then rank = 9;
else if rank = 7 then rank = 9;
else if rank = 8 then rank = 9;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2DCLAAGE;
Merge ranked1 temp;
by rank;
Run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2DCLAAGE;
if rank = 1 then ordeqDCLAAGE = 1;
if rank = 9 then ordeqDCLAAGE = 2;
else rank = rank;
oddseqDCLAAGE  = (avg_dep)/(1-avg_dep);
loddseqDCLAAGE = log ((avg_dep)/(1-avg_dep));
testeqDCLAAGE = (1-avg_dep)/(avg_dep);
run;

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqDCLAAGE oddseqDCLAAGE loddseqDCLAAGE;
Run;
Proc Means data = modelingfile ;
class ordeqDCLAAGE;
var DCLAAGE;
run;


***************************************************************;
%disc2(pval_col = 0.10,var1=TPOPEN,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2TPOPEN;
Merge ranked1 temp;
by rank;
Run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2TPOPEN;
if rank = 0 then ordeqTPOPEN = 1;
if rank = 1 then ordeqTPOPEN = 2;
if rank = 2 then ordeqTPOPEN = 3;
if rank = 3 then ordeqTPOPEN = 4;
if rank = 4 then ordeqTPOPEN = 5;
if rank = 5 then ordeqTPOPEN = 6;
if rank = 6 then ordeqTPOPEN = 7;
if rank = 7 then ordeqTPOPEN = 8;
if rank = 8 then ordeqTPOPEN = 9;
if rank = 9 then ordeqTPOPEN = 10;
else rank = rank;
oddseqTPOPEN  = (avg_dep)/(1-avg_dep);
loddseqTPOPEN = log ((avg_dep)/(1-avg_dep));
testeqTPOPEN = (1-avg_dep)/(avg_dep);
run;

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqTPOPEN oddseqTPOPEN loddseqTPOPEN;
Run;
Proc Means data = modelingfile ;
class ordeqTPOPEN;
var TPOPEN;
run;



***************************************************************;
%disc2(pval_col = 0.10,var1=BRPOPEN,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 9;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2BRPOPEN;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2BRPOPEN;
if rank = 0 then ordeqBRPOPEN = 1;
if rank = 1 then ordeqBRPOPEN = 2;
if rank = 2 then ordeqBRPOPEN = 3;
if rank = 3 then ordeqBRPOPEN = 4;
if rank = 4 then ordeqBRPOPEN = 5;
if rank = 5 then ordeqBRPOPEN = 6;
if rank = 6 then ordeqBRPOPEN = 7;
if rank = 7 then ordeqBRPOPEN = 8;
if rank = 9 then ordeqBRPOPEN = 9;
else rank = rank;
oddseqBRPOPEN  = (avg_dep)/(1-avg_dep);
loddseqBRPOPEN = log ((avg_dep)/(1-avg_dep));
testeqBRPOPEN = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqBRPOPEN oddseqBRPOPEN loddseqBRPOPEN;
Run;
Proc Means data = modelingfile ;
class ordeqBRPOPEN;
var BRPOPEN;
run;




***************************************************************;
%disc2(pval_col = 0.10,var1=BNKINQ2,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 1;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 6;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2BNKINQ2;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2BNKINQ2;
if rank = 1 then ordeqBNKINQ2 = 1;
if rank = 2 then ordeqBNKINQ2 = 2;
if rank = 3 then ordeqBNKINQ2 = 3;
if rank = 4 then ordeqBNKINQ2 = 4;
if rank = 6 then ordeqBNKINQ2 = 5;
if rank = 7 then ordeqBNKINQ2 = 6;
if rank = 8 then ordeqBNKINQ2 = 7;
if rank = 9 then ordeqBNKINQ2 = 8;
else rank = rank;
oddseqBNKINQ2  = (avg_dep)/(1-avg_dep);
loddseqBNKINQ2 = log ((avg_dep)/(1-avg_dep));
testeqBNKINQ2 = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqBNKINQ2 oddseqBNKINQ2 loddseqBNKINQ2;
Run;
Proc Means data = modelingfile ;
class ordeqBNKINQ2;
var BNKINQ2;
run;




***************************************************************;
%disc2(pval_col = 0.10,var1=TCR1BAL,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2TCR1BAL;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2TCR1BAL;
if rank = 0 then ordeqTCR1BAL = 1;
if rank = 1 then ordeqTCR1BAL = 2;
if rank = 2 then ordeqTCR1BAL = 3;
if rank = 3 then ordeqTCR1BAL = 4;
if rank = 4 then ordeqTCR1BAL = 5;
if rank = 5 then ordeqTCR1BAL = 6;
if rank = 6 then ordeqTCR1BAL = 7;
if rank = 7 then ordeqTCR1BAL = 8;
if rank = 8 then ordeqTCR1BAL = 9;
if rank = 9 then ordeqTCR1BAL = 10;
else rank = rank;
oddseqTCR1BAL  = (avg_dep)/(1-avg_dep);
loddseqTCR1BAL = log ((avg_dep)/(1-avg_dep));
testeqTCR1BAL = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqTCR1BAL oddseqTCR1BAL loddseqTCR1BAL;
Run;
Proc Means data = modelingfile ;
class ordeqTCR1BAL;
var TCR1BAL;
run;



***************************************************************;
%disc2(pval_col = 0.10,var1=TRCR49,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 4;
else if rank = 1 then rank = 4;
else if rank = 2 then rank = 4;
else if rank = 3 then rank = 4;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 6;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2TRCR49;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2TRCR49;
if rank = 4 then ordeqTRCR49 = 1;
if rank = 6 then ordeqTRCR49 = 2;
if rank = 7 then ordeqTRCR49 = 3;
if rank = 8 then ordeqTRCR49 = 4;
if rank = 9 then ordeqTRCR49 = 5;
else rank = rank;
oddseqTRCR49  = (avg_dep)/(1-avg_dep);
loddseqTRCR49 = log ((avg_dep)/(1-avg_dep));
testeqTRCR49 = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqTRCR49 oddseqTRCR49 loddseqTRCR49;
Run;
Proc Means data = modelingfile ;
class ordeqTRCR49;
var TRCR49;
run;


***************************************************************;
%disc2(pval_col = 0.10,var1=OBRPTAT,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 5;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2OBRPTAT;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2OBRPTAT;
if rank = 0 then ordeqOBRPTAT = 1;
if rank = 1 then ordeqOBRPTAT = 2;
if rank = 2 then ordeqOBRPTAT = 3;
if rank = 3 then ordeqOBRPTAT = 4;
if rank = 5 then ordeqOBRPTAT = 5;
if rank = 6 then ordeqOBRPTAT = 6;
if rank = 7 then ordeqOBRPTAT = 7;
if rank = 8 then ordeqOBRPTAT = 8;
if rank = 9 then ordeqOBRPTAT = 9;
else rank = rank;
oddseqOBRPTAT  = (avg_dep)/(1-avg_dep);
loddseqOBRPTAT = log ((avg_dep)/(1-avg_dep));
testeqOBRPTAT = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqOBRPTAT oddseqOBRPTAT loddseqOBRPTAT;
Run;
Proc Means data = modelingfile ;
class ordeqOBRPTAT;
var OBRPTAT;
run;


***************************************************************;
%disc2(pval_col = 0.10,var1=OT24PTOT,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2OT24PTOT;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2OT24PTOT;
if rank = 0 then ordeqOT24PTOT = 1;
if rank = 1 then ordeqOT24PTOT = 2;
if rank = 2 then ordeqOT24PTOT = 3;
if rank = 3 then ordeqOT24PTOT = 4;
if rank = 4 then ordeqOT24PTOT = 5;
if rank = 5 then ordeqOT24PTOT = 6;
if rank = 6 then ordeqOT24PTOT = 7;
if rank = 7 then ordeqOT24PTOT = 8;
if rank = 8 then ordeqOT24PTOT = 9;
if rank = 9 then ordeqOT24PTOT = 10;
else rank = rank;
oddseqOT24PTOT  = (avg_dep)/(1-avg_dep);
loddseqOT24PTOT = log ((avg_dep)/(1-avg_dep));
testeqOT24PTOT = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqOT24PTOT oddseqOT24PTOT loddseqOT24PTOT;
Run;
Proc Means data = modelingfile ;
class ordeqOT24PTOT;
var OT24PTOT;
run;



***************************************************************;
%disc2(pval_col = 0.10,var1=OT3PTOT,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 6;
else if rank = 1 then rank = 6;
else if rank = 2 then rank = 6;
else if rank = 3 then rank = 6;
else if rank = 4 then rank = 6;
else if rank = 5 then rank = 6;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2OT3PTOT;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2OT3PTOT;
if rank = 6 then ordeqOT3PTOT = 1;
if rank = 7 then ordeqOT3PTOT = 2;
if rank = 8 then ordeqOT3PTOT = 3;
if rank = 9 then ordeqOT3PTOT = 4;
else rank = rank;
oddseqOT3PTOT  = (avg_dep)/(1-avg_dep);
loddseqOT3PTOT = log ((avg_dep)/(1-avg_dep));
testeqOT3PTOT = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqOT3PTOT oddseqOT3PTOT loddseqOT3PTOT;
Run;
Proc Means data = modelingfile ;
class ordeqOT3PTOT;
var OT3PTOT;
run;




***************************************************************;
%disc2(pval_col = 0.10,var1=RBAL,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2RBAL;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2RBAL;
if rank = 0 then ordeqRBAL = 1;
if rank = 1 then ordeqRBAL = 2;
if rank = 2 then ordeqRBAL = 3;
if rank = 3 then ordeqRBAL = 4;
if rank = 4 then ordeqRBAL = 5;
if rank = 5 then ordeqRBAL = 6;
if rank = 6 then ordeqRBAL = 7;
if rank = 7 then ordeqRBAL = 8;
if rank = 8 then ordeqRBAL = 9;
if rank = 9 then ordeqRBAL = 10;
else rank = rank;
oddseqRBAL  = (avg_dep)/(1-avg_dep);
loddseqRBAL = log ((avg_dep)/(1-avg_dep));
testeqRBAL = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqRBAL oddseqRBAL loddseqRBAL;
Run;
Proc Means data = modelingfile ;
class ordeqRBAL;
var RBAL;
run;


***************************************************************;
%disc2(pval_col = 0.10,var1=RADB6,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2RADB6;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2RADB6;
if rank = 0 then ordeqRADB6 = 1;
if rank = 1 then ordeqRADB6 = 2;
if rank = 2 then ordeqRADB6 = 3;
if rank = 3 then ordeqRADB6 = 4;
if rank = 4 then ordeqRADB6 = 5;
if rank = 5 then ordeqRADB6 = 6;
if rank = 6 then ordeqRADB6 = 7;
if rank = 7 then ordeqRADB6 = 8;
if rank = 8 then ordeqRADB6 = 9;
if rank = 9 then ordeqRADB6 = 10;
else rank = rank;
oddseqRADB6  = (avg_dep)/(1-avg_dep);
loddseqRADB6 = log ((avg_dep)/(1-avg_dep));
testeqRADB6 = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqRADB6 oddseqRADB6 loddseqRADB6;
Run;
Proc Means data = modelingfile ;
class ordeqRADB6;
var RADB6;
run;

***************************************************************;
%disc2(pval_col = 0.10,var1=BRMINB,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2BRMINB;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2BRMINB;
if rank = 0 then ordeqBRMINB = 1;
if rank = 1 then ordeqBRMINB = 2;
if rank = 2 then ordeqBRMINB = 3;
if rank = 3 then ordeqBRMINB = 4;
if rank = 4 then ordeqBRMINB = 5;
if rank = 5 then ordeqBRMINB = 6;
if rank = 6 then ordeqBRMINB = 7;
if rank = 7 then ordeqBRMINB = 8;
if rank = 8 then ordeqBRMINB = 9;
if rank = 9 then ordeqBRMINB = 10;
else rank = rank;
oddseqBRMINB  = (avg_dep)/(1-avg_dep);
loddseqBRMINB = log ((avg_dep)/(1-avg_dep));
testeqBRMINB = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqBRMINB oddseqBRMINB loddseqBRMINB;
Run;
Proc Means data = modelingfile ;
class ordeqBRMINB;
var BRMINB;
run;



***************************************************************;
%disc2(pval_col = 0.10,var1=BRHIC,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2BRHIC;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2BRHIC;
if rank = 0 then ordeqBRHIC = 1;
if rank = 1 then ordeqBRHIC = 2;
if rank = 2 then ordeqBRHIC = 3;
if rank = 3 then ordeqBRHIC = 4;
if rank = 4 then ordeqBRHIC = 5;
if rank = 5 then ordeqBRHIC = 6;
if rank = 6 then ordeqBRHIC = 7;
if rank = 7 then ordeqBRHIC = 8;
if rank = 8 then ordeqBRHIC = 9;
if rank = 9 then ordeqBRHIC = 10;
else rank = rank;
oddseqBRHIC  = (avg_dep)/(1-avg_dep);
loddseqBRHIC = log ((avg_dep)/(1-avg_dep));
testeqBRHIC = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqBRHIC oddseqBRHIC loddseqBRHIC;
Run;
Proc Means data = modelingfile ;
class ordeqBRHIC;
var BRHIC;
run;

***************************************************************;
%disc2(pval_col = 0.10,var1=DCWCRATE,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 1;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 6;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2DCWCRATE;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2DCWCRATE;
if rank = 0 then ordeqDCWCRATE = 1;
if rank = 1 then ordeqDCWCRATE = 2;
if rank = 2 then ordeqDCWCRATE = 3;
if rank = 3 then ordeqDCWCRATE = 4;
if rank = 4 then ordeqDCWCRATE = 5;
if rank = 5 then ordeqDCWCRATE = 6;
if rank = 6 then ordeqDCWCRATE = 7;
if rank = 7 then ordeqDCWCRATE = 8;
if rank = 8 then ordeqDCWCRATE = 9;
if rank = 9 then ordeqDCWCRATE = 10;
else rank = rank;
oddseqDCWCRATE  = (avg_dep)/(1-avg_dep);
loddseqDCWCRATE = log ((avg_dep)/(1-avg_dep));
testeqDCWCRATE = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqDCWCRATE oddseqDCWCRATE loddseqDCWCRATE;
Run;
Proc Means data = modelingfile ;
class ordeqDCWCRATE;
var DCWCRATE;
run;


***************************************************************;
%disc2(pval_col = 0.10,var1=TROPENEX,d1=modelingfile);
Proc Print data=temp;
Run;
Proc sort data=temp;
by rank;
Run;
symbol1 v= square color = black i=join;
Proc gplot data = temp;
plot avg_dep*rank;
run;
quit;

*This code get rid of ranks for that specific variable;
*For Age, we got rid of rank 1 and rank 8;
Data ranked1;
Set ranked;
if rank = 0 then rank = 0;
else if rank = 1 then rank = 2;
else if rank = 2 then rank = 2;
else if rank = 3 then rank = 3;
else if rank = 4 then rank = 4;
else if rank = 5 then rank = 5;
else if rank = 6 then rank = 7;
else if rank = 7 then rank = 7;
else if rank = 8 then rank = 8;
else if rank = 9 then rank = 9;
else rank = rank;
Run;
Proc sort data=ranked1;
by rank;
Run;
Proc sort data=temp;
by rank;
Run;
Proc freq data=ranked1;
tables rank;
run;

Data disc2TROPENEX;
Merge ranked1 temp;
by rank;
run;

*****At this point, you can assign the ranks as 1, 2, 3...;
Data modelingfile;
set disc2TROPENEX;
if rank = 0 then ordeqTROPENEX = 1;
if rank = 2 then ordeqTROPENEX = 2;
if rank = 3 then ordeqTROPENEX = 3;
if rank = 4 then ordeqTROPENEX = 4;
if rank = 5 then ordeqTROPENEX = 5;
if rank = 7 then ordeqTROPENEX = 6;
if rank = 8 then ordeqTROPENEX = 7;
if rank = 9 then ordeqTROPENEX = 8;
else rank = rank;
oddseqTROPENEX  = (avg_dep)/(1-avg_dep);
loddseqTROPENEX = log ((avg_dep)/(1-avg_dep));
testeqTROPENEX = (1-avg_dep)/(avg_dep);

*Note: at this point, there is ALOT of garbage in the file that needs to be removed...;
Data modelingfile (drop = _TYPE_ avg_dep avg_indp max_dep max_indp
min_dep min_indp numobs pvalue rank std_dep std_indp v1);
set modelingfile;
Run;
Proc freq data=modelingfile;
table  ordeqTROPENEX oddseqTROPENEX loddseqTROPENEX;
Run;
Proc Means data = modelingfile ;
class ordeqTROPENEX;
var TROPENEX;
run;

proc export data=modelingfile
    outfile='/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid/full modeling file.csv'
    dbms=csv replace;
run;

proc contents data=modelingfile out=variable_names (keep=Name);
run;

proc print data=variable_names noobs label;
    var Name;
run;



*************************************************************************************************************************;
******************************************************Logistic Regression************************************************;
*************************************************************************************************************************;
FILENAME REFFILE '/gpfs/user_home/os_home_dirs/jsaund21/STAT 4330 SAS Grid/full modeling file.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=csv REPLACE
	OUT=modelingfile;
	GETNAMES=YES;
RUN;


*Sampling your data;
Proc contents data=modelingfile;
run;
Proc sort data=modelingfile out=test1;
by goodbad;
Run;
proc freq data=test1;
	tables goodbad;
Data modelingfile;
set modelingfile;
random = RANUNI(1234321);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid;
Tables goodbad;
Run;
Proc freq data=train;
Tables goodbad;
Run;
data class.train;
set train;
data class.valid;
set valid;
run;
%let inputs = BADPR1
BKPOP
BNKINQ2
BRAGE
BRCRATE2
BRCRATE3
BRCRATE7
BRHIC
BRMINB
BRPOPEN
BRR29P24
BRRATE2
BRRATE3
COLLS
CPAF29
DCCR49
DCLAAGE
DCWCRATE
LAAGE
LOCINQS
OBRPTAT
ORDBNKINQ2
ORDBRAGE
ORDBRHIC
ORDBRMINB
ORDBRPOPEN
ORDDCLAAGE
ORDDCWCRATE
ORDLAAGE
ORDOBRPTAT
ORDOT24PTOT
ORDOT3PTOT
ORDPRMINQS
ORDRADB6
ORDRBAL
ORDTCR1BAL
ORDTOPEN12
ORDTPOPEN
ORDTRCR49
ORDTROPENEX
OT24PTOT
OT3PTOT
PRMINQS
RADB6
RBAL
TCR1BAL
TOPEN12
TOPENB75
TPOPEN
TR4524
TRCR49
TROPENEX
loddsBNKINQ2
loddsBRAGE
loddsBRHIC
loddsBRMINB
loddsBRPOPEN
loddsDCLAAGE
loddsDCWCRATE
loddsLAAGE
loddsOBRPTAT
loddsOT24PTOT
loddsOT3PTOT
loddsPRMINQS
loddsRADB6
loddsRBAL
loddsTCR1BAL
loddsTOPEN12
loddsTPOPEN
loddsTRCR49
loddsTROPENEX
loddseqBNKINQ2
loddseqBRAGE
loddseqBRHIC
loddseqBRMINB
loddseqBRPOPEN
loddseqDCLAAGE
loddseqDCWCRATE
loddseqLAAGE
loddseqOBRPTAT
loddseqOT24PTOT
loddseqOT3PTOT
loddseqPRMINQS
loddseqRADB6
loddseqRBAL
loddseqTCR1BAL
loddseqTOPEN12
loddseqTPOPEN
loddseqTRCR49
loddseqTROPENEX
oddsBNKINQ2
oddsBRAGE
oddsBRHIC
oddsBRMINB
oddsBRPOPEN
oddsDCLAAGE
oddsDCWCRATE
oddsLAAGE
oddsOBRPTAT
oddsOT24PTOT
oddsOT3PTOT
oddsPRMINQS
oddsRADB6
oddsRBAL
oddsTCR1BAL
oddsTOPEN12
oddsTPOPEN
oddsTRCR49
oddsTROPENEX
oddseqBNKINQ2
oddseqBRAGE
oddseqBRHIC
oddseqBRMINB
oddseqBRPOPEN
oddseqDCLAAGE
oddseqDCWCRATE
oddseqLAAGE
oddseqOBRPTAT
oddseqOT24PTOT
oddseqOT3PTOT
oddseqPRMINQS
oddseqRADB6
oddseqRBAL
oddseqTCR1BAL
oddseqTOPEN12
oddseqTPOPEN
oddseqTRCR49
oddseqTROPENEX
ordeqBNKINQ2
ordeqBRAGE
ordeqBRHIC
ordeqBRMINB
ordeqBRPOPEN
ordeqDCLAAGE
ordeqDCWCRATE
ordeqLAAGE
ordeqOBRPTAT
ordeqOT24PTOT
ordeqOT3PTOT
ordeqPRMINQS
ordeqRADB6
ordeqRBAL
ordeqTCR1BAL
ordeqTOPEN12
ordeqTPOPEN
ordeqTRCR49
ordeqTROPENEX
testBNKINQ2
testBRAGE
testBRHIC
testBRMINB
testBRPOPEN
testDCLAAGE
testDCWCRATE
testLAAGE
testOBRPTAT
testOT24PTOT
testOT3PTOT
testPRMINQS
testRADB6
testRBAL
testTCR1BAL
testTOPEN12
testTPOPEN
testTRCR49
testTROPENEX
testeqBNKINQ2
testeqBRAGE
testeqBRHIC
testeqBRMINB
testeqBRPOPEN
testeqDCLAAGE
testeqDCWCRATE
testeqLAAGE
testeqOBRPTAT
testeqOT24PTOT
testeqOT3PTOT
testeqPRMINQS
testeqRADB6
testeqRBAL
testeqTCR1BAL
testeqTOPEN12
testeqTPOPEN
testeqTRCR49
testeqTROPENEX;

proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;

proc logistic inmodel=scoringdata;
      score data=class.valid  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;

****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge 0.22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;

*VIF Assement for Top 12 Predictors (13 predictors shown below);
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24
			  LOCINQS ORDDCWCRATE COLLS TOPENB75 
			  TR4524 loddseqTRCR49 oddseqBRHIC 
			  ordeqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid out=class.score;
Run;

%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24
			  LOCINQS ORDDCWCRATE COLLS TOPENB75 
			  TR4524 loddseqTRCR49 oddseqBRHIC 
			  ordeqBRHIC testeqBRMINB;
proc corr data=class.train;
var &inputs;
run;
proc reg data=class.train;
	model goodbad = &inputs / vif p;
run;


%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24
			  LOCINQS ORDDCWCRATE COLLS TOPENB75 
			  TR4524 loddseqTRCR49 oddseqBRHIC 
			  testeqBRMINB;
proc corr data=class.train;
var &inputs;
run;
proc reg data=class.train;
	model goodbad = &inputs / vif p;
run;
%let inputs = oddseqBRHIC ordeqBRHIC BRR29P24;
proc corr data=class.train;
var &inputs;
run;
*ordeqBRHIC is removed, lower std error, other has higher chi square;


*Sampling your data; *Final Model;
Proc contents data=modelingfile;
run;
Proc sort data=modelingfile out=test1;
by goodbad;
Run;
proc freq data=test1;
	tables goodbad;
Data modelingfile;
set modelingfile;
random = RANUNI(456);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid;
Tables goodbad;
Run;
Proc freq data=train;
Tables goodbad;
Run;
data class.train;
set train;
data class.valid;
set valid;
run;

%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24
			  LOCINQS ORDDCWCRATE COLLS TOPENB75 
			  TR4524 loddseqTRCR49 oddseqBRHIC 
			  testeqBRMINB;			  
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;
proc logistic inmodel=scoringdata;
      score data=class.valid  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge 0.22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;




*********************************************************************************************************************;
***********************************************Validation Of Training Model******************************************;
*********************************************************************************************************************;
Data modelingfile;
set modelingfile;
random = RANUNI(12321);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid;
Tables goodbad;
Run;
Proc freq data=train;
Tables goodbad;
Run;
data class.train;
set train;
data class.valid;
set valid;
run;

%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24 LOCINQS ORDDCWCRATE TOPEN12 
			  TOPENB75 TR4524 loddseqTRCR49 oddseqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;

proc logistic inmodel=scoringdata;
      score data=class.valid  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;

****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge 0.22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;





Data valid2;
set modelingfile;
random = RANUNI(1234321);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;

Data valid2;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid2;
Tables goodbad;
Run;
data class.valid2;
set valid2;
data class.train;
set train;
run;
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24 LOCINQS ORDDCWCRATE TOPEN12 
			  TOPENB75 TR4524 loddseqTRCR49 oddseqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid2 out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;

proc logistic inmodel=scoringdata;
      score data=class.valid2  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;

proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge .22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;




Data valid3;
set modelingfile;
random = RANUNI(123);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid3;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid3;
Tables goodbad;
Run;
data class.valid3;
set valid3;
data class.train;
set train;
run;
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24 LOCINQS ORDDCWCRATE TOPEN12 
			  TOPENB75 TR4524 loddseqTRCR49 oddseqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid3 out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;
proc logistic inmodel=scoringdata;
      score data=class.valid3  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge .22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;

Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;





Data valid4;
set modelingfile;
random = RANUNI(1234);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid4;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid4;
Tables goodbad;
Run;
data class.valid4;
set valid4;
data class.train;
set train;
run;
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24 LOCINQS ORDDCWCRATE TOPEN12 
			  TOPENB75 TR4524 loddseqTRCR49 oddseqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid4 out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;
proc logistic inmodel=scoringdata;
      score data=class.valid4  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge .22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;




Data valid5;
set modelingfile;
random = RANUNI(12345);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid5;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid5;
Tables goodbad;
Run;
data class.valid5;
set valid5;
data class.train;
set train;
run;
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24 LOCINQS ORDDCWCRATE TOPEN12 
			  TOPENB75 TR4524 loddseqTRCR49 oddseqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid5 out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;
proc logistic inmodel=scoringdata;
      score data=class.valid5  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge .22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;




Data valid6;
set modelingfile;
random = RANUNI(123456);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid6;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid6;
Tables goodbad;
Run;
data class.valid6;
set valid6;
data class.train;
set train;
run;
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24 LOCINQS ORDDCWCRATE TOPEN12 
			  TOPENB75 TR4524 loddseqTRCR49 oddseqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid6 out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;
proc logistic inmodel=scoringdata;
      score data=class.valid6  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge .22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;

Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;




Data valid7;
set modelingfile;
random = RANUNI(1234567);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid7;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid7;
Tables goodbad;
Run;
data class.valid7;
set valid7;
data class.train;
set train;
run;
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24 LOCINQS ORDDCWCRATE TOPEN12 
			  TOPENB75 TR4524 loddseqTRCR49 oddseqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid7 out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;
proc logistic inmodel=scoringdata;
      score data=class.valid7  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;

proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge .22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;



Data valid8;
set modelingfile;
random = RANUNI(12345678);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid8;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid8;
Tables goodbad;
Run;
data class.valid8;
set valid8;
data class.train;
set train;
run;
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24 LOCINQS ORDDCWCRATE TOPEN12 
			  TOPENB75 TR4524 loddseqTRCR49 oddseqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid8 out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;
proc logistic inmodel=scoringdata;
      score data=class.valid8  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge .22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;



Data valid9;
set modelingfile;
random = RANUNI(123456789);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid9;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid9;
Tables goodbad;
Run;
data class.valid9;
set valid9;
data class.train;
set train;
run;
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24 LOCINQS ORDDCWCRATE TOPEN12 
			  TOPENB75 TR4524 loddseqTRCR49 oddseqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid9 out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;

proc logistic inmodel=scoringdata;
      score data=class.valid9  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge .22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;



Data valid10;
set modelingfile;
random = RANUNI(1234567898);
Run;
Proc means data=modelingfile;
Var random;
Run;
Data train;
Set modelingfile;
where random < .60;
Run;
Data valid10;
set modelingfile;
where random >=.60;
Run;
Proc freq data=valid10;
Tables goodbad;
Run;
data class.valid10;
set valid10;
data class.train;
set train;
run;
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24 LOCINQS ORDDCWCRATE TOPEN12 
			  TOPENB75 TR4524 loddseqTRCR49 oddseqBRHIC testeqBRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid10 out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;
proc logistic inmodel=scoringdata;
      score data=class.valid10  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge .22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;



**********************************************************************************************************************;
**********************************Credit limit of defaulting and non-defaulting customers;****************************;
**********************************************************************************************************************;
data defaulting_customers;
    set modelingfile;
    where goodbad = 1;
run;

proc means data=defaulting_customers n mean median min max std;
    var crelim;
run;

data nondefaulting_customers;
    set modelingfile;
    where goodbad = 0;
run;

proc means data=nondefaulting_customers n mean median min max std;
    var crelim;
run;


**********************************************************************************************************************;
*****************************************************Cost of Simplicity***********************************************;
**********************************************************************************************************************;
%let inputs = BRCRATE2 BRCRATE3 BRCRATE7 BRR29P24
			  LOCINQS DCWCRATE COLLS TOPENB75 
			  TR4524 TRCR49 BRHIC 
			  BRMINB;
proc freq data=class.train;
	tables goodbad;
Proc Logistic data = class.train des outest = betas outmodel=scoringdata;
model GOODBAD = &inputs/selection = backward 
 CTABLE pprob=(0.17 to 0.19 by .001)
 LACKFIT RISKLIMITS;
output out = output p = predicted;
score data=class.valid out=class.score;
Run;
proc freq data=class.score;
tables F_goodbad*I_goodbad;
run;
proc logistic inmodel=scoringdata;
      score data=class.valid  out=score;
   run;
data score;
set class.score;
if p_1 = . then delete;
run;
Proc Print data = score (obs=50);
var goodbad P_1;
run;
****Print the variables p_1 and p_0 that were created by the score option;
proc contents data=class.score;
Run;
proc print data=class.score (obs=50);
var goodbad p_1 p_0;
where goodbad = 1;
run;
****Set cutoff point at x;
data test;
set class.score;
if P_1 ge .22 then preds = 1;
else preds = 0;
run;
/*proc sort data=probs;*/
/*by GOODBAD;*/
/*RUN;*/
/*******Create table with ^ pred values, no relation to profit but interesting;*/
/**/
proc freq data=test;
table GOODBAD*preds/norow nocol;
run;
data probs1;
set test(keep=preds GOODBAD crelim);
crelim2 = crelim/2;
if preds = 1 AND GOODBAD = 0 then outcometype = "ERROR2";
else if preds = 0 AND GOODBAD = 1 then outcometype = "ERROR1";
else if preds =1 AND GOODBAD=1 then outcometype = "VALID1";
else outcometype = "VALID2";
if outcometype = "ERROR1" then profit = -crelim2;
else if outcometype = "ERROR2" then profit = 0;
else if outcometype = "VALID2" then profit = 250;
else if outcometype = "VALID1" then profit = 0;
run;
PROC REPORT DATA= probs1 nowd;
COLUMN outcometype pct n profit pper1000;
DEFINE outcometype /group width  = 8 ;
DEFINE profit /format=dollar15.2;
define pper1000 / computed format=dollar15.2;
/*get the overall number of obs*/
compute before;
   overall=n;
endcomp;
compute pper1000;
pper1000 = (profit.sum/n)*1000;
endcomp;
compute before outcometype;
totaln=n;
endcomp;
compute pct;
pct = (totaln/overall);
if _break_ = '_RBREAK_' then pct= (overall/overall);
endcomp;
rbreak after/summarize dol;
RUN;
quit;
***********************************KS TEST**************************;
****Column 1 - Quantiles;
Proc Rank data=score out = ranked groups =10 ties = high;
ranks rank;
var p_1;
Run;
Proc Freq data=ranked;
Tables Rank*Goodbad/nocol nopercent;
Run;
Proc means data = ranked;
var goodbad;
class rank;
run;
****Columns 2 and 3 - Min Score and Max Score;
Data ranked1;
set ranked;
score = P_1*1000;
Run; 
Proc means data = ranked1 min max;
output out=rankedks;
Var score;
Class Rank;
where rank ne .;
Run;
***Columns 4 - 10 and 13;
Proc Freq data=ranked1;
Tables Rank*goodbad/nopercent norow nocol;
Run;

			  
			  
