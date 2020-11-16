
FILENAME REFFILE '/folders/myfolders/Data_Regression.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.covidreg;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.covidreg; RUN;

data covidedit;
	set covidreg;
	if gender = "male" then gendernum = 0;
	else if gender = "female" then gendernum = 1;
	if age lt 10 then agegroup = 1;
	else if age >= 10 and age < 20 then agegroup = 2;
	else if age >= 20 and age < 30 then agegroup = 3;
	else if age >= 30 and age < 40 then agegroup = 4;
	else if age >= 40 and age < 50 then agegroup = 5;
	else if age >= 50 and age < 60 then agegroup = 6;
	else if age >= 60 and age < 70 then agegroup = 7;
	else if age >= 70 and age < 80 then agegroup = 8;
	else if age >= 80 and age < 90 then agegroup = 9;
	else if age >=90 then agegroup = 10;
	if tweentyfivedaysintohosp = 0 then tweentyfivedaysintohosp = 2;
	if tweentydaysintohosp = 0 then tweentydaysintohosp = 2;
	if fifteendaysintohosp = 0 then fifteendaysintohosp = 2;
	if tendaysintohosp = 0 then tendaysintohosp = 2;
	if fivedaysintohosp = 0 then fivedaysintohosp = 2;
	run;
	
	proc freq data = covidedit;
	*weight tweentyfivedaysintohosp;
	tables gender*tweentyfivedaysintohosp /  relrisk chisq scorout;
	run;
	
	proc export 
  data=covidedit 
  dbms=xlsx 
  outfile="/folders/myfolders/covidedit.xlsx" 
  replace;
run;
	

data covidedit2;
	set covidedit;
	if gender = "female" and age >=80;
	run;
	
	proc print data = covidedit2;
	
data covidedit3;
	set covidedit;
	if gender = "male" and age >=80;
	proc print data = covidedit3;
	
data covidedit4;
	set covidedit;
	if gender = "female" and age <=20;
	run;

proc print data = covidedit4;

data covidedit5;
	set covidedit;
	if tweentyfivedaysintohosp = 3;
	run;

proc print data = covidedit5;

data covidedit6;
	set covidedit;
	if age >=90;
	run;
	
proc print data = covidedit6;


/*	


proc logistic data = covidedit2 descending; * fit simple linear term in x & check for overdispersion;
class  gender/ param=reference ref=first;
model tweentyfivedaysintohosp  = gender agegroup gender*agegroup / link = logit
influence lackfit iplots aggregate scale=none selection = forward slstay=.15 corrb ctable 
clodds=both clparm=both iplots lackfit outroc=roc1 ;
*output out=diag1 reschi=r xbeta=eta  pred = p;

proc logistic data = covidedit3 descending; * fit simple linear term in x & check for overdispersion;
class  gender/ param=reference ref=first;
model tweentyfivedaysintohosp  = gender agegroup gender*agegroup / link = logit
influence lackfit iplots aggregate scale=none selection = forward slstay=.15 corrb ctable 
clodds=both clparm=both iplots lackfit outroc=roc1 ;
*output out=diag1 reschi=r xbeta=eta  pred = p;
	
	


	
	
	
	
	
proc sgplot data = covidedit;
	hbox age / category = gender;
	
proc sgplot data = covidedit;
	vbox  tweentyfivedaysintohosp;	
	
proc print data = covidedit;


proc freq data = covidedit;
	tables tweentyfivedaysintohosp*agegroup/
	chisq plcorr expected measures nocol norow;
	
proc freq data = covidedit;
	tables tweentyfivedaysintohosp*gendernum/
	chisq plcorr expected measures nocol norow;
	
proc freq data = covidedit;
	tables agegroup*gendernum/chisq plcorr expected measures nocol norow;
	*exact chisq;
	
proc corr data = covidedit;
	var age gendernum;
	
proc freq data = covidedit;
	tables tweentydaysintohosp*agegroup/
	chisq plcorr expected measures nocol norow;

proc freq data = covidedit;
	tables tweentydaysintohosp*gender/
	chisq plcorr expected measures nocol norow;
	
proc freq data = covidedit;
	tables fifteendaysintohosp*agegroup/
	chisq plcorr expected measures nocol norow;
	
proc freq data = covidedit;
	tables fifteendaysintohosp*gender/
	chisq plcorr expected measures nocol norow;
	
proc freq data = covidedit;
	tables tendaysintohosp*agegroup/
	chisq plcorr expected measures nocol norow;
	
proc freq data = covidedit;
	tables tendaysintohosp*gender/
	chisq plcorr expected measures nocol norow;
	
proc freq data = covidedit;
	tables fivedaysintohosp*agegroup/
	chisq plcorr expected measures nocol norow;
	
proc freq data = covidedit;
	tables fivedaysintohosp*gender/
	chisq plcorr expected measures nocol norow;
	
proc sgscatter;
	plot gender*agegroup/loess;
*/

proc logistic data = covidedit descending;
class gender/param=reference ref=first;
model tweentyfivedaysintohosp (ref='2') = gender agegroup gender*agegroup /link=glogit
influence selection = backward slstay = .5 lackfit aggregate scale=none;


ods graphics on;	
proc logistic data = covidedit descending; * fit simple linear term in x & check for overdispersion;
class  gender/ param=reference ref=first;
model tweentyfivedaysintohosp (ref = '2')  = gender age gender*age  / link = glogit
aggregate scale=none  slstay=.15  selection = backward
cl lackfit ;
output out=diag1 reschi=r xbeta=eta  pred = p lower = l upper = u;
ods graphics off;
proc print data = rocdata1;

proc print data = diag1;



proc sort data = diag1;
	by gender;
proc sgplot data=diag1;
title1 "Predicted probabilities";
by gender;
scatter x=age y=p/ group = _LEVEL_;
yaxis min=0 max=1;
run;

data probtable1 (keep=agegroup gender response_level probability);
	set diag1;
	format probability percent7.2;
	rename _LEVEL_ = response_level;
	probability = p;
	
proc sort data = probtable1 noduprecs;
	by agegroup response_level;

proc print data = probtable1;


proc sort data = probtable1;
by gender;
proc means data = probtable1 n mean max min range ;
by gender;
class response_level;
var probability;


proc logistic data = covidedit descending; * fit simple linear term in x & check for overdispersion;
class  gender/ param=reference ref=first;
model tweentydaysintohosp (ref = '2')  = gender age gender*age / link = glogit
aggregate scale=none selection = backward slentry=.15 slstay=.15
cl  lackfit;
output out=diag2 reschi=r xbeta=eta  pred = p lower = l upper = u;

proc sort diag2;
by gender;

proc sgplot data=diag2;
title1 "Predicted probabilities";
by gender;
scatter x=age y=p / group = _LEVEL_;
yaxis min=0 max=1;
run;

data probtable2 (keep=agegroup gender response_level probability);
	set diag2;
	format probability percent7.2;
	rename _LEVEL_ = response_level;
	probability = p;
	
proc sort data = probtable2 noduprecs;
	by agegroup response_level;

proc print data = probtable2;


proc sort data = probtable2;
by gender;
proc means data = probtable2 n mean max min range ;
by gender;
class response_level;
var probability;




proc logistic data = covidedit descending; * fit simple linear term in x & check for overdispersion;
class  gender/ param=reference ref=first;
model fifteendaysintohosp (ref = '2')  = gender age gender*age / link = glogit
aggregate scale=none selection = backward slstay=.15 
CL lackfit;
output out=diag3 reschi=r xbeta=eta  pred = p;

proc sort data = diag3;
by gender;
proc sgplot data=diag3;
title1 "Predicted probabilities";
by gender;
scatter x=age y=p/ group = _LEVEL_;
yaxis min=0 max=1;
run;

proc print data = diag3;

data probtable3 (keep=agegroup gender response_level probability);
	set diag3;
	format probability percent7.2;
	rename _LEVEL_ = response_level;
	probability = p;
	
proc sort data = probtable3 noduprecs;
	by agegroup response_level;

proc print data = probtable3;


proc sort data = probtable3;
by gender;
proc means data = probtable3 n mean max min range ;
by gender;
class response_level;
var probability;



proc logistic data = covidedit descending; * fit simple linear term in x & check for overdispersion;
class  gender/ param=reference ref=first;
model tendaysintohosp (ref = '2')  = gender agegroup gender*agegroup / link = glogit
 aggregate scale=none selection = backward slentry=.15 slstay=.15 cl lackfit ;
output out=diag4 reschi=r xbeta=eta  pred = p;

*proc print data = diag4;

proc sgplot data=diag4;
title1 "Predicted probabilities";
scatter x=agegroup y=p /group = _LEVEL_;
yaxis min=0 max=1;
run;

data probtable4 (keep=agegroup response_level probability);
	set diag4;
	format probability percent7.2;
	rename _LEVEL_ = response_level;
	probability = p;
	
proc sort data = probtable4 noduprecs;
	by agegroup response_level;

proc print data = probtable4;

proc means data = probtable4 n mean max min range ;
class response_level;
var probability;

proc logistic data = covidedit descending; * fit simple linear term in x & check for overdispersion;
class  gender/ param=reference ref=first;
model fivedaysintohosp (ref = '2')  = gender agegroup gender*agegroup / link = glogit
 aggregate scale=none selection = backward slentry=.15 slstay=.15
clodds= wald clparm= wald  lackfit outroc=roc1 ;
output out=diag5 reschi=r xbeta=eta  pred = p;

proc print data = diag5;

proc sgplot data=diag5;
title1 "Predicted probabilities";
scatter x=agegroup y=p /group = _LEVEL_;
yaxis min=0 max=1;
run;

data probtable5 (keep=agegroup response_level probability);
	set diag5;
	format probability percent7.2;
	rename _LEVEL_ = response_level;
	probability = p;
	
proc sort data = probtable5 noduprecs;
	by agegroup response_level;

proc print data = probtable5;

proc means data = probtable5 n mean max min range ;
class response_level;
var probability;

