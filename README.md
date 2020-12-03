# STATA-do-Files-for-STEP-Survey
NCD Researches, GATS , NNS 


************Wealth quintile
gen wi_flus_elec=cex1a=="Yes"
gen wi_flus_toi=cex1b=="Yes"
gen wi_landph=cex1c=="Yes"
gen wi_tm=cex1d=="Yes"
gen wi_tv=cex1e=="Yes"
gen wi_ref=cex1f=="Yes"
gen wi_pcar=cex1g=="Yes"
gen wi_mst=cex1h=="Yes"
gen wi_wash=cex1i=="Yes"
gen wi_bc=cex1j=="Yes"
gen wi_sewi=cex1k=="Yes"
gen wi_almirah=cex1l=="Yes"
gen wi_tc=cex1m=="Yes"
gen wi_kc=cex1n=="Yes" 
gen wi_chib=cex1o=="Yes"
gen wi_watch=cex1p=="Yes"
gen wi_compl=cex1q=="Yes"
gen wi_anidom=cex1r=="Yes"
gen wi_shall=cex1s=="Yes"
gen wi_rick=cex1t=="Yes"
gen wi_cemet=cex2a=="Cement/concrete"
gen wi_katcha=cex2a=="Katcha (bamboo/thatched/straw/gunny)"
gen wi_tin=cex2a=="Tin, Tiles or similar materials"
gen wi_floorcem=cex2b=="Cement"
gen wi_floormud=cex2b=="Mud or sand"
gen wi_wallcem=cex2c=="Cement/concrete"
gen wi_wallkatch=cex2c=="Katcha (bamboo/thatched/straw/gunny)"
gen wi_walltin=cex2c=="Tin, Tiles or similar materials"

factor wi_*, pcf
rotate
predict wealth


xtile quint=wealth , n(5)
la def quint 1 lowest 2 second 3 middle 4 fourth 5 highest

la val quint quint



gen hl8_dist="1" if hl8 =="Dhaka north"
replace hl8_dist="2" if hl8 == "Dhaka south"
replace hl8_dist="3" if hl8 == "Gazipur"
replace hl8_dist="4" if hl8 == "Narsingdi"
destring hl8_dist,replace

label def hl8_dist 1"Dhaka north" 2"Dhaka south" 3"Gazipur" 4"Narsingdi"
la val hl8_dist hl8_dist

****************
xtile wealth_quint_dn =wealth if hl8_dist == 1, n(5)
xtile wealth_quint_ds =wealth if hl8_dist == 2, n(5)
xtile wealth_quint_g =wealth if hl8_dist == 3, n(5)
xtile wealth_quint_n =wealth if hl8_dist == 4, n(5)

gen wealth_quint = .

foreach v of varlist wealth_quint_dn wealth_quint_ds wealth_quint_g wealth_quint_n{
  replace wealth_quint = 1 if `v' == 1
  replace wealth_quint = 2 if `v' == 2
  replace wealth_quint = 3 if `v' == 3
  replace wealth_quint = 4 if `v' == 4
  replace wealth_quint = 5 if `v' == 5
}
///

tab wealth_quint
lab var wealth_quint "Wealth Quintile"
lab def wealth_quint 1 "Poorest" 2 "Poorer" 3 "Middle" 4 "Richer" 5 "Richest", replace
lab val wealth_quint wealth_quint

tab wealth_quint
*****************

**************================ Over all=====
tab wealth_quint [iweight = FinalWeight] if m8sex==1
tab wealth_quint [iweight = FinalWeight] if m8sex==2
tab wealth_quint if m8sex==1
tab wealth_quint if m8sex==2
tab wealth_quint [iweight = FinalWeight]
tab wealth_quint


*********Occupation cat*********


gen occu_cat=c8
replace occu_cat="1" if c8=="Agriculture (day laborer)"
replace occu_cat="1" if c8=="Agriculture (land owner and farmer)"
replace occu_cat="2" if c8=="Business (large)"
replace occu_cat="2" if c8=="Business (small)"
replace occu_cat="3" if c8=="Day laborer"
replace occu_cat="3" if c8=="Industrial laborer"
replace occu_cat="3" if c8=="Transport laborer"
replace occu_cat="4" if c8=="Home maker/Household work"
replace occu_cat="5" if c8=="Government employee"
replace occu_cat="5" if c8=="Non-government employee"
replace occu_cat="6" if c8=="Paid domestic worker (maid servant)"
replace occu_cat="7" if c8=="Students"
replace occu_cat="7" if c8=="Unemployed (able to work)"
replace occu_cat="7" if c8=="Unemployed (unable to work)"
replace occu_cat="7" if c8=="Retired"
replace occu_cat="8" if c8=="Self employed"
replace occu_cat="8" if c8=="Others (Specify)"
replace occu_cat="8" if c8=="Blacksmith/Goldsmith/ Weaver"


destring occu_cat,replace

tab occu_cat

label define occu_cat 1 "Agriculture (day laborer/land owner/farmer)" 2 "Business (small/large)" 3 "Day/Industrial/Transport laborer" 4 "Home maker/Household work" 5 "Government/Non-government employee" 6 "Paid domestic worker (maid servant)" 7 "Retired/Students/Unemployed (able/unable to work)" 8 "Self employed/Blacksmith/Goldsmith/ Weaver/Others"
label values occu_cat occu_cat

//label drop occup_cat

********==========Religion cagtegories==
gen relig_cat=sd2
replace relig_cat="1" if sd2=="Islam"
replace relig_cat="2" if sd2=="Hindu"
replace relig_cat="2" if sd2=="Christian"
replace relig_cat="2" if sd2=="Buddhist"

destring relig_cat, replace

tab relig_cat

label define relig_cat 1 "Muslim" 2 "Others"
label values relig_cat relig_cat
tab relig_cat

***********Education*********************
gen edu_cat=c4
replace edu_cat=0 if c4<1
replace edu_cat=1 if c4>=1 & c4<5
replace edu_cat=2 if c4>=5 & c4<6
replace edu_cat=3 if c4>=6 & c4<10
replace edu_cat=4 if c4>=10 & c4<.
tab edu_cat



label define edu_cat 0 "No education" 1 "Primary incomplete" 2 "Primary complete" 3 "Secondary incomplete" 4 "Secondary and higher education"
label values edu_cat edu_cat
tab edu_cat
*******************Income
gen income_cat=c10

replace income_cat=1 if c10 <=10000
replace income_cat=2 if c10>10000 & c10<=20000
replace income_cat=3 if c10>20000 & c10<=30000
replace income_cat=4 if c10>30000 & c10<.


label define income_cat 1 "0-10000" 2 "10001-20000" 3 "20001-30000" 4 ">30000"
label values income_cat income_cat

tab income_cat



*************************************

*********************************************BODY MASS INDEX****
gen ht1 = m11a 
gen ht2 = m11b
gen ht3 = m11c
 
*selecting two close observations from three observations (if diff between first two measur. were >0.5)
gen ht = (ht1 + ht2)/2 if missing(ht3)
gen ht_min  = min(ht1, ht2, ht3)
gen ht_max  = max(ht1, ht2, ht3)
gen ht_med = (ht1 + ht2 + ht3) - ht_max - ht_min
replace ht = (ht1 + ht2 + ht3)/3 if missing(ht) & ((ht_max - ht_med) == (ht_med - ht_min))
replace ht = cond((ht_max - ht_med) < (ht_med - ht_min), (ht_max + ht_med)/2, (ht_med + ht_min)/2) if missing(ht)

gen height_120=ht+120 if ht!=0
gen ht_m=height_120

gen ht_me = ht_m/100

**Weight
gen wt1 = m12a
gen wt2 = m12b
gen wt3 = m12c

*Selecting two close observations from three observations (if diff between first two measur. were >0.1)
gen wt = (wt1 + wt2)/2 if missing(wt3)
gen wt_min  = min(wt1, wt2, wt3)
gen wt_max  = max(wt1, wt2, wt3)
gen wt_med = (wt1 + wt2 + wt3) - wt_max - wt_min
replace wt = (wt1 + wt2 + wt3)/3 if missing(wt) & ((wt_max - wt_med) == (wt_med - wt_min))
replace wt = cond((wt_max - wt_med) < (wt_med - wt_min), (wt_max + wt_med)/2, (wt_med + wt_min)/2) if missing(wt)



gen bmi_b = wt/ht_me^2

gen bmi_asian_b=bmi_b
replace bmi_asian_b=0 if bmi_b<18.50
replace bmi_asian_b=1 if bmi_b>=18.50 & bmi_b<23
replace bmi_asian_b=2 if bmi_b>=23 & bmi_b<27.5
replace bmi_asian_b=2 if bmi_b>=27.5 & bmi_b!=.

lab var bmi_asian_b "Body Mass Index"
label define bmi_asian_b 0 "Underweight" 1 "Normal" 2 "Overweight" 3"Obese"
label values bmi_asian_b bmi_asian_b

rename bmi_asian_b bmi_cat
tab bmi_cat

************

****** smoking and tobacco ************
gen tobacco=1 if t1=="Yes" & t12!="Yes"
tab tobacco, m
replace tobacco=2 if t12=="Yes" & t1!="Yes"
replace tobacco=3 if t1=="yes" & t12=="Yes"
tab tobacco, m
tab t1
tab t12, m
br t1 t12 tobacco

gen tob=1 if t1=="Yes" & t12=="Yes"
recode tob (1=3)
replace tob=1 if t1=="Yes" & t12!="Yes"
replace tob=2 if t1!="Yes" & t12=="Yes"
tab tob, m
tab t1, m
br t1 t12 tob
br t1 t12 tob if t1=="No" & t12=="No"
drop tobacco
rename tob tobacco
tab tobacco, m
label define tobaccolabel 1 "smoking" 2 "smokless" 3 "both"
label values tobacco tobaccolabel
tab tobacco, m

tab tobacco [iw=FinalWeight], m

tab tobacco [iweight = FinalWeight] if hl8=="Narsingdi" & m8sex==2,m
tab tobacco [iweight = FinalWeight] if hl8=="Narsingdi" & m8sex==1,col m
tab tobacco [iweight = FinalWeight] if m8sex==1,col m
tab tobacco [iweight = FinalWeight] if m8sex==2,col m
tab tobacco [iweight = FinalWeight],col m

****additional tobacco cats***
codebook tobacco
recode tobacco (.=4)
recode tobacco (1 2 3 = 1 "Tobacco Users") (4 = 0 "Non User"), gen (tobacco_all)




 
*** Hypertension (SBP >= 140 / DBP >= 90 / Self-reported) ***********************************************HTN********************

** Systolic  BP_Measured 
gen sbp1 = m4b
gen sbp2 = m5b
gen sbp3 = m6b

gen SBP = (sbp1 + sbp2)/2 if missing(sbp3)
gen sbp_min  = min(sbp1, sbp2, sbp3)
gen sbp_max  = max(sbp1, sbp2, sbp3)
gen sbp_med = (sbp1 + sbp2 + sbp3) - sbp_max - sbp_min
replace SBP = (sbp1 + sbp2 + sbp3)/3 if missing(SBP) & ((sbp_max - sbp_med) == (sbp_med - sbp_min))
replace SBP = cond((sbp_max - sbp_med) < (sbp_med - sbp_min), (sbp_max + sbp_med)/2, (sbp_med + sbp_min)/2) if missing(SBP)

replace SBP = . if SBP == 0


label var SBP "Systolic Blood Pressure, mmHg"
  
 ** Diastolic BP _Measured
gen dbp1 = m4c
gen dbp2 = m5c
gen dbp3 = m6c

gen DBP = (dbp1 + dbp2)/2 if missing(dbp3)
gen dbp_min  = min(dbp1, dbp2, dbp3)
gen dbp_max  = max(dbp1, dbp2, dbp3)
gen dbp_med = (dbp1 + dbp2 + dbp3) - dbp_max - dbp_min
replace DBP = (dbp1 + dbp2 + dbp3)/3 if missing(DBP) & ((dbp_max - dbp_med) == (dbp_med - dbp_min))
replace DBP = cond((dbp_max - dbp_med) < (dbp_med - dbp_min), (dbp_max + dbp_med)/2, (dbp_med + dbp_min)/2) if missing(DBP)

replace DBP = . if DBP == 0
replace DBP = . if DBP < 40

label var DBP "Diastolic Blood Pressure, mmHg"



************phycical activities======
tab p1
tab p2
gen p2_cat=p2
replace p2_cat=. if p2==77


*******fruit and vegetable servings************

tab d1
tab d2
gen d2_cat=d2
replace d2_cat=. if d2==77
replace d2_cat=. if d2==77.7

gen d4_cat=d4
replace d4_cat=. if d4==77
replace d4_cat=. if d4==77.7


gen d1_fruit=(d1*d2_cat)/7
gen d3_veg=(d3*d4_cat)/7

egen fruit_veg= rsum(d1_fruit d3_veg)
replace fruit_veg=. if fruit_veg==0
gen fruitveg_cat= fruit_veg


replace fruitveg_cat = 0 if fruit_veg >= 5 & fruit_veg != . 
replace fruitveg_cat = 1 if fruit_veg < 5 & fruit_veg != . 


tab fruitveg_cat

lab var fruitveg_cat "Fruits and vegetables consumption" 
lab def fruitveg_cat 0 ">=5 servings" 1 "<5 servings"
lab values fruitveg_cat fruitveg_cat 

tab fruitveg_cat


***********************
gen dx1_cat=dx1
replace dx1_cat=. if dx1==77
replace dx1_cat=. if dx1==77.7
replace dx1_cat=. if dx1==77.77
replace dx1_cat=. if dx1==99.9
replace dx1_cat=30 if dx1==300
replace dx1_cat=36 if dx1==360


***************Salat intake
gen dx3_cat=dx3
replace dx3_cat=. if dx3==77
replace dx3_cat=. if dx3==77.7
replace dx3_cat=. if dx3==77.77
replace dx3_cat=. if dx3==777
replace dx3_cat=. if dx3==99.9



codebook hl8 hl9
recode hl8 (1 2 = 1 "Urban") (3 4 = 2 "Rural), gen (residence)
recode hl8 (1 2 = 1 "Urban") (3 4 = 2 "Rural"), gen (residence)
encode hl8, generate(resid)
recode hl8 (1 2 = 1 "Urban") (3 4 = 2 "Rural"), gen (residence)
destring resid, replace float
recast float resid
recode hl8 (1 2 = 1 "Urban") (3 4 = 2 "Rural"), gen (residence)
recode hl8 (1 2 = 1) (3 4 = 2 ), gen (residence)
drop c4==.
drop if c4==.
recode hl8 (1 2 = 1 "Urban") (3 4 = 2 "Rural"), gen (residence)
replace hl8 "1" if hl8=="Dhaka north"
recode hl8_dist (1 2 = 1 "Urban") (3 4 = 2 "Rural"), gen (residence)
describe codebook
codebook
logistic tobacco_all ib4.income_cat ib4.educat ib1. occu_cat ib5.wealth_quint ib2.bmi_cat
logistic tobacco_all ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint ib2.bmi_cat
tab occu_cat, m
logistic tobacco_all ib4.income_cat ib4.edu_cat ib4.occu_cat ib5.wealth_quint ib2.bmi_cat residence
logistic tobacco_all m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint ib2.bmi_cat
logistic tobacco_all m8sex residence ib4.income_cat ib4.edu_cat ib4.occu_cat ib5.wealth_quint ib2.bmi_cat
outreg2 using regression results, replace excel dec(3)
outreg2 using regression_results, replace excel dec(3)
seeout using "regression_results.txt"
search excel package
mlogit tobacco m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint ib2.bmi_cat
mlogit tobacco m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint ib2.bmi_cat,rrr
mlogit tobacco m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint ib2.bmi_cat,rrr
codebook bmi_cat
mlogit tobacco m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint ib1.bmi_cat,rrr
mlogit bmi_cat tobacco m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint,rrr
recode bmi_cat (2 = 0 "fatty") (1 0 = 1 "Underweight"), gen (fatty_cat)
logistic fatty_cat tobacco m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint ib2.bmi_cat
logistic fatty_cat tobacco m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint
logistic tobacco m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint fatty_cat
logistic tobacco m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint
logistic tobacco_all  m8sex residence ib4.income_cat ib4.edu_cat ib1. occu_cat ib5.wealth_quint fatty_cat
tab tobacco_all m8sex
tab tobacco_all m8sex, m
tab tobacco_all m8sex [iweight = FinalWeight], m
tab fatty_cat m8sex [iweight = FinalWeight], m
tab fatty_cat m8sex [iweight = FinalWeight], chi2
tab fatty_cat m8sex, chi2
