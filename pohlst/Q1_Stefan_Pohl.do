use "/Users/stefan/Desktop/FS19/Econometrics II - PhD/PS7/runandjump_sample1500g.dta"

*a)
gen bwtcent=bweight-1500

*b)
gen bin1=bwtcent+0.5
hist bin1, width(1) start(-150) freq

gen modulo10=mod(bwtcent,10)
gen bin2=bwtcent+(5-modulo10)
hist bin2, width(10) start(-150) freq

gen modulo25=mod(bwtcent,25)
gen bin3=bwtcent+(12.5-modulo25)
hist bin3, width(25) start(-150) freq

*d)
*************************
******150 bandwidth******
*************************
***150 bandwidth, 1gr bin
preserve
gen frequency=1
collapse (count) frequency, by(bin1)
*interact bweigth with above cutoff
gen bin1_interact=0
replace bin1_interact=bin1 if bin1<0
gen treatment=0
replace treatment=1 if bin1<0
drop if bin1<-150
drop if bin1>150
***regression***
reg frequency bin1 bin1_interact treatment, robust
predict hat
twoway (line hat bin1 if bin1<0) (line hat bin1 if bin1>=0) (bar frequency bin1, xline(0, lwidth(vthin)))
restore

***150 bandwidth, 10gr bin
preserve
gen frequency=1
collapse (count) frequency, by(bin2)
*interact bweigth with above cutoff
gen bin2_interact=0
replace bin2_interact=bin2 if bin2<0
gen treatment=0
replace treatment=1 if bin2<0
drop if bin2<-150
drop if bin2>150
***regression
reg frequency bin2 bin2_interact treatment, robust
predict hat
twoway (line hat bin2 if bin2<0) (line hat bin2 if bin2>=0) (bar frequency bin2, xline(0, lwidth(vthin)))
restore

***150 bandwidth, 25gr bin
preserve
gen frequency=1
collapse (count) frequency, by(bin3)
*interact bweigth with above cutoff
gen bin3_interact=0
replace bin3_interact=bin3 if bin3<0
gen treatment=0
replace treatment=1 if bin3<0
drop if bin3<-150
drop if bin3>150
***regression
reg frequency bin3 bin3_interact treatment, robust
predict hat
twoway (line hat bin3 if bin3<0) (line hat bin3 if bin3>=0) (bar frequency bin3, xline(0, lwidth(vthin)))
restore

*************************
******100 bandwidth******
*************************
***100 bandwidth, 1gr bin
preserve
gen frequency=1
collapse (count) frequency, by(bin1)
*interact bweigth with above cutoff
gen bin1_interact=0
replace bin1_interact=bin1 if bin1<0
gen treatment=0
replace treatment=1 if bin1<0
drop if bin1<-100
drop if bin1>100
***regression***
reg frequency bin1 bin1_interact treatment, robust
predict hat
twoway (line hat bin1 if bin1<0) (line hat bin1 if bin1>=0) (bar frequency bin1, xline(0, lwidth(vthin)))
restore

***100 bandwidth, 10gr bin
preserve
gen frequency=1
collapse (count) frequency, by(bin2)
*interact bweigth with above cutoff
gen bin2_interact=0
replace bin2_interact=bin2 if bin2<0
gen treatment=0
replace treatment=1 if bin2<0
drop if bin2<-100
drop if bin2>100
***regression***
reg frequency bin2 bin2_interact treatment, robust
predict hat
twoway (line hat bin2 if bin2<0) (line hat bin2 if bin2>=0) (bar frequency bin2, xline(0, lwidth(vthin)))
restore

***100 bandwidth, 25gr bin
preserve
gen frequency=1
collapse (count) frequency, by(bin3)
*interact bweigth with above cutoff
gen bin3_interact=0
replace bin3_interact=bin3 if bin3<0
gen treatment=0
replace treatment=1 if bin3<0
drop if bin3<-100
drop if bin3>100
***regression
reg frequency bin3 bin3_interact treatment, robust
predict hat
twoway (line hat bin3 if bin3<0) (line hat bin3 if bin3>=0) (bar frequency bin3, xline(0, lwidth(vthin)))
restore

************************
******50 bandwidth******
************************
***50 bandwidth, 1gr bin
preserve
gen frequency=1
collapse (count) frequency, by(bin1)
*interact bweigth with above cutoff
gen bin1_interact=0
replace bin1_interact=bin1 if bin1<0
gen treatment=0
replace treatment=1 if bin1<0
drop if bin1<-50
drop if bin1>50
***regression***
reg frequency bin1 bin1_interact treatment, robust
predict hat
twoway (line hat bin1 if bin1<0) (line hat bin1 if bin1>=0) (bar frequency bin1, xline(0, lwidth(vthin)))
restore

***50 bandwidth, 10gr bin
preserve
gen frequency=1
collapse (count) frequency, by(bin2)
*interact bweigth with above cutoff
gen bin2_interact=0
replace bin2_interact=bin2 if bin2<0
gen treatment=0
replace treatment=1 if bin2<0
drop if bin2<-50
drop if bin2>50
***regression***
reg frequency bin2 bin2_interact treatment, robust
predict hat
twoway (line hat bin2 if bin2<0) (line hat bin2 if bin2>=0) (bar frequency bin2, xline(0, lwidth(vthin)))
restore

***50 bandwidth, 25gr bin
preserve
gen frequency=1
collapse (count) frequency, by(bin3)
*interact bweigth with above cutoff
gen bin3_interact=0
replace bin3_interact=bin3 if bin3<0
gen treatment=0
replace treatment=1 if bin3<0
drop if bin3<-50
drop if bin3>50
***regression
reg frequency bin3 bin3_interact treatment, robust
predict hat
twoway (line hat bin3 if bin3<0) (line hat bin3 if bin3>=0) (bar frequency bin3, xline(0, lwidth(vthin)))
restore


*e(discontinutiy in covariates?)
********************************
***Mom white******
***Mom educated***
***i, ii, iii*****
******************

ssc install rd, replace

gen mom_white=0
replace mom_white=1 if mom_race==1
gen treatment=bwtcent<0

rd mom_white bwtcent, z0(0) bwidth(90) kernel(rectangle) cluster(bwtcent)
rd mom_white bwtcent, z0(0) bwidth(60) kernel(rectangle) cluster(bwtcent)
rd mom_white bwtcent, z0(0) bwidth(30) kernel(rectangle) cluster(bwtcent)

rd mom_ed1 bwtcent, z0(0) bwidth(90) kernel(rectangle) cluster(bwtcent)
rd mom_ed1 bwtcent, z0(0) bwidth(60) kernel(rectangle) cluster(bwtcent)
rd mom_ed1 bwtcent, z0(0) bwidth(30) kernel(rectangle) cluster(bwtcent)


***iv, v, vi***********
***triangular kernel***
***(default)***********

rd mom_white bwtcent, z0(0) bwidth(90) cluster(bwtcent)
rd mom_white bwtcent, z0(0) bwidth(60) cluster(bwtcent)
rd mom_white bwtcent, z0(0) bwidth(30) cluster(bwtcent)

rd mom_ed1 bwtcent, z0(0) bwidth(90) cluster(bwtcent)
rd mom_ed1 bwtcent, z0(0) bwidth(60) cluster(bwtcent)
rd mom_ed1 bwtcent, z0(0) bwidth(30) cluster(bwtcent)


*f)
gen ounce_multiple=0
replace ounce_multiple=51 if floor(51*28.3495231)==bweight
replace ounce_multiple=52 if floor(52*28.3495231)==bweight
replace ounce_multiple=53 if floor(53*28.3495231)==bweight
replace ounce_multiple=54 if floor(54*28.3495231)==bweight

replace ounce_multiple=51 if ceil(51*28.3495231)==bweight
replace ounce_multiple=52 if ceil(52*28.3495231)==bweight
replace ounce_multiple=53 if ceil(53*28.3495231)==bweight
replace ounce_multiple=54 if ceil(54*28.3495231)==bweight

gen ounce51_dummy=ounce_multiple==51
gen ounce52_dummy=ounce_multiple==52
gen ounce53_dummy=ounce_multiple==53
gen ounce54_dummy=ounce_multiple==54

***look at jump off trend line at 51, 52, 53, 54 ounces, and 1500gr
***each with bandwidth of 25 and 100, respectively, for mom_white and mom_ed1

****************
***mom_white****
****************

***Bandwidth 25
preserve

drop if bweight<51*28.3495231-25
drop if bweight>51*28.3495231+25
*generate ounce51_interact=0
*replace ounce51_interact=bwtcent if bweight>51*28.3495231
reg mom_white ounce51_dummy bwtcent

restore
preserve

drop if bweight<52*28.3495231-25
drop if bweight>52*28.3495231+25
reg mom_white ounce52_dummy bwtcent

restore
preserve

drop if bweight<53*28.3495231-25
drop if bweight>53*28.3495231+25
drop if bweight==1500
reg mom_white ounce53_dummy bwtcent

restore
preserve

drop if bweight<54*28.3495231-25
drop if bweight>54*28.3495231+25
reg mom_white ounce54_dummy bwtcent

restore
preserve

drop if bweight<1500-25
drop if bweight<1500+25
drop if ounce53_dummy==1
drop if ounce52_dummy==1
gen gr1500_dummy=bweight==1500
reg mom_white gr1500_dummy bwtcent

restore

***Bandwidth 100
***dropping other ounce heaps, and 1500gr observations
preserve
drop if bweight==1500|ounce52_dummy==1|ounce53_dummy==1|ounce54_dummy==1
drop if bweight<51*28.3495231-100
drop if bweight>51*28.3495231+100

reg mom_white ounce51_dummy bwtcent
restore

preserve
drop if bweight==1500|ounce51_dummy==1|ounce53_dummy==1|ounce54_dummy==1
drop if bweight<52*28.3495231-100
drop if bweight>52*28.3495231+100

reg mom_white ounce52_dummy bwtcent
restore

preserve
drop if bweight==1500|ounce52_dummy==1|ounce51_dummy==1|ounce54_dummy==1
drop if bweight<53*28.3495231-100
drop if bweight>53*28.3495231+100

reg mom_white ounce53_dummy bwtcent
restore

preserve
drop if bweight==1500|ounce52_dummy==1|ounce51_dummy==1|ounce53_dummy==1
drop if bweight<54*28.3495231-100
drop if bweight>54*28.3495231+100

reg mom_white ounce54_dummy bwtcent
restore

preserve
gen gr1500_dummy=bweight==1500
drop if ounce54_dummy==1|ounce52_dummy==1|ounce51_dummy==1|ounce53_dummy==1
drop if bweight<1500-100|bweight>1500+100

reg mom_white gr1500_dummy bwtcent
restore


****************
***mom_ed1******
****************

***Bandwidth 25
preserve

drop if bweight<51*28.3495231-25
drop if bweight>51*28.3495231+25
*generate ounce51_interact=0
*replace ounce51_interact=bwtcent if bweight>51*28.3495231
reg mom_ed1 ounce51_dummy bwtcent

restore
preserve

drop if bweight<52*28.3495231-25
drop if bweight>52*28.3495231+25
reg mom_ed1 ounce52_dummy bwtcent

restore
preserve

drop if bweight<53*28.3495231-25
drop if bweight>53*28.3495231+25
drop if bweight==1500
reg mom_ed1 ounce53_dummy bwtcent

restore
preserve

drop if bweight<54*28.3495231-25
drop if bweight>54*28.3495231+25
reg mom_ed1 ounce54_dummy bwtcent

restore
preserve

drop if bweight<1500-25
drop if bweight<1500+25
drop if ounce53_dummy==1
drop if ounce52_dummy==1
gen gr1500_dummy=bweight==1500
reg mom_ed1 gr1500_dummy bwtcent

restore

***Bandwidth 100
***dropping other ounce heaps, and 1500gr observations

preserve
drop if bweight==1500|ounce52_dummy==1|ounce53_dummy==1|ounce54_dummy==1
drop if bweight<51*28.3495231-100
drop if bweight>51*28.3495231+100

reg mom_ed1 ounce51_dummy bwtcent
restore 

preserve
drop if bweight==1500|ounce51_dummy==1|ounce53_dummy==1|ounce54_dummy==1
drop if bweight<52*28.3495231-100
drop if bweight>52*28.3495231+100

reg mom_ed1 ounce52_dummy bwtcent
restore

preserve
drop if bweight==1500|ounce52_dummy==1|ounce51_dummy==1|ounce54_dummy==1
drop if bweight<53*28.3495231-100
drop if bweight>53*28.3495231+100

reg mom_ed1 ounce53_dummy bwtcent
restore
 
preserve
drop if bweight==1500|ounce52_dummy==1|ounce51_dummy==1|ounce53_dummy==1
drop if bweight<54*28.3495231-100
drop if bweight>54*28.3495231+100

reg mom_ed1 ounce54_dummy bwtcent
restore

preserve
gen gr1500_dummy=bweight==1500
drop if ounce54_dummy==1|ounce52_dummy==1|ounce51_dummy==1|ounce53_dummy==1
drop if bweight<1500-100|bweight>1500+100

reg mom_ed1 gr1500_dummy bwtcent
restore


*g)
******Robust*******
***same as in e)***
***rect. kernel****
rd agedth5 bwtcent, z0(0) bwidth(90) kernel(rectangle) robust
rd agedth5 bwtcent, z0(0) bwidth(60) kernel(rectangle) robust
rd agedth5 bwtcent, z0(0) bwidth(30) kernel(rectangle) robust

***triangular kernel
******(default)*****
rd agedth5 bwtcent, z0(0) bwidth(90) robust
rd agedth5 bwtcent, z0(0) bwidth(60) robust
rd agedth5 bwtcent, z0(0) bwidth(30) robust


******Cluster******
***same as in e)***
***rect. kernel****
rd agedth5 bwtcent, z0(0) bwidth(90) kernel(rectangle) robust cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(60) kernel(rectangle) robust cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(30) kernel(rectangle) robust cluster(bwtcent)

***triangular kernel
******(default)*****
rd agedth5 bwtcent, z0(0) bwidth(90) robust cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(60) robust cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(30) robust cluster(bwtcent)


*h)
***drop 1500gr obs.
***repeat exercise g) (same specs.)
preserve
drop if bweight==1500

***Robust***
*same as in e)
*rect. kernel
rd agedth5 bwtcent, z0(0) bwidth(90) kernel(rectangle) robust
rd agedth5 bwtcent, z0(0) bwidth(60) kernel(rectangle) robust
rd agedth5 bwtcent, z0(0) bwidth(30) kernel(rectangle) robust

*triangualr kernel
rd agedth5 bwtcent, z0(0) bwidth(90) robust
rd agedth5 bwtcent, z0(0) bwidth(60) robust
rd agedth5 bwtcent, z0(0) bwidth(30) robust

***Cluster
*same as in e)
*rect. kernel
rd agedth5 bwtcent, z0(0) bwidth(90) kernel(rectangle) robust cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(60) kernel(rectangle) robust cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(30) kernel(rectangle) robust cluster(bwtcent)

***triangular kernel
******(default)*****
rd agedth5 bwtcent, z0(0) bwidth(90) robust cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(60) robust cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(30) robust cluster(bwtcent)

restore

******drop obs. measured in ounces
***repeat exercise g) (same specs.)
preserve
drop if ounce==1

***Robust***
*same as in e)
*rect. kernel
rd agedth5 bwtcent, z0(0) bwidth(90) kernel(rectangle) robust
rd agedth5 bwtcent, z0(0) bwidth(60) kernel(rectangle) robust
rd agedth5 bwtcent, z0(0) bwidth(30) kernel(rectangle) robust

***triangular kernel
rd agedth5 bwtcent, z0(0) bwidth(90) robust
rd agedth5 bwtcent, z0(0) bwidth(60) robust
rd agedth5 bwtcent, z0(0) bwidth(30) robust

***Cluster
*same as in e)
*rect. kernel
rd agedth5 bwtcent, z0(0) bwidth(90) kernel(rectangle) cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(60) kernel(rectangle) cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(30) kernel(rectangle) cluster(bwtcent)

***triangular kernel
******(default)*****
rd agedth5 bwtcent, z0(0) bwidth(90) cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(60) cluster(bwtcent)
rd agedth5 bwtcent, z0(0) bwidth(30) cluster(bwtcent)

*when clustering on bweight, SEs considerably larger. indicates that we should
*cluster on bweight

restore


*i)
*in light of exercise h), I would prefer a specification with clustering on
*bweight. Further, I would use the triangular kernel because whilst considering
*possibly more data points, the fact that it down-weights obs. far away means
*that the bias tend to be smaller.
*Next, I'll use the suggested bandwidth of 60 and compare the results of RD
*with triangular kernel, cluster (bwtcent)
*for both keeping/dropping the obs. of 1500gr

ssc install rdrobust
preserve
drop if bweight==1500
rd agedth5 bwtcent, z0(0) bwidth(60) cluster(bwtcent)
rdplot agedth5 bwtcent, p(1) z0(0) h(60) cluster(bwtcent)
restore
















