
*Brechas indigencia* 

cd "O:\INFANCIA\BECARIOS\NICOLAS\EDSA 2022\BASES INFANCIA"

use base_infa_apilada_FINAL_17_22_7.2.23.dta, clear

rename *, lower


rename indigencia_odsa_sn_max indigencia
rename encuesta year

gen year1 =year+1 
drop year
rename year1 year


drop ieaf_17_22 
gen ieaf_17_22 = ieaf_conest_mi_max_h_17_21 if year<21
replace ieaf_17_22=ieaf_22 if year==21


gen ieaf_p22=ieaf_conest_mi_max_h_c21_17_21 if year<21
replace ieaf_p22=ieaf_22 if year==21


bys year: sum ieaf_17_22


gen brecha_ind=ieaf_17_22/cba_ae
bys year: tabstat brecha_ind [aw=pon_sin_elevar] , by(indigencia) 

gen a=ieaf_17_22<cba_ae
bys year: tab a indigencia 



*Vuelvo a construir indigencia a partir de la Foster, Greer & Thorbecke (FGT) function* 

*Para alpha=0 - count index*
gen bind_a0=.
replace bind_a0=0 if ieaf_17_22>=cba_ae
replace bind_a0=(1-(ieaf_17_22/cba_ae))^0 if ieaf_17_22<cba_ae

*Para alpha=1 - brecha de indigencia* 
gen bind_a1=.
replace bind_a1=0 if ieaf_17_22>=cba_ae
replace bind_a1=((1-(ieaf_17_22/cba_ae))^1) if ieaf_17_22<cba_ae

*Para alpha=2 - severidad de la indigencia* 
gen bind_a2=.
replace bind_a2=0 if ieaf_17_22>=cba_ae
replace bind_a2=(1-(ieaf_17_22/cba_ae))^2 if ieaf_17_22<cba_ae

graph box bind_a1 [aw=pon_sin_elevar] if bind_a1!=0 & bind_a1!=., over(auh)over(year)


*rename year year_1 
*gen year=year_1+1
*set scheme s1mono
forval i = 17/21{
ciplot bind_a1 [aw=pon_sin_elevar] if bind_a1!=0 & bind_a1!=. & year==`i', by(auh) title(Año 20`i') name(g_y_`i', replace) ytitle(" ") note(" ")
graph close _all
}

graph combine g_y_17 g_y_18 g_y_19 g_y_20 g_y_21 g_y_22, ycomm imargin(zero) title("Brechas de indigencia") note("Estimado a partir de la FGT con {&alpha}=1")


*Para intensidad de la indigencia*
forval i = 17/21{
ciplot bind_a2 [aw=pon_sin_elevar] if bind_a2!=0 & bind_a2!=. & year==`i', by(auh) title(Año 20`i') name(g2_y_`i', replace) ytitle(" ") note(" ")
graph close _all
}

graph combine g2_y_17 g2_y_18 g2_y_19 g2_y_20 g2_y_21 g2_y_22, ycomm imargin(zero) title("Severidad de la indigencia") note("Estimado a partir de la FGT con {&alpha}=2")





tab bind_a0 year [aw=pon_sin_elevar], nof col

*collapse bind_a1 bind_a2, by(year auh) 

egen p_auh=mean(auh), by(year)
label var p_auh "Proporción de los NNyA con AUH"


reg bind_a1 auh i.sexo edad i.est_clase4 i.agl_urban_4 i.year [aw=pon_sin_elevar] if indigencia==1,r 
estimate store ols_bindA1


reg bind_a2 auh i.sexo edad i.est_clase4 i.agl_urban_4 i.year [aw=pon_sin_elevar] if indigencia==1,r 
estimate store ols_bindA2

estimates table ols_bindA1 ols_bindA2, star stats(r2 N)


reg bind_a1 auh i.year [aw=pon_sin_elevar] if indigencia==1,r 
estimate store bindA1_m1

reg bind_a1 auh i.sexo i.year [aw=pon_sin_elevar] if indigencia==1,r 
estimate store bindA1_m2

reg bind_a1 auh i.sexo edad i.year [aw=pon_sin_elevar] if indigencia==1,r 
estimate store bindA1_m3

reg bind_a1 auh i.sexo edad i.est_clase4 i.agl_urban_4 i.year [aw=pon_sin_elevar] if indigencia==1,r 
estimate store bindA1_m4

reg bind_a1 auh##i.year i.sexo edad i.est_clase4 i.agl_urban_4 [aw=pon_sin_elevar] if indigencia==1,r 
estimate store bindA1_m5

estimates table bindA1_m1 bindA1_m2 bindA1_m3 bindA1_m4 bindA1_m5, star stats(r2 N)


forval i=17/22{ 
ttest bind_a1, by(auh) une 
}

forval i=17/22{
reg bind_a1 auh [aw=pon_sin_elevar] if year==`i' & indigencia==1, r 
estimate store dif_binda1_`i'
}

estimate table dif_binda1_17 dif_binda1_18 dif_binda1_19 dif_binda1_20 dif_binda1_21 dif_binda1_22, star stat(r2 N)

 forval i=17/22{
reg bind_a1 auh p_auh i.sexo edad i.est_clase4 i.agl_urban_4 [aw=pon_sin_elevar] if indigencia==1 & year==`i' ,r 
estimates store bind_A1_`i'
}

estimate table bind_A1_17 bind_A1_18 bind_A1_19 bind_A1_20 bind_A1_21 bind_A1_22, star stats(r2 N)

egen cant_nn= count(id), by(id)

gen bind_a1_m=bind_a1 if indigencia==1

heckman bind_a1_m auh i.sexo edad i.est_clase4 i.agl_urban_4 [pw=pon_sin_elevar] if year==22, select(indigencia = vinculo_r i.sitocup_odsa_max i.edadgrupo cant_nn i.est_clase4)
outreg2 using heckman1.doc, keep(auh)replace 

reg bind_a1_m auh i.sexo edad i.est_clase4 i.agl_urban_4 [pw=pon_sin_elevar] if year==22,r 
outreg2 using heckman1.doc, keep(auh)

estimates table h_1 r_1, star(0.1 0.05 0.01)



tab indigencia auh [aw=pon_sin_elevar] if year==22, row nof

