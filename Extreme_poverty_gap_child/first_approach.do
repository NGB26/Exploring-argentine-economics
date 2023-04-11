
cd "O:\INFANCIA\BECARIOS\NICOLAS\EDSA 2022\BASES INFANCIA"

use base_infa_apilada_FINAL_17_22_7.2.23.dta, clear

rename *, lower

rename indigencia_odsa_sn_max indigencia

tab auh


tabstat ipcf_22 [aw=pon_sin_elevar], by(indigencia)
tabstat ipcf_22 [aw=pon_sin_elevar], by(auh)

hist ipcf_22 if indigencia==1, by(auh , total) percent

*Sobre indigentes*



kdensity ipcf_22 if auh==0 & indigencia==1,generate(x1  d1)
kdensity ipcf_22 if auh==1 & indigencia==1,generate(x2  d2)

 *
gen zero = 0

twoway rarea d1 zero x1, color("blue%50") ///
    ||  rarea d2 zero x2, color("orange%50") ///
		ytitle("Densidad") ///
		graphregion(color(white)) bgcolor(white) ///
		xtitle("IPCF") ///
	legend(ring(0) pos(2) col(1) order(2 "AUH" 1 "NO AUH"))

	*Sobre Pobres*

kdensity lny if auh==0 & indigencia==1 & encuesta==21,generate(x3  d3)
kdensity lny if auh==1 & indigencia==1 & encuesta==21,generate(x4  d4)


*gen zero = 0

twoway rarea d3 zero x3, color("blue%50") ///
    ||  rarea d4 zero x4, color("orange%50") ///
		ytitle("Densidad") ///
		graphregion(color(white)) bgcolor(white) ///
		xtitle("IPCF") ///
	legend(ring(0) pos(2) col(1) order(2 "AUH" 1 "NO AUH"))


gen lny=ln(ipcf_17_22)

qreg lny auh sexo edad i.est_clase4 i.agl_urban_4 i.encuesta [pw=pon_sin_elevar] if indigencia==1, q(0.25)
estimates store qreg_indi
qreg lny auh sexo edad i.est_clase4 i.agl_urban_4 i.encuesta [pw=pon_sin_elevar] if indigencia==0, q(0.25)
estimate store qreg_noindi

estimates table qreg_indi qreg_noindi, star 

tab est_clase4, gen(est_)
tab agl_urban_4, gen(agl_)
tab encuesta, gen(year_)

global estratos est_1 est_2 est_3 est_4 
global agl agl_1 agl_2 agl_3 agl_4 
global year year_1 year_2 year_3 year_4 year_5 


qreg lny auh sexo edad est_1 est_2 agl_1 agl_2 agl_3 year_2 year_3 year_4  [pw=pon_sin_elevar] if indigencia==1, q(0.5)
grqreg auh sexo edad, ci ols olsci

qreg lny auh sexo edad est_1 est_2 agl_1 agl_2 agl_3 year_2 year_3 year_4  [pw=pon_sin_elevar] if indigencia==0, q(0.5)
grqreg auh sexo edad, ci ols olsci

estimates store qreg_indi
qreg lny auh sexo edad i.est_clase4 i.agl_urban_4 i.encuesta [pw=pon_sin_elevar] if indigencia==0, q(0.25)
estimate store qreg_noindi


reg lny auh sexo edad i.est_clase4 i.agl_urban_4 i.encuesta [pw=pon_sin_elevar] if indigencia==1, r
estimates store ols_indi
reg lny auh sexo edad i.est_clase4 i.agl_urban_4 i.encuesta [pw=pon_sin_elevar] if indigencia==0, r
estimate store ols_noindi

estimates table ols_indi ols_noindi, star


bys indigencia: tabstat ipcf_22 [aw=pon_sin_elevar], stat(sd cv mean p10 p25 p50 p75 p90 p95) by(auh) 


