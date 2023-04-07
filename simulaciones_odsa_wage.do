

rename*, lower

gen lny=ln(ing_lab_2021)

gen age=edad_entrev
gen age2=age^2
gen educa=educacio_entrev
gen sexo=0
replace sexo=1 if sexo_entrev==2


sum lny,d


reg lny sexo age age2 educa [w=pon_respondente_sin_elevar] if encuesta==16,r
predict y_16 if encuesta==16 & ing_lab_2021>0
ereturn list
matrix define b_1=e(b)

scalar define alfa_0=b_1[1,5]

scalar define alfa_1=b_1[1,1]

scalar define alfa_2=b_1[1,2]

scalar define alfa_3=b_1[1,3]

scalar define alfa_4=b_1[1,4]

scalar list _all

predict y_19_s if encuesta==19

estimate store reg16

reg lny sexo age age2 educa [w=pon_respondente_sin_elevar] if encuesta==17,r
estimate store reg17
reg lny sexo age age2 educa [w=pon_respondente_sin_elevar] if encuesta==18,r
estimate store reg18
reg lny sexo age age2 educa [w=pon_respondente_sin_elevar] if encuesta==19,r
estimate store reg19
predict y_19 if encuesta==19
reg lny sexo age age2 educa [w=pon_respondente_sin_elevar] if encuesta==20,r
estimate store reg20
predict y_20 if encuesta==20 & ing_lab_2021>0


ereturn list
matrix define b_2=e(b)

scalar define alfa_02=b_2[1,5]

scalar define alfa_12=b_2[1,1]

scalar define alfa_22=b_2[1,2]

scalar define alfa_32=b_2[1,3]

scalar define alfa_42=b_2[1,4]

gen y_20s= alfa_02+alfa_12*sexo+age*alfa_22+age2*alfa_32+alfa_4*educa if ing_lab_2021>0

kdensity y_20s if encuesta==20, addplot(kdensity y_20 if encuesta==20, legend("Distribución simulada 2020")) legend("Distribución estimada 2020")



estimate table reg*, star stat(r2)


kdensity y_20 if encuesta==20, gen(x1 d1)
kdensity y_20s if encuesta==20, gen(x2 d2)
kdensity lny if encuesta==20, gen (x3 d3)


. gen zero = 0

twoway rarea d1 zero x1, color("blue%50") ///
    ||  rarea d2 zero x2, color("orange%50") ///
	|| rarea d3 zero x3, color("green%50") ///
		ytitle("Densidad") ///
		graphregion(color(white)) bgcolor(white) ///
		xtitle("Logaritmo de ingresos a precios 2021 ") ///
	legend(ring(0) pos(2) col(1) order(3 "Ingresos reales 2021" 2 "Ingresos simulados 2021" 1 "Ingresos estimados 2021"))

	
twoway rarea d1 zero x1, color("blue%50") ///
    ||  rarea d2 zero x2, color("orange%50") ///
		ytitle("Densidad") ///
		graphregion(color(white)) bgcolor(white) ///
		xtitle("Logaritmo de ingresos a precios 2021 ") ///
	legend(ring(0) pos(2) col(1) order(2 "Ingresos simulados 2021" 1 "Ingresos estimados 2021"))

	
gen y_sim_20=exp(y_20s)
gen y_est_20=exp(y_20)


tabstat y_sim_20 y_est_20 if encuesta==20 & ing_lab_2020>0, stats(mean sd cv)

kdensity y_sim_20 if encuesta==20, gen(x11 d11)
kdensity y_est_20 if encuesta==20, gen(x21 d21)
kdensity ing_lab_2021 if encuesta==20 & ing_lab_2021<200000, gen(x31 d31) 

twoway rarea d11 zero x11, color("blue%50") ///
    ||  rarea d21 zero x21, color("orange%50") ///
	|| rarea d31 zero x31, lcolor("grey") color("none") ///
		ytitle("Densidad") ///
		graphregion(color(white)) bgcolor(white) ///
		xtitle("Ingresos a precios 2021 ") ///
	legend(ring(0) pos(2) col(1) order(1 "Ingresos simulados 2021" 2 "Ingresos estimados 2021" 3 "Real income distribution"))

