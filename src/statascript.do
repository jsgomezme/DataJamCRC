clear
cap log close
set more off
cls

global dir "Escribir ruta del directorio"

********
*POBLACIÓN
********
import excel poblacion.xlsx, sheet("Datos") firstrow clear
save POBLACION.dta, replace


********
*INTERNET FIJO
********

clear

global intfijo "IntFijo2010 IntFijo2011 IntFijo2012 IntFijo2013 IntFijo2014 IntFijo2015 IntFijo2016 IntFijo2017 IntFijo2018 IntFijo2019"
global intfijo2 "IntFijo2011 IntFijo2012 IntFijo2013 IntFijo2014 IntFijo2015 IntFijo2016 IntFijo2017 IntFijo2018 IntFijo2019"

foreach var in $intfijo{

import delimited `var'.csv, delimiter(";") clear 
compress
save `var'.dta, replace

}

use IntFijo2010.dta, clear
save INTFIJO.dta, replace

foreach var in $intfijo2{

use INTFIJO.dta, clear
append using `var'.dta, force
compress 
save INTFIJO.dta, replace  

}

clear
use INTFIJO.dta, clear

*Procesando la base y ajustando variables de velocidad
destring velocidaddebajadakbps, replace dpcomma
destring velocidaddesubidakbps, replace dpcomma
gen fecha1 = date(fecha, "DMY")
drop fecha
rename fecha1 fecha
format fecha %td
gen fecha_trimestre=yq(año, trimestre)
gen downstream= velocidaddebajadakbps if año>2017
replace downstream= velocidaddebajadakbps/1024 if año<2017
replace downstream= velocidaddebajadakbps if año==2017 & trimestre!=3
replace downstream= velocidaddebajadakbps/1024 if año==2017 & trimestre==3
gen upstream= velocidaddesubidakbps if año>2017
replace upstream= velocidaddesubidakbps/1024 if año<2017
replace upstream= velocidaddesubidakbps if año==2017 & trimestre!=3
replace upstream= velocidaddesubidakbps/1024 if año==2017 & trimestre==3
gen ajuste= 1 if downstream>=1000 | upstream>=1000
replace ajuste=0 if ajuste==.
replace downstream=downstream /1024 if downstream>=1000
replace upstream=upstream /1024 if upstream>=1000
merge m:m proveedor using idproveedores
drop _merge

collapse (sum) usuarios (mean) meandownstream=downstream (median) mdownstream=downstream (mean) meanupstream=upstream (median) mupstream=upstream, by(fecha_trimestre año codigomunicipio)
save INTERNETFIJO.dta, replace

********
*VOZ FIJA
********
clear
import delimited TPBC.csv, delimiter(";") clear 
gen fecha1 = date(fecha, "DMY")
drop fecha
rename fecha1 fecha
format fecha %td
gen fecha_trimestre=yq(año, trimestre)
collapse (sum) líneas, by(año fecha_trimestre  id_municipio)
save VOZFIJA.dta, replace 

***********
*TV
**********
clear
import delimited "tv.csv", delimiter(";") clear 
destring codigodepartamento codigomunicipio mes tv, replace force dpcomma
gen trimestre=mesno/3
gen fecha_trimestre=yq(año, trimestre)
collapse (sum) tv (count) numempresastm=nit, by(año fecha_trimestre codigomunicipio)
save TV_PAGA.dta, replace 

***********
*TRIMESTRALIZAR LINEALMENTE POBLACIÓN
**********
**Crear trimestres vacios
clear
use POBLACION.dta, clear
gen trimestre=Mes/3
gen fecha_trimestre=yq(Año, trimestre)
save POBLACION4Q.dta, replace

use POBLACION4Q.dta, clear
replace trimestre=3
drop fecha_trimestre
gen fecha_trimestre=yq(Año, trimestre)
replace poblacion=.
save POBLACION3Q.dta, replace

use POBLACION3Q.dta, clear
replace trimestre=2
drop fecha_trimestre
gen fecha_trimestre=yq(Año, trimestre)
save POBLACION2Q.dta, replace

use POBLACION2Q.dta, clear
replace trimestre=1
drop fecha_trimestre
gen fecha_trimestre=yq(Año, trimestre)
save POBLACION1Q.dta, replace

*Unir las bases e interpolar trimestres vacíos
use POBLACION1Q.dta, clear
append using "POBLACION2Q.dta" "POBLACION3Q.dta" "POBLACION4Q.dta", generate(append)
by CódigoEntidad, sort : ipolate poblacion fecha_trimestre, generate(pob)
drop if Año<2013
egen id=concat(CódigoEntidad Año fecha_trimestre)
save POB.dta, replace

***********
*UNIR LAS BASES
**********
*creando variable ID para las bases de telecomunicaciones
clear
use INTERNETFIJO.dta, clear
rename usuarios intfijo
egen id=concat(codigomunicipio año fecha_trimestre)
save IFIJO.dta, replace

clear
use VOZFIJA.dta, clear
rename líneas vozfija
egen id=concat(id_municipio año fecha_trimestre)
save VFIJA.dta, replace

clear
use TV_PAGA.dta, clear
egen id=concat(codigomunicipio año fecha_trimestre)
save TV.dta, replace
**
clear
global bases "IFIJO TV POB"

use VFIJA.dta, clear
save BASE_FINAL.dta, replace

foreach var in $bases{

use BASE_FINAL.dta, clear
merge 1:1 id using `var'.dta
keep if _merge==3
drop _merge
save BASE_FINAL.dta, replace  
}
use BASE_FINAL.dta, clear 
keep año trimestre fecha_trimestre CódigoDepartamento id_municipio Departamento Entidad pob vozfija intfijo meandownstream mdownstream meanupstream mupstream tv
drop if intfijo==.
labmask CódigoDepartamento , values( Departamento )
labmask id_municipio , values( Entidad )
save BASE_FINAL.dta, replace  

**********************************************************************
*TRATAMIENTO BASE FINAL
**********************************************************************
clear
use BASE_FINAL.dta, clear 
replace vozfija = vozfija/pob
replace intfijo = intfijo/pob
replace tv = tv/pob
xtset id_municipio fecha_trimestre, quarterly
*imputar ceros en periodos donde no se reportaron suscripciones
tsfill, full
replace vozfija=0 if vozfija==.
replace intfijo=0 if intfijo==.
replace tv=0 if tv==.
replace meandownstream=0 if meandownstream==.
replace mdownstream =0 if mdownstream ==.
replace meanupstream =0 if meanupstream ==.
replace mupstream =0 if mupstream ==.
xtset id_municipio fecha_trimestre, quarterly
*evaluar si quedaron gaps en las variables, para poder aplicar filtro de HP
global años "2013 2014 2015 2016 2017 2018"
foreach var in $años{

by id_municipio, sort : gen nogap`var'=1 if año==`var'
replace nogap`var'=0 if nogap`var'==.

}
by id_municipio, sort : gen total= nogap2013+ nogap2014+ nogap2015+ nogap2016+ nogap2017 + nogap2018
by id_municipio, sort : gen suma=sum(total)
by id_municipio, sort : egen max= max(suma)
tab max
*drop if max!=24
save BASE_FINAL.dta, replace  

**********************************************************************
*Hodrick&Prescott Filter
**********************************************************************
*guardando variables originales en formato bases en formato long
{
***
clear
use BASE_FINAL.dta, clear 
*format fecha_trimestre %tq
keep id_municipio Entidad fecha_trimestre vozfija
*reshape wide vozfija, i( id_municipio Entidad) j( fecha_trimestre)
export delimited using "lvozfija", delimiter(";") nolabel replace

clear
use BASE_FINAL.dta, clear 
*format fecha_trimestre %tq
keep id_municipio Entidad fecha_trimestre intfijo
*reshape wide intfijo, i( id_municipio Entidad) j( fecha_trimestre)
export delimited using "lintfijo", delimiter(";") nolabel replace

clear
use BASE_FINAL.dta, clear 
*format fecha_trimestre %tq
keep id_municipio Entidad fecha_trimestre tv
*reshape wide tv, i( id_municipio Entidad) j( fecha_trimestre)
export delimited using "ltv", delimiter(";") nolabel replace


clear
use BASE_FINAL.dta, clear 
*format fecha_trimestre %tq
keep id_municipio Entidad fecha_trimestre meandownstream
*reshape wide meandownstream, i( id_municipio Entidad) j( fecha_trimestre)
export delimited using "lmeandownstream", delimiter(";") nolabel replace

}
**Aplicando filtro y exportando en formato wide
{
clear
use BASE_FINAL.dta, clear 
global var "vozfija intfijo tv meandownstream"
foreach var in $var{
	clear
	use BASE_FINAL.dta, clear 
	format fecha_trimestre %tq
	keep id_municipio fecha_trimestre `var'
	tsset id_municipio fecha_trimestre, quarterly
	tsfilter hp `var'c = `var', trend(`var't)
	keep id_municipio fecha_trimestre `var't
	reshape wide `var't, i( id_municipio) j( fecha_trimestre)
	export delimited using `var't, delimiter(";") nolabel replace
}
}
