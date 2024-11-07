//Definición de directorios
global usuario "Hiarela" 
if "$usuario"=="Hiarela" {
	global directorio "C:/Users/Hiare/OneDrive/Escritorio/UChile/PRIMAVERA 2023/Métodos Cuantitativos II (AYU)/Tarea 2"
	global datos "$directorio\Datos"
	global figuras "$directorio\Figuras"
	global tablas "$directorio\Tablas"
}

* Cargamos la base
{

import delimited "$datos\MROZ.csv", clear
}

* 1. Genere un gráfico de barras que ilustre cóomo varía la participación laboral de mujeres casadas en Estados Unidos en 1975 en relación a la cantidad de hijos menores de 6 años. Cada barra en el gráfico deberá representar la proporción de mujeres que trabajan y no trabajan, con el eje horizontal mostrando diferentes categorías de cantidad de hijos menores de 6 años y el eje vertical expresado en porcentaje.
{

graph bar, over(inlf) over(kidslt6) asyvars bar(1, color(blue)) bar(2, color(red)) ytitle("Porcentaje (%)") b1title("Cantidad de Hijos Menores de 6 Años") legend(label(1 "No Trabaja") label(2 "Trabaja")) title("Comparación de Participación Laboral Femenina") subtitle("Según Cantidad de Hijos Menores de 6 Años")
graph export "graph1.pdf", replace

graph bar, over(inlf) over(kidslt6) percent asyvars bar(1, color(blue)) bar(2, color(red)) ytitle("Porcentaje (%)") b1title("Cantidad de Hijos Menores de 6 Años") legend(label(1 "No Trabaja") label(2 "Trabaja")) title("Comparación de Participación Laboral Femenina") subtitle("Según Cantidad de Hijos Menores de 6 Años")
graph export "graph2.pdf", replace


* Guardar el gráfico en un archivo PDF en el directorio de figuras
graph export "$figuras\grafico1STATA.pdf", width(8.5) replace
	
	
}

* 2. Estime un modelo de probabilidad lineal, en el cual la variable dependiente sea la participación laboral de las mujeres casadas, que es una variable dicotómica que toma el valor 1 si la mujer trabaja y 0 si no. La variable explicativa principal es el ingreso no laboral de la familia (nwifeinc), que mide el efecto de la renta sobre la decisión laboral. Esta variable representa la suma de los ingresos del esposo y de otras fuentes distintas al trabajo de la mujer. Además incorpore como regresores: el nivel educativo, la experiencia laboral, la edad y el número de hijos menores de 6 años. Presente los resultados en una tabla e interprete los coeficientes estimados. 
{
	
regress inlf nwifeinc educ exper age kidslt6
outreg2 using LPM.tex, replace 

}

* 3. Calcule e interprete los efectos marginales del modelo de probabilidad lineal estimado. Además, comente la significancia de los efectos marginales. Muestre los resultados en una tabla.
{
regress inlf nwifeinc educ exper age kidslt6
outreg2 using LPMmargins.tex, replace 
	
}

* 4. Estime un modelo Probit para la participación laboral de las mujeres casadas en Estados Unidos utilizando todas las variables explicativas mencionadas en la pregunta anterior, y presente sus estimaciones en una tabla. Discuta e interprete sus resultados para las variables.
{
	
probit inlf nwifeinc educ exper age kidslt6
outreg2 using Probit.tex, replace
	
}

* 5. En base al modelo Probit estimado, calcule e interprete los efectos marginales de cada variable, evaluados en la media, y comente si son significativos estadísticamente. Presente los resultados en una tabla.
{
	
margins, dydx(nwifeinc educ exper age kidslt6) atmeans
outreg2 using Probitmargins.tex, replace
}

* 6. Estime un modelo Logit, que incluya las mismas variables que los modelos anteriores. Presente los resultados en una tabla, interprete los coeficientes estimados y comente la significancia del modelo.
{
	
logit inlf nwifeinc educ exper age kidslt6	
outreg2 using Logit.tex, replace
}

* 7. Calcule e interprete los efectos marginales del modelo Logit estimado. Muestre los resultados en una tabla.
{
	
margins, dydx(nwifeinc educ exper age kidslt6) atmeans
	
}

* 8. Matriz de confusión por modelo
{
	// Para modelo MPL se debe hacer lineal
reg inlf nwifeinc educ exper age kidslt6 // MPL
predict mpl // Predecimos segun MPL
gen pred_inlf = (mpl >= 0.5) // DUmmy que indica si mpl es >= 0.5

matrix ConfMPL = J(2,2,.) // Creamos la matriz
matrix rownames ConfMPL = "Predict 1" "Predict 0"
matrix colnames ConfMPL = "Real 1" "Real 0"

count if inlf == 1 & pred_inlf == 1 // Vemos lo que están bien predichos
matrix define ConfMPL[1,1] = r(N) // Almacenamos

count if inlf == 0 & pred_inlf == 0 // Vemos lo que están bien predichos
matrix define ConfMPL[2,2] = r(N) // Almacenamos

count if inlf == 0 & pred_inlf == 1 // Vemos los mal predichos
matrix define ConfMPL[1,2] = r(N) // Almacenamos

count if inlf == 1 & pred_inlf == 0 // Vemos los mal predichos
matrix define ConfMPL[2,1] = r(N) // Almacenamos

matrix list ConfMPL // Para ver la matriz

// Para Probit
probit inlf nwifeinc educ exper age kidslt6
estat classification, cutoff(0.5)
matrix ConfProbit = J(2,2,.) // Creamos la matriz
matrix rownames ConfProbit = "Predict 1" "Predict 0"
matrix colnames ConfProbit = "Real 1" "Real 0"
matrix define ConfProbit[1,1] = r(ctable)[1,1]
matrix define ConfProbit[1,2] = r(ctable)[1,2]
matrix define ConfProbit[2,1] = r(ctable)[2,1]
matrix define ConfProbit[2,2] = r(ctable)[2,2]
matrix list ConfProbit

// Para Logit
logit inlf nwifeinc educ exper age kidslt6
estat classification, cutoff(0.5)
matrix ConfLogit = J(2,2,.) // Creamos la matriz
matrix rownames ConfLogit = "Predict 1" "Predict 0"
matrix colnames ConfLogit = "Real 1" "Real 0"
matrix define ConfLogit[1,1] = r(ctable)[1,1]
matrix define ConfLogit[1,2] = r(ctable)[1,2]
matrix define ConfLogit[2,1] = r(ctable)[2,1]
matrix define ConfLogit[2,2] = r(ctable)[2,2]
matrix list ConfLogit

// Para exportar matrices a tablas tienen frmttable (MC1)
}






