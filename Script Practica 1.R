#############################################################################
#
# PRACTICA 1
#
# Expresión diferencial de genes de ratón
# Microarray de Affymetrix (Affymetrix Murine Genome U74A version 2 MG_U74Av2
# Origen de los datos: GEO GSE5583 (http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5583)
# Publicación: Mol Cell Biol 2006 Nov;26(21):7913-28.  16940178 (http://www.ncbi.nlm.nih.gov/pubmed/16940178)
#
# Muestras: 3 Wild Type x 3 Histone deacetylase 1 (HDAC1)
#
# R código original (credits): Ahmed Moustafa
#
## ENTREGA EL 01 OCTUBRE 23:59
## Se requiere la entrega de este script completado con los códigos más las imágenes y las respuestas a las preguntas
## Adjuntar en la entrega el PDF final y el archivo con los genes
#
##############################################################################

# Instalar RCurl

if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("RCurl")

# Si esto falla, que seguro lo hace tratar de instalarlo usando el menú, Paquetes, Servidor Spain A Coruña, RCurl

# Cargamos el paquete y los datos
library(RCurl)
url = getURL ("http://bit.ly/GSE5583_data", followlocation = TRUE)
data = as.matrix(read.table (text = url, row.names = 1, header = T))

# Chequeamos las dimensiones de los datos, y vemos las primeras y las últimas filas

dim(data)

# Hacemos un primer histograma para explorar los datos

head(data)
tail(data)
hist(data)


# Transformamos los datos con un logaritmo 
data_log = log2(data)
# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve?

hist(data_log, col = "red")
#Para hacer gráficas mas bonitas para publicar :)

# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado?
# ¿Qué es un boxplot?

boxplot(data_log, col=c("red", "red", "red", "yellow", "yellow", "yellow"), main= "El mejor boxplot", las=2)
#col nos deja poner el color y con la c podemos elegir el color para cada ratón; main nos deja elejir el titulo; las pone los titulillos en vertical

# Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación
# de los valores de expresión. ¿Es correcta la separación?

hc=hclust(as.dist(1-cor(data_log)))
plot(hc,main= "el mejor hierarchical clustering")

Si

#######################################
# Análisis de Expresión Diferencial 
#######################################

# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado?

wt <- data[,1:3]
ko <- data[,4:6]
#hemos generado dos tablas diferentes (en vez de una conjunta), una con los wild tipe y otra on los ko

# Calcula las medias de las muestras para cada condición. Usa apply

wt.media = apply(wt, 1, mean)
ko.media = apply(ko, 1, mean)

# ¿Cuál es la media más alta?

max(wt.media)
max(ko.media)

# Ahora hacemos un scatter plot (gráfico de dispersión)

plot(wt.media ~ ko.media, col = "blue", main = "el mejor grafico de dispersión", xlab = "media de los ko", ylab = "media de los wt")

# Añadir una línea diagonal con abline

abline(0, 1, col="red") #esto lo añadimos mientras la gráfica esta abierta

# ¿Eres capaz de añadirle un grid?

grid() #también miesntras esta el gráfico abierto

# Calculamos la diferencia entre las medias de las condiciones

diff.media = wt.media - ko.media

# Hacemos un histograma de las diferencias de medias

hist(diff.media, col= "yellow", main = "el mejor histograma 2.0")

# Calculamos la significancia estadística con un t-test.
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test.
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué?
# ¿Cuántas valores tiene cada muestra?
# Ahora comprobamos que hemos hecho TODOS los cálculos

pvalue = NULL
tstat = NULL
for(i in 1 : nrow(data)) {
  x = wt[i,]
  y = ko[i,]
  
  t = t.test(x, y)
  
  pvalue[i] = t$p.value
  tstat[i] = t$statistic
}

head(pvalue) #esto solo para ver que ha salido bien el bucle

# Hacemos un histograma de los p-values.

hist(pvalue, col = "magenta", main = "El mejor histograma de p-values")

# ¿Qué pasa si le ponemos con una transformación de -log10?

hist(-log10(pvalue), col= "magenta", main = "el mejor histograma de p-values")

# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística

plot(diff.media, -log10(pvalue), main = "GSE5583 - El mejor volcano plot", col= "orange")

# Queremos establecer que el mínimo para considerar una diferencia significativa, es con una diferencia de 2 y un p-value de 0.01
# ¿Puedes representarlo en el gráfico?

diff.media_cutoff = 2
pvalue_cutoff = 0.01
abline(v = diff.media_cutoff, col= "blue", lwd =3)
abline(h = -log10(pvalue_cutoff), col = "red", lwd =3) #lwd es el ancho de linea

# Ahora buscamos los genes que satisfagan estos criterios
# Primero hacemos el filtro para la diferencia de medias (fold)

filter_by_diff.media = abs(diff.media) >= diff.media_cutoff #para filtrar todos los valores (absolutos) por encima de 2 (es decir que tengan cambios en expresión)
dim(data[filter_by_diff.media,])

# Ahora el filtro de p-value

filter_by_pvalue = pvalue <= pvalue_cutoff #aqui parecido a lo de arriba pero no hace falta que buscemos los datos absolutos porque los pvalues no pueden ser negativos (bueno y estamos buscansdo por debajo de 0,01 en este caso)
dim(data[filter_by_pvalue,])

# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios? ==> me da 426 (con el dim se puede ver)

filter_combined = filter_by_diff.media & filter_by_pvalue #aqui combinamos los dos filtros
filtered = data[filter_combined,]
dim(filtered)
head(filtered)

# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo

plot(diff.media, -log10(pvalue), main= "GSE5583 - El mejor Volcano plus", col = "blue")
points(diff.media[filter_combined], -log10(pvalue[filter_combined]), col= "red")
abline(v = diff.media_cutoff, col= "cyan", lwd =3)
abline(h = -log10(pvalue_cutoff), col = "orange", lwd =3) 
grid()

# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés?

plot(diff.media, -log10(pvalue), main = "GSE5583 - El mejor volcano plus plus")
points(diff.media[filter_combined & diff.media < 0], -log10(pvalue[filter_combined &diff.media < 0]), col = "red")
points(diff.media[filter_combined & diff.media > 0], -log10(pvalue[filter_combined &diff.media > 0]), col = "blue")
abline(v = diff.media_cutoff, col= "cyan", lwd =3)
abline(h = -log10(pvalue_cutoff), col = "orange", lwd =3) 
grid()
#Parece que están al reves porque al utilizar el -log10 los datos "se dan la vuelta" 

# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap?
# ¿Eres capaz de cambiar los colores del heatmap? Pista: usar el argumento col y hcl.colors

rowv = as.dendrogram(hclust(as.dist(1-cor(t(filtered)))))
colv = as.dendrogram(hclust(as.dist(1-cor(filtered))))
heatmap(filtered, Rowv = rowv, Colv = colv, cexCol = 0.7, labRow = FALSE, main = "El mejor heatmap pero feo", col=hcl.colors(50)) #para cambiar los colores se utiliza el col=hcl.colors (aunque sale mas feo)

# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#if (!requireNamespace("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install(c("gplots","RColorBrewer"))

install.packages("gplots")		
install.packages("RColorBrewer")	

library(gplots)

# Hacemos nuestro heatmap



# Lo guardamos en un archivo PDF



# Guardamos los genes diferencialmente expresados y filtrados en un fichero

write.table(filtered, "GSE5583_DE.txt", sep = "\t", quote = FALSE)
