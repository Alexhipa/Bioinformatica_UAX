###############################################################################
##
# TRABAJO R ALEJANDRO HENAR IZQUIERDO
##
###############################################################################

## En el archivo de HenarIzquierdoAlejandroTrabajo_imagenes.pdf estan todos los outputs, pero aqui estan todos los comentarios e inputs

### EJERCICIO 1: Carga los datos y exáminalos en R ¿Cuántas variables hay? ¿Cuántos tratamientos?

datos_trabajo <- read.table(file.choose(), header = TRUE) #Con file.choose() podemos elegir el archivo que queremos abrir nosotros(en este caso, datos_trabajoR.txt)
head(datos_trabajo) #con el head() podemos ver las primeras 6 filas de nuestro archivo, en este caso ademas puedo ver que hay 2 variables mas el tratamiento como columnas
tail(datos_trabajo) #con tail() vemos los ultimos 6 en vez de los 6 primeros
summary(datos_trabajo) #con summary() podemos ver las caracteristicas básicas de nuestra tabla como el mínimo/máximo, media, mediana y demás
dim(datos_trabajo) #dim() nos dice las filas(50) y columnas(3) que tiene el archivo
str(datos_trabajo) #str() nos da un resumen de las características del archivo como complemento a summary() y dim()

# Respuestas ejercicio 1: Hay 2 variables y 5 tratamientos

### EJERCICIO 2: Haz un boxplot para nuestros datos. Uno para cada variable. Colorea a Variable 1 y a Variable 2 de forma diferente 

variables_solo <- datos_trabajo[,2:3] #Hacemos otra tabla que solo contenga las variables y no los tratamientos para hacer el boxplot
boxplot(variables_solo, #primero elegimos los datos
        main= "boxplot Variables", #main= sirve para darle un título al boxplot
        col = c("darkorchid1","darkgoldenrod2"), #con col=c() podemos elegir un color distinto para cada boxplot
        las=2)

### EJERCICIO 3: Haz un gráfico de dispersión con las dos variables. Cada tratamiento debe de ir de un color distinto. 

plot(datos_trabajo$Variable2 ~ datos_trabajo$Variable1, #como con el boxplot primero elegimos los datos (para el eje x y para el y), en este caso podemos utilizar $ para elegir una columna en especifico de nuestros datos originales
     xlab = "Variable 1", ylab = "Variable 2", #xlab= e ylab= sirven para poner títulos a cada eje
     col = datos_trabajo$Tratamiento, main = "grafico de dispersión") #al utilizar el tratamiento para el color de los datos, R nos pone predeterminadamente los colores

### EJERCICIO 4: Ponle leyenda al gráfico del apartado anterior. En el margen inferior derecho

legend(48, #indicamos la posición de la leyenda en el eje x
       5, #indicamos la posición de la leyenda en el eje y
       legend =c("Tratamiento 1", "Tratamiento 2", "Tratamiento 3", "Tratamiento 4", "Tratamiento 5"), #ponemos los nombres que apareceran en la leyenda
       fill = c("black","red", "green", "cyan", "blue")) #fill= nos crea cuadrados rellenos del color que eligamos. Utilizamos fill= y no col= porque col serviria para darle color a lineas o puntos que luego habría que "crear" con otras funciones como lty= o cex=

### EJERCICIO 5: Haz un histograma para cada variable. Recuerda mantener los colores.

hist(datos_trabajo$Variable1, #primero utilizamos la funcion hist() y depues elegimos los datos, en este caso, solo la variable 1 de los datos originales
     col = "darkorchid1", #elegimos el color (el mismo que el de antes)
     main = "Histograma de la Variable 1", #ponemos el título
     xlab = "Variable 1", ylab = "Frecuencia") #ponemos los titulos de los ejes
#lo mismo para la variable 2
hist(datos_trabajo$Variable2, col = "darkgoldenrod2", main = "Histograma de la Variable 2", xlab = "Variable 2", ylab = "Frecuencia")

### EJERCICIO 6: Haz un factor en la columna tratamiento y guárdalo en una variable

factor_tratamiento <- factor(datos_trabajo$Tratamiento) #hacemos el factor de la columna de tratmiento y la guardamos en una variable, en este caso la he llamado "factor_tratamiento"
#lo que hace la función factor() es basicamente dividirnos una columna de un data.frame en diferentes niveles, en el caso del tratamiento, lo ha dividido en 5 niveles, es decir, los 5 tratamientos
factor_tratamiento <- factor(factor_tratamiento, levels = c("1","2", "3", "4", "5"), labels = c("tratamiento 1", "tratamiento 2", "tratamiento 3", "tratamiento 4", "tratamiento 5")) #con esto intercambiamos los nombres de los niveles de 1,2,3,4,5 a sus respectivos tratamientos para que sea mas fácil de ver

### EJERCICIO 7: Calcula la media y la desviación estándar para cada tratamiento.

tapply(datos_trabajo$Variable1, #utilizamos tapply() para conseguir las medias de cada tratamiento en la variable 1. primero tenemos que elegir los datos de la variable
       factor_tratamiento, #luego utilizamos el factor de antes para que relacione cada nivel con un tratamiento
       mean) #luego elegimos la formula, en este caso la media
tapply(datos_trabajo$Variable2, factor_tratamiento, mean) #hacemos la media para cada tratamiento en la variable 2
tapply(datos_trabajo$Variable1, factor_tratamiento, sd) #sd sirve para hacer la desviación estandar (en este caso para la variable 1)
tapply(datos_trabajo$Variable2, factor_tratamiento, sd) #ahora la desvuación estandar de la variable 2

### EJERCICIO 8: Averigua cuántos elementos tiene cada tratamiento.

table(factor_tratamiento) #Utilizamos table() para que podamos ver de manera mas facil el número de tratamientos 

# Respuestas ejercicio 8: Nos salen 10 elementos por tratamiento

### EJERCICIO 9: Extrae los datos para el tratamiento 1 y el tratamiento 4 y guárdalos cada uno en una variable diferente.

tratamiento_1 <- datos_trabajo[1:10,] #al saber que cada tratamiento tiene 10 elementos, podemos sacar los 10 primeros para el tratamiento 1 y...
tratamiento_4 <- datos_trabajo[31:40,] #del 31 al 40 para el tratamiento 4

### EJERCICIO 10: Nuestra hipótesis nula es que las medias de tratamiento 1 y tratamiento 4 para la Variable 1 son iguales. ¿Puedes comprobarlo? Para ello, necesitarás comprobar
### primero si los datos se distribuyen de forma normal. En función del resultado de la prueba de normalidad, ¿qué test usarías? ** En general, asumimos que las muestras
### son independientes, pero ¿son sus varianzas iguales? Actúa de acuerdo a tus resultados.

shapiro.test(tratamiento_1$Variable1) #shapiro.test para saber si los datos tienen una distribución normal
shapiro.test(tratamiento_4$Variable1) #como nos dan los dos p-value > 0,05 es una distribución normal
t.test(tratamiento_1$Variable1, tratamiento_4$Variable1) #hacemos el t-test para comprobar la hipótesis nula. En este caso, p-value es < 0,05 y las medias no son iguales
var.test(tratamiento_1$Variable1, tratamiento_4$Variable1) #con el var.test comprobamos las varianzas. En este caso son diferentes con un p-value < 0,05

# Respuestas ejercicio 10: podemos comprobar la hipotesis nula a traves de los test que acabamos de hacer. Para saber si los datos se distrubuyen de manera normal (campana de Gauss) utilizamos el
# shapiro test y luego miramos las medias y las varianzas a traves del t test y el var.test (F test). En este caso, nos sale que las medias y varianzas no son iguales por lo que rechazamos la hipótesis nula

##### Gracias por corregir el trabajo :)