vector1 = c(46.4,46.1,45.8,47.0,46.1,45.9,45.8,46.9,45.2,46.0)
scuadrado = var(vector1)
scuadrado

linferior = (9*0.28)/2.7
linferior
lsuperior = (9*0.28)/19
lsuperior

qchisq(0.975,9)

baseAutos <- cars
baseAutos
g1 <- ggplot(baseAutos,aes(x=dist,y=speed))+geom_point()
g1
g2 <- ggplot(baseAutos,aes(sample=dist))+geom_qq()+geom_qq_line()
g2
g3 <- ggplot(baseAutos,aes(sample=speed))+geom_qq()+geom_qq_line()
g3
g4 <- ggplot(baseAutos, aes(sample = dist)) + geom_qq_band(color = "red",
    fill = "green", alpha = 0.3, linetype = "dashed", linewidth = 1) + stat_qq_line() +
    stat_qq_point(color = "blue", shape = 21, alpha = 0.4)
g4
x<-mean(baseAutos$speed)
s<-sd(baseAutos$speed)
n = 50
z_90 <- qnorm(0.95)
z_90
L_i <- x - z_90 * (s/sqrt(n))
L_i
L_s <- x + z_90 * (s/sqrt(n))
L_s