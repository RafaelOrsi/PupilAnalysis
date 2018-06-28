# TÍTULO: ARTIGO CBA
# AUTOR:  RAFAEL NOBRE ORSI
# DATA:   22 DE MARÇO DE 2018

getwd() # Comando para apontar um diretório de trabalho
#setwd('C:/Users/Rafa/Desktop/Adicione1') #(Note)
#setwd('C:/Users/wp/Desktop/Adicione1') # (Desk work)
setwd('C:/Users/Trabalho/Desktop/Adicione1') #Configuração do diretório (Desk Home).

# Comando para ler a base e dados e atribuí-la a uma variável
pupil <- read.csv("Dados59.tsv", header = TRUE, sep = "\t")  #variavel <- read.tsv("aqruivo.csv", header = TRUE(usa primeira linha como título), sep = "\t" (separador \t = tab)) 
# ----------------------------------------------------------------------------------------------------
# ETAPA 2:FILTRO E ORGANIZAÇÃO DOS DADOS
# Nesta etapa são removidos todos os dados desnecessários, como telas de instruções e intervalos entre tarefas.
# Após a aplicação dos filtros, os dados são organizados para faciltiar as prócimas etapas.

# Filtros
pupil <- pupil[!is.na(pupil$MediaName),] #Filtra onde MediaName = NA
#pupil <- pupil[!is.na(pupil$PupilLeft),] #Filtra onde PupilLeft = NA
#pupil <- pupil[!is.na(pupil$PupilRight),] #Filtra onde PupilLeft = NA
pupil <- pupil[pupil$MediaName!="", ] #Filtra onde MediaName = vazio
pupil <- pupil[pupil$MediaName!="Inst2.jpg", ] #Filtra Slide de instrução (Inst2.jpg)
pupil <- pupil[pupil$MediaName!="Slide2.JPG", ] #Filtra Slide de instrução (Slide2.jpg)
pupil <- pupil[pupil$MediaName!="teste7.wmv", ] #Filtra Slide de teste (teste7.wmv)
pupil <- pupil[pupil$MediaName!="RESPOSTA.jpg", ] #Filtra Slide de resposta do teste (RESPOSTA.jpg)
pupil <- pupil[pupil$MediaName!="Slide19.JPG", ] #Filtra Slide de instrução (Slide19.JPG)
pupil <- pupil[pupil$MediaName!="Slide20.JPG", ] #Filtra Slide de instrução (Slide20.JPG)
pupil <- pupil[pupil$MediaName!="Slide31.JPG", ] #Filtra Slide de instrução (Slide31.JPG)
pupil <- pupil[pupil$MediaName!="FIM.jpg", ] #Filtra Slide de instrução (FIM.jpg)

# Organização dos dados
pupil$X <- NULL #Remove a coluna X (coluna em branco inserida pelo arquivo TSV)
pupil$Participante <- pupil$ï..ParticipantName #Copia a coluna ï..ParticipantName para Participante
pupil$Slide <- pupil$MediaName #Copia a coluna MediaName para Slide
pupil$Diametro <- (pupil$PupilLeft + pupil$PupilRight) / 2 # Calcula a media entre pupila direita e esquerda
pupil$ï..ParticipantName <- NULL #Não usa mais, remove para diminuir o processamento
pupil$MediaName <- NULL #Não usa mais, remove para diminuir o processamento
pupild <- pupil # Copia a base para preservar diâmetro das pupilas usado na análise do PCA
pupil <- pupil[!is.na(pupil$Diametro),] #Filtra onde PupilLeft = NA
pupil$PupilLeft <- NULL #Não usa mais, remove para diminuir o processamento
pupil$PupilRight <- NULL #Não usa mais, remove para diminuir o processamento

# Armazenamento da base de dados. 
write.csv(pupild, "DadosDiametro.csv") # Comando para gravar arquivo filtrado e organizado.
write.csv(pupil, "Dadosfiltrados.csv") # Comando para gravar arquivo filtrado e organizado.
# ----------------------------------------------------------------------------------------------------
# ETAPA 3:PROCESSAMENTO DOS DADOS.

# Participantes ---- Removi os participantes P10, P19, P31 e P49. Motivo: sinal baixo
participante<-c("P01","P02","P03","P04","P05","P06","P07","P08","P09",
                "P11","P12","P13","P14","P15","P16","P17","P18","P20",
                "P21","P22","P23","P24","P25","P26","P27","P28","P29","P30",
                "P32","P33","P34","P35","P36","P37","P38","P39","P40",
                "P41","P42","P43","P44","P45","P46","P47","P48","P50",
                "P51","P52","P53","P54","P55","P56","P57","P58","P59")

# Slide do cartão = 2 segundos.
slidecard<-c("Q1-3897.wmv","Q2-8015.wmv","Q3-4267.wmv","Q4-9035.wmv","Q5-3768.wmv",
             "Q6-2743.wmv","Q7-5482.wmv","Q8-8461.wmv","Q9-0721.wmv","Q10-8539.wmv")

# Slide de resposta = 20 segundos.
slideresp<-c("R1.jpg","R2.jpg","R3.jpg","R4.jpg","R5.jpg",
             "R6.jpg","R7.jpg","R8.jpg","R9.jpg","R10.jpg")

# Função pra gerar lista de variância por participante/slide.
# Entradas: participante<-c("conjunto de participantes"), slide<-c("conjunto de slides").
fvar = function(participante,slide){
  lp<-length(participante) # Quantidade de amostras do mesmo participante.
  ls<-length(slide) # Quantidade de slides.
  lista<-matrix(0,lp,ls) # Define matriz conforme comprimento dos conjuntos.
  r<-1
  for (j in c(participante)){
    c<-1
    for(i in c(slide)){
      x1 <- pupil[pupil$Participante==j,] # Filtra o participante.
      x2 <- x1[x1$Slide==i,] # Filtra o slide.
      lista[r,c] <- var(x2$Diametro) # Calcula a Variância de cada participante por Slide e monta uma lista.
      c<-c+1
    }
    r<-r+1
  }
  return(lista) #Retorna lista com coeficientes de variância de cada participante.
}

# Função para gerar lista de variância por tarefa (Tarefa =  R1 + Q1)
ftask = function(participante,pupil){
  var2tarefa<-matrix(0,length(participante),10) # Define matriz conforme comprimento dos conjuntos.
  id<-1
  for (j in c(participante)){
    pupil2 <- pupil[pupil$Participante==j,] # Filtra o participante.
    
    # Filtra a R1 e Q1 
    T1<-pupil2[(pupil2$Slide=="R1.jpg"|pupil2$Slide=="Q1-3897.wmv"),]
    T2<-pupil2[(pupil2$Slide=="R2.jpg"|pupil2$Slide=="Q2-8015.wmv"),]
    T3<-pupil2[(pupil2$Slide=="R3.jpg"|pupil2$Slide=="Q3-4267.wmv"),]
    T4<-pupil2[(pupil2$Slide=="R4.jpg"|pupil2$Slide=="Q4-9035.wmv"),]
    T5<-pupil2[(pupil2$Slide=="R5.jpg"|pupil2$Slide=="Q5-3768.wmv"),]
    T6<-pupil2[(pupil2$Slide=="R6.jpg"|pupil2$Slide=="Q6-2743.wmv"),]
    T7<-pupil2[(pupil2$Slide=="R7.jpg"|pupil2$Slide=="Q7-5482.wmv"),]
    T8<-pupil2[(pupil2$Slide=="R8.jpg"|pupil2$Slide=="Q8-8461.wmv"),]
    T9<-pupil2[(pupil2$Slide=="R9.jpg"|pupil2$Slide=="Q9-0721.wmv"),]
    T10<-pupil2[(pupil2$Slide=="R10.jpg"|pupil2$Slide=="Q10-8539.wmv"),]
    
    # Calcula a Variância de cada participante por tarefa e monta uma lista.
    var2tarefa[id,1] <- var(T1$Diametro) 
    var2tarefa[id,2] <- var(T2$Diametro) 
    var2tarefa[id,3] <- var(T3$Diametro) 
    var2tarefa[id,4] <- var(T4$Diametro) 
    var2tarefa[id,5] <- var(T5$Diametro) 
    var2tarefa[id,6] <- var(T6$Diametro) 
    var2tarefa[id,7] <- var(T7$Diametro) 
    var2tarefa[id,8] <- var(T8$Diametro) 
    var2tarefa[id,9] <- var(T9$Diametro) 
    var2tarefa[id,10] <- var(T10$Diametro)
    id<-id+1
  }
  return(var2tarefa) #Retorna lista com coeficientes de variância de cada participante.
}

varianciaporcartao <- fvar(participante,slidecard)
varianciaporresposta <- fvar(participante,slideresp)
varianciaportarefa <- ftask(participante,pupil)

# Função para agrupar os coeficientes de variância
# Coluna 1: Média dos coeficientes de variancia das 5 tarefas de Adicione 1
# Coluna 2: Média dos coeficientes de variancia das 5 tarefas de Adicione 3
# Coluna 3: Média dos coeficientes de variancia das 10 tarefas (Adicione 1 + Adcione 3)
fmean = function(lista){
  lista2<-matrix(0,length(lista[,1]),3) # Define matriz conforme comprimento dos conjuntos.
  for(j in 1:length(lista[,1])){
    lista2[j,1] <- mean(lista[j,1:5])
    lista2[j,2] <- mean(lista[j,6:10])
    lista2[j,3] <- mean(lista[j,])
  }
  return(lista2) #Retorna lista com coeficientes de variância de cada participante.
}

# Listas de coeficientes agrupados
cvarcartao <- fmean(varianciaporcartao)  
cvarresposta <- fmean(varianciaporresposta)
cvartarefa <- fmean(varianciaportarefa)
cvargacertos <- fmean(varianciagrupoacertos)  
cvargerros <- fmean(varianciagrupoerros)      

# Agrupamento das variâveis da tarefa adcione 1.
# Coluna 1: Coeficientes da etapa de memorização;
# Coluna 2: Coeficientes da etapa de reprodução e transformaçã;
# Coluna 3: Coeficientes globais.
add1 <- matrix(0,length(cvarcartao[,1]),3)
for(j in 1:length(cvarcartao[,1])){
  add1[j,1] <- cvarcartao[j,1]
  add1[j,2] <- cvarresposta[j,1]
  add1[j,3] <- cvartarefa[j,1]
}

# Agrupamento das variâveis da tarefa adcione 3.
# Coluna 1: Coeficientes da etapa de memorização;
# Coluna 2: Coeficientes da etapa de reprodução e transformaçã;
# Coluna 3: Coeficientes globais.
add3 <- matrix(0,length(cvarcartao[,1]),3)
for(j in 1:length(cvarcartao[,1])){
  add3[j,1] <- cvarcartao[j,2]
  add3[j,2] <- cvarresposta[j,2]
  add3[j,3] <- cvartarefa[j,2]
}

# Variância por grupos.  ----------------- REMOVI P06, P23, P24 E P36 - OUTLIERS
# Grupo que acertou 100%.
pacertos<-c("P03","P04","P11","P13","P15","P17","P18","P20","P22",
            "P25","P26","P28","P32","P35","P37","P38","P39",
            "P40","P41","P42","P43","P44","P45","P46","P50","P51","P54",
            "P55","P57","P58","P59")
# Grupo que não acertou 100%.
perros<-c("P01","P02","P05","P07","P08","P09","P12","P14","P16","P21",
          "P27","P29","P30","P33","P34","P47","P48","P52","P53",
          "P56")

varianciagrupoacertos <- ftask(pacertos,pupil)
varianciagrupoerros <- ftask(perros,pupil)

# Preparação para plotar comparação entre grupos
coefacertostotal1 <- fvar(pacertos,slidecard)
coefacertostotal2 <- fvar(pacertos,slideresp)
coefacertos1 <- fmean(coefacertostotal1)
coefacertos2 <- fmean(coefacertostotal2)
coeferrostotal1 <- fvar(perros,slidecard)
coeferrostotal2 <- fvar(perros,slideresp)
coeferros1 <- fmean(coeferrostotal1)
coeferros2 <- fmean(coeferrostotal2)

# Agrupamento das variâveis os grupos.
# Coluna 1: Add1 (Fase A);
# Coluna 2: Add1 (Fase B);
# Coluna 3: Add3 (Fase A);
# Coluna 4: Add3 (fase B).
acertos <- matrix(0,length(coefacertos1[,1]),4)
for(j in 1:length(coefacertos1[,1])){
  acertos[j,1] <- coefacertos1[j,1]
  acertos[j,2] <- coefacertos2[j,1]
  acertos[j,3] <- coefacertos1[j,2]
  acertos[j,4] <- coefacertos2[j,2]
}
erros <- matrix(0,length(coeferros1[,1]),4)
for(j in 1:length(coeferros1[,1])){
  erros[j,1] <- coeferros1[j,1]
  erros[j,2] <- coeferros2[j,1]
  erros[j,3] <- coeferros1[j,2]
  erros[j,4] <- coeferros2[j,2]
}

acertos2 <- matrix(0,length(coefacertos1[,1]),2)
for(j in 1:length(coefacertos1[,1])){
  acertos2[j,1] <- coefacertos1[j,3]
  acertos2[j,2] <- coefacertos2[j,3]
}
erros2 <- matrix(0,length(coeferros1[,1]),2)
for(j in 1:length(coeferros1[,3])){
  erros2[j,1] <- coeferros1[j,3]
  erros2[j,2] <- coeferros2[j,3]
}

# Dados do matlab para boxplot
classedisper <- matrix(0,31,2)
classedisper[1,1] =49.6235;
classedisper[2,1] =31.1734;
classedisper[3,1] =35.5411;
classedisper[4,1] =3.4162;
classedisper[5,1] =10.8616;
classedisper[6,1] =36.8801;
classedisper[7,1] =35.0996;
classedisper[8,1] =21.6493;
classedisper[9,1] =-5.1634;
classedisper[10,1] =10.3041;
classedisper[11,1] =30.9215;
classedisper[12,1] =10.9801;
classedisper[13,1] =4.1984;
classedisper[14,1] =22.2990;
classedisper[15,1] =34.7351;
classedisper[16,1] =12.9940;
classedisper[17,1] =21.3349;
classedisper[18,1] =-8.2214;
classedisper[19,1] =67.2417;
classedisper[20,1] =60.3807;
classedisper[21,1] =41.2655;
classedisper[22,1] =48.3154;
classedisper[23,1] =40.3831;
classedisper[24,1] =-0.3470;
classedisper[25,1] =47.3601;
classedisper[26,1] =20.3637;
classedisper[27,1] =3.1898;
classedisper[28,1] =6.6041;
classedisper[29,1] =50.7735;
classedisper[30,1] =37.1386;
classedisper[31,1] =20.3413;

classedisper[1,2] =-36.3090;
classedisper[2,2] =-64.8033;
classedisper[3,2] =-17.3275;
classedisper[4,2] =-41.2980;
classedisper[5,2] =-56.4364;
classedisper[6,2] =-24.5230;
classedisper[7,2] =-45.4713;
classedisper[8,2] =-42.8303;
classedisper[9,2] =-25.4421;
classedisper[10,2] =-39.6944;
classedisper[11,2] =-56.1127;
classedisper[12,2] =-57.3599;
classedisper[13,2] =-65.3949;
classedisper[14,2] =-39.1531;
classedisper[15,2] =-27.1116;
classedisper[16,2] =-45.3619;
classedisper[17,2] =-68.3405;
classedisper[18,2] =-27.2221;
classedisper[19,2] =-21.4458;
classedisper[20,2] =NA;
classedisper[21,2] =NA;
classedisper[22,2] =NA;
classedisper[23,2] =NA;
classedisper[24,2] =NA;
classedisper[25,2] =NA;
classedisper[26,2] =NA;
classedisper[27,2] =NA;
classedisper[28,2] =NA;
classedisper[29,2] =NA;
classedisper[30,2] =NA;
classedisper[31,2] =NA;



#PLOTS
#1
pdf("CBA 1 - Variância por cartão.pdf",width = 5, height = 3.5)
par(mfrow=c(1,2),mar=c(4,4,1,1),bty="l",cex.axis=0.4,cex.lab=1,cex.main=1)
boxplot(x = as.list(as.data.frame(varianciaporcartao)),ylim=c(0,.22),xaxt = "n",
        main="Fase A", 
        xlab="Cartões", ylab="Variância do diâmetro da pupila (cm)")
axis(1,at=1:10,labels=c(1:10))
boxplot(x = as.list(as.data.frame(varianciaporresposta)),ylim=c(0,.226),xaxt = "n",
        main="Fase B",
        xlab="Cartões")
axis(1,at=1:10,labels=c(1:10))
dev.off()

#2
pdf("CBA 4 - Variância por tarefa agrupado.pdf",width = 5, height = 3.5)
par(mfrow=c(1,2),mar=c(4,4,1,1),bty="l",cex.axis=0.5,cex.lab=1)
boxplot(x = as.list(as.data.frame(add1)),ylim=c(0,.226),xaxt = "n",
        main="Tarefa Adicione 1",xlab="", 
        ylab="Variância do diâmetro da pupila (mm)")
axis(1,at=1:3,labels=c("Fase A","Fase B","Global (A+B)"))
boxplot(x = as.list(as.data.frame(add3)),ylim=c(0,.226),xaxt = "n",
        main="Tarefa Adicione 3",xlab="", 
        ylab="Variância do diâmetro da pupila (mm)")
axis(1,at=1:3,labels=c("Fase A","Fase B","Global (A+B)"))
dev.off()

#3
pdf("CBA 9c - Comparação de grupos Global.pdf",width = 5, height = 3.5)
par(mfrow=c(1,2),mar=c(2,4,1,1),bty="l",cex.axis=0.6,cex.lab=1)
boxplot(x = as.list(as.data.frame(acertos2)),ylim=c(0,.226),xaxt = "n",
        main="Proficientes",ylab="Variância do diâmetro da pupila (cm)")
axis(1,at=1:2,labels=c("Fase A", "Fase B"))
boxplot(x = as.list(as.data.frame(erros2)),ylim=c(0,.226),xaxt = "n",
        main="Não-proficientes", ylab="Variância do diâmetro da pupila (cm)")
axis(1,at=1:2,labels=c("Fase A", "Fase B"))
dev.off()

#4
pdf("CBA 10 - Dispersão dos dados de treinamento.pdf",width = 5, height = 3.5)
par(mfrow=c(1,1),mar=c(2,4,1,1),bty="l",cex.axis=0.8,cex.lab=1)
boxplot(x = as.list(as.data.frame(classedisper)),xaxt = "n",
        ylab="Características mais discriminantes")
axis(1,at=1:2,labels=c("Proficientes", "Não-proficientes"))
dev.off()


#TESTE DE SIGNIFICÂNCIA
#1
t.test(varianciaporcartao,varianciaporresposta)
#2
t.test(add1[,1],add3[,1])
t.test(add1[,2],add3[,2])
t.test(add1[,3],add3[,3])
#3
t.test(acertos2[,1],erros2[,1])
t.test(acertos2[,2],erros2[,2])
%t.test(acertos2,erros2)
#4
t.test(classedisper[,1],classedisper[,2])

#t.test(add1[,1],add1[,2])
