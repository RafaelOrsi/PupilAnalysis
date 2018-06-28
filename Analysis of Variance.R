# TÍTULO: PROCESSAMENTO DE DADOS PUPILARES
# AUTOR:  RAFAEL NOBRE ORSI
# DATA:   05 DE MARÇO DE 2017
# ----------------------------------------------------------------------------------------------------
# ------------------------------ EXPERIMENTO: CONTAGEM COGNITIVA -------------------------------------
# INTRODUÇÃO: 
# ESTE ALGORITMO FOI DESENVOVIDO COMO PARTE DE UM TRABALHO DE MESTRADO EM ENGENHARIA ELÉTRICA,
# CUJO OBJETIVO É ESTUDAR O SISTEMA COGNITIVO POR MEIO DO PROCESSAMENTO DE SINAL PUPILAR. 

# FUNÇÃO DO ALGORITMO: PROCESSAMENTO DE DADOS.
# TIPO DE DADO: DIÂMETRO PUPILAR.
# EQUIPAMENTO PARA AQUISIÇÃO DE DADOS: TOBII STUDIO TX300.

# ETAPAS DO ALGORITMO:
# ETAPA 1: ENTRADA DE DADOS;
# ETAPA 2: FILTRO E ORGANIZAÇÃO DOS DADOS;
# ETAPA 3: PROCESSAMENTO DOS DADOS;
# ETAPA 4: (DESCREVER).

# IMPORTANTE: QUALQUER PARTE DESTE ALGORITMO PODE SER COPIADA E REUTILIZADA, DESDE QUE CITADA A FONTE. 
# ----------------------------------------------------------------------------------------------------
# ETAPA 1:ENTRADA DE DADOS.
# Este algoritmo espera como entrada uma base de dados em formato TSV (Tab Separated Value).
# A base de dados deve conter as variáveis: ParticipantName, MediaName, PupilLeft e PupilRight.

getwd() # Comando para apontar um diretório de trabalho
#setwd('C:/Users/Rafa/Desktop/Adicione1') #(Note)
#setwd('C:/Users/wp/Desktop/Adicione1') # (Desk work)
setwd('C:/Users/Trabalho/Desktop/ContagemCognitiva') #Configuração do diretório (Desk Home).

# Comando para ler a base e dados e atribuí-la a uma variável
pupil <- read.csv("pupila.tsv", header = TRUE, sep = "\t")  #variavel <- read.tsv("aqruivo.csv", header = TRUE(usa primeira linha como título), sep = "\t" (separador \t = tab)) 

pupil <- pupil[!is.na(pupil$MediaName),] #Filtra onde MediaName = NA
pupil <- pupil[!is.na(pupil$PupilLeft),] #Filtra onde PupilLeft = NA
pupil <- pupil[!is.na(pupil$PupilRight),] #Filtra onde PupilLeft = NA
pupil <- pupil[pupil$MediaName!="", ] #Filtra onde MediaName = vazio
pupil <- pupil[pupil$MediaName!="6x6.jpg", ] #Filtra Slide teste6x6.jpg
pupil$mean <- (pupil$PupilLeft + pupil$PupilRight) / 2 # Media entre pupila direita e esquerda
pupil$X <- NULL #Remove a coluna X (que apareceu não sei de onde)
pupil$PupilLeft <- NULL #Não usa mais, remove para diminuir o processamento
pupil$PupilRight <- NULL #Não usa mais, remove para diminuir o processamento

#write.csv(pupil,"pupilfilter.csv") #Grava arquivo filtrado 

#Função pra gerar lista de variância por participante/slide
#Inputs: rec<-c("conjunto de participantes"), slide<-c("conjunto de slides")
fvar = function(rec,slide){
  lr<-length(rec) #comprimento de rec
  ls<-length(slide) #Comprimento de slide
  lista<-matrix(0,lr,ls) #define matriz conforme comprimento dos conjuntos
  r<-1
  for (j in c(rec)){
    c<-1
    for(i in c(slide)){
      x1 <- pupil[pupil$ï..RecordingName==j,] #Filtra o participante
      x2 <- x1[x1$MediaName==i,] #Filtra o slide
      lista[r,c] <- var(x2$mean) #Calcula a Variância de cada participante por Slide e monta lista
      c<-c+1
    }
    r<-r+1
  }
  return(lista)
}

#Participantes
rec<-c("Rec 02","Rec 03","Rec 04","Rec 05","Rec 06","Rec 07","Rec 08","Rec 09","Rec 10","Rec 11",
       "Rec 12","Rec 13","Rec 14","Rec 15","Rec 16","Rec 17","Rec 18","Rec 19","Rec 20","Rec 22",
       "Rec 23","Rec 24","Rec 25","Rec 26","Rec 27","Rec 28","Rec 29","Rec 30","Rec 31","Rec 32")

#Sequência de aplicação do teste
slidet<-c("7x8a.JPG","4x5a.JPG","7x8b.JPG","12x13a.JPG","4x5b.JPG","7x12a.JPG","7x8c.JPG",
          "8x11a.JPG","4x5c.JPG","12x13b.JPG","7x12c.JPG","12x13c.JPG","11x14a.JPG",
          "8x11b.JPG","7x12b.JPG","8x11c.JPG","11x14b.JPG","11x14c.JPG")

#Sequência por quantidade
slideq<-c("4x5a.JPG","4x5b.JPG","4x5c.JPG",
          "7x8a.JPG","7x8b.JPG","7x8c.JPG",
          "7x12a.JPG","7x12b.JPG","7x12c.JPG",
          "8x11a.JPG","8x11b.JPG","8x11c.JPG",
          "11x14a.JPG","11x14b.JPG","11x14c.JPG",
          "12x13a.JPG","12x13b.JPG","12x13c.JPG")

#SequÊncia por padrões A / B / C
slidep<-c("4x5a.JPG","7x8a.JPG","7x12a.JPG","8x11a.JPG","11x14a.JPG","12x13a.JPG",
          "4x5b.JPG","7x8b.JPG","7x12b.JPG","8x11b.JPG","11x14b.JPG","12x13b.JPG",
          "4x5c.JPG","7x8c.JPG","7x12c.JPG","8x11c.JPG","11x14c.JPG","12x13c.JPG")

#Legenda do eixo das abscissas por quantidade
ableg<-c("4x5a","4x5b","4x5c","7x8a","7x8b","7x8c","7x12a","7x12b","7x12c","8x11a",
         "8x11b","8x11c","11x14a","11x14b","11x14c","12x13a","12x13b","12x13c")

squant<-fvar(rec,slideq)
spad<-fvar(rec,slidep)

#Agrupamento por quantidade
gsquant<-matrix(0,30,6)
for(j in 1:30){
  for(i in 1:6){
    gsquant[j,i]<-(squant[j,i*3-2]+squant[j,i*3-1]+squant[j,i*3])/3
  }
}

#Agrupamento por padrão
gspad<-matrix(0,30,3)
for(j in 1:30){
  for(i in 1:3){
    gspad[j,i]<-(spad[j,i*6-5]+spad[j,i*6-4]+spad[j,i*6-3]+spad[j,i*6-2]+spad[j,i*6-1]+spad[j,i*6])/6
  }
}

pdf("1-Variância por quantidade.pdf",width = 5, height = 4)
par(mfrow=c(1,1),mar=c(4,4,1,1),bty="l", cex.axis=0.85,cex.lab=0.85)
boxplot(x = as.list(as.data.frame(gsquant)),ylim=c(0,.08),xaxt = "n",
        xlab="Quantidade", ylab="Variância do diâmetro da pupila (cm)")
axis(1,at=1:6,labels=c("20","56","84","88","154","156"))
dev.off()

#subdivisão da matriz original para plotar quantidades separados
spad20<-matrix(0,30,3)
spad56<-matrix(0,30,3)
spad84<-matrix(0,30,3)
spad88<-matrix(0,30,3)
spad154<-matrix(0,30,3)
spad156<-matrix(0,30,3)
for(j in 1:30){
  for(i in 1:3){
    spad20[j,i]<-squant[j,i]
    spad56[j,i]<-squant[j,i+3]
    spad84[j,i]<-squant[j,i+6]
    spad88[j,i]<-squant[j,i+9]
    spad154[j,i]<-squant[j,i+12]
    spad156[j,i]<-squant[j,i+15]
  }
}

pdf("1.1-Variância por quantidade.pdf",width = 5, height = 4)
par(mfrow=c(1,6),oma=c(2,3.5,0,0),mar=c(4,1,1,1),bty="l", cex.axis=0.85,cex.lab=0.85)
boxplot(x = as.list(as.data.frame(spad20)),ylim=c(0,.11),xaxt = "n",
        xlab="20", ylab="Variância do diâmetro da pupila (cm)")
mtext(c("Variância do diâmetro da pupila (cm)"),side=2,line=3) # escreve na margem
axis(1,at=1:3,labels=c("A","B","C"))
boxplot(x = as.list(as.data.frame(spad56)),ylim=c(0,.11),xaxt="n",xlab="56")
axis(1,at=1:3,labels=c("A","B","C"))
boxplot(x = as.list(as.data.frame(spad84)),ylim=c(0,.11),xaxt="n",xlab="84")
mtext(c("             Padrão/Quantidade"),side=1,line=5) # escreve na margem
axis(1,at=1:3,labels=c("A","B","C"))
boxplot(x = as.list(as.data.frame(spad88)),ylim=c(0,.11),xaxt="n",xlab="88")
axis(1,at=1:3,labels=c("A","B","C"))
boxplot(x = as.list(as.data.frame(spad154)),ylim=c(0,.11),xaxt="n",xlab="154")
axis(1,at=1:3,labels=c("A","B","C"))
boxplot(x = as.list(as.data.frame(spad156)),ylim=c(0,.11),xaxt="n",xlab="156")
axis(1,at=1:3,labels=c("A","B","C"))
dev.off()






pdf("2-Variância por padrão.pdf",width = 5, height = 4)
par(mfrow=c(1,1),mar=c(4,4,1,1),bty="l", cex.axis=0.85,cex.lab=0.85)
boxplot(x = as.list(as.data.frame(gspad)),ylim=c(0,.08),xaxt = "n",
        xlab="Padrão", ylab="Variância do diâmetro da pupila (cm)")
axis(1,at=1:3,labels=c("A","B","C"))
dev.off()

#subdivisão da matriz original para plotar padrões separados
spada<-matrix(0,30,6)
spadb<-matrix(0,30,6)
spadc<-matrix(0,30,6)
for(j in 1:30){
  for(i in 1:6){
    spada[j,i]<-spad[j,i]
    spadb[j,i]<-spad[j,i+6]
    spadc[j,i]<-spad[j,i+12]
  }
}

pdf("3-Variância por padrão.pdf",width = 5, height = 4)
par(mfrow=c(1,3),oma=c(0,3.5,0,0),mar=c(4,1,1,1),bty="l", cex.axis=0.85,cex.lab=0.85)
boxplot(x = as.list(as.data.frame(spada)),ylim=c(0,.11),xaxt = "n",
        ylab="Variância do diâmetro da pupila (cm)")
mtext(c("Variância do diâmetro da pupila (cm)"),side=2,line=3) # escreve na margem
mtext(c("Padrão A"),side=1,line=3) # escreve na margem
axis(1,at=1:6,labels=c(1:6))
boxplot(x = as.list(as.data.frame(spadb)),ylim=c(0,.11),xaxt="n")
mtext(c("Padrão B"),side=1,line=3) # escreve na margem
axis(1,at=1:6,labels=c(1:6))
boxplot(x = as.list(as.data.frame(spadc)),ylim=c(0,.11),xaxt="n")
mtext(c("Padrão C"),side=1,line=3) # escreve na margem
axis(1,at=1:6,labels=c(1:6))
dev.off()


###Ranking de Volk 
#Desempenho: Por quartil q1=quartil1, q2=quartil2, q3=quartil3, q4=quartil4 ----------------------- 
recq1<-c("Rec 25","Rec 28","Rec 14","Rec 19","Rec 03","Rec 02","Rec 30","Rec 27")
q1<-fvar(recq1,slideq)

recq2<-c("Rec 24","Rec 29","Rec 13","Rec 05","Rec 18","Rec 06","Rec 26")
q2<-fvar(recq2,slideq)

recq3<-c("Rec 22","Rec 23","Rec 20","Rec 07","Rec 16","Rec 15","Rec 10","Rec 32")
q3<-fvar(recq3,slideq)
q3[7,14]<-mean(q3[1,14],q3[2,14],q3[3,14],q3[4,14],q3[5,14],q3[6,14],q3[7,14])#Gambiarra para remover NA 

recq4<-c("Rec 04","Rec 09","Rec 12","Rec 11","Rec 31","Rec 17","Rec 08")
q4<-fvar(recq4,slideq)

Gq1<-matrix(0,18,1)
Gq2<-matrix(0,18,1)
Gq3<-matrix(0,18,1)
Gq4<-matrix(0,18,1)
for(j in 1:18){
  Gq1[j,1]<-mean(q1[,j])
  Gq2[j,1]<-mean(q2[,j])
  Gq3[j,1]<-mean(q3[,j])
  Gq4[j,1]<-mean(q4[,j])
}

pdf("4-Estimativa por quartil.pdf",width = 6, height = 4)
par(mfrow=c(1,1),mar=c(4,4,1,1),bty="l",cex=0.85)
plot(c(1:18),Gq1,col="blue",type="p",xaxt="n",ylim=c(0,.12),
     xlab="Quantidade",ylab="Variância média do quartil (cm)")
lines(lowess(Gq1),col="blue",lwd=1)#lowess usa regressão polinomial ponderada localmente
lines(Gq2,col="green",type="p")
lines(lowess(Gq2),col="green",lwd=1)
lines(Gq3,col="orange",type="p")
lines(lowess(Gq3),col="orange",lwd=1)
lines(Gq4,col="red",type="p")
lines(lowess(Gq4),col="red",lwd=1)
legend("topleft", legend=c("1º Quartil","2º Quartil","3º Quartil","4º Quartil"),cex = 0.85,
       col=c("blue","green","orange","red"),lty=1,lwd=1, bty="n")
axis(1,at=1:18,labels=c("","20","","","56","","","84","","","88","","","154","","","156",""))
dev.off()

#Função para facilitar plot 5
gfl = function(lg,corset){
  for(j in 1:18){
    for(i in 1:5){
      lines(j,lg[i,j],type="p",col=corset)
    }
  }
}

pdf("5-Estimativa polinomial com todos os pontos.pdf",width = 8, height = 8) #Gráfico OK
par(mfrow=c(1,1),mar=c(5.1,4.5,4.1,2.1),bty="l",cex.axis=1.2,cex.lab=2)
plot(c(1,18),c(0,.18),type='n',xaxt="n" ,xlab="Quantidade",ylab="Variância média do quartil (mm)")
gfl(q1,"blue")
gfl(q2,"green")
gfl(q3,"orange")
gfl(q4,"red")
lines(lowess(Gq1),col="blue",lwd=2)
lines(lowess(Gq2),col="green",lwd=2)
lines(lowess(Gq3),col="orange",lwd=2)
lines(lowess(Gq4),col="red",lwd=2)
legend("topleft", legend=c("1º Quartil","2º Quartil","3º Quartil","4º Quartil"),cex = 1.5,
       col=c("blue","green","orange","red"),lty=1,lwd=2, bty="n")
axis(1,at=1:18,labels=c("","20","","","56","","","84","","","88","","","154","","","156",""))
dev.off()


#-----------------------------------------------------------------------------------------------
boxpadraoq1<-matrix(0,8,18)
boxpadraoq2<-matrix(0,7,18)
boxpadraoq3<-matrix(0,8,18)
boxpadraoq4<-matrix(0,7,18)
boxpadraototal<-matrix(NA,8,72)
boxpadraoq1<-fvar(recq1,slidep)
boxpadraoq2<-fvar(recq2,slidep)
boxpadraoq3<-fvar(recq3,slidep)
boxpadraoq4<-fvar(recq4,slidep)

for(i in 1:8){
  x<-0
  for(j in 1:18){
    boxpadraototal[i,x+j]<-boxpadraoq1[i,j]
    boxpadraototal[i,x+j+2]<-boxpadraoq3[i,j]
    x<-x+3
  }
}

for(i in 1:7){
  x<-0
  for(j in 1:18){
    boxpadraototal[i,x+j+1]<-boxpadraoq2[i,j]
    boxpadraototal[i,x+j+3]<-boxpadraoq4[i,j]
    x<-x+3
  }
}


pdf("6-Boxplot por quartil.pdf",width = 14, height = 10)
par(mfrow=c(3,6),mar=c(4.5,5,4.5,0),bty="l", cex.axis=1,cex.lab=1.5)
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,1:4])),ylim=c(0,.13),xaxt = "n",xlab="20A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,5:8])),ylim=c(0,.13),xaxt="n",xlab="56A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,9:12])),ylim=c(0,.13),xaxt="n",xlab="84A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,13:16])),ylim=c(0,.13),xaxt="n",xlab="88A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,17:20])),ylim=c(0,.13),xaxt="n",xlab="154A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,21:24])),ylim=c(0,.13),xaxt="n",xlab="156A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))

boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,25:28])),ylim=c(0,.13),xaxt="n",xlab="20B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,29:32])),ylim=c(0,.13),xaxt="n",xlab="56B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,33:36])),ylim=c(0,.13),xaxt="n",xlab="84B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,37:40])),ylim=c(0,.13),xaxt="n",xlab="88B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,41:44])),ylim=c(0,.13),xaxt="n",xlab="154B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,45:48])),ylim=c(0,.13),xaxt="n",xlab="156B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))

boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,49:52])),ylim=c(0,.13),xaxt="n",xlab="20C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,53:56])),ylim=c(0,.13),xaxt="n",xlab="56C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,57:60])),ylim=c(0,.13),xaxt="n",xlab="84C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,61:64])),ylim=c(0,.13),xaxt="n",xlab="88C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,65:68])),ylim=c(0,.13),xaxt="n",xlab="154C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxpadraototal[1:8,69:72])),ylim=c(0,.13),xaxt="n",xlab="156C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
dev.off()

#Não usar este gráfico
pdf("7-Boxplot por quartil.pdf",width = 14, height = 10)
par(mfrow=c(2,2),mar=c(4.5,5,4.5,0),bty="l", cex.axis=1,cex.lab=1.5)
boxplot(x = as.list(as.data.frame(boxpadraoq1)),ylim=c(0,.11),xaxt = "n",
        ylab="Variância média do quartil (mm)",xlab="Quartil 1")
axis(1,at=1:18,labels=c(1:18))
boxplot(x = as.list(as.data.frame(boxpadraoq2)),ylim=c(0,.11),xaxt = "n",
        ylab="Variância média do quartil (mm)",xlab="Quartil 2")
axis(1,at=1:18,labels=c(1:18))
boxplot(x = as.list(as.data.frame(boxpadraoq3)),ylim=c(0,.11),xaxt = "n",
        ylab="Variância média do quartil (mm)",xlab="Quartil 3")
axis(1,at=1:18,labels=c(1:18))
boxplot(x = as.list(as.data.frame(boxpadraoq4)),ylim=c(0,.11),xaxt = "n",
        ylab="Variância média do quartil (mm)",xlab="Quartil 4")
axis(1,at=1:18,labels=c(1:18))
dev.off()

#Não usar este gráfico
pdf("8-Boxplot por quartil.pdf",width = 14, height = 10)
par(mfrow=c(1,1),mar=c(4.5,5,4.5,0),bty="l", cex.axis=1,cex.lab=1.5)
boxplot(x = as.list(as.data.frame(boxpadraototal)),ylim=c(0,.11),xaxt = "n",
        ylab="Variância média do quartil (mm)",xlab="(Q1|Q2|Q3|Q4)*(Padrões)")
axis(1,at=1:72,labels=c(1:72))
dev.off()

boxqtdeq1<-matrix(0,8,18)
boxqtdeq2<-matrix(0,7,18)
boxqtdeq3<-matrix(0,8,18)
boxqtdeq4<-matrix(0,7,18)
boxqtdetotal<-matrix(NA,8,72)
boxqtdeq1<-fvar(recq1,slideq)
boxqtdeq2<-fvar(recq2,slideq)
boxqtdeq3<-fvar(recq3,slideq)
boxqtdeq4<-fvar(recq4,slideq)

for(i in 1:8){
  x<-0
  for(j in 1:18){
    boxqtdetotal[i,x+j]<-boxqtdeq1[i,j]
    boxqtdetotal[i,x+j+2]<-boxqtdeq3[i,j]
    x<-x+3
  }
}

for(i in 1:7){
  x<-0
  for(j in 1:18){
    boxqtdetotal[i,x+j+1]<-boxqtdeq2[i,j]
    boxqtdetotal[i,x+j+3]<-boxqtdeq4[i,j]
    x<-x+3
  }
}

#Não usar este gráfico
pdf("9-Boxplot por quartil.pdf",width = 12, height = 10)
par(mfrow=c(3,6),mar=c(4.5,5,4.5,0),bty="l", cex.axis=1,cex.lab=1.5)
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,1:4])),ylim=c(0,.11),xaxt = "n",xlab="20A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,5:8])),ylim=c(0,.11),xaxt="n",xlab="20B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,9:12])),ylim=c(0,.11),xaxt="n",xlab="20C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,13:16])),ylim=c(0,.11),xaxt="n",xlab="56A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,17:20])),ylim=c(0,.11),xaxt="n",xlab="56B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,21:24])),ylim=c(0,.11),xaxt="n",xlab="56C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,25:28])),ylim=c(0,.11),xaxt="n",xlab="84A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,29:32])),ylim=c(0,.11),xaxt="n",xlab="84B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,33:36])),ylim=c(0,.11),xaxt="n",xlab="84C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,37:40])),ylim=c(0,.11),xaxt="n",xlab="88A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,41:44])),ylim=c(0,.11),xaxt="n",xlab="88B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,45:48])),ylim=c(0,.11),xaxt="n",xlab="88C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,49:52])),ylim=c(0,.11),xaxt="n",xlab="154A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,53:56])),ylim=c(0,.11),xaxt="n",xlab="154B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,57:60])),ylim=c(0,.11),xaxt="n",xlab="154C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,61:64])),ylim=c(0,.11),xaxt="n",xlab="156A")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,65:68])),ylim=c(0,.11),xaxt="n",xlab="156B")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(boxqtdetotal[1:8,69:72])),ylim=c(0,.11),xaxt="n",xlab="156C")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
dev.off()

pdf("10-Boxplot por quartil.pdf",width = 8, height = 8)
par(mfrow=c(4,1),mar=c(4.5,5,4.5,0),bty="l", cex.axis=1,cex.lab=1.5)
boxplot(x = as.list(as.data.frame(boxqtdeq1)),ylim=c(0,.13),xaxt = "n",
        ylab="Variância média do quartil (mm)",xlab="Quartil 1")
axis(1,at=1:18,labels=c(1:18))
boxplot(x = as.list(as.data.frame(boxqtdeq2)),ylim=c(0,.13),xaxt = "n",
        ylab="Variância média do quartil (mm)",xlab="Quartil 2")
axis(1,at=1:18,labels=c(1:18))
boxplot(x = as.list(as.data.frame(boxqtdeq3)),ylim=c(0,.13),xaxt = "n",
        ylab="Variância média do quartil (mm)",xlab="Quartil 3")
axis(1,at=1:18,labels=c(1:18))
boxplot(x = as.list(as.data.frame(boxqtdeq4)),ylim=c(0,.13),xaxt = "n",
        ylab="Variância média do quartil (mm)",xlab="Quartil 4")
axis(1,at=1:18,labels=c(1:18))
dev.off()

#Não usar este gráfico
pdf("11-Boxplot por quartil.pdf",width = 14, height = 10) 
par(mfrow=c(1,1),mar=c(4.5,5,4.5,0),bty="l", cex.axis=1,cex.lab=1.5)
boxplot(x = as.list(as.data.frame(boxqtdetotal)),ylim=c(0,.11),xaxt = "n",
        ylab="Variância média do quartil (mm)",xlab="(Q1|Q2|Q3|Q4)*(Padrões)")
axis(1,at=1:72,labels=c(1:72))
dev.off()

Gboxqtdetotal<-matrix(NA,24,24)
for(i in 1:8){
  for(j in 1:4){
    Gboxqtdetotal[i,j]<-boxqtdetotal[i,j]
    Gboxqtdetotal[i+8,j]<-boxqtdetotal[i,j+4]
    Gboxqtdetotal[i+16,j]<-boxqtdetotal[i,j+8]
    
    Gboxqtdetotal[i,j+4]<-boxqtdetotal[i,j+12]
    Gboxqtdetotal[i+8,j+4]<-boxqtdetotal[i,j+16]
    Gboxqtdetotal[i+16,j+4]<-boxqtdetotal[i,j+20]
    
    Gboxqtdetotal[i,j+8]<-boxqtdetotal[i,j+24]
    Gboxqtdetotal[i+8,j+8]<-boxqtdetotal[i,j+28]
    Gboxqtdetotal[i+16,j+8]<-boxqtdetotal[i,j+32]
    
    Gboxqtdetotal[i,j+12]<-boxqtdetotal[i,j+36]
    Gboxqtdetotal[i+8,j+12]<-boxqtdetotal[i,j+40]
    Gboxqtdetotal[i+16,j+12]<-boxqtdetotal[i,j+44]
    
    Gboxqtdetotal[i,j+16]<-boxqtdetotal[i,j+48]
    Gboxqtdetotal[i+8,j+16]<-boxqtdetotal[i,j+52]
    Gboxqtdetotal[i+16,j+16]<-boxqtdetotal[i,j+56]
    
    Gboxqtdetotal[i,j+20]<-boxqtdetotal[i,j+60]
    Gboxqtdetotal[i+8,j+20]<-boxqtdetotal[i,j+64]
    Gboxqtdetotal[i+16,j+20]<-boxqtdetotal[i,j+68]
  }
}

#Não usar este gráfico
pdf("12-Boxplot por quartil.pdf",width = 14, height = 6)
par(mfrow=c(1,6),mar=c(4.5,5,4.5,0),bty="l", cex.axis=1,cex.lab=1.5)
boxplot(x = as.list(as.data.frame(Gboxqtdetotal[1:24,1:4])),ylim=c(0,.11),xaxt = "n",xlab="20")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(Gboxqtdetotal[1:24,5:8])),ylim=c(0,.11),xaxt="n",xlab="56")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(Gboxqtdetotal[1:24,9:12])),ylim=c(0,.11),xaxt="n",xlab="84")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(Gboxqtdetotal[1:24,13:16])),ylim=c(0,.11),xaxt="n",xlab="88")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(Gboxqtdetotal[1:24,17:20])),ylim=c(0,.11),xaxt="n",xlab="154")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
boxplot(x = as.list(as.data.frame(Gboxqtdetotal[1:24,21:24])),ylim=c(0,.11),xaxt="n",xlab="156")
axis(1,at=1:4,labels=c("q1","q2","q3","q4"))
dev.off()

#Não usar este gráfico
pdf("13-Boxplot por quartil.pdf",width = 14, height = 6)
par(mfrow=c(1,1),mar=c(4.5,5,4.5,0),bty="l", cex.axis=1,cex.lab=1.5)
boxplot(x = as.list(as.data.frame(Gboxqtdetotal)),ylim=c(0,.11),xaxt = "n",xlab="Quantidade")
axis(1,at=1:24,labels=c(1:24))
dev.off()

#Prova Real
#----------------------------------------------------------------------
pupilpr <- read.csv("pupilapr.tsv", header = TRUE, sep = "\t") #Lê arquivo: nomedavariavel <- read.csv("nomedoaqruivo.csv", header = TRUE(usa primeira linha como título), sep = "\t" (separador \t = tab)) 

pupilpr <- pupilpr[!is.na(pupilpr$MediaName),] #Filtra onde MediaName = NA
pupilpr <- pupilpr[!is.na(pupilpr$PupilLeft),] #Filtra onde PupilLeft = NA
pupilpr <- pupilpr[!is.na(pupilpr$PupilRight),] #Filtra onde PupilLeft = NA
pupilpr <- pupilpr[pupilpr$MediaName!="", ] #Filtra onde MediaName = vazio
pupilpr <- pupilpr[pupilpr$MediaName!="6x6.jpg", ] #Filtra Slide teste6x6.jpg
pupilpr$mean <- (pupilpr$PupilLeft + pupilpr$PupilRight) / 2 # Media entre pupila direita e esquerda
pupilpr$X <- NULL #Remove a coluna X (que apareceu não sei de onde)
pupilpr$PupilLeft <- NULL #Não usa mais, remove para diminuir o processamento
pupilpr$PupilRight <- NULL #Não usa mais, remove para diminuir o processamento

fvarpr = function(rec,slide){
  lr<-length(rec) #comprimento de rec
  ls<-length(slide) #Comprimento de slide
  lista<-matrix(0,lr,ls) #define matriz conforme comprimento dos conjuntos
  r<-1
  for (j in c(rec)){
    c<-1
    for(i in c(slide)){
      x1 <- pupilpr[pupilpr$ï..RecordingName==j,] #Filtra o participante
      x2 <- x1[x1$MediaName==i,] #Filtra o slide
      lista[r,c] <- var(x2$mean) #Calcula a Variância de cada participante por Slide e monta lista
      c<-c+1
    }
    r<-r+1
  }
  return(lista)
}

pr1<-fvarpr("Rec 33",slideq)
pr2<-fvarpr("Rec 34",slideq)
pr3<-fvarpr("Rec 35",slideq)

Gpr1<-matrix(0,18,1)
Gpr2<-matrix(0,18,1)
Gpr3<-matrix(0,18,1)

for(i in 1:18){
  Gpr1[i,1]<-pr1[1,i]
  Gpr2[i,1]<-pr2[1,i]
  Gpr3[i,1]<-pr3[1,i]
}

pdf("14-PROVA REAL.pdf",width = 6, height = 4)
par(mfrow=c(1,1),mar=c(4,4,1,1),bty="l",cex=0.85)
plot(c(1:18),Gq1,col="blue",type="p",xaxt="n",ylim=c(0,.12),
     xlab="Quantidade",ylab="Variância média do quartil (cm)")
lines(lowess(Gq1),col="blue",lwd=1)#lowess usa regressão polinomial ponderada localmente
lines(Gq2,col="green",type="p")
lines(lowess(Gq2),col="green",lwd=1)
lines(Gq3,col="orange",type="p")
lines(lowess(Gq3),col="orange",lwd=1)
lines(Gq4,col="red",type="p")
lines(lowess(Gq4),col="red",lwd=1)

lines(Gpr1,col="black",type="p")
lines(lowess(Gpr1),col="black",lwd=1,lty=2)
lines(Gpr2,col="brown",type="p")
lines(lowess(Gpr2),col="brown",lwd=1,lty=2)
lines(Gpr3,col="purple",type="p")
lines(lowess(Gpr3),col="purple",lwd=1,lty=2)
legend("topleft", legend=c("1º Quartil","2º Quartil","3º Quartil","4º Quartil"),cex = 0.85,
       col=c("blue","green","orange","red"),lty=1,lwd=1, bty="n")
legend("topright", legend=c("Indivíduo 1","Indivíduo 2","Indivíduo 3"),cex = 0.85,
       col=c("black","brown","purple"),lty=2,lwd=1, bty="n")
axis(1,at=1:18,labels=c("","20","","","56","","","84","","","88","","","154","","","156",""))
dev.off()

# K-means
#-----------------------------------------------------------------------------------------------------
#Com 30
recteste<-c("Rec 02","Rec 03","Rec 04","Rec 05","Rec 06","Rec 07","Rec 08","Rec 09","Rec 10","Rec 11",
            "Rec 12","Rec 13","Rec 14","Rec 15","Rec 16","Rec 17","Rec 18","Rec 19","Rec 20","Rec 22",
            "Rec 23","Rec 24","Rec 25","Rec 26","Rec 27","Rec 28","Rec 29","Rec 30","Rec 31","Rec 32")

#Com 28 (tirando Rec 27 e Rec 30 - Clusters isolados)
# recteste<-c("Rec 02","Rec 03","Rec 04","Rec 05","Rec 06","Rec 07","Rec 08","Rec 09","Rec 10","Rec 11",
#             "Rec 12","Rec 13","Rec 14","Rec 15","Rec 16","Rec 17","Rec 18","Rec 19","Rec 20","Rec 22",
#             "Rec 23","Rec 24","Rec 25","Rec 26","Rec 27","Rec 28","Rec 30","Rec 31")

#Com 25 (tirando Rec 7, Rec 17 e Rec 12 - Clusters isolados)
# recteste<-c("Rec 02","Rec 03","Rec 04","Rec 05","Rec 06","Rec 08","Rec 09","Rec 10","Rec 11",
#             "Rec 13","Rec 14","Rec 15","Rec 16","Rec 18","Rec 19","Rec 20","Rec 22",
#             "Rec 23","Rec 24","Rec 25","Rec 26","Rec 27","Rec 28","Rec 30","Rec 31")

varianciateste <- fvar(recteste,slideq)
varianciateste[9,14] <- 0.046088473 #valor médio inserido para substituir erro
#kmeansteste <- kmeans(varianciateste,4, nstart = 25) # Opção com 25 configurações iniciais
kmeansteste <- kmeans(varianciateste,4)
varianciakmeansteste <- data.frame(recteste,kmeansteste$cluster)
distanciaeclidiana <- dist(varianciateste)
cluster <- hclust(distanciaeclidiana)

pdf("Cluster Kmeans 30.pdf",width = 10, height = 8) #Gráfico OK
par(mfrow=c(1,1),mar=c(5.1,4.5,4.1,2.1),bty="l",cex.axis=1.2,cex.lab=1.2)
plot(cluster,sub = "", main="Dendograma de agrupamento em cluster",
     xlab="Indíviduos",ylab="Distância euclidiana")
rect.hclust(cluster,4)
dev.off()