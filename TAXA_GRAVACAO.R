# Instala biblioteca para exportar dados em Excel
# Remova o coment�rio desta linha para instalar na 1� utiliza��o
#install.packages("xlsx") 

# 0. ================== Limpa todas vari�veis armazenadas ====================== 

if(!is.null(dev.list())) dev.off()    # Limpa plots
cat("\014")                           # Limpa console
rm(list=ls())                         # Limpa workspace

# 1. ==================== Configura diret�rio de trabalho ====================== 

setwd('C:/Users/rafae/Desktop')       # Insira o diret�rio com a barra invertida

# 2. ======================== Entrada de dados =================================

# Exporte os dados do Tobii para o mesmo diret�rio deste arquivo
# Insira o nome da base de dados com a extens�o .tsv 
base <- read.csv("Piloto-UNIFESP Data Export_pr�_4_rn.tsv", header = TRUE, sep = "\t") 

# 3. =========== Sele��o da vari�vel com maior taxa de grava��o ================

# Filtra amostras n�o gravadas
amostras_gravadas <- base[!is.na(base$Fixation.point.X),]  

# 4. ================== Identifique os participantes ===========================

# Utilize a vari�vel ParticipantName ou RecordingName  
p <- c(unique(base$Participant.name))
# rec <- c(unique(base$Recording.Name))

# 5. ===================== An�lise da perda de sinal ===========================

relatorio <- matrix(0,length(p),3)                # Cria matriz para relat�rio
id <- 1                                           # �ndice do participante

# La�o de repti��o para analisar todos os participantes
for (j in c(p)){
  # Filtra amostras totais por participante
  relatorio[id,1] <- dim(base[base$Participant.name==j,])[1]
  # Filtra amostras gravadas por participante
  relatorio[id,2] <- dim(amostras_gravadas[amostras_gravadas$Participant.name==j,])[1] 
  # Calcula taxa de grava��o
  relatorio[id,3] <- relatorio[id,2]/relatorio[id,1]
  # Incrementa um passo no la�o de repeti��o
  id <- id + 1
}
# Renomeia Matriz
row.names(relatorio)[1:length(p)] <- c(p)
colnames(relatorio)[1:3] <- c("Amostras totais","Amostras gravadas","Taxa de Grava��o")

# Imprime relat�rio no console
relatorio

# Exporta relat�rio em Excel
library("xlsx")
write.xlsx(relatorio, file="Piloto-UNIFESP Data Export_pr�_4_rn.xlsx")

