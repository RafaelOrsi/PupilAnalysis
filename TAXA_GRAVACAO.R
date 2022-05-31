# Instala biblioteca para exportar dados em Excel
# Remova o comentário desta linha para instalar na 1º utilização
#install.packages("xlsx") 

# 0. ================== Limpa todas variáveis armazenadas ====================== 

if(!is.null(dev.list())) dev.off()    # Limpa plots
cat("\014")                           # Limpa console
rm(list=ls())                         # Limpa workspace

# 1. ==================== Configura diretório de trabalho ====================== 

setwd('C:/Users/rafae/Desktop')       # Insira o diretório com a barra invertida

# 2. ======================== Entrada de dados =================================

# Exporte os dados do Tobii para o mesmo diretório deste arquivo
# Insira o nome da base de dados com a extensão .tsv 
base <- read.csv("Piloto-UNIFESP Data Export_pré_4_rn.tsv", header = TRUE, sep = "\t") 

# 3. =========== Seleção da variável com maior taxa de gravação ================

# Filtra amostras não gravadas
amostras_gravadas <- base[!is.na(base$Fixation.point.X),]  

# 4. ================== Identifique os participantes ===========================

# Utilize a variável ParticipantName ou RecordingName  
p <- c(unique(base$Participant.name))
# rec <- c(unique(base$Recording.Name))

# 5. ===================== Análise da perda de sinal ===========================

relatorio <- matrix(0,length(p),3)                # Cria matriz para relatório
id <- 1                                           # Índice do participante

# Laço de reptição para analisar todos os participantes
for (j in c(p)){
  # Filtra amostras totais por participante
  relatorio[id,1] <- dim(base[base$Participant.name==j,])[1]
  # Filtra amostras gravadas por participante
  relatorio[id,2] <- dim(amostras_gravadas[amostras_gravadas$Participant.name==j,])[1] 
  # Calcula taxa de gravação
  relatorio[id,3] <- relatorio[id,2]/relatorio[id,1]
  # Incrementa um passo no laço de repetição
  id <- id + 1
}
# Renomeia Matriz
row.names(relatorio)[1:length(p)] <- c(p)
colnames(relatorio)[1:3] <- c("Amostras totais","Amostras gravadas","Taxa de Gravação")

# Imprime relatório no console
relatorio

# Exporta relatório em Excel
library("xlsx")
write.xlsx(relatorio, file="Piloto-UNIFESP Data Export_pré_4_rn.xlsx")

