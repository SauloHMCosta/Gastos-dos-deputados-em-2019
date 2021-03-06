# Gastos dos deputados em 2019
# Gastos parlamentares registrados na C�mara dos Deputados no ano de 2019
# dados retirados do site Brasil.IO, LINK: https://brasil.io/dataset/gastos-deputados/cota_parlamentar/

# Definindo o diret�rio
setwd("C:/Users/uniesacosta/Desktop/Minhas an�lises em R/Gastos dos deputados/Gastos-dos-deputados-em-2019/files")
getwd()

# Merging Multiples Csv files
library(dplyr)
library(readr)

df <- list.files(path="C:/Users/uniesacosta/Desktop/Minhas an�lises em R/Gastos dos deputados/Gastos-dos-deputados-em-2019/files", 
                 full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

# Exploring 
nrow(df) # 281453
ncol(df) # 12
colnames(df) 
'''
> colnames(df) 
 [1] "numano"            "nummes"            "datemissao"       
 [4] "txnomeparlamentar" "sgpartido"         "sguf"             
 [7] "txtdescricao"      "txtcnpjcpf"        "txtfornecedor"    
[10] "vlrdocumento"      "vlrglosa"          "vlrliquido" 
'''

head(df)

'''
numano nummes datemissao          txnomeparlament~ sgpartido sguf 
   <dbl>  <dbl> <dttm>              <chr>            <chr>     <chr>
1   2019      6 NA                  Norma Ayub       DEM       ES   
2   2019      6 NA                  Fausto Pinato    PP        SP   
3   2019      6 NA                  Iracema Portella PP        PI   
4   2019      6 NA                  F�bio Henrique   PDT       SE   
5   2019      6 NA                  Bacelar          PODE      BA   
6   2019      6 NA                  Felipe Carreras  PSB       PE   
# ... with 6 more variables: txtdescricao <chr>, txtcnpjcpf <chr>,
#   txtfornecedor <chr>, vlrdocumento <dbl>, vlrglosa <dbl>,
#   vlrliquido <dbl>

'''
summary(df)


str(df)

'''
> str(df)
tibble [281,453 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
 $ numano           : num [1:281453] 2019 2019 2019 2019 2019 ...
 $ nummes           : num [1:281453] 6 6 6 6 6 6 6 6 6 6 ...
 $ datemissao       : POSIXct[1:281453], format: NA NA ...
 $ txnomeparlamentar: chr [1:281453] "Norma Ayub" "Fausto Pinato" "Iracema Portella" "F�bio Henrique" ...
 $ sgpartido        : chr [1:281453] "DEM" "PP" "PP" "PDT" ...
 $ sguf             : chr [1:281453] "ES" "SP" "PI" "SE" ...
 $ txtdescricao     : chr [1:281453] "TELEFONIA" "TELEFONIA" "TELEFONIA" "TELEFONIA" ...
 $ txtcnpjcpf       : chr [1:281453] "00000000000006" "00000000000006" "00000000000006" "00000000000006" ...
 $ txtfornecedor    : chr [1:281453] "RAMAL" "RAMAL" "RAMAL" "RAMAL" ...
 $ vlrdocumento     : num [1:281453] 109.6 24.8 124.3 162.4 86.8 ...
 $ vlrglosa         : num [1:281453] 0 0 0 0 0 0 0 0 0 0 ...
 $ vlrliquido       : num [1:281453] 109.6 24.8 124.3 162.4 86.8 ...
> 
'''
# Columns Layout

colnames(df) <- c("Ano", "M�s", "Data da Emiss�o", "Parlamentar",
                  "Partido", "UF", "Descri��o", "CPF/CNPJ do Fornecedor",
                  "Fornecedor", "Valor do Documento", "Valor Glosa", 
                  "Valor L�quido")


df$`Data da Emiss�o` <- NULL


# Total de Gastos Por m�s no ano de 2019
?aggregate
Gasto_Mes <- aggregate(df$`Valor L�quido`, by=list(M�s=df$M�s), FUN=sum)
colnames(Gasto_Mes) <- c("M�s", "Gasto Total")
Gasto_Mes # Meses com maiores gastos  (10, 8, 5, 9, 4)

Gasto_Mes[order(Gasto_Mes$`Gasto Total`, decreasing = TRUE),]


# Total de Gastos Por Partido 
Gasto_partido <- aggregate(df$`Valor L�quido`, by = list(Partido = df$Partido), FUN = sum)
colnames(Gasto_partido) <- c("Partido", "Gasto Total 2019")
Gasto_partido <- Gasto_partido[order(Gasto_partido$`Gasto Total 2019`, decreasing = TRUE),]
Gasto_partido # Partidos que mais gastaram (PT, PP, PSL, PL, PSD)


# Principais Categorias de Gastos 
top_Gastos <- aggregate(df$`Valor L�quido`, by = list(Descri��o = df$Descri��o), FUN = sum)
colnames(top_Gastos) <- c("Descri��o", "Gasto Total 2019")
top_Gastos <- top_Gastos[order(top_Gastos$`Gasto Total 2019`, decreasing = TRUE),]
top_Gastos
#Principais gastos dos deputados (Emiss�o Bilhete A�reo, DIVULGA��O DA ATIVIDADE PARLAMENTAR)


install.packages("ggplot2")
library(ggplot2)

filter_Partido <- (df$Partido == "PT")| (df$Partido == "PP")| (df$Partido == "PSL")| 
  (df$Partido == "PL")| (df$Partido == "PSD")|(df$Partido == "MDB")|(df$Partido == "PSDB")|
  (df$Partido == "REPUBLICANOS")| (df$Partido == "PDT")|(df$Partido == "DEM")|(df$Partido == "PSB")

df_Partido <- df[filter_Partido, ] 

# Construindo vetor com top Gastos no ANO

top_gastos_vector = c("DIVULGA��O DA ATIVIDADE PARLAMENTAR.", 
                      "LOCA��O OU FRETAMENTO DE VE�CULOS AUTOMOTORES",
                      "MANUTEN��O DE ESCRIT�RIO DE APOIO � ATIVIDADE PARLAMENTAR",
                      "CONSULTORIAS, PESQUISAS E TRABALHOS T�CNICOS.",
                      "COMBUST�VEIS E LUBRIFICANTES.",
                      "TELEFONIA",
                      "PASSAGENS A�REAS",
                      "SERVI�OS POSTAIS",
                      "HOSPEDAGEM ,EXCETO DO PARLAMENTAR NO DISTRITO FEDERAL.",
                      "LOCA��O OU FRETAMENTO DE AERONAVES",
                      "FORNECIMENTO DE ALIMENTA��O DO PARLAMENTAR",
                      "SERVI�O DE SEGURAN�A PRESTADO POR EMPRESA ESPECIALIZADA.",
                      "SERVI�O DE T�XI, PED�GIO E ESTACIONAMENTO",
                      "ASSINATURA DE PUBLICA��ES",
                      "PASSAGENS TERRESTRES, MAR�TIMAS OU FLUVIAIS",
                      "LOCA��O OU FRETAMENTO DE EMBARCA��ES",
                      "PARTICIPA��O EM CURSO, PALESTRA OU EVENTO SIMILAR")

# Criando multiplos gr�ficos dos gastos por partido no ano de 2019
# Partidos em an�lise Top 11 Gastos sendo eles: (PT, PP, PSL, PL, PSD, MDB, PSDB, REPUBLICANOS, PDT, DEM, PSB)

setwd("C:/Users/uniesacosta/Desktop/Minhas an�lises em R/Gastos dos deputados/Gastos-dos-deputados-em-2019/Charts and tables")

for (i in top_gastos_vector) {
  filter_is <- (df_Partido$Descri��o == i)
  new_df <- df_Partido[filter_is,]
  u <- ggplot(data = new_df, aes(x = Partido, y = `Valor L�quido`)) +
    ggtitle(i)
  
  g <- u + geom_col(aes(fill = Partido))
  
  
  png(paste0(i, ".png"))
  print(g)
  dev.off()
}


write.csv(Gasto_Mes, file = "Gasto_por_Mes_em_2019.csv")
write.csv(Gasto_partido, file = "Gasto_por_Partido_em_2019.csv")
write.csv(top_Gastos, file = "Top_Gastos_em_2019.csv")
