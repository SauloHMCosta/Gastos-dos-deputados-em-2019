# Gastos dos deputados em 2019
# Gastos parlamentares registrados na Câmara dos Deputados no ano de 2019
# dados retirados do site Brasil.IO, LINK: https://brasil.io/dataset/gastos-deputados/cota_parlamentar/

# Definindo o diretório
setwd("C:/Users/uniesacosta/Desktop/Minhas análises em R/Gastos dos deputados/Gastos-dos-deputados-em-2019/files")
getwd()

# Merging Multiples Csv files
library(dplyr)
library(readr)

df <- list.files(path="C:/Users/uniesacosta/Desktop/Minhas análises em R/Gastos dos deputados/Gastos-dos-deputados-em-2019/files", 
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
4   2019      6 NA                  Fábio Henrique   PDT       SE   
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
 $ txnomeparlamentar: chr [1:281453] "Norma Ayub" "Fausto Pinato" "Iracema Portella" "Fábio Henrique" ...
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

colnames(df) <- c("Ano", "Mês", "Data da Emissão", "Parlamentar",
                  "Partido", "UF", "Descrição", "CPF/CNPJ do Fornecedor",
                  "Fornecedor", "Valor do Documento", "Valor Glosa", 
                  "Valor Líquido")


df$`Data da Emissão` <- NULL


# Total de Gastos Por mês no ano de 2019
?aggregate
Gasto_Mes <- aggregate(df$`Valor Líquido`, by=list(Mês=df$Mês), FUN=sum)
colnames(Gasto_Mes) <- c("Mês", "Gasto Total")
Gasto_Mes # Meses com maiores gastos  (10, 8, 5, 9, 4)

Gasto_Mes[order(Gasto_Mes$`Gasto Total`, decreasing = TRUE),]


# Total de Gastos Por Partido 
Gasto_partido <- aggregate(df$`Valor Líquido`, by = list(Partido = df$Partido), FUN = sum)
colnames(Gasto_partido) <- c("Partido", "Gasto Total 2019")
Gasto_partido <- Gasto_partido[order(Gasto_partido$`Gasto Total 2019`, decreasing = TRUE),]
Gasto_partido # Partidos que mais gastaram (PT, PP, PSL, PL, PSD)


# Principais Categorias de Gastos 
top_Gastos <- aggregate(df$`Valor Líquido`, by = list(Descrição = df$Descrição), FUN = sum)
colnames(top_Gastos) <- c("Descrição", "Gasto Total 2019")
top_Gastos <- top_Gastos[order(top_Gastos$`Gasto Total 2019`, decreasing = TRUE),]
top_Gastos
#Principais gastos dos deputados (Emissão Bilhete Aéreo, DIVULGAÇÃO DA ATIVIDADE PARLAMENTAR)


install.packages("ggplot2")
library(ggplot2)

filter_Partido <- (df$Partido == "PT")| (df$Partido == "PP")| (df$Partido == "PSL")| 
  (df$Partido == "PL")| (df$Partido == "PSD")|(df$Partido == "MDB")|(df$Partido == "PSDB")|
  (df$Partido == "REPUBLICANOS")| (df$Partido == "PDT")|(df$Partido == "DEM")|(df$Partido == "PSB")

df_Partido <- df[filter_Partido, ] 

# Construindo vetor com top Gastos no ANO

top_gastos_vector = c("DIVULGAÇÃO DA ATIVIDADE PARLAMENTAR.", 
                      "LOCAÇÃO OU FRETAMENTO DE VEÍCULOS AUTOMOTORES",
                      "MANUTENÇÃO DE ESCRITÓRIO DE APOIO À ATIVIDADE PARLAMENTAR",
                      "CONSULTORIAS, PESQUISAS E TRABALHOS TÉCNICOS.",
                      "COMBUSTÍVEIS E LUBRIFICANTES.",
                      "TELEFONIA",
                      "PASSAGENS AÉREAS",
                      "SERVIÇOS POSTAIS",
                      "HOSPEDAGEM ,EXCETO DO PARLAMENTAR NO DISTRITO FEDERAL.",
                      "LOCAÇÃO OU FRETAMENTO DE AERONAVES",
                      "FORNECIMENTO DE ALIMENTAÇÃO DO PARLAMENTAR",
                      "SERVIÇO DE SEGURANÇA PRESTADO POR EMPRESA ESPECIALIZADA.",
                      "SERVIÇO DE TÁXI, PEDÁGIO E ESTACIONAMENTO",
                      "ASSINATURA DE PUBLICAÇÕES",
                      "PASSAGENS TERRESTRES, MARÍTIMAS OU FLUVIAIS",
                      "LOCAÇÃO OU FRETAMENTO DE EMBARCAÇÕES",
                      "PARTICIPAÇÃO EM CURSO, PALESTRA OU EVENTO SIMILAR")

# Criando multiplos gráficos dos gastos por partido no ano de 2019
# Partidos em análise Top 11 Gastos sendo eles: (PT, PP, PSL, PL, PSD, MDB, PSDB, REPUBLICANOS, PDT, DEM, PSB)

setwd("C:/Users/uniesacosta/Desktop/Minhas análises em R/Gastos dos deputados/Gastos-dos-deputados-em-2019/Charts and tables")

for (i in top_gastos_vector) {
  filter_is <- (df_Partido$Descrição == i)
  new_df <- df_Partido[filter_is,]
  u <- ggplot(data = new_df, aes(x = Partido, y = `Valor Líquido`)) +
    ggtitle(i)
  
  g <- u + geom_col(aes(fill = Partido))
  
  
  png(paste0(i, ".png"))
  print(g)
  dev.off()
}


write.csv(Gasto_Mes, file = "Gasto_por_Mes_em_2019.csv")
write.csv(Gasto_partido, file = "Gasto_por_Partido_em_2019.csv")
write.csv(top_Gastos, file = "Top_Gastos_em_2019.csv")
