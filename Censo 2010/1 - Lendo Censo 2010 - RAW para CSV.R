rm(list=ls())
options(scipen=999,
        stringsAsFactors=FALSE)
library(RCurl)
library(downloader)
library(SAScii)
library(bit64)      # para que o data.table possa comportar integers de 64 bits
library(data.table) # data.frames mais eficientes...

###########################################

setwd("C:/users/hellen/Dropbox/Posts/Post - Perfil Manifestacoes/Censo 2010")

###########
# Define arquivos temprários que guardarão os dicionários de códigos
inputPess <- tempfile()

# Função que lê os microdados e já os salva diretamente no banco de dados 
source("read.SAScii.csv2.R")

# Leitura dos dados para o SAS 
download( "https://raw.github.com/ajdamico/usgsd/master/Censo%20Demografico/SASinputPes.txt" , inputPess )

###########

url_base <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/"
UF <- c("SP2-RM", "RJ", "MG", "RS", "PE", "CE", "BA", "DF")

# Codigo das capitais de cada uma das UFs -- segundo os codigos da 
# Divisao Territorial Brasileira (DTB), IBGE
Cod_Capital <- c(50308, 04557, 06200,
                 14902, 11606, 04400,
                 27408, 00108)

# Carrega uma planilha CSV que contem o nome das variaveis de interesse 
# e deflator 
variaveis <- read.csv2("variaveis_utilizadas.csv",
                       header=TRUE, dec=",")

# lista o nome das variáveis que serao abertas
variaveis_nome <- as.character(variaveis)
variaveis_nome <- variaveis_nome[grep("[[:alpha:]]",variaveis_nome)]
variaveis_nome  <- tolower(variaveis_nome)

###################################################


for(j in 1:length(UF)){
        
        cat(paste("============================== Lendo dados de", UF[j], "\n"))
        url <- paste0(url_base, UF[j], ".zip")
        
        zipDados <- tempfile()
        dirDados <- tempdir()
        
        # Download dos dados
        cat(paste("Fazendo download dos dados\n"))
        download(url, zipDados)
        
        # Extracao dos dados
        cat("Descompactando\n")
        unzipped <- unzip(zipDados, exdir = dirDados)
        
        while(length(unzipped)==0){
                cat(paste("Fazendo download dos dados\n"))
                download(url, zipDados)
                
                # Extracao dos dados
                cat("Descompactando\n")
                unzipped <- unzip(zipDados, exdir = dirDados)
        }
                
        # Listando arquivos txt (RAW data) para pessoas
        dados_pes <- unzipped[4]
        
        ###########
        # Lendo e salvando os dados
        cat("Preparando para ler o RAW Data\n")
        tempo_inicio <- Sys.time()
        arquivo_pes <- tempfile()
        

        # "Importando os dados de pessoas para o CSV
        cat("Lendo e salvando os dados\n")
        read.SAScii.csv2(
                fn = dados_pes ,
                sas_ri = inputPess ,
                tl = TRUE ,
                select.cases   = paste("v0002 ==", Cod_Capital[j]), #infoma a capital da UF
                select.columns = variaveis_nome,
                nome_arquivo = arquivo_pes
        )
        
        tempo_fim <- Sys.time()
        print(tempo_fim-tempo_inicio)
        
        ########################################################
        # Abrindo o banco e recodificando variaveis
        cat("Abrindo o banco e recodificando variaveis\n")
        censoMunic <- fread(arquivo_pes)
        
        #################################
        # Recodificacoes
        
        cat("Recodificando...\n")
        
        #Iniciando as recodificacoes 
        setnames(censoMunic, names(censoMunic), tolower(names(censoMunic)))
        
        #sexo 
        censoMunic$sexo       <- ifelse(censoMunic[[variaveis$sexo]] == variaveis$sexo_h, 1, 0)
        
        #cor 
        censoMunic$cor          <- censoMunic[[variaveis$cor]] 
        censoMunic$ind_brancos  <- ifelse(
                (  (censoMunic$cor == variaveis$brancos) | (censoMunic$cor == variaveis$amarelos)  ), 
                1, 0)
        
        #idade 
        censoMunic$idade      <- censoMunic[[variaveis$idade]] 
        
        #educ 
        censoMunic$educ          <- censoMunic[[variaveis$educ]] 
        censoMunic$educ_rec <- 0
        censoMunic$educ_rec[censoMunic$educ == variaveis$medio_incomp]         <-  1
        censoMunic$educ_rec[censoMunic$educ == variaveis$medio_com_sup_incomp] <-  2
        censoMunic$educ_rec[censoMunic$educ == variaveis$superior]             <-  3 
        censoMunic$educ_rec[censoMunic$educ == 5]                              <-  NA 
                
        #id do domicilio 
        censoMunic$id_dom     <- censoMunic[[tolower(variaveis$id_dom)]] 
        
        #peso 
        censoMunic$peso       <- censoMunic[[variaveis$peso]]
        
        # rendas...
        #renda individual de todas as fontes
        censoMunic$rendaTotal   <- censoMunic[[variaveis$rendaTotal]] * variaveis$deflator
        
        #renda domiciliar per capita
        censoMunic$rendaDom     <- censoMunic[[variaveis$rendaDom]] * variaveis$deflator
        
        #renda individual de todas as fontes (em salários mínimos de 2010)
        censoMunic$rendaTotalSM <- censoMunic[[variaveis$rendaTotalSM]]
        
        #renda domiciliar per capita (em salários mínimos de 2010)
        censoMunic$rendaDomSm   <- censoMunic[[variaveis$rendaDomSm]]
        
        #deletando variaveis
        censoMunic[, 1:length(variaveis_nome) := NULL, with = F] 
        
        censoMunic$UF <- UF[j]
                
        cat("Salvando o banco...\n")
        write.table(censoMunic, paste0("censo2010_", UF[j], "_pes.csv"),
                  row.names=FALSE)
                  
        #liberando memoria
        rm(list=setdiff(ls(),c("variaveis", "variaveis_nome", 
                               "UF", "Cod_Capital","j", "url_base", "inputPess",
                               "read.SAScii.csv2")))
        gc()

        cat("Pronto!\n")
}


########################################################
