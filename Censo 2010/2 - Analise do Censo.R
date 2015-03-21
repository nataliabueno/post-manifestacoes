rm(list=ls());gc()
########################################################
# Carregando pacotes e definindo opcoes
options(scipen=999,
        stringsAsFactors=FALSE)
library(data.table)
library(descr)

#######################################################
# Definindo diretorio de trabalho
setwd("C:/users/hellen/Dropbox/Posts/Post - Perfil Manifestacoes/Censo 2010")

########################################################
# Abrindo os bancos

UF <- c("RJ", "MG", "RS", "PE", "CE", "BA", "DF")
censos <- fread("censo2010_SP2-RM_pes.csv")

for(uf in UF){
        print(uf)
        censos <- rbind(censos, 
                        fread(paste0("censo2010_", uf, "_pes.csv"))
                        )
} 

#######################################################
# Recodificacao de variaveis

# idade
censos[ , faixas_idade_2013 := cut(idade, 
                                   breaks = c(0, 13, 24, 29, 39, 130)
)
]

censos[ , faixas_idade_2015 := cut(idade, 
                                   breaks = c(0, 11, 20, 25, 35, 50, 130)
)
]

# Renda individual
censos[ , faixas_rendaInd_SMs := cut(rendaTotalSM, 
                                     breaks = c(0, 2, 5, 10, 10000)
)
]

# Renda domiciliar per capita
censos[ , faixas_rendaDom_SMs := cut(rendaDomSm, 
                                     breaks = c(0, 2, 5, 10, 10000)
)
]

########################################################
# Analise para as 7 capitais + DF

# sexo
censos[idade >= 14, freq(sexo, peso)]

# idade
censos[idade >= 14, freq(faixas_idade_2013, peso)]

# educacao
censos[idade >= 14, freq(educ_rec, peso)]

# renda individual 
censos[idade >= 14, freq(faixas_rendaInd_SMs, peso)]

# renda domiciliar per capita 
censos[idade >= 14, freq(faixas_rendaDom_SMs, peso)]

# cor
censos[idade >= 14, freq(sexo, peso)]

##################################################################
# Apenas municipio de SP

# sexo
censos[UF == "SP2-RM" & idade >= 12, freq(sexo, peso)]

# idade
censos[UF == "SP2-RM" & idade >= 12, freq(faixas_idade_2015, peso)]

# educacao
censos[UF == "SP2-RM" & idade >= 12, freq(educ_rec, peso)]

# renda individual 
censos[UF == "SP2-RM" & idade >= 12, freq(faixas_rendaInd_SMs, peso)]

# renda domiciliar per capita 
censos[UF == "SP2-RM" & idade >= 12, freq(faixas_rendaDom_SMs, peso)]

# cor
censos[UF == "SP2-RM" & idade >= 12, freq(sexo, peso)]
