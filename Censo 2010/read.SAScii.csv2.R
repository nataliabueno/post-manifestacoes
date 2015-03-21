# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Funcao que le microdados a partir de dados brutos (Raw Data) com
# input em SAS
# Adaptado de Anthony Damico
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

read.SAScii.csv2 <-
        function( 
                fn ,
                sas_ri , 
                beginline = 1 ,
                select.cases = NULL,
                select.columns = NULL,
                lrecl = NULL , 
                skip.decimal.division = FALSE , # skipping decimal division is a modified option
                tl = F ,			# convert all column names to lowercase?
                nome_arquivo
                
        ) {
                
                user.defined.scipen <- getOption( 'scipen' )
                
                # install.packages( c( 'SAScii' , 'descr' , 'bit64' , 'data.table') )
                # install.packages("bigmemory", repos="http://R-Forge.R-project.org")
                # install.packages("bigmemory.sri", repos="http://R-Forge.R-project.org")
                # install.packages("biganalytics", repos="http://R-Forge.R-project.org")
                # install.packages("bigalgebra", repos="http://R-Forge.R-project.org")
                # install.packages(c("BH","biglm"))
                cat("Carregando pacotes\n")
                library(SAScii)
                library(descr)
                library(bigmemory)
                library(data.table)
                library(bit64)
                
                options( scipen = 1000000 , 
                         stringsAsFactors = FALSE, 
                         bigmemory.allow.dimnames=TRUE)
                
                cat("Lendo arquivo de INPUT SAS\n")
                x <- parse.SAScii( sas_ri , beginline , lrecl )
                
                cat("Formatando as informacoes de INPUT\n")
                if( tl ) x$varname <- tolower( x$varname )
                y <- x[ !is.na( x[ , 'varname' ] ) , ]
                num.gaps <- nrow( x ) - nrow( y )
                
                if ( num.gaps > 0 ){
                        
                        x[ is.na( x[ , 'varname' ] ) , 'char' ] <- TRUE
                        x[ is.na( x[ , 'varname' ] ) , 'divisor' ] <- 1
                        x[ is.na( x[ , 'varname' ] ) , 'width' ] <- abs( x[ is.na( x[ , 'varname' ] ) , 'width' ] )
                        x[ is.na( x[ , 'varname' ] ) , 'varname' ] <- paste( 'toss' , 1:num.gaps , sep = "_" )
                        y <- x
                }
                
                w <- abs ( x$width )
                s <- 1
                e <- w[ 1 ]
                for ( i in 2:length( w ) ) {
                        s[ i ] <- s[ i - 1 ] + w[ i - 1 ]
                        e[ i ] <- e[ i - 1 ] + w[ i ]
                }
                
                cat("Transformando RAW data em CSV\n")
                temp.data <- tempfile()
                
                fwf2csv( fn , temp.data , names = x$varname , begin = s , end = e , verbose = FALSE )
                
                cat("Abrindo o CSV como data.table\n")
                dados <- fread(temp.data)
                        
                cat("Aplicando formatacao de decimais\n")
                if( skip.decimal.division ) y[ , 'divisor' ] <- 1
                for(i in 1:ncol(dados)){
                        dados[[i]] <- dados[[i]] * y[ , "divisor" ][i]
                }
                
                if(!is.null(select.cases)){
                        dados <- dados[eval(parse(text=select.cases)), ]
                }
                
                if(!is.null(select.columns)){
                        vars <- names(dados)
                        vars <- tolower(vars)
                        select.columns <- tolower(select.columns)
                        selected <- which(vars %in% select.columns)
                        dados <- dados[,selected, with=F]
                }
                
                variaveis <- names(dados)
                
                cat("Transformando o objeto em big.matrix\n")
                dados <- as.big.matrix(dados)
                colnames(dados) <- variaveis
                
                cat("Salvando em CSV2\n")
                write.big.matrix(dados, nome_arquivo, row.names = FALSE,
                                 col.names = TRUE, sep=';')
                
                options( scipen = user.defined.scipen )
                
                TRUE
        }

