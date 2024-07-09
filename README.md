## Estudo de caso para uma empresa de aluguel de bicicletas da cidade de Chicago

Nesse projeto foram analisados dados de um período de um ano, os dados originalmente 12 arquivos em formato .CSV foram importados dentro do R.

A etapa inicial da análise envolveu verificar se todos esses dados tinham a mesma estrutura, algo facilmente verificável através do pacote /janitor/.

### Transformando tudo em um único Dataframe para facilitar a manipulação

Com a verificação da estrutura desses dados, decidi unir todos eles em um único dataset.
Como o volume de dados era gigantesco, a operação demorou um pouco para ser executada.

* Talvez um acesso ao banco de dados SQL e Spark pudessem otimizar o processo, mas não fazia parte do escopo estruturar um banco com essas informações.

### Objetivo da análise

A empresa posssui duas modalidades de usuários, os assinantes e os esporádicos, e é importante identificar qual a diferença nos hábitos de uso desses dois grupos para poder realizar ações de marketing significativas, promoções ou novos modelos de pacotes eidentificar possíveis gargalos.
É pertinente analisar o dia da semana que a viagem foi realizada e a duração dessa viagem.

```r
    #cyclistic_infoviagens_2022 <- read.csv("cyclistic_infoviagens_2022.csv") #apenas pra facilitar leitura

    #verificar se só tem duas categorias de membro
    unique(cyclistic_infoviagens_2022$member_casual) 
    
    #excluir as colunas de lat e lon
    cyclistic_infoviagens <- cyclistic_infoviagens_2022[, -c(9,10,11,12)] 
    
    #duração da viagem
    cyclistic_infoviagens$duration <- difftime(cyclistic_infoviagens$ended_at, 
                                               cyclistic_infoviagens$started_at, units="mins") #converter pra numerico?
    
    #Também parece ser pertinente analisar os dias da semana, então vamos criar uma nova coluna
    cyclistic_infoviagens$weekday <- weekdays(as.Date(cyclistic_infoviagens$started_at))
      
    str(cyclistic_infoviagens)
```

Além de criar as novas colunas, precisamos agora proceder com a limpeza... é esperado um grande volume de dados nulos ou não disponíveis.

```r
 #Verificar colunas de inicio de viagem
    as_tibble(cyclistic_infoviagens) %>% 
                            filter((is.na(start_station_name) | start_station_name =="" | start_station_name ==" ")& 
                                     is.na(start_station_id) | (start_station_id =="" | start_station_id ==" "))

    #Verificar colunas de fim de viagem
    as_tibble(cyclistic_infoviagens) %>% 
                            filter((is.na(end_station_name) | end_station_name =="" | end_station_name ==" ")& 
                                     is.na(end_station_id) | (end_station_id =="" | end_station_id ==" "))
    
    #Verificar colunas de início e fim de viagem simultaneamente
    as_tibble(cyclistic_infoviagens) %>% 
                            filter(((is.na(end_station_name) | end_station_name =="" | end_station_name ==" ") & 
                                    (is.na(end_station_id) | end_station_id =="" | end_station_id ==" "))
                                    | 
                                  ((is.na(start_station_name)| start_station_name =="" |start_station_name ==" ") & 
                                    (is.na(start_station_id) | start_station_id =="" | start_station_id ==" ")))
    
    #Verificar se existe alguma duração negativa de viagem
    as_tibble(cyclistic_infoviagens) %>% filter(duration <= 0) 
    
    #escrever novo dataframe limpo - cláusula ! extremamente importante, queremos tudo exceto os que tem problema
    cyclistic_viagens_l <- cyclistic_infoviagens %>% 
                              filter(!(((is.na(end_station_name) | end_station_name =="" | end_station_name ==" ") & 
                                      (is.na(end_station_id) | end_station_id =="" | end_station_id ==" "))
                                      | 
                                    ((is.na(start_station_name)| start_station_name =="" |start_station_name ==" ") & 
                                      (is.na(start_station_id) | start_station_id =="" | start_station_id ==" "))
                                      |
                                    (duration <=0)
                                    ))
    
    #No caso de haver qualquer outra coluna com algum valor NA - Só funciona para NAs
    cyclistic_viagens_l <- na.omit(cyclistic_viagens_l)
    
    #Colocar uma coluna de hora de início, vai ajudar a encontrar padrões mais tarde
    cyclistic_viagens_l$hora_i <- format(as.POSIXct(cyclistic_viagens_l$started_at), format= "%H:%M:%S")
    
    #Escrever novo arquivo CSV, limpa, quase pronto para análise
    write.csv(cyclistic_viagens_l, "cyclistic_viagens_l.csv", row.names = FALSE)  
```

No processo de limpeza foram encontrados:

* Linhas sem id e nome da estação de início: 833.064
* Linhas sem id e nome da estação de fim: 892.742
* Linhas sem id e nome da estação de início ou fim juntas: 1,298,357
* Linhas onde a duração da viagem é 0 ou negativa: 531
* Linhas onde não há informação do local de saída, chegada ou a duração é zero: 1,298,665

O número de viagem com problema de preenchimento é extremamente significativo, aconselhável contato com os engenheiros de dados para verificar porque tais informações não foram registradas, já que não é possível delimitar o motivo apenas com os dados fornecidos para esta análise.

Um novo arquivo com os dados limpos foi criado para a análise (cyclistic_viagens_l), a intenção é poupar processamento no caso dos passos dessa análise precisar ser refeita, o novo dataset tem 4.369.052 registros com 11 variáveis.

### Análise exploratória de dados

Esse é o ponto de partida para os insights a seguir, obtidos basicamente usando funcções do próprio R, o pacote ggplot2 e o MS Excel.

![image](https://github.com/LuizDML/Cyclistic/assets/121454564/6a9da75a-aad9-42a8-8fee-4f3bc783be0d)

![image](https://github.com/LuizDML/Cyclistic/assets/121454564/a9f58894-503d-4a7d-b2a4-e29484de67b4)

![image](https://github.com/LuizDML/Cyclistic/assets/121454564/ca1b0145-d43c-4c27-b7cf-c01dc3de7d17)

![image](https://github.com/LuizDML/Cyclistic/assets/121454564/fb54d985-d010-464c-b998-f7f61f99eed3)

![image](https://github.com/LuizDML/Cyclistic/assets/121454564/1bee1fcf-d3c6-4c2f-8343-e9b2d2b2ad67)

![image](https://github.com/LuizDML/Cyclistic/assets/121454564/269e6d74-94e1-4793-bcbb-c0bd94ff1762)

![image](https://github.com/LuizDML/Cyclistic/assets/121454564/c6d20cc8-263d-4c0a-be8a-989d90b80c6b)

### Descobertas

* Primeiro vale notar que há 74.464 registros onde a duração da viagem é inferior a 1 minuto e a bike foi pega e devolvida no mesmo ponto, ou seja, é possível que esses usuários tenham tido algum problema ou mesmo tenham desistido de utilizar as bikes, é necessário uma pesquisa a respeito desses usuários para descobrir os motivos de tanta desistência. Para a nossa análise, esses registros serão deixados de lado por podem influenciar nas estatisticas básicas.
* Há também alguns outliers de corridas com duração muito elevada, de mais de um mês, mas são poquíssimos registros que se enquadram nesses outliers, então foram mantidos na análise porque não vão impactar na tarefa de negócios que é objeto dessa análise.
* Há uma tendência clara dos membros em utilizar as bicicletas como meio de transporte para trabalho/escola pelo cruzamento dos dias da semana onde são mais utilizadas juntamente com o horário de uso.
* O marketing deve trabalhar de modo a convencer os usuários casuais de que vale a pena utilizar as bikes para evitar trânsito, por serem menos poluentes e, principalmente, porque a media de tempo de cada corrida desses usuários é de 24 minutos - ao se tornar membro para bicicletas tradicionais você não paga pra retirar a bike e nem nenhum valor até 45 minutos de corrida (INFORMAÇÃO RETIRADA DO SITE QUE REALIZA O ALUGUEL DAS BICILETAS) - sendo muito vantajoso o plano.
* O ranking de estações mais utilizadas pelos casuais também ajuda a direcionar a campanha de marketing para lojas e estabeleciomentos específicos dessas regiões - é uma pena que o banco de dados não contem a faixa etaria dos usuários, uma vez que seria ainda mais fácil direcionar a campanha de marketing para um público extremamente selecionado.










