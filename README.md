# Genero-nas-eleicoes-estaduais-2018
Dados sobre desigualdade de genero nas eleicoes estaduais 2018
# Essa base serve como um passo a passo para reproduzir od dados do artigo "desiguldades de genero nas eleições de 2018"
# Primeiro foi construído um subconjunto da base de dados "candidatos_legislativo 2018" como segue abaixo.
partidos <- CANDIDATOS_2018_LEGISLATIVO %>% 
  select(DS_CARGO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, 
         DS_COR_RACA, DS_GENERO, VOTOS, VOTOSRELATIVO,
         LOG_RECEITAS ,NM_URNA_CANDIDATO,SG_PARTIDO,
         SG_UF, CD_SIT_TOT_TURNO, DS_SIT_TOT_TURNO,LOG_VOTOSRELATIVO2,
         DS_OCUPACAO, TIPO_CANDIDATURA, PARTIDOS_NA_COLIGACAO, RECEITAS) %>% 
  filter(DS_CARGO == "DEPUTADO ESTADUAL") %>% 
  arrange(desc(VOTOS)) %>% 
  mutate(log2votos=log10(VOTOS), log3receitas=log10(RECEITAS))
  
  # A partir dela plotamos os dados abaixo.
 partidos %>% 
  group_by(DS_GRAU_INSTRUCAO, DS_GENERO) %>% 
  count() %>% 
  ggplot(., aes(x = reorder(DS_GRAU_INSTRUCAO, n), y = n/sum(n)*100,  fill = DS_GENERO)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey", "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, size = 7)) +
  coord_flip() +
labs(title = "OFERTA DE CANDIDATURAS MASCULINAS E FEMININAS 
POR GRAU DE INSTRUÇÃO",
       x = "GRAU DE INSTRUÇÃO",
       y = "%",
       fill = "GENERO",
       caption = "Fonte: Elaboração própria a partir de dados do TSE")
       
partidos %>% 
  group_by(DS_ESTADO_CIVIL, DS_GENERO) %>% 
  count() %>% 
  ggplot(., aes(x = reorder(DS_ESTADO_CIVIL, -n), y = n/sum(n)*100, fill = DS_GENERO)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey", "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, size = 7)) +
  labs(title = "OFERTA DE CANDIDATURAS MASCULINAS E FEMININAS
       POR ESTADO CIVIL",
       x = "ESTADO CIVIL",
       y = "%",
       fill = "GENERO",
       caption = "Fonte: Elaboração própria a partir de dados do TSE")
      
partidos %>% 
  group_by(DS_COR_RACA, DS_GENERO) %>% 
  count() %>% 
  ggplot(., aes(x = reorder(DS_COR_RACA, -n), y = n/sum(n)*100, fill = DS_GENERO)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey", "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, size = 7)) +
  labs(title = "OFERTA DE CANDIDATURAS MASCULINAS E FEMININAS
       POR COR/RAÇA",
       x = "COR/RAÇA",
       y = "%",
       fill = "GENERO",
       caption = "Fonte: Elaboração própria a partir de dados do TSE")
       
 
       
       
