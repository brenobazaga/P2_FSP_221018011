# REGISTRO DE ALTERAÇÕES, LIMITAÇÕES E RECONSTRUÇÃO DA BASE DE DADOS

**Disciplina:** Finanças do Setor Público  
**Aluno:** Breno Martins Bazaga (Matrícula: 221018011)  
**Unidades Analisadas:** Municípios de Alagoinhas/BA e Juazeiro/BA | País: Filipinas (Republic of the Philippines)

## 1. Diagnóstico do Trabalho Anterior (Base Original)

Conforme verificação detalhada do material atribuído à matrícula no repositório da disciplina (arquivos MARIA FERNANDA PROVA_FSP1.pdf e anexos das Leis de Diretrizes Orçamentárias):

1. **Inexistência de Base de Dados Editável:**
   - O trabalho anterior não disponibilizou planilhas eletrônicas (.xlsx ou .csv).
   - Constatou-se apenas a inserção de quadros fragmentados em PDF, sem os arquivos de dados brutos ou tratados subjacentes.
   - **Procedimento Adotado:** Conforme item 1 das Diretrizes da Disciplina (*"Se o trabalho anterior não disponibilizar a base editável ou exigir autorização de acesso, registre a limitação e reconstrua a base a partir das fontes oficiais"*), registrou-se a limitação e procedeu-se à reconstrução integral e auditável da base de dados do absoluto zero.
2. **Inconsistências e Fragilidades Metodológicas do Trabalho Anterior:**
   - **Omissão injustificada de observações:** O trabalho anterior declarou a exclusão de anos de Juazeiro/BA alegando falta de clareza do portal Siconfi frente aos portais municipais, contrariando o rigor científico e gerando perda de graus de liberdade.
   - **Erro de Escala na Estimação da Regressão:** Na Questão 7 da prova original, a regressão da receita municipal foi estimada utilizando o PIB Estadual da Bahia ($PIB_{BA}$) em vez do PIB Municipal ($PIB_{muni}$), distorcendo a análise de elasticidade local e mascarando a dinâmica de base tributária do município.
   - **Fragilidade das Séries Internacionais (Filipinas):** Os dados nacionais das Filipinas limitavam-se a menções pontuais de dívida pública agregada e liberações orçamentárias discricionárias do DBM, sem construir a série temporal de descentralização fiscal das Unidades de Governo Local (*Local Government Units* - LGUs) e sem detalhar o marco estrutural da decisão judicial *Mandanas-Garcia Ruling* (transição de *Internal Revenue Allotment* - IRA para *National Tax Allotment* - NTA).
   - **Ausência de Metodologia Explícita de Deflação:** Não foi documentada a tabela com os fatores de correção monetária, ano-base nem as séries temporais dos índices oficiais de inflação (IPCA para o Brasil e CPI para as Filipinas).

## 2. Reconstrução e Ampliação da Base (Versão Atualizada 2015–2024)

A nova base de dados estruturada abrange o decênio mais recente com dados contábeis consolidados (2015 a 2024), incorporando:

1. **Dados Orçamentários Municipais Completos (Siconfi/STN):**
   - Receita Orçamentária Total Arrecadada.
   - Receita Tributária Própria discriminada (IPTU, ISS, ITBI, Taxas e Contribuições de Melhoria).
   - Transferências Intergovernamentais Constitucionais e Legais (FPM, Cota-Parte ICMS, Cota-Parte IPVA, Fundeb, Transferências do SUS Fundo a Fundo e Royalties).
   - Despesa Total Liquidada e Paga.
   - Despesas Correntes (Pessoal e Encargos Sociais, Outras Despesas Correntes).
   - Despesas de Capital (Investimentos, Inversões Financeiras, Amortização da Dívida).
   - Despesas por Funções Essenciais (Educação - Função 12; Saúde - Função 10).
   - Indicador de Resultado Fiscal Primário e Dívida Consolidada Líquida (DCL).
2. **Dados Socioeconômicos e Demográficos (IBGE):**
   - Série histórica do Produto Interno Bruto (PIB) Municipal a preços correntes (Tabela 5938 - SIDRA/IBGE).
   - População Residente (Censos Demográficos 2010 e 2022, com estimativas intercensitárias oficiais do IBGE).
   - Cálculo das variáveis em termos *per capita* e como proporção do PIB local (% PIB).
3. **Deflação e Valores Reais:**
   - Série de deflatores do IPCA (Índice Nacional de Preços ao Consumidor Amplo - IBGE) com **ano-base fixado em 2024 = 100**, permitindo comparabilidade temporal rigorosa sem distorção inflacionária.
4. **Série Temporal das Filipinas (BLGF / Bureau of the Treasury / PSA / Banco Mundial):**
   - Receitas Centrais do Governo Nacional e Despesas Totais.
   - Transferências Intergovernamentais aos governos locais (IRA/NTA).
   - Arrecadação Própria das LGUs (*Locally Sourced Revenues* - LSR: Real Property Tax e Business Tax).
   - Deflator oficial: *Consumer Price Index* (CPI Filipinas, base 2024 = 100).
   - PIB das Filipinas em moeda local (PHP) e População oficial (*Philippine Statistics Authority*).
