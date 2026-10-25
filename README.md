# P2 – Finanças do Setor Público (UnB, 2026.2)

**Aluno:** Breno Martins Bazaga · **Matrícula:** 221018011 · **Curso:** Ciências Contábeis (UnB)

Artigo sobre **federalismo fiscal, efeito flypaper e ilusão fiscal**, com análise empírica de **Alagoinhas/BA**, **Juazeiro/BA** e das **Filipinas** (2015–2024) e mapeamento bibliométrico da literatura com o **bibliometrix** (R).

## Ponto de partida

O trabalho parte de uma prova anterior (cópia em `trabalho_anterior_original/`). Como ela não trazia a base editável, a base foi **reconstruída a partir de fontes oficiais**. As alterações em `base_de_dados_atualizada/Registro_de_Alteracoes_e_Limitacoes.md`.

## Principais resultados

- **Bibliometria:** 3.683 artigos (1970–2024, OpenAlex); produção cresce 7,8% ao ano; quatro grupos de co-citação (instituições e desenvolvimento, teoria do federalismo fiscal, efeito flypaper, ilusão fiscal).
- **Alagoinhas e Juazeiro:** transferências somam cerca de 74% da receita bruta; autonomia tributária de 12,6% e 10,7%. A receita tributária real **dobrou** entre 2015 e 2024.
- **Efeito flypaper:** com 9 a 18 observações, não se rejeita a igualdade entre as elasticidades de transferências e de renda.
- **Filipinas:** o IRA/NTA subiu 37,9% de 2021 para 2022 (decisão Mandanas-Garcia) e recuou em 2023; o IRA respondia por 63,6% da renda corrente das unidades locais em 2021.

## Estrutura do repositório

```
├── artigo/
│   ├── Artigo Científico ... (versão ampliada).docx / .pdf
│   ├── figuras_fiscais/            figuras da análise fiscal
│   └── bibliometria/               figuras, tabelas, corpus (CSV) e scripts do bibliometrix
├── base_de_dados_atualizada/
│   ├── CSV/                        bases corrigidas e comparação com a base original
│   ├── dados_brutos_siconfi/       DCA (Siconfi) e séries do IBGE em JSON
│   ├── scripts/                    R: extração, cálculo, regressões e tabelas
│   ├── Revisao_da_Base_de_Dados.md
│   └── Registro_de_Alteracoes_e_Limitacoes.md
└── trabalho_anterior_original/     prova anterior (base de partida)
```

## Fontes de dados

| Dado | Fonte |
|---|---|
| Receitas e despesas municipais (DCA, anexos I-C, I-D, I-E) | Tesouro Nacional – Siconfi |
| População, PIB municipal e IPCA | IBGE – SIDRA (tabelas 6579, 4714, 5938, 1737) |
| PIB, população, inflação e finanças do governo central (Filipinas) | Banco Mundial – WDI |
| IRA/NTA | DBM (Filipinas), Senate Economic Planning Office e fontes secundárias |
| Renda das unidades locais (2020–2021) | DOF/BLGF (Filipinas) |
| Literatura | OpenAlex (via openalexR) |

## Reprodução

Requer **R 4.6** com os pacotes `bibliometrix`, `openalexR`, `jsonlite`, `ggplot2`, `igraph` e `ggrepel`. Os scripts usam a variável de ambiente `S_DIR` (pasta de trabalho). Ordem sugerida:

1. `base_de_dados_atualizada/scripts/extract_br.R` → extrai o Siconfi (JSONs já versionados em `dados_brutos_siconfi/`).
2. `build_base.R` e `build_ph.R` → montam as bases dos municípios e das Filipinas.
3. `econ.R`, `econ2.R`, `mktables.R` → indicadores, regressões, figuras e tabelas.
4. `artigo/bibliometria/scripts/oa_fetch2.R` → coleta na OpenAlex; `bib*.R` → análises e figuras do bibliometrix.

## Decisões de tratamento

- Valores em **reais de 2024**, deflacionados pela média anual do IPCA (Filipinas: inflação ao consumidor do Banco Mundial).
- Receita **exceto intraorçamentária**; despesa **liquidada**.
- Dado ausente **nunca é substituído por zero**. O PIB municipal de 2024 não foi divulgado pelo IBGE e está vazio; a população de 2023 é interpolada e sinalizada.
- O plano de contas da receita mudou durante o período; IPTU, ISS e ITBI foram identificados pelo texto das contas, o que limita a comparação entre 2015–2017 e os anos seguintes.

## Limitações

IRRF contabilizado como receita tributária própria; valores do IRA de 2015, 2016 e 2018–2020 vêm de fontes secundárias e devem ser conferidos nos Local Budget Memoranda do DBM. Detalhes na seção 5 do artigo.
