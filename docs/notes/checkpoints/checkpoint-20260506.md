# Checkpoint 20260506

## Contexto e objetivo
- Repo em migração (`legacy_repo` -> workflow novo no root) com regra forte: reproduzível de input bruto + correções manuais explícitas.
- Execução via Docker (`amazon-amnesty:dev`), `renv.lock` como fonte de verdade de pacotes.
- Prioridade atual: avançar na migração do `1_mapbiomas.R` para pipeline modular sem alterar lógica substantiva (só estrutura, paths, I/O, DAG, `here()`).

## O que foi concluído nesta sessão
- Diagnóstico detalhado do `legacy_repo/code/1_mapbiomas.R`:
  - Script é monolítico, mistura pipeline com blocos exploratórios e paths pessoais hardcoded.
  - Há variáveis/trechos dangling (ex.: `control_areas2` usado com criação comentada).
- Conclusão-chave validada:
  - Muitos artefatos “aparentemente mágicos” no `1_mapbiomas` têm lógica de geração no próprio repo (principalmente entre `1_mapbiomas` e `2_empirics`), então uma faxina estrutural resolve.
  - Gargalo real que **não** tem produtor in-repo permanece `CleanCARShapes_robust/*.shp`.
- `PROBLEMS.md` (root) atualizado para deixar isso explícito:
  - `1_mapbiomas` está sujo/dangling, mas boa parte é recuperável com refatoração de pipeline.
  - Exceção crítica: `CleanCARShapes_robust`.
  - `mapbiomas_amazon_official.xlsx` documentado como benchmark/validação tardia, não como dependência do core `transitions_combined`.

## O que está pendente com o Pedro (gargalo)
- Pedido já enviado: origem e workflow de geração de `CleanCARShapes_robust` a partir do SICAR bruto.
- Informação esperada:
  - script(s)/repo/pasta de origem;
  - ordem de execução;
  - regras de limpeza (overlap, validação geométrica, deduplicação, filtros, etc.).

## Estado técnico atual (resumo rápido)
- `transitions_combined`:
  - Lógica de geração existe em `1_mapbiomas` (não é caixa-preta por definição).
  - Precisa modularização + padronização de paths para DAG novo.
- `car_all_cleaned` e `car_eligible_cleaned`:
  - Lógica de geração existe em `2_empirics`.
  - Dependem da cadeia CAR upstream que hoje para em `CleanCARShapes_robust`.
- `mapbiomas_amazon_official.xlsx`:
  - Usado para comparação/plot de série oficial, não para gerar core raster.

## Próximos passos imediatos (enquanto espera resposta do Pedro)
### Trilha A: Migração independente do `1_mapbiomas` (sem depender de CleanCAR)
1. Criar pasta `code/01_build/04_mapbiomas/`.
2. Extrair primeiro o núcleo upstream raw-only:
   - `mapbiomas tif + amazon_biome_border -> grids -> legacy -> transitions -> transitions_combined`.
3. Manter lógica idêntica (sem mudar regra), só:
   - paths relativos com `here()`;
   - I/O explícito em `data/input` e `data/intermediate`;
   - scripts modulares com responsabilidade única.
4. Criar targets no `analysis.mk` com stamps para cada etapa.
5. Rodar e validar bit a bit (ou checks estruturais consistentes) contra outputs esperados.

### Trilha B: Higiene de monólito
1. Isolar blocos exploratórios/plot ad hoc em scripts separados (ou arquivar).
2. Eliminar dependências implícitas entre blocos no mesmo arquivo.
3. Remover ambiguidade `data/processing` vs `data/input` para derivados.

## Sequência quando o Pedro responder
1. Implementar estágio explícito de geração `CleanCARShapes_robust` no workflow novo (ex.: `code/01_build/01_car/00_clean_sicar_from_raw.R`).
2. Encadear:
   - raw SICAR -> `CleanCARShapes_robust` -> `car_combined` -> `car_combined_amazonBiome2` -> `car_all_cleaned`/`car_eligible_cleaned`.
3. Ligar isso no DAG (`analysis.mk`) com dependências formais.
4. Rodar bloco CAR completo em Docker e registrar checksums/metadados.
5. Destravar downstream:
   - `02_vtn` passos 6-8;
   - depois Lavoura/NB/VNP/Pedro track conforme regra de migração gradual.

## Regra operacional para retomada (lembrete)
- Não alterar uma letra sequer da lógica substantiva nesta fase; só estrutura, modularização, paths, DAG, reprodutibilidade.
- Qualquer nova dependência de pacote: `install.packages()` + `renv::snapshot()` no host, rebuild da imagem, re-run no container.

