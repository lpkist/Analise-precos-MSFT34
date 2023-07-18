# Analise-precos-MSFT34
O repositório contém uma análise dos preços das ações MSFT34, realizada para a disciplina ME607 - Séries Temporais (mais informações em https://www.dac.unicamp.br/sistemas/catalogos/grad/catalogo2021/disciplinas/me.html#disc-me607 e https://ctruciosm.github.io/ME607). Foi realizada uma análise através do ajuste de cerca de 80 modelos, seleção daqueles que passaram pelo diagnóstico e, por fim, uma validação cruzada via rolling window para compará-los. O produto final foi um dashboard em shiny disponível em https://ruxwwj-lucas-perondi0kist.shinyapps.io/TrabalhoMicrosoft/, onde é apresentada uma situação fantasia que motivou tais análises, bem como as conclusões.

Além disso, foi realizada uma automação utilizando o GitHub Actions para enviar e-mails diariamente com as previsões dos retornos, Value at Risk (VaR) de 1% e Expected Shortfall (ES) de 1% dessas ações. Ela utiliza arquivos da pasta `workflows`, bem como atualiza dados constantes no mesmo diretório.
