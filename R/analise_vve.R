#############################################
## Valor de uma vida estatística: uma revisão da literatura empírica para o Brasil
## Data da 1ª versão: 18/02/2021
## Data desta versão: 19/10/2022
##
## Esse código reproduz os cálculos feitos no Excel e gera os gráficos.
#############################################

library("tidyverse")
library("PRISMAstatement")
library("rsvg")
library("DiagrammeRsvg")
library("metafor")
library("here")
library("readxl")



#############################################
## Figura 1: Curva de indiferença
#############################################

x <- seq(0, 1, by = 0.001)
y <- 100/(x+0.3)
dados <- as.data.frame(cbind(x,y))
subset(dados, x == 0.37)
dados[371,]
##        x        y
## 371 0.37 149.2537
m = -100 * (0.37+0.3)^(-2)
## y - y0 = m (x - x0)
y1 <- m * x + (-0.37 * m) + 149.2537

dados <- as.data.frame(cbind(x,y,y1))
subset(dados, x == 0.43)
subset(dados, x == 0.75)

ci <-
  ggplot(dados) +
  geom_line(aes(x = x, y = y, color = "CI"), size = 1) +
  geom_line(aes(x = x, y = y1, color = "TMS"), size = 1, linetype = 2) +
  geom_vline(xintercept = 1, size = 1) + 
  geom_point(aes(x = 0.37, y = 149.2537, size = 2)) +
  geom_point(aes(x = 0.43, y = 136.9863, size = 2)) +
  geom_point(aes(x = 0.75, y = 95.2381, size = 2)) + 
  annotate("text", x = 0.395, y = 153, label = "A") +  
  annotate("text", x = 0.455, y = 140, label = "B") +
  annotate("text", x = 0.775, y = 100, label = "C") + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(0, 1, by = 0.25),
                     limits = c(0,1.1)) +
  scale_y_continuous(expand = c(0, 0)) + 
  labs(x = "Probabilidade de Sobrevivência",
       y = "Renda (unidades monetárias)") +
  scale_color_manual(name = "none",
                     values = c("CI" = "red", 
                                "TMS" = "blue")) +
  theme(
    legend.position = "none",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_line(colour = "grey"),
    panel.background = element_blank(),
    text = element_text(size = 16))

ggplot2::ggsave(filename = here("outputs/figures", 
                                "Figura 1_Curva_de_indiferença.eps"),
                plot = ci, 
                device = "eps", 
                dpi = 1200, 
                # width = 30,
                units = "cm")


#############################################
## Figura 2: PRISMA Flow chart
#############################################


## Tradução
pnl <- function(...)
  paste(..., sep = "\n")
paren <- function(n)
  sprintf("(n = %d)", n)

prisma_graph_br <-
  function (found, found_other, no_dupes, screened, screen_exclusions, 
            full_text, full_text_exclusions, qualitative, quantitative = NULL, 
            labels = NULL, extra_dupes_box = FALSE, ..., dpi = 72, font_size = 10) 
  {
    stopifnot(length(found) == 1)
    stopifnot(length(found_other) == 1)
    stopifnot(length(no_dupes) == 1)
    stopifnot(length(screened) == 1)
    stopifnot(length(screen_exclusions) == 1)
    stopifnot(length(full_text) == 1)
    stopifnot(length(full_text_exclusions) == 1)
    stopifnot(length(qualitative) == 1)
    stopifnot(is.null(quantitative) || length(quantitative) == 
                1)
    stopifnot(found == floor(found))
    stopifnot(found_other == floor(found_other))
    stopifnot(no_dupes == floor(no_dupes))
    stopifnot(screened == floor(screened))
    stopifnot(screen_exclusions == floor(screen_exclusions))
    stopifnot(full_text == floor(full_text))
    stopifnot(full_text_exclusions == floor(full_text_exclusions))
    stopifnot(qualitative == floor(qualitative))
    stopifnot(is.null(quantitative) || quantitative == floor(quantitative))
    stopifnot(found >= 0)
    stopifnot(found_other >= 0)
    stopifnot(no_dupes >= 0)
    stopifnot(screened >= 0)
    stopifnot(screen_exclusions >= 0)
    stopifnot(full_text >= 0)
    stopifnot(full_text_exclusions >= 0)
    stopifnot(qualitative >= 0)
    stopifnot(is.null(quantitative) || quantitative >= 0)
    stopifnot(no_dupes <= found + found_other)
    stopifnot(screened <= no_dupes)
    stopifnot(full_text <= screened)
    stopifnot(qualitative <= full_text)
    stopifnot(quantitative <= qualitative)
    stopifnot(screen_exclusions <= screened)
    stopifnot(full_text_exclusions <= full_text)
    if (screened - screen_exclusions != full_text) 
      warning("After screening exclusions, a different number of remaining ", 
              "full-text articles is stated.")
    if (full_text - full_text_exclusions != qualitative) 
      warning("After full-text exclusions, a different number of remaining ", 
              "articles for qualitative synthesis is stated.")
    dupes <- found + found_other - no_dupes
    labels_orig <- list(found = pnl("Registros identificados por meio", 
                                    "das bases de periódicos", paren(found)), found_other = pnl("Registros adicionais identificados", 
                                                                                                "por meio de outras fontes", paren(found_other)), no_dupes = pnl("Registros após a remoção de duplicatas", 
                                                                                                                                                                 paren(no_dupes)), dupes = pnl("Duplicatas excluidas", 
                                                                                                                                                                                               paren(dupes)), screened = pnl("Registros selecionados", paren(screened)), 
                        screen_exclusions = pnl("Registros excluídos na triagem", paren(screen_exclusions)), 
                        full_text = pnl("Trabalhos completos avaliados", "quanto a  elegibilidade", 
                                        paren(full_text)), full_text_exclusions = pnl("Trabalhos completos excluídos,", 
                                                                                      "com motivação", paren(full_text_exclusions)), qualitative = pnl("Estudos incluídos na síntese qualitativa", 
                                                                                                                                                       paren(qualitative)), quantitative = pnl("Estudos incluídos na", 
                                                                                                                                                                                               "síntese quantitativa", "[Meta-análise]", paren(quantitative)))
    for (l in names(labels)) labels_orig[[l]] <- labels[[l]]
    labels <- labels_orig
    dupes_box <- sprintf("nodups -> incex;\n    nodups [label=\"%s\"];", 
                         labels$no_dupes)
    if (extra_dupes_box) 
      dupes_box <- sprintf("nodups -> {incex; dups};\n       nodups [label=\"%s\"];\n       dups [label=\"%s\"]; {rank=same; nodups dups}", 
                           labels$no_dupes, labels$dupes)
    dot_template <- "digraph prisma {\n    node [shape=\"box\", fontsize = %d];\n    graph [splines=ortho, nodesep=1, dpi = %d]\n    a -> nodups;\n    b -> nodups;\n    a [label=\"%s\"];\n    b [label=\"%s\"]\n    %s\n    incex -> {ex; ft}\n    incex [label=\"%s\"];\n    ex [label=\"%s\"];\n    {rank=same; incex ex}\n    ft -> {qual; ftex};\n    ft [label=\"%s\"];\n    {rank=same; ft ftex}\n    ftex [label=\"%s\"];\n    qual -> quant\n    qual [label=\"%s\"];\n    quant [label=\"%s\"];\n  }"
    sprintf(dot_template, font_size, dpi, labels$found, labels$found_other, 
            dupes_box, labels$screened, labels$screen_exclusions, 
            labels$full_text, labels$full_text_exclusions, labels$qualitative, 
            labels$quantitative)
  }

prisma_br <-
  function (found, found_other, no_dupes, screened, screen_exclusions, 
            full_text, full_text_exclusions, qualitative, quantitative = NULL, 
            labels = NULL, extra_dupes_box = FALSE, ..., dpi = 72, font_size = 10) 
  {
    DiagrammeR::grViz(prisma_graph_br(found = found, found_other = found_other, 
                                      no_dupes = no_dupes, screened = screened, screen_exclusions = screen_exclusions, 
                                      full_text = full_text, full_text_exclusions = full_text_exclusions, 
                                      qualitative = qualitative, quantitative = quantitative, 
                                      labels = labels, extra_dupes_box = extra_dupes_box, dpi = dpi, 
                                      font_size = font_size, ...))
  }



## Aplicação
prsm <- 
  prisma_br(found = 25,
            found_other = 3,
            no_dupes = 24, 
            screened = 16, 
            screen_exclusions = 8, 
            full_text = 13,
            full_text_exclusions = 3, 
            qualitative = 10, 
            quantitative = 5,
            width = 800, height = 800)

filename <- here("outputs/figures", 
                 "Figura 2_PRISMA Flow.pdf")
PRISMAstatement:::prisma_pdf(prsm, filename)
knitr::include_graphics(filename)

setEPS()
postscript(here("outputs/figures", 
                "Figura 2_PRISMA Flow.eps"))
prisma_br(found = 25,
          found_other = 3,
          no_dupes = 24, 
          screened = 16, 
          screen_exclusions = 8, 
          full_text = 13,
          full_text_exclusions = 3, 
          qualitative = 10, 
          quantitative = 5,
          width = 800, height = 800)
dev.off()


#############################################
## Figuras 3 e 4: Gráficos de floresta
#############################################


## Dataframe com os valores
VVE <- read_excel(here("data", 
                       "vve_calculos.xlsx"),
                  sheet = "Cálculos para Meta-análise",
                  range = "A1:G18")

VVE$id_study <- c(1,1,2,2,2,2,2,3,3,4,4,4,4,5,5,5,5)
VVE$id_estim <- seq(1:17)

View(VVE)

FP1_VVE <- 
        forest(x = VVE$`VVE (R$ jan/2022) PPP`,
               ci.lb = VVE$`LI (R$ jan/2022)`,
               ci.ub = VVE$`LS (R$ jan/2022)`,
               slab = VVE$Trabalho,
               xlab = "Estimativas do VVE (milhões de R$ de janeiro de 2022)",
               refline = mean(VVE$`VVE (R$ jan/2022) PPP`), psize = 1.2, cex = 0.9,
               header=c("Trabalho","VVE [IC]"))

## Efeitos fixos

VVE_EF <-
  rma.uni(yi = VVE$`VVE (R$ jan/2022) PPP`, 
          vi = VVE$`Var (R$ jan/2022)`, 
          method="FE", 
          slab = VVE$Trabalho)

pdf(here("outputs/figures", 
         "Figura 3_Floresta_EF.pdf"))
par(family="mono", mar=c(4,3.2,0.5,1.3), mgp=c(3,0,0), tcl=0.15)
forest(VVE_EF,
       #main = "Modelo de Efeitos Fixos"),
       xlab = "Estimativas do VVE (milhões de R$ de janeiro de 2022)",
       header=c("Estudo","Estimativa [IC 95%]"))
dev.off()

svg(here("outputs/figures", 
         "Figura 3_Floresta_EF.svg"))
par(family="mono", mar=c(4,3.2,0.5,1.3), mgp=c(3,0,0), tcl=0.15)
forest(VVE_EF,
       #main = "Modelo de Efeitos Fixos"),
       xlab = "Estimativas do VVE (milhões de R$ de janeiro de 2022)",
       header=c("Estudo","Estimativa [IC 95%]"))
dev.off()


setEPS()
postscript(here("outputs/figures", 
                "Figura 3_Floresta_EF.eps"))
par(mar=c(4,3.2,0.5,1.3), mgp=c(3,0,0), tcl=0.15)
forest(VVE_EF,
       #main = "Modelo de Efeitos Fixos"),
       xlab = "Estimativas do VVE (milhões de R$ de janeiro de 2022)",
       header=c("Estudo","Estimativa [IC 95%]"))
dev.off()


## Efeitos aleatórios
## As formas de cálculo da "variabilidade" entre estudos do metafor estão 
## zerando o tau2 tanto no rma.uni quanto no rma.mv. 
## A forma de o resultado ficar consistente com o feito em Excel foi
## fixar o tau2 diretamente.

tau_2 <- var(VVE$`VVE (R$ jan/2022) PPP`)

VVE_EA <-
  rma.uni(yi = VVE$`VVE (R$ jan/2022) PPP`, 
          vi = VVE$`Var (R$ jan/2022)`, 
          slab = VVE$Trabalho,
          tau2 = tau_2)

pdf(here("outputs/figures", 
         "Figura 4_Floresta_EA.pdf"))
par(family="mono", mar=c(4,3.2,0.5,1.3), mgp=c(3,0,0), tcl=0.15)
forest(VVE_EA,
       #main = "Modelo de Efeitos Aleatórios",
       xlab = "Estimativas do VVE (milhões de R$ de janeiro de 2022)",
       header=c("Estudo","Estimativa [IC 95%]"))
dev.off() 


svg(here("outputs/figures", 
         "Figura 4_Floresta_EA.svg")) 
par(family="mono", mar=c(4,3.2,0.5,1.3), mgp=c(3,0,0), tcl=0.15)
forest(VVE_EA,
       #main = "Modelo de Efeitos Aleatórios",
       xlab = "Estimativas do VVE (milhões de R$ de janeiro de 2022)",
       header=c("Estudo","Estimativa [IC 95%]"))
dev.off() 



setEPS()
postscript(here("outputs/figures", 
                "Figura 4_Floresta_EA.eps"))
par(mar=c(4,3.2,0.5,1.3), mgp=c(3,0,0), tcl=0.15)
forest(VVE_EA,
       #main = "Modelo de Efeitos Fixos"),
       xlab = "Estimativas do VVE (milhões de R$ de janeiro de 2022)",
       header=c("Estudo","Estimativa [IC 95%]"))
dev.off()