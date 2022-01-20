---
title: "Kinship Approaches in Demography"
author: |
   | Diego Alburez-Gutierrez,
   | Lab of Digital and Computational Demography,
   | Max Planck Institute for Demographic Research
date: "Formal Demography Working Group - Feb 2022"
header-includes:
  - \usepackage{amsmath}
bibliography: kinship.bib
output:
  bookdown::html_document2:
    keep_md: true
    number_sections: true
    code_folding: show
    toc: true
    toc_depth: 1
    toc_float: 
      collapsed: true
---

<style type="text/css">
  body{
  font-size: 12pt;
}
</style>



# Why kinship matters

- Socialisation, protection, and sustenance
- Inter-generational solidarity: exchanges and bequests
- Social structure and identity
- Early-life conditions $\rightarrow$ later-life outcomes

# The Elementary Structures of Kinship

Principle of demographic ergodicity:

> A closed population with unchanging mortality and fertility rates has an implied (a) Intrinsic rate of natural increase $r$ and (2) age structure. 

Goodman et al [-@goodman_family_1974]: **Stable (and non-stable) populations also have an intrinsic kinship structure**:

> A ﬁxed set of age-speciﬁc rates implies the probability that a girl aged *a* has a living mother and great-grandmother, as well as her expected number of daughters, sisters, aunts, nieces, and cousins. [@Keyfitz2005]

For example, this is the expected number of kin for an woman aged 50 ('Ego') in a female population that experiences the 2015 Swedish demographic rates ad-infinitum:


```{=html}
<div id="htmlwidget-8b4b211b646a2a3886e6" style="width:672px;height:480px;" class="DiagrammeR html-widget"></div>
<script type="application/json" data-for="htmlwidget-8b4b211b646a2a3886e6">{"x":{"diagram":"graph TD\n\n  GGM(ggm: <br>0)\n  GGM ==> GM(gm: <br>0.006)\n  GM  --> AOM(oa: <br>0.214)\n  GM  ==> M(m: <br>0.675)\n  GM  --> AYM(ya: <br>0.38)\n  AOM  --> CAOM(coa: <br>0.376)\n  M   --> OS(os: <br>0.419)\n  M   ==> E((Ego))\n  M   --> YS(ys: <br>0.467)\n  AYM  --> CAYM(cya: <br>0.42)\n  OS   --> NOS(nos: <br>0.388)\n  E   ==> D(d: <br>0.903)\n  YS   --> NYS(nys: <br>0.408)\n  D   ==> GD(gd: <br>0.034)\n  style GGM fill:#D9E9BE, stroke:#333, stroke-width:2px;\n  style GM  fill:#BF62CB, stroke:#333, stroke-width:2px, text-align: center;\n  style M   fill:#94C2DB, stroke:#333, stroke-width:2px, text-align: center\n  style D   fill:#dddbdb, stroke:#333, stroke-width:2px, text-align: center\n  style YS  fill:#79D297, stroke:#333, stroke-width:2px, text-align: center\n  style OS  fill:#79D297, stroke:#333, stroke-width:2px, text-align: center\n  style CAOM fill:#79D297, stroke:#333, stroke-width:2px, text-align: center\n  style AYM fill:#94C2DB, stroke:#333, stroke-width:2px, text-align: center\n  style AOM fill:#94C2DB, stroke:#333, stroke-width:2px, text-align: center\n  style CAYM fill:#79D297, stroke:#333, stroke-width:2px, text-align: center\n  style NOS fill:#CDA76A, stroke:#333, stroke-width:2px, text-align: center\n  style NYS fill:#CDA76A, stroke:#333, stroke-width:2px, text-align: center\n  style E   fill:#FFF, stroke:#333, stroke-width:4px, text-align: center\n  style D   fill:#CDA76A, stroke:#333, stroke-width:2px, text-align: center\n  style GD  fill:#C8695B, stroke:#333, stroke-width:2px, text-align: center"},"evals":[],"jsHooks":[]}</script>
```

## Assumptions and parameters

1. Stable vs non-stable populations
1. One-sex vs two-sex populations
1. Recursive vs matrix implementations

## Ascendants

The probability that a girl aged $a$ has a living mother in a stable population is:

\begin{equation}
M_1(a) = \int_{\alpha}^{\beta}{\frac{l(x+a)}{l(x)} e^{-rx}l(x)m(x) dx}.
(\#eq:m1a)
\end{equation}

We can visualise this in a Lexis Diagram [@Keyfitz2005]:

![](index_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

We can define $W(x)$ as the age distribution of the women of girls [@goodman_family_1974].

\begin{equation}
W(x) = e^{-rx}l(x)m(x)
    (\#eq:wx)
\end{equation}

In this case, Eq.~\ref{m1a} becomes:

\begin{equation}
M_1(a) = \int_{\alpha}^{\beta}{\frac{l(x+a)}{l(x)} W(x)  dx}.
(\#eq:m1a)
\end{equation}

This allows us to generalise to older generations. The average number of grandmothers is:

\begin{equation}
M_2(a) = \int_{\alpha}^{\beta}{ M_1(x+a) W(x) \:dx}.
\label{eq:m2a}
\end{equation}

For great-grandmothers:

\begin{equation}
M_3(a) = \int_{\alpha}^{\beta}{ M_2(x+a) W(x) \:dx}.
\label{eq:m3a}
\end{equation}

and so on.



## Descendants



### Children

\begin{equation}
B_1(a) = \int_{\alpha}^{a}{m(x) l(a-x) \: dx}
 (\#eq:b1)
\end{equation}

## Removing the stable assumption

Demographic rates change constantly in the real world - populations are rarely stable. 

## Implementation in R



## A Matrix Formulation

A recursive approach can get messy quickly For example, the expected number of younger sisters surviving to ego's age $a$ is:

\begin{equation}
S^{young}(a) = \int_{\alpha}^{\beta}{ \int_{0}^{a}{ \left[ \frac{l(x+u)}{l(x)} \right] m(x+u) l(a-u)\: du } \: e^{-rx}l(x)m(x) \: dx}
\label{eq:sis_young}
\end{equation}

where $l(x+u)/l(x)$ is the probability that the mother survives $u$ years after ego's birth. 

New developments by [@caswell_formal_2019;@caswell_formal_2020;@caswell_formal_2021;@caswell2021formal_two-sex].

# DemoKin: An R package to estimate kinship networks in stable and non-stable populations

**Iván Williams, Diego Alburez-Gutierrez, and Xi Song**; https://github.com/IvanWilli/DemoKin

# Rsoc: Demographic microsimulations in R made easy

**Tom Theile and Diego Alburez-Gutierrez**; https://github.com/tomthe/rsoc


```r
library(rsoc)
folder <- getwd()
seed <- 33

# name of the supplement-file, relative to the above folder:
supfile <- "CousinDiversity.sup" 

# run1simulationwithfile starts a simulation with the specified sup-file
rsoc::run1simulationwithfile(folder,supfile,seed)
```

# Applications

## Women's experience of child death

We [@alburez-gutierrez_womens_2021] showed that the number of children ever born to a woman aged $a$ born in cohort $c$ standing before us will be equal to the number of children who are currently alive plus the children who died before the woman reached age $a$:


\begin{equation}
\underbrace{CD(a,c,n)}_{\text{Child deaths}}= \underbrace{\sum_{x=15}^{x=a} {_1F_{x}(c,n)}}_{\text{Children born}}-\underbrace{\sum_{x=15}^{x=a} {_1F_{x}(c,n)} l_{a-x}(c+x,n)}_{\text{Children surviving or } CS(a,c,n) }
(\#eq:CD)
\end{equation}

where 

- $_1F_{x}(c,n)$ are single-year age-specific fertility rates for cohort $c$ and country $n$, at age $x$. The lower age boundary in this and all models is 15, representing the start of a woman's reproductive life. 
- $l_{a-x}(c+x,n)$ is the survival probability until age $(a-x)$ for the cohort born in year $(c+x)$ in country $n$. It is the probability that the children of a woman who gave birth at age $x$ will survive until the woman potentially reaches age $a$.   

## Sandwich Generation

Given constant age-specific schedules of fertility and mortality rates [@alburezgutierrez_sandwich_2021], we can express the probability of maternal sandwichness at age $a$, $S(a)$, as:

\begin{equation}
S(a) = \underbrace{\left(1 - \prod_{x=1}^{\kappa} [1 - m_{a-x})] \right)}_{\substack{\text{fertility risk in the}\\ \text{$\kappa$ years preceding age \emph{a}}}} \times \underbrace{M_1(a)}_{\substack{\text{Prob. that mother of ego}\\ \text{is alive when ego is \emph{a} years old}}} \times  \underbrace{\left(1-  \frac{M_1(a+\tau)}{M_1(a)}\right)}_{\substack{\text{Prob. that mother of ego}\\ \text{would die within $\tau$ years}}}    
\label{eq:sand}
\end{equation}


where $m_{a-x}$ is the fertility of women at age $a-x$ and $M_1(a)$ is the probability of having a living mother at age $a$ in a stable population. 
These estimates refer to an average woman in a female population, ignoring the role of offspring mortality.
Conditional on ego's survival, $M_1{(a)}$ can be thought of as a survival probability in a life table: it has to be equal to one when $a$ is equal to zero (the mother is alive when she gives birth), and goes monotonically to zero. 
Following the GKP equations, we can estimate $M_1(a)$ given a vector of age-specific fertility rates $m_x$, survival probabilities $l_x$, and the implicit rate of population growth $r$ as:

\begin{equation}
M_1(a) = \int_{\alpha}^{\beta}{\frac{l_{x+a}}{l_x}e^{-rx}l_{x}m_{x}dx}
\label{eq:m1a}
\end{equation}

where $\alpha$ and $\beta$ represent the start and the end of the reproductive period. 
For the sake of simplicity, we can also approximate $M_1(a)$ in relation to the mean age at childbearing $\mu$.
Assuming $\mu$ to be the average length of a generation in a stable population, we can rewrite Eq. \ref{eq:m1a} as follows:

\begin{equation}
M_1(a) \approx \frac{l_{\mu + a}}{l_{\mu}}.
\label{eq:approx}
\end{equation}

Eq. \ref{eq:approx} states that the probability that a girl alive at age $a$ has a living mother is approximately equal to the probability that women in the population are alive $a$ years past the mean age at childbearing, conditional on them being alive at the mean age at childbearing.

## Other resources and studies

- Mortality shocks on kinship structure: Zagheni 
- Mortality multiplier: Verdery et. al 
- Kinship transitions: Murphy 
- Bereavement: Smith-Greenaway 
- Unemployment in family networks: Song and Caswel 
- Kinship and historical memory: Alburez-Gutierrez
- Kin availability: Kolk 
- Inheritance of demographic behaviour: Kolk 
- Bequests: Zagheni 
- Generational overlap: 
- Kinlesness: 


## References