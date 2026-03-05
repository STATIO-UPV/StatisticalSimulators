######################### LIBRERÍAS (PUEDES AÑADIR, NO BORRAR) #########################
library(shiny)
library(shinyjs)
library(tidyverse)
library(car)
library(agricolae)

############################ SISTEMA DE TEXTOS Y TRADUCCIÓN ############################

showparams <- TRUE

# Diccionario uniforme (todas las cadenas aquí; nada de if(language()==...) con textos hardcodeados)
texts <- list(
  # Idioma (botones)
  lang_es = c(ES="ES", EN="ES", VAL="ES"),
  lang_en = c(ES="EN", EN="EN", VAL="EN"),
  lang_va = c(ES="VAL", EN="VAL", VAL="VAL"),
  
  # Título y explicación
  title = c(
    ES = "ANOVA unifactorial",
    EN = "One-way ANOVA",
    VAL = "ANOVA unifactorial"
  ),
  explanation = c(
    ES  = "ANOVA es una técnica estadística utilizada para comparar las medias de tres o más grupos con el objetivo de determinar si existen diferencias significativas entre ellos. Este método analiza la variabilidad total de los datos, separándola en variabilidad entre grupos y dentro de los grupos, para evaluar si las diferencias observadas se deben al azar o a un efecto real del factor estudiado.",
    EN  = "ANOVA is a statistical technique used to compare the means of three or more groups in order to determine whether there are significant differences between them. This method analyses the total variability of the data, separating it into variability between groups and within groups, to assess whether the differences observed are due to chance or to a real effect of the factor under study.",
    VAL = "ANOVA és una tècnica estadística utilitzada per a comparar les mitjanes de tres o més grups amb l'objectiu de determinar si existixen diferències significatives entre ells. Este mètode analitza la variabilitat total de les dades, separant-la en variabilitat entre grups i dins dels grups, per a avaluar si les diferències observades es deuen a l'atzar o a un efecte real del factor estudiat."
    ),
  
  # Botón sidebar
  button_parameters = c(
    ES = "Parámetros",
    EN = "Parameters",
    VAL = "Paràmetres"
  ),
  
  optType = c(
    ES = "Tipo de problema",
    EN = "Problem type",
    VAL = "Tipus de problema"
  ),
  opt_max = c(ES="Maximización", EN="Maximization", VAL="Maximització"),
  opt_min = c(ES="Minimización", EN="Minimization", VAL="Minimització"),
  
  n_label = c(
    ES = "Número de observaciones por grupo (n):",
    EN = "Observations per group (n):",
    VAL = "Nombre d'observacions per grup (n):"
  ),
  
  means_title = c(ES="Medias:", EN="Means:", VAL="Mitjanes:"),
  sds_title   = c(ES="Desviación estándar:", EN="Standard deviation:", VAL="Desviació estàndard:"),
  
  muA = c(ES="Grupo A (μA):", EN="Group A (μA):", VAL="Grup A (μA):"),
  muB = c(ES="Grupo B (μB):", EN="Group B (μB):", VAL="Grup B (μB):"),
  muC = c(ES="Grupo C (μC):", EN="Group C (μC):", VAL="Grup C (μC):"),
  
  sdA = c(ES="Grupo A (σA):", EN="Group A (σA):", VAL="Grup A (σA):"),
  sdB = c(ES="Grupo B (σB):", EN="Group B (σB):", VAL="Grup B (σB):"),
  sdC = c(ES="Grupo C (σC):", EN="Group C (σC):", VAL="Grup C (σC):"),
  
  posthoc = c(ES="Análisis post-hoc:", EN="Post-hoc analysis:", VAL="Anàlisi post-hoc:"),
  posthoc_lsd = c(ES="Intervalos LSD", EN="LSD intervals", VAL="Intervals LSD"),
  posthoc_tuk = c(ES="HSD de Tukey", EN="Tukey HSD", VAL="HSD de Tukey"),
  
  # Tabs
  panel1 = c(ES="Resumen", EN="Summary", VAL="Resum"),
  panel2 = c(ES="Análisis ANOVA", EN="ANOVA analysis", VAL="Anàlisi ANOVA"),
  panel3 = c(ES="Condiciones", EN="Assumptions", VAL="Condicions"),
  panel4 = c(ES="Condición Operativa Óptima", EN="Optimal operating condition", VAL="Condició Operativa Òptima"),
  
  # Headings inside tabs
  dist_data = c(ES="Distribución de los datos:", EN="Data distribution:", VAL="Distribució de les dades:"),
  anova_title = c(ES="ANOVA:", EN="ANOVA:", VAL="ANOVA:"),
  table_title = c(ES="Tabla:", EN="Table:", VAL="Taula:"),
  interpretation = c(ES="Interpretación:", EN="Interpretation:", VAL="Interpretació:"),
  posthoc_title = c(ES="Análisis post-hoc:", EN="Post-hoc analysis:", VAL="Anàlisi post-hoc:"),
  coo_title = c(ES="Condición operativa óptima:", EN="Optimal operating condition:", VAL="Condició operativa òptima:"),
  coo_dev_title = c(ES="Desviación de la Condición Operativa Óptima:", EN="Std. dev. of the optimal condition:", VAL="Desviació de la COO:"),
  coo_dist_title = c(ES="Distribución:", EN="Distribution:", VAL="Distribució:"),
  
  # Conditions headings
  independence = c(ES="Independencia:", EN="Independence:", VAL="Independència:"),
  normality = c(ES="Normalidad:", EN="Normality:", VAL="Normalitat:"),
  homosced = c(
    ES="Homocedasticidad (ANOVA residuos al cuadrado):",
    EN="Homoscedasticity (ANOVA on squared residuals):",
    VAL="Homocedasticitat (ANOVA residus al quadrat):"
  ),
  res_dist = c(ES="Distribución de los residuos al cuadrado:", EN="Squared residuals distribution:", VAL="Distribució dels residus al quadrat:"),
  res_anova_explain = c(
    ES="Para analizar la homocedasticidad realizamos un ANOVA de los residuos al cuadrado:",
    EN="To assess homoscedasticity we run an ANOVA on squared residuals:",
    VAL="Per a analitzar l'homocedasticitat fem un ANOVA dels residus al quadrat:"
  ),
  
  # Plot labels + títulos
  plot_x = c(ES="Grupo", EN="Group", VAL="Grup"),
  plot_y = c(ES="Valor", EN="Value", VAL="Valor"),
  obs_order = c(ES="Orden de observación", EN="Observation order", VAL="Ordre d'observació"),
  residuals2_y = c(ES="Residuos²", EN="Residuals²", VAL="Residus²"),
  
  independence_plot_title = c(
    ES = "Plot para Análisis de Independencia",
    EN = "Plot for Independence Assessment",
    VAL = "Gràfic per a l'Anàlisi d'Independència"
  ),
  normality_plot_title = c(
    ES = "QQ Plot para Análisis de Normalidad",
    EN = "QQ Plot for Normality Assessment",
    VAL = "QQ Plot per a l'Anàlisi de Normalitat"
  ),
  
  # Textos explicativos condiciones
  independence_text = c(
    ES  = "Este plot no debería tener tendencia si el muestreo es aleatorio simple (condición de independencia).",
    EN  = "This plot should not show trends if simple random sampling was used (independence assumption).",
    VAL = "Este gràfic no hauria de tindre tendències si el mostreig és aleatori simple (condició d'independència)."
  ),
  normality_text = c(
    ES  = "Para analizar la normalidad utilizamos el papel probabilistico normal. Si todos los valores forman una recta, la distribución de nuestros datos seguirán una distribución normal. Para ello nos fijamos si están dentro del intervalo de confianza marcado con la zona azul. En caso de tener valores fuera, no podemos asegurar la condición de normalidad.",
    EN  = "To analyse normality, we use the normal probability paper. If all values form a straight line, the distribution of our data will follow a normal distribution. To do this, we check whether they are within the confidence interval marked with the blue area. If there are values outside this area, we cannot guarantee normality.",
    VAL = "Per a analitzar la normalitat utilitzem el paper *probabilistico normal. Si tots els valors formen una recta, la distribució de les nostres dades seguiran una distribució normal. Per a això ens fixem si estan dins de l'interval de confiança marcat amb la zona blava. En cas de tindre valors fora, no podem assegurar la condició de normalitat."
  ),
  
  # ANOVA / F-test blocks (Panel 2)
  f_hypotheses_title = c(ES="Hipótesis del Test F:", EN="F-test hypotheses:", VAL="Hipòtesis del Test F:"),
  h0_means_equal = c(
    ES  = "$$H_0: \\mu_1 = \\mu_2 = \\mu_3$$",
    EN  = "$$H_0: \\mu_1 = \\mu_2 = \\mu_3$$",
    VAL = "$$H_0: \\mu_1 = \\mu_2 = \\mu_3$$"
  ),
  h1_means_diff = c(
    ES  = "$$H_1: \\text{Al menos un par de medias es diferente}$$",
    EN  = "$$H_1: \\text{At least one pair of means is different}$$",
    VAL = "$$H_1: \\text{Almenys un parell de mitjanes és diferent}$$"
  ),
  f_distribution_title = c(ES="Distribución a utilizar:", EN="Distribution to use:", VAL="Distribució a utilitzar:"),
  f_rejection_title = c(ES="Calculamos las regiones de rechazo:", EN="We compute the rejection regions:", VAL="Calculem les regions de rebuig:"),
  f_critical_explain = c(
    ES  = "El F crítico es el valor de la distribución F con {df1} grados de libertad en el numerador y {df2} grados de libertad en el denominador, que deja por la derecha el error de tipo I ({alpha}).<br>$$F_{\\alpha, gl1, gl2} = qf(1 - \\alpha, gl1, gl2) = qf({p}, {df1}, {df2}) = {fcrit}$$",
    EN  = "The critical F is the value of the F distribution with {df1} degrees of freedom in the numerator and {df2} in the denominator, leaving a Type I error ({alpha}) in the right tail.<br>$$F_{\\alpha, df1, df2} = qf(1 - \\alpha, df1, df2) = qf({p}, {df1}, {df2}) = {fcrit}$$",
    VAL = "El F crític és el valor de la distribució F amb {df1} graus de llibertat al numerador i {df2} al denominador, que deixa per la dreta l'error de tipus I ({alpha}).<br>$$F_{\\alpha, gl1, gl2} = qf(1 - \\alpha, gl1, gl2) = qf({p}, {df1}, {df2}) = {fcrit}$$"
  ), 
  f_statistic_title = c(ES="Calculamos el estadístico:", EN="We compute the statistic:", VAL="Calculem l'estadístic:"),
  # =========================
  # F plot (títulos/ejes/leyenda) ✅ (nuevo)
  # =========================
  f_plot_main = c(
    ES  = "Distribución F",
    EN  = "F distribution",
    VAL = "Distribució F"
  ),
  f_plot_ylab = c(
    ES  = "Densidad",
    EN  = "Density",
    VAL = "Densitat"
  ),
  f_plot_xlab = c(
    ES  = "F",
    EN  = "F",
    VAL = "F"
  ),
  f_leg_fcrit = c(
    ES  = "F crítico",
    EN  = "Critical F",
    VAL = "F crític"
  ),
  f_leg_fcalc = c(
    ES  = "F calculado",
    EN  = "Computed F",
    VAL = "F calculat"
  ),
  f_leg_rej = c(
    ES  = "Región rechazo",
    EN  = "Rejection region",
    VAL = "Regió de rebuig"
  ),
  f_leg_pval = c(
    ES  = "p-valor",
    EN  = "p-value",
    VAL = "p-valor"
  ),
  pvalue_explain = c(
    ES = "El p-valor es el área a la derecha del estadístico F calculado.<br>
        $$P(F_{gl1,gl2} > F_{calc}) =
        1 - pf(F_{calc}, gl1, gl2) =
        1 - pf({fvalue}, {df1}, {df2}) = {pvalue}$$",
    
    EN = "The p-value is the area to the right of the computed F statistic.<br>
        $$P(F_{df1,df2} > F_{calc}) =
        1 - pf(F_{calc}, df1, df2) =
        1 - pf({fvalue}, {df1}, {df2}) = {pvalue}$$",
    
    VAL = "El p-valor és l'àrea a la dreta de l'estadístic F calculat.<br>
        $$P(F_{gl1,gl2} > F_{calc}) =
        1 - pf(F_{calc}, gl1, gl2) =
        1 - pf({fvalue}, {df1}, {df2}) = {pvalue}$$"
  ),
  
  f_pvalue_title = c(ES="Calculamos el p-valor:", EN="We compute the p-value:", VAL="Calculem el p-valor:"),
  f_plot_title = c(ES="Mostramos las regiones, el estadístico y el p-valor:", EN="We show the regions, the statistic and the p-value:", VAL="Vam mostrar les regions, l'estadístic i el p-valor:"),
  f_conclusion_title = c(ES="Conclusión:", EN="Conclusion:", VAL="Conclusió:"),
  
  # Distribution text (Panel 2) (placeholders)
  distribution_text = c(
    ES = "Vamos a trabajar con la distribución F de Snedecor con {df1} grados de libertad en el numerador y con {df2} grados de libertad en el denominador.",
    EN = "We will work with Snedecor's F distribution with {df1} degrees of freedom in the numerator and {df2} degrees of freedom in the denominator.",
    VAL = "Treballarem amb la distribució F de *Snedecor amb {df1} graus de llibertat en el numerador i amb {df2} graus de llibertat en el denominador."
  ),
  
  # ANOVA conclusion (Panel 1) based on p-value
  anova_concl_sig = c(
    ES  = "El análisis ANOVA muestra que hay una diferencia estadísticamente significativa entre los grupos (p < 0.05).\nPor lo tanto, hay al menos un par de medias diferentes (H1).",
    EN  = "ANOVA indicates a statistically significant difference between groups (p < 0.05). Therefore, at least one pair of means differs (H1).",
    VAL = "L'ANOVA mostra diferències estadísticament significatives entre grups (p < 0.05). Per tant, almenys un parell de mitjanes és diferent (H1)."
  ),
  anova_concl_nsig = c(
    ES  = "El análisis ANOVA no muestra una diferencia estadísticamente significativa entre los grupos (p ≥ 0.05).\nPor lo tanto, no se puede afirmar que las medias son diferentes (H0).",
    EN  = "ANOVA does not show a statistically significant difference between groups (p ≥ 0.05). Therefore, we cannot claim the means differ (H0).",
    VAL = "L'ANOVA no mostra diferències estadísticament significatives entre grups (p ≥ 0.05). Per tant, no podem afirmar que les mitjanes són diferents (H0)."
  ),
  
  # F-test conclusion (Panel 2) based on Fcalc vs Fcrit (texto largo)
  f_concl_reject = c(
    ES  = "Dado que el estadístico cae en la región de rechazo (p-valor < 0.05), rechazamos la hipótesis nula (H0). Hay diferencias significativas entre las medias de los grupos.",
    EN  = "Since the statistic falls in the rejection region (p-value < 0.05), we reject the null hypothesis (H0). There are significant differences between the group means.",
    VAL = "Atés que l'estadístic cau a la regió de rebuig (p-valor < 0.05), rebutgem la hipòtesi nul·la (H0). Hi ha diferències significatives entre les mitjanes dels grups."
  ),
  f_concl_fail = c(
    ES  = "Dado que el estadístico cae en la región de aceptación (p-valor > 0.05), no podemos rechazar la hipótesis nula (H0). No hay diferencias significativas entre las medias de los grupos.",
    EN  = "Since the statistic falls in the acceptance region (p-value > 0.05), we fail to reject the null hypothesis (H0). There are no significant differences between the group means.",
    VAL = "Com que l'estadístic cau en la regió d'acceptació (p-valor > 0.05), no podem rebutjar la hipòtesi nul·la (H0). No hi ha diferències significatives entre les mitjanes dels grups."
  ),
  
  # Homoscedasticity hypotheses (residuals^2 ANOVA)
  h0_var_equal = c(
    ES  = "$$H_0: \\sigma_1^2 = \\sigma_2^2 = \\sigma_3^2$$",
    EN  = "$$H_0: \\sigma_1^2 = \\sigma_2^2 = \\sigma_3^2$$",
    VAL = "$$H_0: \\sigma_1^2 = \\sigma_2^2 = \\sigma_3^2$$"
  ),
  h1_var_diff = c(
    ES  = "$$H_1: \\text{Al menos un par de varianzas es diferente}$$",
    EN  = "$$H_1: \\text{At least one pair of variances is different}$$",
    VAL = "$$H_1: \\text{Almenys un parell de variàncies és diferent}$$"
  ),
  
  # Conclusión ANOVA residuos^2 (homocedasticidad) (también al diccionario)
  res_concl_sig = c(
    ES  = "El análisis ANOVA muestra que hay una diferencia estadísticamente significativa entre los grupos (p < 0.05).\nPor lo tanto, las distribuciones son heterocedásticas ya que hay al menos un par de varianzas diferentes (H1).",
    EN  = "ANOVA on squared residuals shows a statistically significant difference between groups (p < 0.05). Therefore, the distributions are heteroscedastic: at least one pair of variances differs (H1).",
    VAL = "L'ANOVA dels residus al quadrat mostra diferències estadísticament significatives entre grups (p < 0.05). Per tant, les distribucions són heterocedàstiques: almenys un parell de variàncies és diferent (H1)."
  ),
  res_concl_nsig = c(
    ES  = "El análisis ANOVA no muestra una diferencia estadísticamente significativa entre los grupos (p ≥ 0.05).\nPor lo tanto, las distribuciones son homocedásticas ya que no se puede afirmar que las varianzas son diferentes (H0).",
    EN  = "ANOVA on squared residuals does not show a statistically significant difference between groups (p ≥ 0.05). Therefore, the distributions are homoscedastic: we cannot claim variances differ (H0).",
    VAL = "L'ANOVA dels residus al quadrat no mostra diferències estadísticament significatives entre grups (p ≥ 0.05). Per tant, les distribucions són homocedàstiques: no podem afirmar que les variàncies són diferents (H0)."
  ),
  
  # Comparisons (post-hoc text)  (NO TOCAR si lo usas)
  # =========================
  comparison_prefix = c(ES="Comparación", EN="Comparison", VAL="Comparació"),
  vs_text = c(ES="vs", EN="vs", VAL="vs"),
  diff_sig_lsd = c(
    ES = "hay DIFERENCIA significativa (intervalos LSD no solapan).",
    EN = "there IS a significant difference (LSD intervals do not overlap).",
    VAL = "hi ha DIFERÈNCIA significativa (els intervals LSD no se solapen)."
  ),
  no_diff_sig_lsd = c(
    ES = "no hay diferencia significativa (intervalos LSD solapan).",
    EN = "there is no significant difference (LSD intervals overlap).",
    VAL = "no hi ha diferència significativa (els intervals LSD se solapen)."
  ),
  padj_na = c(
    ES = "p-ajustada no disponible.",
    EN = "adjusted p-value not available.",
    VAL = "p-ajustada no disponible."
  ),
  diff_sig_tuk = c(
    ES = "hay DIFERENCIA significativa (p-ajustada = ",
    EN = "there IS a significant difference (adjusted p-value = ",
    VAL = "hi ha DIFERÈNCIA significativa (p-ajustada = "
  ),
  no_diff_sig_tuk = c(
    ES = "no hay diferencia significativa (p-ajustada = ",
    EN = "there is no significant difference (adjusted p-value = ",
    VAL = "no hi ha diferència significativa (p-ajustada = "
  ),
  
  # =========================
  # COO (Resumen)  ✅ (nuevo, sin hardcode)
  # =========================
  no_coo_short = c(
    ES  = "No hay diferencias significativas entre las condiciones según el ANOVA.",
    EN  = "No significant differences between conditions according to ANOVA.",
    VAL = "No hi ha diferències significatives entre les condicions segons l'ANOVA."
  ),
  
  best_lsd_single_summary = c(
    ES  = "La mejor condición operativa es el grupo {pick}.",
    EN  = "The best operating condition is group {pick}.",
    VAL = "La millor condició operativa és el grup {pick}."
  ),
  best_lsd_multi_summary = c(
    ES  = "La mejor condición operativa puede ser cualquiera de: {cand_groups} (según LSD; no difieren significativamente de la mejor).",
    EN  = "The best operating condition can be any of: {cand_groups} (according to LSD; they do not differ significantly from the best).",
    VAL = "La millor condició operativa pot ser qualsevol de: {cand_groups} (segons LSD; no difereixen significativament de la millor)."
  ),
  
  best_tukey_single_summary = c(
    ES  = "La mejor condición operativa es el grupo {pick} (según Tukey HSD).",
    EN  = "The best operating condition is group {pick} (according to Tukey HSD).",
    VAL = "La millor condició operativa és el grup {pick} (segons Tukey HSD)."
  ),
  best_tukey_multi_summary = c(
    ES  = "La mejor condición operativa puede ser cualquiera de: {cand_groups} (según Tukey HSD; no difieren significativamente de la mejor).",
    EN  = "The best operating condition can be any of: {cand_groups} (according to Tukey HSD; they do not differ significantly from the best).",
    VAL = "La millor condició operativa pot ser qualsevol de: {cand_groups} (segons Tukey HSD; no difereixen significativament de la millor)."
  ),
  
  # =========================
  # COO (Pestaña COO) ✅ (EXACTOS)
  # =========================
  no_coo_full = c(
    ES  = "No hay COO, ya que no hay diferencias significativas entre los grupos según el ANOVA.",
    EN  = "There is no OOC because ANOVA shows no significant differences between groups.",
    VAL = "No hi ha COO, ja que no hi ha diferències significatives entre els grups segons l'ANOVA."
  ),
  
  no_coo_dev_exact = c(
    ES  = "No hay COO, por lo tanto no se calculará la desviación asociada.",
    EN  = "There is no OOC, therefore the associated standard deviation will not be computed.",
    VAL = "No hi ha COO; per tant, no es calcularà la desviació associada."
  ),
  
  best_lsd_single = c(
    ES  = "La mejor condición operativa es el grupo {pick} con un valor promedio de {pick_mean} (según LSD).",
    EN  = "The best operating condition is group {pick} with an average value of {pick_mean} (according to LSD).",
    VAL = "La millor condició operativa és el grup {pick} amb un valor mitjà de {pick_mean} (segons LSD)."
  ),
  
  best_lsd_multi = c(
    ES  = paste0(
      "La mejor condición operativa puede ser cualquiera de: {cand_desc} ",
      "(según LSD; no difieren significativamente de la mejor). ",
      "Si necesitas elegir una sola por criterio operativo, se suele reportar la de mejor media: {pick} ({pick_mean})."
    ),
    EN  = paste0(
      "The best operating condition can be any of: {cand_desc} ",
      "(according to LSD; they do not differ significantly from the best). ",
      "If you need to choose just one for operational reasons, it is common to report the one with the best mean: {pick} ({pick_mean})."
    ),
    VAL = paste0(
      "La millor condició operativa pot ser qualsevol de: {cand_desc} ",
      "(segons LSD; no difereixen significativament de la millor). ",
      "Si necessites triar-ne només una per criteri operatiu, se sol reportar la de millor mitjana: {pick} ({pick_mean})."
    )
  ),
  
  best_tukey_single = c(
    ES  = "La mejor condición operativa es el grupo {pick} con un valor promedio de {pick_mean} (según Tukey HSD).",
    EN  = "The best operating condition is group {pick} with an average value of {pick_mean} (according to Tukey HSD).",
    VAL = "La millor condició operativa és el grup {pick} amb un valor mitjà de {pick_mean} (segons Tukey HSD)."
  ),
  
  best_tukey_multi = c(
    ES  = paste0(
      "La mejor condición operativa puede ser cualquiera de: {cand_desc} ",
      "(según Tukey HSD; no difieren significativamente de la mejor). ",
      "Si necesitas elegir una sola por criterio operativo, se suele reportar la de mejor media: {pick} ({pick_mean})."
    ),
    EN  = paste0(
      "The best operating condition can be any of: {cand_desc} ",
      "(according to Tukey HSD; they do not differ significantly from the best). ",
      "If you need to choose just one for operational reasons, it is common to report the one with the best mean: {pick} ({pick_mean})."
    ),
    VAL = paste0(
      "La millor condició operativa pot ser qualsevol de: {cand_desc} ",
      "(segons Tukey HSD; no difereixen significativament de la millor). ",
      "Si necessites triar-ne només una per criteri operatiu, se sol reportar la de millor mitjana: {pick} ({pick_mean})."
    )
  ),
  
  coo_dev_common_exact = c(
    ES  = "Todos los grupos tienen la misma desviación. La desviación asociada a la COO es la raiz del cuadrado medio residual del ANOVA de medias: $$\\sigma_{\\text{COO}} = \\sqrt{\\text{CMR}} = \\sqrt{{ms_error}} = {deviation}$$",
    EN  = "All groups share the same standard deviation. The deviation associated with the OOC is the square root of the residual mean square from ANOVA: $$\\sigma_{\\text{OOC}} = \\sqrt{\\text{MSE}} = \\sqrt{{ms_error}} = {deviation}$$",
    VAL = "Tots els grups tenen la mateixa desviació. La desviació associada a la COO és l'arrel del quadrat mitjà residual de l'ANOVA: $$\\sigma_{\\text{COO}} = \\sqrt{\\text{CMR}} = \\sqrt{{ms_error}} = {deviation}$$"
  ),
  
  coo_var_corr_exact = c(
    ES  = paste0(
      "La varianza corregida de la COO es $$\\sigma_{\\text{COO}}^{2} = \\frac{\\overline{x}_{\\text{COO res}^{2}} \\cdot N_{\\text{total}}}{gl_{residual}} = \\frac{",
      "{mean_res2} \\cdot {n_total}}{df_res} = {var_corr}$$",
      "<br>La desviación asociada a la COO es $$\\sigma_{\\text{COO}} = \\sqrt{{var_corr}} = {dev_corr}$$"
    ),
    EN  = paste0(
      "The corrected variance of the OOC is $$\\sigma_{\\text{OOC}}^{2} = \\frac{\\overline{x}_{\\text{OOC res}^{2}} \\cdot N_{\\text{total}}}{df_{res}} = \\frac{",
      "{mean_res2} \\cdot {n_total}}{df_res} = {var_corr}$$",
      "<br>The deviation associated with the OOC is $$\\sigma_{\\text{OOC}} = \\sqrt{{var_corr}} = {dev_corr}$$"
    ),
    VAL = paste0(
      "La variància corregida de la COO és $$\\sigma_{\\text{COO}}^{2} = \\frac{\\overline{x}_{\\text{COO res}^{2}} \\cdot N_{\\text{total}}}{gl_{res}} = \\frac{",
      "{mean_res2} \\cdot {n_total}}{df_res} = {var_corr}$$",
      "<br>La desviació associada a la COO és $$\\sigma_{\\text{COO}} = \\sqrt{{var_corr}} = {dev_corr}$$"
    )
  ),
  
  coo_dist_common_exact = c(
    ES  = "La COO sigue una distribución $$N({mean_coo}, {dev_coo})$$",
    EN  = "The OOC follows a distribution $$N({mean_coo}, {dev_coo})$$",
    VAL = "La COO segueix una distribució $$N({mean_coo}, {dev_coo})$$"
  ),
  coo_dist_common_exact2 = c(
    ES  = "La COO sigue una distribución: $$N({mean_coo}, {dev_coo})$$",
    EN  = "The OOC follows a distribution: $$N({mean_coo}, {dev_coo})$$",
    VAL = "La COO segueix una distribució: $$N({mean_coo}, {dev_coo})$$"
  ),
  # Créditos
  credits = c(
    ES  = "STATIO es un Proyecto de Innovación y Mejora Educativa (PIME/25-26/562) desarrollado por el DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
    EN  = "STATIO is an Educational Innovation and Improvement Project (PIME/25-26/562) developed by DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
    VAL = "STATIO és un Projecte d'Innovació i Millora Educativa (PIME/25-26/562) desenrotllat pel DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>."
  )
)

tr <- function(id, lang) texts[[id]][[lang]]
fmt <- function(txt, ...) {
  dots <- list(...)
  for (nm in names(dots)) {
    txt <- gsub(paste0("\\{", nm, "\\}"), as.character(dots[[nm]]), txt)
  }
  txt
}

##################### USER INTERFACE ######################################

ui <- fluidPage(
  useShinyjs(),
  
  absolutePanel(
    top = 10, right = 10, fixed = TRUE,
    actionButton("lang_es", textOutput("lang_es_txt")),
    actionButton("lang_en", textOutput("lang_en_txt")),
    actionButton("lang_va", textOutput("lang_va_txt"))
  ),
  
  # CSS base de la plantilla
  tags$head(
    tags$style(HTML("
        :root{
          --sidebar-w: clamp(280px, 24vw, 420px);
        }

        #sidebarWrapper {
          width: var(--sidebar-w);
          background: #f7f7f7;
          padding: 15px;
          border-right: 1px solid #ddd;
          position: fixed;
          top: 0;
          bottom: 0;
          left: 0;
          overflow-y: auto;
          transition: transform .3s ease;
          z-index: 2000;
          transform: translateX(-100%);
        }

        #sidebarWrapper:not(.closed) {
          transform: translateX(0);
        }

        #contentWrapper {
          transition: margin-left .3s ease;
          margin-left: 0px;
        }

        #contentWrapper.shifted {
          margin-left: var(--sidebar-w);
        }

        #toggleSidebar {
          position: fixed;
          top: 10px;
          left: 10px;
          z-index: 3000;
        }
        
        /* Hace que todo dentro del sidebar use todo el ancho disponible */
        
        #sidebarWrapper .shiny-input-container {
          width: 100% !important;
        }
        
        #sidebarWrapper .form-control {
          width: 100% !important;
        }
        
        #sidebarWrapper .selectize-control {
          width: 100% !important;
        }
        
        #sidebarWrapper .irs {
          width: 100% !important;
        }
        
        /* títulos */
        #sidebarWrapper h4 {
          width: 100%;
        }
        
        /* mejora el espaciado vertical */
        #sidebarWrapper .shiny-input-container {
          margin-bottom: 15px;
        }
  "))
  ),
  
  tags$head(
    tags$style(HTML("
      .tab-content h4 { font-weight: bold; }
    "))
  ),
  
  if (showparams) {
    actionButton("toggleSidebar", textOutput("button_parameters"))
  },
  
  # Sidebar
  div(
    id = "sidebarWrapper",
    style = "padding-top: 50px;",
    if (showparams) {
      div(
        uiOutput("optType_ui"),
        uiOutput("n_ui"),
        
        h4(textOutput("means_title")),
        uiOutput("uA_ui"),
        uiOutput("uB_ui"),
        uiOutput("uC_ui"),
        
        h4(textOutput("sds_title")),
        uiOutput("sdA_ui"),
        uiOutput("sdB_ui"),
        uiOutput("sdC_ui"),
        
        uiOutput("posthoc_ui"),
        
        br()
      )
    }
  ),
  
  # Content
  div(
    id = "contentWrapper", class = "shifted",
    
    div(
      style = "padding-top:50px; margin-bottom:30px",
      h2(textOutput("title"), align = "center"),
      div(
        style = "display:flex; justify-content:center;",
        div(
          style = "border:2px solid #4a90e2; border-radius:12px; padding:12px;
                   max-width:600px; background:white; text-align:center;",
          uiOutput("explanation")
        )
      )
    ),
    
    tabsetPanel(
      tabPanel(
        textOutput("panel1_title"),
        h4(textOutput("dist_data_title")),
        plotOutput("violinPlot"),
        uiOutput("sampleStats"),
        
        br(),
        h4(textOutput("anova_title_out")),
        h5(textOutput("table_title_out")),
        verbatimTextOutput("aov"),
        
        h5(textOutput("interpretation_out")),
        textOutput("conclusionText"),
        br(),
        
        h5(textOutput("posthoc_title_out")),
        plotOutput("posthocPlot"),
        textOutput("posthocConclusion"),
        br(),
        
        h5(textOutput("coo_title_out")),
        textOutput("optimalConditionText"),
        br()
      ),
      
      tabPanel(
        textOutput("panel2_title"),
        h4(textOutput("anova_title_out")),
        h5(textOutput("table_title_out")),
        verbatimTextOutput("aov2"),
        br(),
        
        h5(textOutput("f_hypotheses_title_out")),
        uiOutput("hypothesesText"),
        br(),
        
        h5(textOutput("f_distribution_title_out")),
        textOutput("distributionText"),
        br(),
        
        h5(textOutput("f_rejection_title_out")),
        uiOutput("fCriticalText"),
        br(),
        
        h5(textOutput("f_statistic_title_out")),
        uiOutput("fRatioFormula"),
        br(),
        
        h5(textOutput("f_pvalue_title_out")),
        uiOutput("pValueTextDetailed"),
        br(),
        
        h5(textOutput("f_plot_title_out")),
        plotOutput("fDistributionPlot"),
        br(),
        
        h5(textOutput("f_conclusion_title_out")),
        textOutput("fConclusionText"),
        br()
      ),
      
      tabPanel(
        textOutput("panel3_title"),
        h4(textOutput("independence_out")),
        plotOutput("independencePlot"),
        textOutput("independenceText"),
        br(),
        
        h4(textOutput("normality_out")),
        plotOutput("normalityPlot"),
        textOutput("normalityText"),
        br(),
        
        h4(textOutput("homosced_out")),
        h5(textOutput("res_dist_out")),
        plotOutput("residualsViolinPlot"),
        
        h5(textOutput("res_anova_explain_out")),
        uiOutput("hypothesesText_res"),
        br(),
        h5(textOutput("table_title_out")),
        verbatimTextOutput("aov_res"),
        h5(textOutput("interpretation_out")),
        textOutput("conclusionText_res"),
        br()
      ),
      
      tabPanel(
        textOutput("panel4_title"),
        h4(textOutput("coo_title_out")),
        textOutput("optimalConditionText2"),
        br(),
        
        h4(textOutput("coo_dev_title_out")),
        uiOutput("optimalConditionDeviation"),
        br(),
        
        h4(textOutput("coo_dist_title_out")),
        uiOutput("cooDistribution"),
        br()
      )
    ),
    
    # Créditos + logos
    div(
      style = "margin-top:40px; text-align:center; margin-bottom:40px;",
      div(
        style = "display:flex; justify-content:center; align-items:center; gap:40px;",
        tags$img(src = "UPV.png", style = "height:85px; max-height:85px;"),
        tags$img(src = "DEIOAC.png", style = "height:65px; max-height:70px;")
      ),
      div(style = "margin-top:15px;", htmlOutput("creditos"))
    )
  )

)

############################ SERVER #######################################

server <- function(input, output) {
  
  # Panel lateral (no tocar)
  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(id = "sidebarWrapper", class = "closed")
    shinyjs::toggleClass(id = "contentWrapper", class = "shifted")
    shinyjs::runjs("setTimeout(function() { $(window).trigger('resize'); }, 350);")
  })
  
  # Idioma (no tocar)
  language <- reactiveVal("ES")
  observeEvent(input$lang_es, { language("ES") })
  observeEvent(input$lang_en, { language("EN") })
  observeEvent(input$lang_va, { language("VAL") })
  
  # Labels botones idioma
  output$lang_es_txt <- renderText({ tr("lang_es", language()) })
  output$lang_en_txt <- renderText({ tr("lang_en", language()) })
  output$lang_va_txt <- renderText({ tr("lang_va", language()) })
  
  # Textos UI
  output$title <- renderText({ tr("title", language()) })
  output$explanation <- renderUI({ HTML(tr("explanation", language())) })
  output$button_parameters <- renderText({ tr("button_parameters", language()) })
  output$means_title <- renderText({ tr("means_title", language()) })
  output$sds_title <- renderText({ tr("sds_title", language()) })
  
  output$panel1_title <- renderText({ tr("panel1", language()) })
  output$panel2_title <- renderText({ tr("panel2", language()) })
  output$panel3_title <- renderText({ tr("panel3", language()) })
  output$panel4_title <- renderText({ tr("panel4", language()) })
  
  output$f_hypotheses_title_out <- renderText({ tr("f_hypotheses_title", language()) })
  output$f_distribution_title_out <- renderText({ tr("f_distribution_title", language()) })
  output$f_rejection_title_out <- renderText({ tr("f_rejection_title", language()) })
  output$f_statistic_title_out <- renderText({ tr("f_statistic_title", language()) })
  output$f_pvalue_title_out <- renderText({ tr("f_pvalue_title", language()) })
  output$f_plot_title_out <- renderText({ tr("f_plot_title", language()) })
  output$f_conclusion_title_out <- renderText({ tr("f_conclusion_title", language()) })
  
  output$dist_data_title <- renderText({ tr("dist_data", language()) })
  output$anova_title_out <- renderText({ tr("anova_title", language()) })
  output$table_title_out <- renderText({ tr("table_title", language()) })
  output$interpretation_out <- renderText({ tr("interpretation", language()) })
  output$posthoc_title_out <- renderText({ tr("posthoc_title", language()) })
  output$coo_title_out <- renderText({ tr("coo_title", language()) })
  output$coo_dev_title_out <- renderText({ tr("coo_dev_title", language()) })
  output$coo_dist_title_out <- renderText({ tr("coo_dist_title", language()) })
  
  output$independence_out <- renderText({ tr("independence", language()) })
  output$normality_out <- renderText({ tr("normality", language()) })
  output$homosced_out <- renderText({ tr("homosced", language()) })
  output$res_dist_out <- renderText({ tr("res_dist", language()) })
  output$res_anova_explain_out <- renderText({ tr("res_anova_explain", language()) })
  
  output$creditos <- renderUI({ HTML(tr("credits", language())) })
  
  # Sidebar inputs (dinámicos para traducir labels)
  output$optType_ui <- renderUI({
    selectInput(
      "optType",
      tr("optType", language()),
      choices = setNames(
        c("Maximización", "Minimización"),
        c(tr("opt_max", language()), tr("opt_min", language()))
      )
    )
  })
  
  output$n_ui <- renderUI({
    sliderInput("n", tr("n_label", language()), value = 30, min = 1, max = 100)
  })
  
  output$uA_ui <- renderUI({ sliderInput("uA", tr("muA", language()), value = 50, min = 40, max = 60) })
  output$uB_ui <- renderUI({ sliderInput("uB", tr("muB", language()), value = 50, min = 40, max = 60) })
  output$uC_ui <- renderUI({ sliderInput("uC", tr("muC", language()), value = 50, min = 40, max = 60) })
  
  output$sdA_ui <- renderUI({ sliderInput("sdA", tr("sdA", language()), value = 5, min = 1, max = 20) })
  output$sdB_ui <- renderUI({ sliderInput("sdB", tr("sdB", language()), value = 5, min = 1, max = 20) })
  output$sdC_ui <- renderUI({ sliderInput("sdC", tr("sdC", language()), value = 5, min = 1, max = 20) })
  
  output$posthoc_ui <- renderUI({
    selectInput(
      "posthocType",
      tr("posthoc", language()),
      choices = setNames(
        c("Intervalos LSD", "HSD de Tukey"),
        c(tr("posthoc_lsd", language()), tr("posthoc_tuk", language()))
      )
    )
  })
  
  ##################### CALCULOS DATASET ####################
  
  d <- reactive({
    n <- input$n
    uA <- input$uA; uB <- input$uB; uC <- input$uC
    sdA <- input$sdA; sdB <- input$sdB; sdC <- input$sdC
    
    set.seed(123)
    
    bind_rows(
      data.frame(group = "A", value = rnorm(n, uA, sdA)),
      data.frame(group = "B", value = rnorm(n, uB, sdB)),
      data.frame(group = "C", value = rnorm(n, uC, sdC))
    )
  })
  
  ##################### VIOLIN PLOT ####################
  
  output$violinPlot <- renderPlot({
    ggplot(d(), aes(x = group, y = value, fill = group)) +
      geom_violin(trim = FALSE) +
      geom_jitter(width = 0.1, size = 0.5) +
      labs(x = tr("plot_x", language()), y = tr("plot_y", language())) +
      theme_minimal()
  })
  
  output$sampleStats <- renderUI({
    data <- d()
    means <- data %>% group_by(group) %>% summarise(mean = mean(value), .groups = "drop")
    sds   <- data %>% group_by(group) %>% summarise(sd   = sd(value),   .groups = "drop")
    
    withMathJax(HTML(paste0(
      "$$\\bar{x}_A=", round(means$mean[means$group=="A"],2),
      ",\\;\\bar{x}_B=", round(means$mean[means$group=="B"],2),
      ",\\;\\bar{x}_C=", round(means$mean[means$group=="C"],2), "$$",
      "$$\\;s_A=", round(sds$sd[sds$group=="A"],2),
      ",\\;s_B=", round(sds$sd[sds$group=="B"],2),
      ",\\;s_C=", round(sds$sd[sds$group=="C"],2), "$$"
    )))
  })
  
  ##################### ANOVA ####################
  
  anova_result <- reactive({
    aov(value ~ group, data = d())
  })
  
  p_value <- reactive({
    summary(anova_result())[[1]][["Pr(>F)"]][1]
  })
  
  output$aov  <- renderPrint({ summary(anova_result()) })
  output$aov2 <- renderPrint({ summary(anova_result()) })
  
  ##################### CONCLUSIÓN ANOVA (Panel 1) ####################
  
  output$conclusionText <- renderText({
    if (p_value() < 0.05) tr("anova_concl_sig", language()) else tr("anova_concl_nsig", language())
  })
  
  ##################### POST-HOC: LSD vs TUKEY ####################
  
  mse_and_df <- reactive({
    s <- summary(anova_result())[[1]]
    list(mse = s["Residuals","Mean Sq"], df = s["Residuals","Df"])
  })
  
  lsd_intervals <- reactive({
    data <- d()
    means <- data %>% group_by(group) %>% summarise(mean = mean(value), .groups = "drop")
    
    mse <- mse_and_df()$mse
    df  <- mse_and_df()$df
    n   <- input$n
    alpha <- 0.05
    t_value <- qt(1 - alpha/2, df)
    
    lsd_factor <- (sqrt(2)/2) * t_value * sqrt(mse / n)
    
    means %>% mutate(LCL = mean - lsd_factor, UCL = mean + lsd_factor)
  })
  
  lsd_conclusion3 <- reactive({
    lsd_data <- lsd_intervals()
    comparisons <- combn(lsd_data$group, 2, simplify = FALSE)
    
    lines <- mapply(function(i, pair) {
      g1 <- lsd_data %>% filter(group == pair[1])
      g2 <- lsd_data %>% filter(group == pair[2])
      
      intro <- paste0(tr("comparison_prefix", language()), " ", i, " — ",
                      pair[1], " ", tr("vs_text", language()), " ", pair[2], ": ")
      
      if (g1$UCL < g2$LCL | g1$LCL > g2$UCL) paste0(intro, tr("diff_sig_lsd", language()))
      else paste0(intro, tr("no_diff_sig_lsd", language()))
    }, seq_along(comparisons), comparisons, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    
    paste(lines, collapse = "\n\n")
  })
  
  tukey_conclusion3 <- reactive({
    tuk <- TukeyHSD(anova_result())
    term <- if ("group" %in% names(tuk)) "group" else names(tuk)[1]
    tab <- as.data.frame(tuk[[term]])
    comps <- rownames(tab)
    
    lines <- mapply(function(i, comp, p) {
      pair <- strsplit(comp, "-")[[1]]
      intro <- paste0(tr("comparison_prefix", language()), " ", i, " — ",
                      pair[1], " ", tr("vs_text", language()), " ", pair[2], ": ")
      
      if (!is.finite(p)) paste0(intro, tr("padj_na", language()))
      else if (p < 0.05) paste0(intro, tr("diff_sig_tuk", language()), signif(p, 3), ").")
      else paste0(intro, tr("no_diff_sig_tuk", language()), signif(p, 3), ").")
    }, seq_along(comps), comps, tab$`p adj`, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    
    paste(lines, collapse = "\n\n")
  })
  
  output$posthocPlot <- renderPlot({
    req(input$posthocType)
    if (input$posthocType == "Intervalos LSD") {
      lsd_data <- lsd_intervals()
      ggplot(lsd_data, aes(x = group, y = mean)) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
        labs(x = tr("plot_x", language()), y = tr("plot_y", language())) +
        theme_minimal()
    } else {
      plot(TukeyHSD(anova_result()))
    }
  })
  
  output$posthocConclusion <- renderText({
    req(input$posthocType)
    if (input$posthocType == "Intervalos LSD") lsd_conclusion3() else tukey_conclusion3()
  })
  
  ##################### COO (Resumen) ####################
  
  # Devuelve stats numéricas para NO hacer regex sobre texto
  coo_stats <- reactive({
    req(input$posthocType, input$optType)
    
    pval <- p_value()
    if (pval >= 0.05) return(list(has_coo = FALSE))
    
    alpha <- 0.05
    
    # =========================
    # 1) RAMA LSD
    # =========================
    if (input$posthocType == "Intervalos LSD") {
      
      lsd_data <- lsd_intervals()
      
      best_mean <- if (input$optType == "Maximización") max(lsd_data$mean) else min(lsd_data$mean)
      best_rows <- lsd_data %>% dplyr::filter(mean == best_mean)
      
      candidates <- lsd_data$group[sapply(lsd_data$group, function(g) {
        if (g %in% best_rows$group) return(TRUE)
        
        g_row <- lsd_data %>% dplyr::filter(group == g)
        
        any(sapply(best_rows$group, function(bg) {
          bg_row <- best_rows %>% dplyr::filter(group == bg)
          !(bg_row$UCL < g_row$LCL | bg_row$LCL > g_row$UCL)
        }))
      })]
      
      cand_tbl <- lsd_data %>% dplyr::filter(group %in% candidates)
      cand_tbl <- if (input$optType == "Maximización") {
        cand_tbl %>% dplyr::arrange(dplyr::desc(mean))
      } else {
        cand_tbl %>% dplyr::arrange(mean)
      }
      
      pick <- cand_tbl$group[1]
      pick_mean <- round(cand_tbl$mean[1], 2)
      
      return(list(
        has_coo   = TRUE,
        method   = "LSD",
        cand_tbl  = cand_tbl,
        pick      = pick,
        pick_mean = pick_mean
      ))
    }
    
    # =========================
    # 2) RAMA TUKEY
    # =========================
    means <- d() %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(mean = mean(value), .groups = "drop")
    
    best_mean <- if (input$optType == "Maximización") max(means$mean) else min(means$mean)
    best_groups <- means %>% dplyr::filter(mean == best_mean) %>% dplyr::pull(group)
    
    tuk_all <- TukeyHSD(anova_result())
    term <- if ("group" %in% names(tuk_all)) "group" else names(tuk_all)[1]
    tuk <- as.data.frame(tuk_all[[term]])
    tuk$comp <- rownames(tuk)
    
    get_padj <- function(g1, g2) {
      if (g1 == g2) return(1)
      c1 <- paste0(g1, "-", g2)
      c2 <- paste0(g2, "-", g1)
      if (c1 %in% tuk$comp) return(tuk$`p adj`[tuk$comp == c1][1])
      if (c2 %in% tuk$comp) return(tuk$`p adj`[tuk$comp == c2][1])
      NA_real_
    }
    
    all_groups <- means$group
    
    candidates <- all_groups[sapply(all_groups, function(g) {
      if (g %in% best_groups) return(TRUE)
      pvals <- sapply(best_groups, function(bg) get_padj(bg, g))
      any(!is.na(pvals) & pvals >= alpha)
    })]
    
    cand_tbl <- means %>% dplyr::filter(group %in% candidates)
    cand_tbl <- if (input$optType == "Maximización") {
      cand_tbl %>% dplyr::arrange(dplyr::desc(mean))
    } else {
      cand_tbl %>% dplyr::arrange(mean)
    }
    
    pick <- cand_tbl$group[1]
    pick_mean <- round(cand_tbl$mean[1], 2)
    
    list(
      has_coo   = TRUE,
      method   = "TUKEY",
      cand_tbl  = cand_tbl,
      pick      = pick,
      pick_mean = pick_mean
    )
  })
  
  optimal_condition <- reactive({
    req(input$posthocType, input$optType)
    
    pval <- p_value()
    if (pval >= 0.05) return(tr("no_coo_short", language()))
    
    st <- coo_stats()
    cand_tbl <- st$cand_tbl
    
    cand_groups_txt <- paste(cand_tbl$group, collapse = ", ")
    pick <- st$pick
    
    if (nrow(cand_tbl) == 1) {
      if (st$method == "LSD") {
        return(fmt(tr("best_lsd_single_summary", language()), pick = pick))
      } else {
        return(fmt(tr("best_tukey_single_summary", language()), pick = pick))
      }
    } else {
      if (st$method == "LSD") {
        return(fmt(tr("best_lsd_multi_summary", language()), cand_groups = cand_groups_txt))
      } else {
        return(fmt(tr("best_tukey_multi_summary", language()), cand_groups = cand_groups_txt))
      }
    }
  })
  
  output$optimalConditionText <- renderText({ optimal_condition() })
  
  ##################### CONDICIONES: RESIDUOS / INDEPENDENCIA / NORMALIDAD ####################
  
  output$hypothesesText_res <- renderUI({
    withMathJax(HTML(paste(tr("h0_var_equal", language()), tr("h1_var_diff", language()))))
  })
  
  output$residualsViolinPlot <- renderPlot({
    data <- d()
    data$residuals_squared <- residuals(anova_result())^2
    ggplot(data, aes(x = group, y = residuals_squared, fill = group)) +
      geom_violin(trim = FALSE) +
      geom_jitter(width = 0.1, size = 0.5) +
      labs(x = tr("plot_x", language()), y = tr("residuals2_y", language())) +
      theme_minimal()
  })
  
  residuals_data <- reactive({
    data <- d()
    data.frame(group = data$group, residuals = residuals(anova_result()))
  })
  
  anova_res_result <- reactive({
    aov(residuals^2 ~ group, data = residuals_data())
  })
  
  p_value_res <- reactive({
    summary(anova_res_result())[[1]][["Pr(>F)"]][1]
  })
  
  output$conclusionText_res <- renderText({
    if (p_value_res() < 0.05) tr("res_concl_sig", language()) else tr("res_concl_nsig", language())
  })
  
  output$aov_res <- renderPrint({ summary(anova_res_result()) })
  
  ##################### MOSTRAR CÁLCULO ANOVA (Panel 2) ####################
  
  output$hypothesesText <- renderUI({
    withMathJax(HTML(paste(
      tr("h0_means_equal", language()),
      tr("h1_means_diff", language())
    )))
  })
  
  
  output$distributionText <- renderText({
    s <- summary(anova_result())[[1]]
    df1 <- s["group","Df"]; df2 <- s["Residuals","Df"]
    txt <- tr("distribution_text", language())
    txt <- gsub("\\{df1\\}", as.character(df1), txt)
    txt <- gsub("\\{df2\\}", as.character(df2), txt)
    txt
  })
  
  output$fRatioFormula <- renderUI({
    s <- summary(anova_result())[[1]]
    ms_between <- s["group","Mean Sq"]
    ms_within  <- s["Residuals","Mean Sq"]
    f_value <- ms_between / ms_within
    withMathJax(paste0(
      "$$\\frac{CM_{factor}}{CM_{res}}=\\frac{", round(ms_between,4), "}{", round(ms_within,4), "}=", round(f_value,4), "$$"
    ))
  })
  
  output$fCriticalText <- renderUI({
    s <- summary(anova_result())[[1]]
    df1 <- s["group", "Df"]
    df2 <- s["Residuals", "Df"]
    
    alpha <- 0.05
    p <- 1 - alpha
    fcrit <- qf(p, df1, df2)
    
    txt <- tr("f_critical_explain", language())
    txt <- gsub("\\{df1\\}", as.character(df1), txt)
    txt <- gsub("\\{df2\\}", as.character(df2), txt)
    txt <- gsub("\\{alpha\\}", format(alpha, digits = 2), txt)
    txt <- gsub("\\{p\\}", format(p, digits = 2), txt)
    txt <- gsub("\\{fcrit\\}", format(round(fcrit, 4), nsmall = 4), txt)
    
    withMathJax(HTML(txt))
  })
  
  
  output$pValueTextDetailed <- renderUI({
    
    s <- summary(anova_result())[[1]]
    
    ms_between <- s["group","Mean Sq"]
    ms_within  <- s["Residuals","Mean Sq"]
    
    f_value <- ms_between / ms_within
    df1 <- s["group","Df"]
    df2 <- s["Residuals","Df"]
    
    pval <- 1 - pf(f_value, df1, df2)
    
    txt <- tr("pvalue_explain", language())
    
    txt <- gsub("\\{fvalue\\}", format(round(f_value,4), nsmall=4), txt)
    txt <- gsub("\\{df1\\}", as.character(df1), txt)
    txt <- gsub("\\{df2\\}", as.character(df2), txt)
    txt <- gsub("\\{pvalue\\}", format(pval, digits=5), txt)
    
    withMathJax(HTML(txt))
  })
  
  
  output$fDistributionPlot <- renderPlot({
    s <- summary(anova_result())[[1]]
    f_value <- s["group","Mean Sq"] / s["Residuals","Mean Sq"]
    df1 <- s["group","Df"]; df2 <- s["Residuals","Df"]
    f_critical <- qf(0.95, df1, df2)
    
    x_max <- max(f_value * 1.2, f_critical * 1.2)
    curve(df(x, df1, df2), from = 0, to = x_max, n = 1000,
          ylab = tr("f_plot_ylab", language()),
          xlab = tr("f_plot_xlab", language()),
          main = tr("f_plot_main", language()))
    
    x_fill_rej <- seq(f_critical, x_max, length.out = 200)
    polygon(c(f_critical, x_fill_rej, x_max),
            c(0, df(x_fill_rej, df1, df2), 0),
            col = rgb(1,0,0,0.3))
    
    x_fill_p <- seq(f_value, x_max, length.out = 200)
    polygon(c(f_value, x_fill_p, x_max),
            c(0, df(x_fill_p, df1, df2), 0),
            col = rgb(0,0,1,0.3))
    
    abline(v = f_critical, col = "red", lwd = 2, lty = 2)
    abline(v = f_value, col = "blue", lwd = 2, lty = 1)
    
    legend("topright",
           legend = c(
             tr("f_leg_fcrit", language()),
             tr("f_leg_fcalc", language()),
             tr("f_leg_rej", language()),
             tr("f_leg_pval", language())
           ),
           col = c("red", "blue", rgb(1,0,0,0.3), rgb(0,0,1,0.3)),
           lwd = 2, lty = c(2,1,NA,NA),
           fill = c(NA,NA, rgb(1,0,0,0.3), rgb(0,0,1,0.3)),
           border = NA)
  })
  
  
  output$fConclusionText <- renderText({
    s <- summary(anova_result())[[1]]
    f_value <- s["group","Mean Sq"] / s["Residuals","Mean Sq"]
    df1 <- s["group","Df"]; df2 <- s["Residuals","Df"]
    f_critical <- qf(0.95, df1, df2)
    if (f_value > f_critical) tr("f_concl_reject", language()) else tr("f_concl_fail", language())
  })
  
  ##################### INDEPENDENCIA / NORMALIDAD ####################
  
  output$independencePlot <- renderPlot({
    data <- d()
    ggplot(data, aes(x = 1:nrow(data), y = value)) +
      geom_line() + geom_point() +
      labs(title = tr("independence_plot_title", language()),
           x = tr("obs_order", language()),
           y = tr("plot_y", language())) +
      theme_minimal()
  })
  
  output$independenceText <- renderText({ tr("independence_text", language()) })
  
  output$normalityPlot <- renderPlot({
    qqPlot(residuals(anova_result()), main = tr("normality_plot_title", language()))
  })
  
  output$normalityText <- renderText({ tr("normality_text", language()) })

  
  ##################### COO (Pestaña COO) ####################
  
  
  optimal_condition2 <- reactive({
    req(input$posthocType, input$optType)
    
    pval <- p_value()
    if (pval >= 0.05) return(tr("no_coo_full", language()))
    
    st <- coo_stats()
    cand_tbl <- st$cand_tbl
    
    cand_desc <- paste0(cand_tbl$group, " (", round(cand_tbl$mean, 2), ")", collapse = ", ")
    pick <- st$pick
    pick_mean <- st$pick_mean
    
    if (nrow(cand_tbl) == 1) {
      if (st$method == "LSD") {
        return(fmt(tr("best_lsd_single", language()), pick = pick, pick_mean = pick_mean))
      } else {
        return(fmt(tr("best_tukey_single", language()), pick = pick, pick_mean = pick_mean))
      }
    } else {
      if (st$method == "LSD") {
        return(fmt(tr("best_lsd_multi", language()), cand_desc = cand_desc, pick = pick, pick_mean = pick_mean))
      } else {
        return(fmt(tr("best_tukey_multi", language()), cand_desc = cand_desc, pick = pick, pick_mean = pick_mean))
      }
    }
  })
  
  
  # Renderizar la condición óptima
  output$optimalConditionText2 <- renderText({
    optimal_condition2()
  })
  
  
  output$optimalConditionDeviation <- renderUI({
    pval <- p_value()
    if (pval >= 0.05) return(tr("no_coo_dev_exact", language()))
    
    pv_res <- p_value_res()
    
    # Homogénea
    if (pv_res >= 0.05) {
      ms_error <- summary(anova_result())[[1]]["Residuals", "Mean Sq"]
      deviation <- sqrt(ms_error)
      
      txt <- fmt(tr("coo_dev_common_exact", language()),
                 ms_error  = round(ms_error, 2),
                 deviation = round(deviation, 2))
      
      return(withMathJax(HTML(txt)))
    }
    
    # No homogénea: usa el "pick" calculado (misma lógica matemática)
    st <- coo_stats()
    best_group <- st$pick
    
    data <- d()
    data$residuals_squared <- residuals(anova_result())^2
    group_residuals <- data %>% dplyr::filter(group == best_group)
    mean_res2 <- mean(group_residuals$residuals_squared)
    
    df_res <- summary(anova_result())[[1]]["Residuals", "Df"]
    n_total <- nrow(data)
    
    var_corr <- mean_res2 * n_total / df_res
    dev_corr <- sqrt(var_corr)
    
    txt <- fmt(tr("coo_var_corr_exact", language()),
               mean_res2 = round(mean_res2, 2),
               n_total   = n_total,
               df_res    = df_res,
               var_corr  = round(var_corr, 2),
               dev_corr  = round(dev_corr, 2))
    
    withMathJax(HTML(txt))
  })
  
  
  # Describir la COO como una distribución normal
  output$cooDistribution <- renderUI({
    pval <- p_value()
    
    if (pval >= 0.05) {
      return(tr("no_coo_full", language()))
    }
    
    st <- coo_stats()
    if (isFALSE(st$has_coo)) {
      return(tr("no_coo_dev_exact", language()))
    }
    
    mean_coo <- st$pick_mean
    pv_res <- p_value_res()
    
    if (pv_res >= 0.05) {
      ms_error <- summary(anova_result())[[1]]["Residuals", "Mean Sq"]
      dev_coo <- round(sqrt(ms_error), 2)
      
      txt <- fmt(tr("coo_dist_common_exact", language()),
                 mean_coo = mean_coo,
                 dev_coo  = dev_coo)
      return(withMathJax(HTML(txt)))
    } else {
      data <- d()
      data$residuals_squared <- residuals(anova_result())^2
      
      group_residuals <- data %>% dplyr::filter(group == st$pick)
      mean_res2 <- mean(group_residuals$residuals_squared)
      
      df_res <- summary(anova_result())[[1]]["Residuals", "Df"]
      n_total <- nrow(data)
      var_corr <- mean_res2 * n_total / df_res
      dev_coo <- round(sqrt(var_corr), 2)
      
      txt <- fmt(tr("coo_dist_common_exact2", language()),
                 mean_coo = mean_coo,
                 dev_coo  = dev_coo)
      return(withMathJax(HTML(txt)))
    }
  })
  
}

shinyApp(ui, server)
