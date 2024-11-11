# Cargar librerías
library(dplyr)#para hacer estadística descriptiva y manipulación de bases de datos
library(ggplot2) #para graficar
library(gridExtra)
library(glmmTMB) #modelos lineales generalizados
library(emmeans) # para hacer comparaciones
library(DHARMa) #supuestos
library(performance) #sobre o subdispersion

#Exploracion del data frame
str(conteo_frases) #¿cuál es la estructura de la base?
class(conteo_frases) #qué tipo de objeto es?
dim(conteo_frases) #qué dimensiones tiene?
head(conteo_frases) #muestra las primeras filas
tail(conteo_frases) # muestra las últimas
summary(conteo_frases) #resume a cada variable

#Exploracion preliminar de los datos

# Calcular la proporción de conteo por frases totales
conteo_frases <- conteo_frases %>%
  mutate(Proporción = Conteo / Frases_total)

# Crear el plot
ggplot(conteo_frases, aes(x = Estímulo, y = Proporción, group = interaction(Profundidad, Tetrodo, Ave), color = Ave)) +
  geom_line() +  # Líneas que conectan los puntos para cada combinación de Ave, Profundidad y Tetrodo
  geom_point() +  # Puntos para cada estímulo
  labs(title = "Proporciones de Conteo por Estímulo",
       x = "Estímulo",
       y = "Proporción (Conteo / Frases Totales)",
       color = "Ave") +
  theme_minimal() +  # Tema para estética del gráfico
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotación de etiquetas en x para legibilidad

# Crear la variable no-respuesta
conteo_frases$NR <- conteo_frases$Frases_total - conteo_frases$Conteo

# Ajustar el modelo con una distribución beta-binomial
m_beta <- glmmTMB(
  cbind(Conteo, NR) ~ Estímulo + as.factor(Profundidad) + as.factor(Tetrodo) + (1|Ave),
  data = conteo_frases,
  family = betabinomial()
)

# Verificar si hay sobre- o subdispersión
rp <- resid(m_beta, type = "pearson") # residuos de Pearson
pearson_chisq <- sum(rp^2) 
df_res <- m_beta$df.residual
dispersion_ratio <- pearson_chisq / df_res
dispersion_ratio

#PH para sobredispersion chi2 
check_overdispersion(m_beta)

#Supuestos con Dharma
simulationOutput <- simulateResiduals(fittedModel = m_beta, plot = T)
hist(simulationOutput)
testDispersion(simulationOutput)

#Miro resultados
summary(m_beta)
Anova(m_beta)


# Comparaciones de medias marginales (Tukey) para Estímulo
Comp <- emmeans(m_beta, pairwise ~ Estímulo, adjust = "tukey")
summary(Comp)
plot(Comp, comparisons = TRUE)
confint(Comp)


# Gráfico de medias marginales con intervalos de confianza en escala del predictor lineal
estad2 <- as.data.frame(Comp$emmeans)

ggplot(estad2, aes(x = Estímulo, y = emmean)) + 
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  geom_point(colour = "blue", size = 3) +
  ylab("Estimación del Logit (Media Marginal)") + 
  theme_grey(base_size = 16) +
  annotate("text", x = c(1, 2, 3), y = c(4.2, 5.3, 3.5), label = c("A", "B", "B"))

# Comparaciones en la escala original de la variable de respuesta (proporción, en tu caso)
Comp2 <- emmeans(m_beta, pairwise ~ Estímulo, type = "response")
Comp2
confint(Comp2)

# Gráfico de medias marginales con IC en escala de la respuesta
estad <- as.data.frame(Comp2$emmeans)

ggplot(estad, aes(x = Estímulo, y = prob)) + 
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1) +
  geom_point(colour = "blue", size = 3) +
  ylab("Proporción estimada de frases con respuesta") + 
  theme_grey(base_size = 16) +
  annotate("text", x = c(1, 2, 3), y = c(0.8, 0.9, 0.7), label = c("A", "B", "B"))

