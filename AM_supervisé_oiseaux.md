---
title: "02_Racicot"
output: 
  html_document:
    keep_md: true
---

## Peut-on savoir à quel ordre d'oiseau appartient un oeuf selon ses proportions?




### Télécherger tidyverse et les données


Importation des données

```r
oiseaux <- readr::read_csv2("DataS1_Egg_shape_by_species_v2.csv")
```

```
## Using ',' as decimal and '.' as grouping mark. Use read_delim() for more control.
```

```
## Parsed with column specification:
## cols(
##   Order = col_character(),
##   Family = col_character(),
##   MVZDatabase = col_character(),
##   Species = col_character(),
##   Asymmetry = col_double(),
##   Ellipticity = col_double(),
##   `AvgLength (cm)` = col_double(),
##   `Number of images` = col_double(),
##   `Number of eggs` = col_double()
## )
```

```r
glimpse(oiseaux)
```

```
## Observations: 1,402
## Variables: 9
## $ Order              <chr> "ACCIPITRIFORMES", "ACCIPITRIFORMES", "ACCI...
## $ Family             <chr> "Accipitridae", "Accipitridae", "Accipitrid...
## $ MVZDatabase        <chr> "Accipiter badius", "Accipiter cooperii", "...
## $ Species            <chr> "Accipiter badius", "Accipiter cooperii", "...
## $ Asymmetry          <dbl> 0.1378, 0.0937, 0.1114, 0.0808, 0.0749, 0.0...
## $ Ellipticity        <dbl> 0.3435, 0.2715, 0.3186, 0.2391, 0.2543, 0.3...
## $ `AvgLength (cm)`   <dbl> 3.8642, 4.9008, 5.9863, 4.0355, 3.8700, 8.9...
## $ `Number of images` <dbl> 1, 27, 7, 13, 15, 1, 191, 1, 7, 2, 5, 130, ...
## $ `Number of eggs`   <dbl> 2, 103, 18, 61, 57, 1, 391, 2, 17, 4, 12, 3...
```
 
Nettoyer les données et sélectionner les colonnes d'intérêt

```r
oiseaux <- oiseaux[complete.cases(oiseaux),]
oiseaux <-select(oiseaux, Order, Asymmetry, Ellipticity)
```

Visualiser les données pour choisir deux groupes pour entrainer et tester le Knn

```r
oiseaux %>%
  ggplot(aes(x=Asymmetry, y=Ellipticity, colour=Order))+ # Testing the columns and visualising to select two orders to test in Knn
  geom_point()
```

![](AM_supervisé_oiseaux_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Sélectionner 15 données de deux ordres et les combiner pour créer un dataset pour entrainer le Knn

```r
oiseau_1 <- oiseaux[which(oiseaux$Order == 'CHARADRIIFORMES'),] # Selecting one order
oiseau_1 <- oiseau_1[sample(1:nrow(oiseau_1), 15, #Selecting 15 random rows
   replace=FALSE),]
#view(oiseau_1)
```

```r
oiseau_2 <- oiseaux[which(oiseaux$Order == 'CORACIIFORMES'),] # Selecting one order
oiseau_2 <- oiseau_2[sample(1:nrow(oiseau_2), 15, #Selecting 15 random rows
   replace=FALSE),]
#view(oiseau_2)
```

```r
oiseaux_data <-rbind(oiseau_1,oiseau_2) # Making a dataset with both orders
#view(oiseaux_data)
```

Visualisation des différences entre les deux ordres

```r
oiseaux_data %>%
  ggplot(aes(x=Asymmetry, y=Ellipticity, colour=Order))+ # Visualising the differences between the two orders
  geom_point()
```

![](AM_supervisé_oiseaux_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Création de la liste de labels pour entraîner et tester de Knn

```r
oiseaux_order <- oiseaux_data %>% # Creating the label list to train and test the Knn
  select(Order) %>% 
  mutate_if(is.character, as.factor)%>%
  unlist()
head(oiseaux_order) 
```

```
##          Order1          Order2          Order3          Order4 
## CHARADRIIFORMES CHARADRIIFORMES CHARADRIIFORMES CHARADRIIFORMES 
##          Order5          Order6 
## CHARADRIIFORMES CHARADRIIFORMES 
## Levels: CHARADRIIFORMES CORACIIFORMES
```

Sélection des données à tester 

```r
oiseaux_train <- oiseaux_data%>%
  select(Asymmetry, Ellipticity) # Selecting the data to train the model 
```

Entrainement du modèle

```r
# Making the Knn model
k <- data.frame(k = 5) 
model_knn <- train(x = data.frame(oiseaux_train), y = oiseaux_order, method='knn', tuneGrid = k)
```

Test du modèle avec une obervation fictive

```r
new_obs <- data.frame(Asymmetry = 0.3, Ellipticity = 0.4) # Fabricating a new observation
predict(object = model_knn, new_obs) # Testing the model
```

```
## [1] CHARADRIIFORMES
## Levels: CHARADRIIFORMES CORACIIFORMES
```

Sélection d'un dataset pour tester le modèle

```r
#Making a test dataframe
oiseau_1_test <- oiseaux[which(oiseaux$Order == 'CHARADRIIFORMES'),] # Selecting one order
oiseau_1_test <- oiseau_1_test[sample(1:nrow(oiseau_1_test), 10, #Selecting random rows
   replace=FALSE),]

oiseau_2_test <- oiseaux[which(oiseaux$Order == 'CORACIIFORMES'),] # Selecting one order
oiseau_2_test <- oiseau_2_test[sample(1:nrow(oiseau_2_test), 10, #Selecting random rows
   replace=FALSE),]


oiseaux_data_test <-rbind(oiseau_1_test,oiseau_2_test) # Making a dataset with both orders


oiseaux_order_test <- oiseaux_data %>% # Creating the label list to train and test the Knn
  select(Order) %>% 
  mutate_if(is.character, as.factor)%>%
  unlist()

oiseaux_test <-oiseaux_data_test %>%
  select(Asymmetry, Ellipticity)
```

Visualisation des données par rapport à ceux d'entraînement

```r
#Pour visualiser les données testées par rapport aux données de l'entraînement
#Ne marche pas pour l'instant, oiseaux_2_test n'affiche que 2 points et on ne peut pas vraiment changer la couleur de test sans affecter trained
ggplot() +
      # Test
      geom_point(data=oiseau_1_test, aes(x=Asymmetry, y=Ellipticity)) +
      geom_point(data=oiseau_2_test, aes(x=Asymmetry, y=Ellipticity)) +

      # Trained
      geom_point(data=oiseaux_data, aes(x=Asymmetry, y=Ellipticity, colour=Order))
```

![](AM_supervisé_oiseaux_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Comparaison des résultats avec les labels

```r
results <- predict(object = model_knn, oiseaux_test)
results
```

```
##  [1] CHARADRIIFORMES CHARADRIIFORMES CHARADRIIFORMES CHARADRIIFORMES
##  [5] CHARADRIIFORMES CHARADRIIFORMES CHARADRIIFORMES CORACIIFORMES  
##  [9] CHARADRIIFORMES CHARADRIIFORMES CORACIIFORMES   CORACIIFORMES  
## [13] CORACIIFORMES   CORACIIFORMES   CORACIIFORMES   CORACIIFORMES  
## [17] CORACIIFORMES   CORACIIFORMES   CORACIIFORMES   CORACIIFORMES  
## Levels: CHARADRIIFORMES CORACIIFORMES
```

```r
oiseaux_data_test$Results = results
#view(oiseaux_data_test)
```

On peut voir une erreur à la ligne 13, où le Knn dicte qu'il devrait être un oeuf de CHARADRIIFORMES alors qu'il est un oeuf de CORACIIFORMES.


Tentative de normaliser les données

```r
oiseaux_data_scaled <-oiseaux_data %>%
  select(Asymmetry, Ellipticity) %>%
  scale(center = TRUE)
head(oiseaux_data_scaled)
```

```
##      Asymmetry Ellipticity
## [1,] 1.1855551   1.4522380
## [2,] 0.5794169   0.3475388
## [3,] 0.7925968  -0.2511475
## [4,] 0.4271456   0.6468819
## [5,] 0.7297235   0.9847706
## [6,] 1.1354529   1.1405930
```
Données ne sont plus vraiment manipulables, arrêt de la tentative



