library(dplyr)
library(DT)
library(shiny)
library(htmlwidgets)
library(D3TableFilter)
library(reshape)
library(gtools)
library(RMySQL)
FoodDatabase <- readRDS("Data/FoodDatabase.rds")

protein_per_snack2 <- function(snackcalories, protein, calories) {(snackcalories*protein)/calories
}
calories_all_meals2 <- function(calories, snackcalories) {calories-snackcalories
}
protein_per_all_meals2 <- function(protein,protein_per_snack) {protein-protein_per_snack
}
calories_per_meal2 <- function(calories_all_meals,mealnumber) {calories_all_meals/mealnumber
}

kcal_per_unit2 <- function(ratio) {ratio*9 + 4
}
units_per_day2 <- function(calories_all_meals,kcal_per_unit) {calories_all_meals/kcal_per_unit
}
units_per_meal2 <- function(units_per_day,mealnumber) {units_per_day/mealnumber
}
prescribed_protein_per_meal2 <- function(protein,protein_per_snack,mealnumber) {(protein-protein_per_snack)/mealnumber
}
prescribed_fat_per_meal2 <- function(units_per_meal,ratio) {units_per_meal*ratio
}
prescribed_carbohydrate_per_meal2 <- function(units_per_meal,prescribed_protein_per_meal) {units_per_meal-prescribed_protein_per_meal
}

CHOdiff <- function(CHOsum,CHOgoal) {
  CHOdiff = CHOgoal- CHOsum
  return (CHOdiff)
}

FATdiff <- function(FATsum,FATgoal) {
  FATdiff = FATgoal - FATsum
  return (FATdiff)
}

PROdiff <- function(PROsum,PROgoal) {
  PROdiff = PROgoal - PROsum
  return (PROdiff)
}


CALsum <- function(CHOsum, FATsum, PROsum)  {
          CALsum = (FATsum*9 + CHOsum*4 + PROsum*4)
          return (CALsum)
}

CALdiff <- function(calories,CALsum)  {
  CALdiff = calories - CALsum
  return (CALdiff)
}

RATIOsum <- function(CHOsum, FATsum, PROsum)  {
            RATIOsum = FATsum / (CHOsum + PROsum)
            return (RATIOsum)
}

RATIOdiff <- function(ratio,RATIOsum)  {
  RATIOdiff = ratio - RATIOsum
  return (RATIOdiff)
}