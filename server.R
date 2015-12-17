library(shiny)
library(DT)
library(plyr)
library(magrittr)
library(rHighcharts)
library(rCharts)
source("helper.R")

shinyServer(function(input, output, session) {
  
  ###########
  #CHART!!!! 
  ###########
output$Composition = renderChart2({
FoodDatabase <- rename(FoodDatabase, c("Cat. 1"="Category"))
freq <- count(FoodDatabase, "Category")
a <- hPlot(x = "Category", y = "freq", data = freq, type = "pie")
a$plotOptions(pie = list(size = 320))
a$chart(backgroundColor = NULL)
a$set(dom = 'Composition')
a$tooltip( formatter = "#! function() { return this.point.name + ' has ' + 
                                                 this.point.y  + ' products'
                                                } !#")
a
})
  
  
  
  ###########
  #MAIN TABLE 
  ###########
  # server-side processing of the main table
  output$maintable = DT::renderDataTable(FoodDatabase[, input$show_vars, drop = TRUE], extensions = c('Responsive'),
                                    server = TRUE, options = list(columnDefs = list(list(width = '200px', targets = "c(1)")),searchHighlight = TRUE,pageLength = 10, autoWidth = TRUE,initComplete = JS(
                                    "function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': 'orange', 'color': '#2c2c2c'});",
                                    "}")), filter='top', class="stripe compact nowrap", rownames = FALSE)

  proxy = dataTableProxy('maintable')
  
  observeEvent(input$clear1, {
    selectRows(proxy, NULL)
  })
  
  
  # download option for whole table
  output$wholeDB = downloadHandler('fooddatabasewhole.csv', content = function(file) {
    write.csv(FoodDatabase, file)
  })




  #############################
  #FAVORITES TABLE AND DOWNLOAD 
  #############################
  
  # print the selected indices from the main table (these will be used as inputs to the editable table)
  output$x4 = renderPrint({
    s = input$maintable_rows_selected
    Products<-FoodDatabase[s, c("Product"), drop = FALSE]
    if (length(s)) {
      cat('These products were selected:\n\n')
      cat(paste(as.matrix(Products), "\n\n", sep=","))
    }
  })
  
  # subset main table by selected indices to obtain favorite foods
  favs<-reactive({ 
    rs = input$maintable_rows_selected
    fav<-FoodDatabase[rs, , drop = FALSE]
    fav })
  
  # server-side processing of the main table to put favorite foods in a datatable object
  output$favs<-DT::renderDataTable({
    DT::datatable(favs(),
                  options = list(searchHighlight = TRUE,pageLength = 5, autoWidth = TRUE,initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'orange', 'color': '#2c2c2c'});",
                    "}")), filter='top', class="stripe compact nowrap", rownames = FALSE, selection = "none")
  })
  
  # download the selected indices tables (for favorites)
  output$favoritestable = downloadHandler('fooddatabase-filtered.csv', content = function(file) {
    rs = input$maintable_rows_selected
    write.csv(FoodDatabase[rs, , drop = FALSE], file)
  })
  
  #############################
  #OPTIONAL CATEGORIES DOWNLOAD 
  #############################
  
  # download the other filtered data

  datum<-reactive({ d<-subset(FoodDatabase, `Cat. 1`==input$col) 
                    d })
  
  output$table<-renderDataTable({datum()})
  
  output$x6 = downloadHandler('fooddatabase.csv', content = function(file) {
    download<- FoodDatabase$`Cat. 1`==input$col
    write.csv(datum(), file)
  })
  
  ##########################
  #PKT RECIPE BUILDER TABLE
  #########################
  
  
  # Generate recipe builder table with selected indices in favorites tab plus some extra columns
  formeals<-reactive({ 
    favs = input$maintable_rows_selected
    fav<-FoodDatabase[favs, , drop = FALSE]
    mealtemp<-cbind(as.data.frame(matrix(0, ncol=3, nrow=length(favs))), fav[,c(1:5)])
    mealtemp<-rename(mealtemp, c("V1"="Adjust PRO", "V2"="Adjust FAT", "V3"="Adjust CHO"))
    mealtemp
  })
  
  
  # Server side processing of the recipe builder table created above
  output$meals2 <- renderD3tf({
    
    # define table properties
    tableProps <- list(
      btn_reset = TRUE,
      rows_counter = TRUE,  
      rows_counter_text = "Rows: ",
      sort = TRUE,
      on_keyup = TRUE,  
      on_keyup_delay = 800
    );
    
    d3tf(formeals(),
         enableTf = TRUE,
         tableProps = tableProps,
         edit=TRUE,
         showRowNames = TRUE, 
         tableStyle = "table table-bordered table-condensed",
         rowStyles = c(rep("", 7), rep("info", 7)),
         filterInput = TRUE)
  })
  
  
  ##########################
  #PRESCRIPTION CALCULATIONS
  ##########################
  
  # Total Calories in Meals
  output$result1 <- renderText({
    calories_all_meals = calories_all_meals2(calories=as.numeric(input$calories),snackcalories=as.numeric(input$snackcalories))
    paste0("Your Total Meal Calories is ", "<code>", round(calories_all_meals, 2), "</code>")})
  # Total Protein in Meals    
  output$result2 <- renderText({
    protein_per_all_meals = protein_per_all_meals2(protein=as.numeric(input$protein), protein_per_snack=as.numeric(protein_per_snack2(snackcalories=as.numeric(input$snackcalories),protein=as.numeric(input$protein), calories=as.numeric(input$calories))))
    paste0("Your Protein in Meals is ", "<code>", round(protein_per_all_meals, 2), "</code>")})
  # Total Calories in Meals    
  output$result3 <- renderText({
    calories_per_meal = calories_per_meal2(calories_all_meals=as.numeric(calories_all_meals2(calories=as.numeric(input$calories),snackcalories=as.numeric(input$snackcalories))),mealnumber=as.numeric(input$mealnumber))
    paste0("Your Calories in Meals is ", "<code>", round(calories_per_meal, 2), "</code>")})
  # KCAL Per Unit
  output$result4 <- renderText({
    kcal_per_unit = kcal_per_unit2(ratio=as.numeric(input$ratio))
    paste0("Your KCAL Per Unit is ", "<code>", round(kcal_per_unit, 2), "</code>")})
  # Total Units Per Day
  output$result5 <- renderText({
    units_per_day =  units_per_day2(calories_all_meals=as.numeric(calories_all_meals2(calories=as.numeric(input$calories),snackcalories=as.numeric(input$snackcalories))), kcal_per_unit=as.numeric(kcal_per_unit2(ratio=as.numeric(input$ratio))))
    paste0("Your Total Units Per Day is ", "<code>", round(units_per_day, 2), "</code>")})
  # Total Protein Per Snack
  output$result6 <- renderText({
    protein_per_snack = protein_per_snack2(snackcalories=as.numeric(input$snackcalories),protein=as.numeric(input$protein), calories=as.numeric(input$calories))
    paste0("Your Total Protein Per Snack is ", "<code>", round(protein_per_snack, 2), "</code>")})
  # Total Units Per Meal
  output$result7 <- renderText({
    units_per_meal = units_per_meal2(units_per_day=units_per_day2(calories_all_meals=as.numeric(calories_all_meals2(calories=as.numeric(input$calories),snackcalories=as.numeric(input$snackcalories))), kcal_per_unit=as.numeric(kcal_per_unit2(ratio=as.numeric(input$ratio)))),mealnumber=as.numeric(input$mealnumber))
    paste0("Your Total Units Per Meal is ", "<code>", round(units_per_meal, 2), "</code>")})
  # Prescribed Protein Per Meal
  output$result8 <- renderText({
    prescribed_protein_per_meal  = prescribed_protein_per_meal2(protein=as.numeric(input$protein),protein_per_snack=protein_per_snack2(snackcalories=as.numeric(input$snackcalories),protein=as.numeric(input$protein), calories=as.numeric(input$calories)),mealnumber=as.numeric(input$mealnumber))
    paste0("Your Prescribed Protein Per Meal is ", "<code>", round(prescribed_protein_per_meal, 2), "</code>")})
  # Prescribed Fat Per Meal
  output$result9 <- renderText({
    prescribed_fat_per_meal = prescribed_fat_per_meal2(units_per_meal=units_per_meal2(units_per_day=units_per_day2(calories_all_meals=as.numeric(calories_all_meals2(calories=as.numeric(input$calories),snackcalories=as.numeric(input$snackcalories))), kcal_per_unit=as.numeric(kcal_per_unit2(ratio=as.numeric(input$ratio)))),mealnumber=as.numeric(input$mealnumber)),ratio=as.numeric(input$ratio))
    paste0("Your Prescribed Fat Per Meal is ", "<code>", round(  prescribed_fat_per_meal , 2), "</code>")})
  # Prescribed Carbohydrate Per Meal
  output$result10 <- renderText({
    prescribed_carbohydrate_per_meal =    prescribed_carbohydrate_per_meal2(units_per_meal=units_per_meal2(units_per_day=units_per_day2(calories_all_meals=as.numeric(calories_all_meals2(calories=as.numeric(input$calories),snackcalories=as.numeric(input$snackcalories))), kcal_per_unit=as.numeric(kcal_per_unit2(ratio=as.numeric(input$ratio)))),mealnumber=as.numeric(input$mealnumber)),prescribed_protein_per_meal=prescribed_protein_per_meal2(protein=as.numeric(input$protein),protein_per_snack=protein_per_snack2(snackcalories=as.numeric(input$snackcalories),protein=as.numeric(input$protein), calories=as.numeric(input$calories)),mealnumber=as.numeric(input$mealnumber)))
    paste0("Your Prescribed Carbohydrate Per Meal is ", "<code>", round(  prescribed_carbohydrate_per_meal, 2), "</code>")})
  
  ########EDITS TO TABLE
  revals <- reactiveValues();
  editable=reactive({
    favs = input$maintable_rows_selected
    fav<-FoodDatabase[favs, , drop = FALSE]
    mealtemp<-cbind(as.data.frame(matrix(0, ncol=3, nrow=length(favs))), fav[,c(1:5)])
    mealtemp<-rename(mealtemp, c("V1"="Adjust PRO", "V2"="Adjust FAT", "V3"="Adjust CHO"))
    mealtemp
  })
  
  isolate({
    revals$editable <- editable()
    revals$rowIndex <- 1:nrow(editable())  
    
    observe({
      print(input$meals2_edit)
      if(is.null(input$meals2_edit)) return(NULL);
      edit <- input$meals2_edit;
      
      
      isolate({
        # need isolate, otherwise this observer would run twice
        # for each edit
        id <- edit$id;
        row <- as.integer(edit$row);
        col <- as.integer(edit$col);
        val <- edit$val;
        
        # validate input 
        if(col == 0) {
          # rownames
          oldval <- rownames(revals$editable)[row];
          
        } else if (col %in% c(1, 2, 3)){
          # numeric columns
          if(is.na(suppressWarnings(as.numeric(val)))) {
            oldval <- revals$editable[row, col];
            # reset to the old value
            # input will turn red briefly, than fade to previous color while
            # text returns to previous value
            rejectEdit(session, tbl = "editable", row = row, col = col, id = id, value = oldval);
            return(NULL);
          } 
        }
        
        # accept edits
        if(col == 0) {
          rownames(revals$editable)[row] <- val;
        } else if (col %in% c(1,2,3)) {
          revals$editable[row, col] <- as.numeric(val);
          val = round(as.numeric(val), 2)
        }
        # confirm edits
        confirmEdit(session, tbl = "editable", row = row, col = col, id = id, value = val);
        
        
      })
    })})
  
 
  
  ###TOOK OUT OF UI
  output$filteredmeals3 <- renderTable({
    favs = input$maintable_rows_selected
    fav<-FoodDatabase[favs, , drop = FALSE]
    mealtemp<-cbind(as.data.frame(matrix(0, ncol=3, nrow=length(favs))), fav[,c(1:5)])
    mealtemp<-rename(mealtemp, c("V1"="Adjust PRO", "V2"="Adjust FAT", "V3"="Adjust CHO"))
    mealtemp<-mealtemp[favs, , drop = FALSE]
    if(is.null(revals$rowIndex)) return(invisible());    
    if(is.null(revals$editable)) return(invisible());
    cbind(revals$editable[,c(1:3)],mealtemp[, c(1:5)])
    
  });
  
  ###################
  #GOAL CALCULATIONS
  ##################

  #Initialize a matrix of foods in the edited table along with their adjust amounts
  
  output$weighedfoods2 <- renderD3tf({
    favs = input$maintable_rows_selected
    fav<-FoodDatabase[favs, , drop = FALSE]
    editsontable<-revals$editable[,c(1:3)]

    temp<-cbind(editsontable,fav[, c(1:5,9)])
    temp$maxadjust<-apply(temp[,1:3], 1, max,na.rm=TRUE)

    temp$maxpercent<-apply(temp[,6:8], 1, max,na.rm=TRUE)
    temp$weighout<-temp$maxadjust/temp$maxpercent * 100 
    temp$PRO<-temp$weighout*temp$`Pro (%)`/100
    temp$FAT<-temp$weighout*temp$`Fat (%)`/100
    temp$CHO<-temp$weighout*temp$`CHO (%)`/100
    sums<-apply(temp[,c("PRO","FAT","CHO")], 2, sum)

    temp2<-smartbind(temp,sums)
    temp3<-temp2[,c(4:5,9,12:15)]
    
    bgColScales <- list(
      col_4 = ".domain([0, colMax(col_4)]):white:green",
      col_5 = ".domain([0, colMax(col_5)]):white:orange",
      col_6 = ".domain([0, colMax(col_5)]):white:blue"
    );
    
    fgColScales <- list(
      col_1 = JS('function colorScale(tbl, i){
        var color = d3.scale.threshold()
        .domain([130, 130, 200.1])
        .range(["black", "black", "white"]);
        return color(i);
      }')
    )
  
    # define table properties
    tableProps <- list(
      showRowNames = FALSE,
      btn_reset = TRUE,
      rows_counter = TRUE,  
      rows_counter_text = "Rows: ",
      sort = TRUE,
      on_keyup = TRUE,  
      on_keyup_delay = 800
      
    );
    
    d3tf(temp3,
         enableTf = TRUE,
         tableProps = tableProps,
         showRowNames = FALSE, 
         tableStyle = "table table-bordered table-condensed",
         rowStyles = c(rep("", 7), rep("info", 7)),
         filterInput = TRUE,
         bgColScales = bgColScales,
         fgColScales = fgColScales)
  })
  
  #GOALS
  output$CHOresult <- renderText({
    favs = input$maintable_rows_selected
    fav<-FoodDatabase[favs, , drop = FALSE]
    editsontable<-revals$editable[,c(1:3)]
    print(editsontable)
    temp<-cbind(editsontable,fav[, c(1:5,9)])
    temp$maxadjust<-apply(temp[,1:3], 1, max,na.rm=TRUE)
    print(temp$maxadjust)
    temp$maxpercent<-apply(temp[,6:8], 1, max,na.rm=TRUE)
    temp$weighout<-temp$maxadjust/temp$maxpercent * 100 
    temp$PRO<-temp$weighout*temp$`Pro (%)`/100
    temp$FAT<-temp$weighout*temp$`Fat (%)`/100
    temp$CHO<-temp$weighout*temp$`CHO (%)`/100
    CHOsum2<-sum(temp$CHO)
    print(CHOsum2)
    CHOdiff = CHOdiff(CHOsum = CHOsum2, CHOgoal = prescribed_carbohydrate_per_meal2(units_per_meal=units_per_meal2(units_per_day=units_per_day2(calories_all_meals=as.numeric(calories_all_meals2(calories=as.numeric(input$calories),snackcalories=as.numeric(input$snackcalories))), kcal_per_unit=as.numeric(kcal_per_unit2(ratio=as.numeric(input$ratio)))),mealnumber=as.numeric(input$mealnumber)),prescribed_protein_per_meal=prescribed_protein_per_meal2(protein=as.numeric(input$protein),protein_per_snack=protein_per_snack2(snackcalories=as.numeric(input$snackcalories),protein=as.numeric(input$protein), calories=as.numeric(input$calories)),mealnumber=as.numeric(input$mealnumber)))
    )
    
    
    if      (CHOdiff <=  0.05 && CHOdiff >= -0.05) info = "<span style='color: green'> you do not need to add more carbs.</span>"
    else if    (CHOdiff > 0.05)   info = "<span style='color: red'> please add more carbs.</span>"
    else if   (CHOdiff < -0.05)   info = "<span style='color: red'> please take away carbs.</span>"
    
    paste0("CARBS: The carbs in this meal is ", "<code>", round(CHOdiff, 2), "</code>", "grams away from the goal,", info)
  })
  
  
  
  
  output$FATresult <- renderText({
    
    favs = input$maintable_rows_selected
    fav<-FoodDatabase[favs, , drop = FALSE]
    editsontable<-revals$editable[,c(1:3)]
    print(editsontable)
    temp<-cbind(editsontable,favs()[, c(1:5,9)])
    temp$maxadjust<-apply(temp[,1:3], 1, max,na.rm=TRUE)
    print(temp$maxadjust)
    temp$maxpercent<-apply(temp[,6:8], 1, max,na.rm=TRUE)
    temp$weighout<-temp$maxadjust/temp$maxpercent * 100 
    temp$PRO<-temp$weighout*temp$`Pro (%)`/100
    temp$FAT<-temp$weighout*temp$`Fat (%)`/100
    temp$CHO<-temp$weighout*temp$`CHO (%)`/100
    FATsum2<-sum(temp$FAT)
    
    FATdiff = FATdiff(FATsum = FATsum2, FATgoal = prescribed_fat_per_meal2(units_per_meal=units_per_meal2(units_per_day=units_per_day2(calories_all_meals=as.numeric(calories_all_meals2(calories=as.numeric(input$calories),snackcalories=as.numeric(input$snackcalories))), kcal_per_unit=as.numeric(kcal_per_unit2(ratio=as.numeric(input$ratio)))),mealnumber=as.numeric(input$mealnumber)),ratio=as.numeric(input$ratio)))
    
    if      (FATdiff <=  0.05 && FATdiff >= -0.05) info = "<span style='color: green'> you do not need to add more fat.</span>"
    else if    (FATdiff > 0.05)   info = "<span style='color: red'> please add more fat.</span>"
    else if      (FATdiff < -0.05)   info = "<span style='color: red'> please take away fat.</span>"
    paste0("FAT: The fat in this meal is ", "<code>", round(FATdiff, 2), "</code>", "grams away from the goal,", info)
  })
  
  output$PROresult <- renderText({
    
    favs = input$maintable_rows_selected
    fav<-FoodDatabase[favs, , drop = FALSE]
    editsontable<-revals$editable[,c(1:3)]
    print(editsontable)
    temp<-cbind(editsontable,favs()[, c(1:5,9)])
    temp$maxadjust<-apply(temp[,1:3], 1, max,na.rm=TRUE)
    print(temp$maxadjust)
    temp$maxpercent<-apply(temp[,6:8], 1, max,na.rm=TRUE)
    temp$weighout<-temp$maxadjust/temp$maxpercent * 100 
    temp$PRO<-temp$weighout*temp$`Pro (%)`/100
    temp$FAT<-temp$weighout*temp$`Fat (%)`/100
    temp$CHO<-temp$weighout*temp$`CHO (%)`/100
    PROsum2<-sum(temp$PRO)
    
    PROdiff = PROdiff(PROsum = PROsum2, PROgoal = prescribed_protein_per_meal2(protein=as.numeric(input$protein),protein_per_snack=protein_per_snack2(snackcalories=as.numeric(input$snackcalories),protein=as.numeric(input$protein), calories=as.numeric(input$calories)),mealnumber=as.numeric(input$mealnumber)))
    
    if      (PROdiff <=  0.05 && PROdiff >=  -0.05) {info = "<span style='color: green'> you do not need to add more protein.</span>"}
    else if    (PROdiff > 0.05)   {info = "<span style='color: red'> please add more protein.</span>"}
    else if      (PROdiff < -0.05)   {info = "<span style='color: red'> please take away protein.</span>"}
    paste0("PROTEIN: The protein in this meal is ", "<code>", round(PROdiff, 2), "</code>", "grams away from the goal,", info)
  })
  
  
 output$CALresult <- renderText({

   favs = input$maintable_rows_selected
   fav<-FoodDatabase[favs, , drop = FALSE]
   editsontable<-revals$editable[,c(1:3)]
   print(editsontable)
   temp<-cbind(editsontable,favs()[, c(1:5,9)])
   temp$maxadjust<-apply(temp[,1:3], 1, max,na.rm=TRUE)
   print(temp$maxadjust)
   temp$maxpercent<-apply(temp[,6:8], 1, max,na.rm=TRUE)
   temp$weighout<-temp$maxadjust/temp$maxpercent * 100 
   temp$PRO<-temp$weighout*temp$`Pro (%)`/100
   temp$FAT<-temp$weighout*temp$`Fat (%)`/100
   temp$CHO<-temp$weighout*temp$`CHO (%)`/100
   PROsum2<-sum(temp$PRO)
   FATsum2<-sum(temp$FAT)
   CHOsum2<-sum(temp$CHO)
   
   CALsum= CALsum(PROsum=PROsum2, CHOsum=CHOsum2, FATsum=FATsum2)
   
   CALdiff = CALdiff(calories=as.numeric(input$calories),CALsum=as.numeric(CALsum(PROsum=PROsum2, CHOsum=CHOsum2, FATsum=FATsum2)))
   
   if      (CALdiff <=  0.5 && CALdiff >=  -0.5) {info = "<span style='color: green'> you do not need to add more Calories.</span>"}
   else if    (CALdiff > 0.5)   {info = "<span style='color: red'> please add more Calories.</span>"}
   else if      (CALdiff < -0.5)   {info = "<span style='color: red'> please take away Calories.</span>"}
   paste0("CALORIES: There are", "<code>", round(CALsum, 2), "</code>", "calories in this meal which is", "<code>", round(CALdiff, 2), "</code>", "away from the goal",  info)
 })
 
 
 output$RATIOresult <- renderText({
   
   
   favs = input$maintable_rows_selected
   fav<-FoodDatabase[favs, , drop = FALSE]
   editsontable<-revals$editable[,c(1:3)]
   print(editsontable)
   temp<-cbind(editsontable,favs()[, c(1:5,9)])
   temp$maxadjust<-apply(temp[,1:3], 1, max,na.rm=TRUE)
   print(temp$maxadjust)
   temp$maxpercent<-apply(temp[,6:8], 1, max,na.rm=TRUE)
   temp$weighout<-temp$maxadjust/temp$maxpercent * 100 
   temp$PRO<-temp$weighout*temp$`Pro (%)`/100
   temp$FAT<-temp$weighout*temp$`Fat (%)`/100
   temp$CHO<-temp$weighout*temp$`CHO (%)`/100
   PROsum2<-sum(temp$PRO)
   FATsum2<-sum(temp$FAT)
   CHOsum2<-sum(temp$CHO)
   
   
   RATIOsum= RATIOsum(PROsum=PROsum2, CHOsum=CHOsum2, FATsum=FATsum2)
   RATIOdiff = RATIOdiff(ratio=as.numeric(input$ratio),RATIOsum=as.numeric(RATIOsum(PROsum=PROsum2, CHOsum=CHOsum2, FATsum=FATsum2)))
   
   
   if      (RATIOdiff <=  0.5 && RATIOdiff >=  -0.5) {info = "<span style='color: green'> you do not need adjust the ratio</span>"}
   else if    (RATIOdiff > 0.5)   {info = "<span style='color: red'> please increase the ratio.</span>"}
   else if      (RATIOdiff < -0.5)   {info = "<span style='color: red'> please decrease the ratio.</span>"}
   paste0("RATIO: This meal's ratio is", "<code>", round(RATIOsum, 2), "</code>", "which is", "<code>", round(RATIOdiff, 2), "</code>", "away from the goal",  info)
   
   
   
 })
 
 
 
  output$FINAL <- renderText({
    
    
    favs = input$maintable_rows_selected
    fav<-FoodDatabase[favs, , drop = FALSE]
    editsontable<-revals$editable[,c(1:3)]
    print(editsontable)
    temp<-cbind(editsontable,favs()[, c(1:5,9)])
    temp$maxadjust<-apply(temp[,1:3], 1, max,na.rm=TRUE)
    print(temp$maxadjust)
    temp$maxpercent<-apply(temp[,6:8], 1, max,na.rm=TRUE)
    temp$weighout<-temp$maxadjust/temp$maxpercent * 100 
    temp$PRO<-temp$weighout*temp$`Pro (%)`/100
    temp$FAT<-temp$weighout*temp$`Fat (%)`/100
    temp$CHO<-temp$weighout*temp$`CHO (%)`/100
    PROsum2<-sum(temp$PRO)
    FATsum2<-sum(temp$FAT)
    CHOsum2<-sum(temp$CHO)
    
    CALsum= CALsum(PROsum=PROsum2, CHOsum=CHOsum2, FATsum=FATsum2)
    RATIOsum= RATIOsum(PROsum=PROsum2, CHOsum=CHOsum2, FATsum=FATsum2)
    
    CHOdiff = CHOdiff(CHOsum = CHOsum2, CHOgoal = prescribed_carbohydrate_per_meal2(units_per_meal=units_per_meal2(units_per_day=units_per_day2(calories_all_meals=as.numeric(calories_all_meals2(calories=as.numeric(input$calories),snackcalories=as.numeric(input$snackcalories))), kcal_per_unit=as.numeric(kcal_per_unit2(ratio=as.numeric(input$ratio)))),mealnumber=as.numeric(input$mealnumber)),prescribed_protein_per_meal=prescribed_protein_per_meal2(protein=as.numeric(input$protein),protein_per_snack=protein_per_snack2(snackcalories=as.numeric(input$snackcalories),protein=as.numeric(input$protein), calories=as.numeric(input$calories)),mealnumber=as.numeric(input$mealnumber)))
    )
    FATdiff = FATdiff(FATsum = FATsum2, FATgoal = prescribed_fat_per_meal2(units_per_meal=units_per_meal2(units_per_day=units_per_day2(calories_all_meals=as.numeric(calories_all_meals2(calories=as.numeric(input$calories),snackcalories=as.numeric(input$snackcalories))), kcal_per_unit=as.numeric(kcal_per_unit2(ratio=as.numeric(input$ratio)))),mealnumber=as.numeric(input$mealnumber)),ratio=as.numeric(input$ratio)))
    PROdiff = PROdiff(PROsum = PROsum2, PROgoal = prescribed_protein_per_meal2(protein=as.numeric(input$protein),protein_per_snack=protein_per_snack2(snackcalories=as.numeric(input$snackcalories),protein=as.numeric(input$protein), calories=as.numeric(input$calories)),mealnumber=as.numeric(input$mealnumber)))
    CALdiff = CALdiff(calories=as.numeric(input$calories),CALsum=as.numeric(CALsum))
    RATIOdiff = RATIOdiff(ratio=as.numeric(input$ratio),RATIOsum=as.numeric(RATIOsum))
    
    if  (PROdiff <=  0.05 && FATdiff <=  0.05 && CHOdiff <=  0.05 && PROdiff >=  -0.05 && FATdiff >=-0.05 && CHOdiff >= -0.05)
    {print("Your meal is ready!")}
    else 
    {print("Your meal isn't ready yet.")}
    
    
  })
  


 
 
 
})