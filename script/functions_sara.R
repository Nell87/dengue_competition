#### 0. INCLUDES / PREPARING DATA #### 

#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 

pacman::p_load(corrplot, dplyr)

#### 1. EXPLORATORY ANALYSIS ####

# Plots geom density + variable in facet_wrap (ex. source in facet_wrap, for 
# comparing training and validation)
geom.density.function<- function(data, variables, fillby){
  
  # Melt with the selected variables
  data<- data[variables] %>%
    melt(fillby) %>%
    rename("id"=fillby)
  
  # geom_density
  ggplot(data, aes(x = value, fill = id)) + 
    geom_density(alpha = 0.2) + facet_wrap(~variable, scales = "free_x")
  
}

# Plotly with lines. Example: datetime and different variables 
# (val and train together)
plotly.line.function <- function(data, variables, x){
  
  # Melt with the selected variables
  data<- data[variables] %>%
    melt(x) %>%
    rename("id"=x)
  
  # plotly line
  plot_ly(data, x = ~id, y = ~value, 
          color = ~variable, type = 'scatter', mode = 'lines') 

}



#### 3. RELATIONSHIP BETWEEEN VARIABLES #### 

# REMOVE COLLINEARITY
# By default, the threshold is going to be 0.85

remove.collinearity<- function(data, indep_var, threshold=0.85){
  
  # 1.Check if all the variables are numeric
  if(sum(sapply(data, is.numeric))!=ncol(data)){
    print("You have non-numeric variables!")
  }
  
  # 2. Create the correlation matrix
  cor_indep_var<- cor(within(data, rm("total_cases")))
  cor_indep_var[upper.tri(cor_indep_var, diag=TRUE)] <- NA
  cor_indep_var<-as.data.frame(reshape2::melt(cor_indep_var, na.rm=TRUE, 
                                              value.name="cor"))

  cor_dep_var<- as.data.frame(cor(data)) %>% select(indep_var)
  
  # 3. Show collinearity
  cor_indep_var_toremove<-cor_indep_var %>% filter(cor>=threshold)
  
  # 4. Remove collinearity
  
  # Let's create an empty vector
  variables_to_remove<-c()
  
  for (i in 1:nrow(cor_indep_var_toremove)){
    
    # Variables to check in the correlation matrix with the dep. var.  
    v1<- cor_indep_var_toremove[i,"Var1"] 
    v2<- cor_indep_var_toremove[i,"Var2"]
    
    corv1<-abs(cor_dep_var[which(rownames(cor_dep_var)==v1),"total_cases"])
    corv2<-abs(cor_dep_var[which(rownames(cor_dep_var)==v2),"total_cases"])
    
    print(paste(v1, " = ", corv1, " ", v2, " = ", corv2))
    
    if(corv1 >= corv2){
      print(paste("Let's remove ", v2))
      variables_to_remove<-append(variables_to_remove, v2)
    
    } 
    
    else{
      print(paste("Let's remove ", v1))
      variables_to_remove<-append(variables_to_remove, v1)
    }
    print("____________________________________")
      
  }
  
  data <- data %>% select(-variables_to_remove)
}
  
 

