library(shiny)
library(shinyjs)
library(gsheet)
# load coefficient table
ctab = gsheet2tbl("https://docs.google.com/spreadsheets/d/1eU_eywdp52pJzhVHuAEyEc1cmVKE-qj3MHcGkGK33yE/edit#gid=1506178737")
  # type:	1~4, corresponding to female_white, female_african, male_white, male_african
  # population: female_white, female_african, male_white, male_african
  # c_ln_age: coefficient of ln(age)
  # c_ln_age_sq: coefficient of ln(age)^2
  # c_ln_tcho: coefficient of ln(total cholesterol)
  # c_ln_age_ln_tcho: coefficient of ln(age)*ln(total cholesterol)
  # c_ln_hdl: coefficient of ln(high-density lipoprotein)
  # c_ln_age_ln_hdl: coefficient of ln(age)*ln(high-density lipoprotein)
  # c_ln_tsbp: coefficient of ln(treated systolic blood pressure)
  # c_ln_age_ln_tsbp: coefficient of ln(age)*ln(treated systolic blood pressure)
  # c_ln_sbp: coefficient of ln(systolic blood pressure)
  # c_ln_age_ln_sbp: coefficient of ln(age)*ln(systolic blood pressure)
  # c_smoker: coefficient of current smoker
  # c_ln_age_smoker: coefficient of ln(age) * (current smoker)
  # c_dm: coefficient of diabetes

shiny::shinyApp(
  #################### UI part ####################
  ui = fluidPage(
    # Set up shinyjs
    useShinyjs(),  
    titlePanel("心血管事件10年風險估計 (ACC/AHA 2013 ASCVD Risk Calculator)"),
    # text input box
    textInput("text", label = h5(tags$div( "請依序輸入資料, 數值間以空格分開: ", tags$br(), tags$br(),
                                           "1. 性別 (1=女,2=男) ", tags$br(),
                                           "2. 族裔 (1=其他,2=非裔) ", tags$br(),
                                           "3. 年紀 (歲) ", tags$br(),
                                           "4. 總膽固醇 (mg/dl) ", tags$br(),
                                           "5. 高密度膽固醇 (mg/dl) ", tags$br(),
                                           "6. 收縮壓 (mmHg) ", tags$br(),
                                           "7. 是否服用高血壓藥物 (1=有,0=無) ", tags$br(),
                                           "8. 是否有糖尿病 (1=有,0=無) ", tags$br(),
                                           "9. 目前是否為吸菸者 (1=有,0=無)")), 
              value = "1 1 40 210 30 150 1 0 0"),
    # record the last keystroke on the input
    tags$script('
        $(document).on("keydown", function (e) {
        Shiny.onInputChange("lastkeypresscode", e.keyCode);
        });
        '),
    # action button
    actionButton("action", label = "開始計算"),
    hr(),
    fluidRow(column(6, wellPanel(verbatimTextOutput("value"))))
  ),
  
  #################### server part ####################
  server = 
    function(input, output) {

      observeEvent(input$action, {
          #split input$text into sex, race, age, TCHO, HDL, SBP, HTN, DM, Smoker
          text = strsplit(input$text, ' ')[[1]]
          
          #store input data into "inptab"
          inptab = NULL
          inptab$sex = if(text[1]==1) 'female' else 'male'
          inptab$race = if(text[2]==1) 'other' else 'african'
          inptab$age = as.numeric(text[3])
          inptab$tcho = as.numeric(text[4])
          inptab$hdl = as.numeric(text[5])
          inptab$sbp = as.numeric(text[6])
          inptab$htn = as.numeric(text[7])
          inptab$dm = as.numeric(text[8])
          inptab$smoker = as.numeric(text[9])
          
          # determine the coefficient by input table variable sex and race
          population_type = NULL # population_type = 1~4 corresponding to ctab$type
          if (inptab$sex == 'female') {
            population_type <- if(inptab$race == 'other') 1 else 2
          } else {
            population_type <- if(inptab$race == 'other') 3 else 4    
          }
          
          # present the coefficients as matrix with dimention 1*13 (r*c)
          cmat <- as.matrix(ctab[ctab$type==population_type, ][-1:-4])
          
          # construct the input matrix with dimention 13*1 (r*c)
          inpmat <- matrix(c(1:13), nrow = 13, ncol = 1)
          row.names(inpmat) = c("ln_age", "ln_age_sq", "ln_tcho", "ln_age_ln_tcho", "ln_hdl",
                                "ln_age_ln_hdl", "ln_tsbp", "ln_age_ln_tsbp", "ln_sbp", "ln_age_ln_sbp", 
                                "smoker", "ln_age_smoker", "dm") # corresponding to ctab variables with prefix "c_"
          inpmat[1,] = log(inptab$age)
          inpmat[2,] = log(inptab$age)^2
          inpmat[3,] = log(inptab$tcho)
          inpmat[4,] = inpmat[1,] * log(inptab$tcho)
          inpmat[5,] = log(inptab$hdl)
          inpmat[6,] = inpmat[1,] * inpmat[5,]
          inpmat[7,] <- if(inptab$htn ==0) 0 else log(inptab$sbp)
          inpmat[8,] <- if(inptab$htn ==0) 0 else inpmat[1,] * inpmat[7,]
          inpmat[9,] <- if(inptab$htn ==1) 0 else log(inptab$sbp)
          inpmat[10,] <- if(inptab$htn ==1) 0 else inpmat[1,] * inpmat[9,]
          inpmat[11,] = inptab$smoker
          inpmat[12,] = inpmat[1,] * inpmat[11,]
          inpmat[13,] = inptab$dm
          
          # calculate ASCVD 10-y risk 
          sum = cmat %*% inpmat # matrix multiplication of coefficient matrix to input matrix
          baseline_survival = ctab$baseline_survival[ctab$type == population_type] # baseline survival according to the population type
          mean_sum = ctab$mean_sum[ctab$type == population_type] # mean result of matrix multiplication according to the population type
          ascvd_risk = 1-baseline_survival ^ exp(sum - mean_sum)
          
          # show the result
          output_text = '動脈粥樣硬化性心血管事件之10年風險預估值為 '
          #Encoding(output_text) <- 'UTF-8'
          output_text2 = round(ascvd_risk*100, 2)
          output_text3 = ' %'
          final_output = paste0(output_text, output_text2, output_text3)
          
          output$value <- renderText({isolate(final_output)})
        }
      )
    }
)
