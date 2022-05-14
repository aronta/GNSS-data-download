library(curl)
library(stringr)
library(lubridate)
library(shiny)
library(shinyjs)

# options(shiny.trace = TRUE)
original_downloaded_file_path <- "D:/Desktop/Radio/Tema3-projekt/"
original_link <- "https://cddis.nasa.gov/archive/gnss/data/daily/"
authorization_link <- "http://cddis.nasa.gov/archive/gnss/data/daily/1998/007/98o/chur0070.98o.Z"
link_end_for_list <- "*?list"

ui <- fluidPage(
  useShinyjs(),
  h2("Download GNSS data in" ,em("RINEX"), "format as", em("o"), ",", em("n"), "and", em("p"), " files in selected time period"),
  wellPanel(
    h3("Notes before using:"),
    p("Make sure you have a valid account from NASA EarthData, or register ",a(href="https://urs.earthdata.nasa.gov/oauth/authorize?client_id=gDQnv1IO0j9O2xXdwS8KMQ&response_type=code&redirect_uri=https%3A%2F%2Fcddis.nasa.gov%2Fproxyauth&state=aHR0cDovL2NkZGlzLm5hc2EuZ292L2FyY2hpdmUvZ25zcy9kYXRhL2RhaWx5", "here", target="_blank")),
    p("Setup your local download path by changing variable original_downloaded_file_path in code, line 8"),
    code("original_downloaded_file_path <- 'absolute_locale_directory_path'"),
    hr(),
    p("Original Daily 30-second data is located on website - ",a(href="https://cddis.nasa.gov/Data_and_Derived_Products/GNSS/daily_30second_data.html", "NASA EarthData", target="_blank")),
    p("Keep check of your downloading progress by looking at the 'Event Log' down below"),
  ),
  textInput(
    'user_name',
    'Enter Username: ',
    value = "",
  ),
  passwordInput(
    'password',
    'Enter Password: ', 
    value = "",
  ),
  dateInput(
    'start_date',
    'Enter Start date: ',
    min = '1992-01-01',
    max = Sys.Date(),
    format = "yyyy-mm-dd",
    startview = "month",
    weekstart = 0,
    language = "en",
  ),
  dateInput(
    'end_date',
    'Enter End date: ',
    min = Sys.Date(),
    max = Sys.Date(),
    format = "yyyy-mm-dd",
    startview = "month",
    weekstart = 0,
    language = "en",
  ),
  actionButton("get_data", "Submit"),
  uiOutput("spinner"),
  hr(),
  verbatimTextOutput('download_data_period'),
  verbatimTextOutput('message_output'),
  tags$script('
    Shiny.addCustomMessageHandler("status_text", function(text) {
        document.getElementById("message_output").innerHTML += text;
    })
    Shiny.addCustomMessageHandler("clear_status_text", function(text) {
        document.getElementById("message_output").innerHTML = "";
    })
  '),
)

server <- function(input, output, session) {
  # Ispis koji opisuje od kad do kad se ocekuju skinuti podaci
  output$download_data_period <- renderText({
    sprintf('Get RINEX data from Start date: %s to End date: %s', 
            input$start_date, input$end_date)
  })
  
  # Kontrola End date ovisno o Start Date
  observeEvent(input$start_date,  ignoreInit = TRUE, {
    start_date <- input$start_date
    updateDateInput(session, 
                    "end_date",
                    min   = paste(start_date),
    )
  })
  
  session$sendCustomMessage("status_text", "Event Log:\n")
  
  observeEvent(input$get_data, {
    
    disable("get_data")
    session$sendCustomMessage("clear_status_text", "remove_text")
    session$sendCustomMessage("status_text", "Event Log:\n")
    
    # Iz formatirat podatke sve sto nam treba
    all_dates <-seq(as.Date(input$start_date), as.Date(input$end_date), by="days")
    all_days <- yday(all_dates)
    all_years <- year(all_dates)
    
    # Test username, password
    #username: umr18840zcrcdcom
    #password: umr18840@zcrcd.coM
    usrpwd <- paste(isolate(input$user_name), isolate(input$password), sep=":")
    
    # Slozit handler
    h <- curl::new_handle()
    curl::handle_setopt(
      handle = h,
      httpauth = 1,
      userpwd = usrpwd
    )
    
    session$sendCustomMessage("status_text", "Checking user credentials\n")
    
    resp <- curl::curl_fetch_memory(authorization_link, handle = h)
  
    # Pocetni upit s kojim se hendla autentifikacija
    if(resp$status_code == 200) {
      session$sendCustomMessage("status_text", "Authorization successful\n")
      session$sendCustomMessage("status_text", "Started Downloading RINEX Data\n")
     
      for (i in seq_along(all_dates)){
        
        download_file_path <- original_downloaded_file_path
        
        # Kreiraj direktoriji za zadanu godinu
        dir.create(file.path(download_file_path, all_years[i]), showWarnings = FALSE)
        #Promjeni trenutni download direktoriji path
        download_file_path <- paste(download_file_path, all_years[i], "/", sep="")
        
        # Kreiraj direktoriji za zadani dan u godini
        dir.create(file.path(download_file_path, all_dates[i]), showWarnings = FALSE)
        download_file_path <- paste(download_file_path, all_dates[i], "/", sep="")
        
        # Kreiraj direktoriji za o n i p
        current_date_in_loop <- as.Date(all_dates[i])
        two_digit_year <- format(current_date_in_loop, "%y")
        
        two_digit_o <- paste(two_digit_year, "o", sep="")
        two_digit_n <- paste(two_digit_year, "n", sep="")
        two_digit_p <- paste(two_digit_year, "p", sep="")
        
        dir.create(file.path(download_file_path, two_digit_o), showWarnings = FALSE)
        dir.create(file.path(download_file_path, two_digit_n), showWarnings = FALSE)
        dir.create(file.path(download_file_path, two_digit_p), showWarnings = FALSE)
        
        download_link <- original_link
        download_link <- paste(download_link, all_years[i], "/", sep="")
        
        # Dan u godini sa 3 znamenke
        day_with_three_digits <- formatC(all_days[i], width = 3, format = "d", flag = "0")
        download_link <- paste(download_link, day_with_three_digits, "/", sep="")
        
        session$sendCustomMessage("status_text", paste("Downloading data for date:", all_dates[i],  "\n"))
        
        # Skidanje O datoteka
        tryCatch(
        {
          tmp <- tempfile()
          
          session$sendCustomMessage("status_text", paste("\t Downloading o folder for date:", current_date_in_loop,  "\n"))
          curl_download(paste(download_link, two_digit_o, "/", link_end_for_list, sep=""), tmp, handle = h)
          directory_list_from_web <- readLines(tmp)
          
          # Da vidim sta se skida
          print(paste(download_link, two_digit_o, "/", sep=""))
          print(directory_list_from_web[length(directory_list_from_web) - 1])
          print(directory_list_from_web[length(directory_list_from_web)])
          
          for (i in seq_along(directory_list_from_web)){
            
            # Zadnja dva ispisa u listi su Total number of files i Total file size
            if (i == length(directory_list_from_web) - 1) break
            
            squished_spaces_data_name = str_squish(directory_list_from_web[i])
            squished_spaces_data_name = strsplit(squished_spaces_data_name, split = " ")
            
            file_name <- squished_spaces_data_name[[1]][1]
            
            req <- curl::curl_download(paste(download_link, two_digit_o, "/", file_name, sep=""),
                                       paste(download_file_path, "/", two_digit_o, "/", file_name, sep=""),
                                       handle = h
                                        )
            
          }
        },
        error=function(cond){
          # HACK: tehnicki tu nemozemo znat da je uvijek 404 al pretpostavljamo yolo
          session$sendCustomMessage("status_text", paste("\t Error: Folder o for date:", current_date_in_loop , "not existing\n"))
        })
        
        # Skidanje N datoteka
        tryCatch(
        {
          tmp <- tempfile()
          session$sendCustomMessage("status_text", paste("\t Downloading n folder for date:", current_date_in_loop,  "\n"))
          curl_download(paste(download_link, two_digit_n, "/", link_end_for_list, sep=""), tmp, handle = h)
          directory_list_from_web <- readLines(tmp)
          
          # Da vidim sta se skida
          print(paste(download_link, two_digit_n, "/", sep=""))
          print(directory_list_from_web[length(directory_list_from_web) - 1])
          print(directory_list_from_web[length(directory_list_from_web)])
          
          for (i in seq_along(directory_list_from_web)){
            
            # Zadnja dva ispisa u listi su Total number of files i Total file size
            if (i == length(directory_list_from_web) - 1) break
            
            squished_spaces_data_name = str_squish(directory_list_from_web[i])
            squished_spaces_data_name = strsplit(squished_spaces_data_name, split = " ")
            
            file_name <- squished_spaces_data_name[[1]][1]
            
            req <- curl::curl_download(paste(download_link, two_digit_n, "/", file_name, sep=""),
                                       paste(download_file_path, "/", two_digit_n, "/", file_name, sep=""),
                                       handle = h
                                        )
          }
        },
        error=function(cond){
          # HACK: tehnicki tu nemozemo znat da je uvijek 404 al pretpostavljamo yolo
          session$sendCustomMessage("status_text", paste("\t Error: Folder n for date:", current_date_in_loop , "not existing\n"))
        })
        
        # Skidanje P datoteka
        tryCatch(
        {
          tmp <- tempfile()
          session$sendCustomMessage("status_text", paste("\t Downloading p folder for date:", current_date_in_loop,  "\n"))
          curl_download(paste(download_link, two_digit_p, "/", link_end_for_list, sep=""), tmp, handle = h)
          directory_list_from_web <- readLines(tmp)
          
          # Da vidim sta se skida
          print(paste(download_link, two_digit_p, "/", sep=""))
          print(directory_list_from_web[length(directory_list_from_web) - 1])
          print(directory_list_from_web[length(directory_list_from_web)])
          
          for (i in seq_along(directory_list_from_web)){
            
            # Zadnja dva ispisa u listi su Total number of files i Total file size
            if (i == length(directory_list_from_web) - 1) break
            
            squished_spaces_data_name = str_squish(directory_list_from_web[i])
            squished_spaces_data_name = strsplit(squished_spaces_data_name, split = " ")
            
            file_name <- squished_spaces_data_name[[1]][1]
            
            req <- curl::curl_download(paste(download_link, two_digit_p, "/", file_name, sep=""),
                                       paste(download_file_path, "/", two_digit_p, "/", file_name, sep=""),
                                       handle = h
                                        )
            
          }
        },
        error=function(cond){
          # HACK: tehnicki tu nemozemo znat da je uvijek 404 al pretpostavljamo yolo
          session$sendCustomMessage("status_text", paste("\t Error: Folder p for date:", current_date_in_loop , "not existing\n"))
        })
        
      }
      enable("get_data")
      session$sendCustomMessage("status_text", "All files Downloaded!")
      
    } else {
      # TODO: ispisat error ako autentikacija ne prode
      enable("get_data")
      session$sendCustomMessage("status_text", "Authorization failed\n")
      
    }
    
  })
  
}

shinyApp(ui, server)




