# This script runs on the first of everymonth at midnight
# To edit when the crontab starts, use the terminal command: sudo crontab -u rdirocco -e
# View the log by entering: grep CRON /var/log/syslog


# library rfishbase
library(rfishbase)

# Import list of common and scientific names of Canadian Species
# Downloadeded from here: http://fishbase.org/Country/CountryChecklist.php?showAll=yes&c_code=124&vhabitat=fresh
SpeciesList <- read.csv("~/RMarkdownWebsite/apps/lengthmaturity/FishList.csv", stringsAsFactors = FALSE)

# Download species code (SpecCode) for each species, this is needed for maturity data
# The database has multiple species codes for some species so a new table is required
SpecCodes <- synonyms(species_list = SpeciesList$ScientificName, limit = 500, fields = c("SpecCode","SynGenus","SynSpecies"))

# Download maturity data
MaturityData <- maturity(species_list = SpecCodes$SpecCode, limit = 5000)

# Delete rows that don't have Lm or Minimum Mature Length
MaturityData <- MaturityData[!with(MaturityData,is.na(Lm) & is.na(LengthMatMin)),]

# Add common names to dataframe
MaturityData$comname <- SpeciesList$CommonName[match(MaturityData$sciname,SpeciesList$ScientificName)]

# Import list of countries and their respective country_codes
CountryCodes <- read.csv("~/RMarkdownWebsite/apps/lengthmaturity/Country_Codes.csv", stringsAsFactors = FALSE)

# Replace country codes with country names
MaturityData$Country <- CountryCodes$Country.English[match(MaturityData$C_Code, CountryCodes$C_Code)]

# Some sources use LengthMatRef and others use MaturityRefNo
# If there is no LengthMatRef, replace with MaturityRefNo
MaturityData$LengthMatRef[is.na(MaturityData$LengthMatRef)] <- MaturityData$MaturityRefNo[is.na(MaturityData$LengthMatRef)]

# Reorder columns and delete fluff
MaturityData <- MaturityData[,c("sciname","comname","Sex","LengthMatMin","Lm", "Type1", "Country","Locality","LengthMatRef")]

# Convert reference number to a clickable link
MaturityData$LengthMatRef <- paste0("<a href=http://fishbase.us/references/ReferencesList.php?Author=&Year=&Title=&Source=&RefNo=",MaturityData$LengthMatRef," target='_blank' >Reference</a>")

# Rename columns
colnames(MaturityData) <- c("ScientificName", "CommonName", "Sex", "Minimum length at first maturity", "Mean length at first maturity (Lm)", "Measurement type", "Country", "Locality", "Reference")

# Create function to make first letter uppercase
# From: http://stackoverflow.com/questions/18509527/first-letter-to-upper-case
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

MaturityData$Sex <- firstup(MaturityData$Sex)
MaturityData$Locality <- firstup(MaturityData$Locality)


#Get current date for archive and name file
CurrentDate <- Sys.Date()
csvFileName <- paste("~/RMarkdownWebsite/apps/lengthmaturity/ArchivedData/MaturityData_",CurrentDate,".csv",sep="") 

# Use if statement to make sure the dataframe isn't empty
if(nrow(MaturityData) > 200){
  # Write CSV and make it available for length at maturity app
  write.csv(MaturityData, file = "~/RMarkdownWebsite/apps/lengthmaturity/MaturityData.csv")
  # Write CSV and place it in the archive
  write.csv(MaturityData, file = csvFileName)
}


