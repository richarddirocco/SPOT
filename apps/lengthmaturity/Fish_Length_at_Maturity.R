# This script runs on the first of every month at midnight
# To edit when the crontab starts, use the terminal command: sudo crontab -u rdirocco -e
# Run monthly using this command:
# 0 0 1 * * Rscript  /home/rdirocco/RMarkdownWebsite/apps/lengthmaturity/Fish_Length_at_Maturity.R
# View the log by entering: grep CRON /var/log/syslog

# Load packages
library(rfishbase)
library(readr)
library(tools)

# Import list of common and scientific names of Canadian Species
# Downloadeded from here: http://fishbase.org/Country/CountryChecklist.php?showAll=yes&c_code=124&vhabitat=fresh
SpeciesList <- read_csv("~/RMarkdownWebsite/apps/lengthmaturity/FishList.csv")

# Download species code (SpecCode) for each species, this is needed for maturity data
# The database has multiple species codes for some species so a new table is required
SpecCodes <- synonyms(species_list = SpeciesList$'Scientific Name', limit = 500, fields = c("SpecCode","SynGenus","SynSpecies"))

# Download maturity data
MaturityData <- maturity(species_list = SpecCodes$ScientificName, limit = 5000)

# Delete rows that don't have Lm or Minimum Mature Length
MaturityData <- MaturityData[!with(MaturityData,is.na(Lm) & is.na(LengthMatMin)),]

# Add common names to dataframe
MaturityData$comname <- SpeciesList$`English Common Name`[match(MaturityData$Species,SpeciesList$'Scientific Name')]
MaturityData$frenchname <- SpeciesList$'French Common Name'[match(MaturityData$Species,SpeciesList$'Scientific Name')]

# Delete rows that don't have English Common Name
MaturityData <- MaturityData[!with(MaturityData,is.na(comname)),]

# Import list of countries and their respective country_codes
CountryCodes <- read.csv("~/RMarkdownWebsite/apps/lengthmaturity/Country_Codes.csv", stringsAsFactors = FALSE)

# Replace country codes with country names
MaturityData$Country <- CountryCodes$Country.English[match(MaturityData$C_Code, CountryCodes$C_Code)]

# Some sources use LengthMatRef and others use MaturityRefNo
# If there is no LengthMatRef, replace with MaturityRefNo
MaturityData$LengthMatRef[is.na(MaturityData$LengthMatRef)] <- MaturityData$MaturityRefNo[is.na(MaturityData$LengthMatRef)]

# Create function to make clickable links with short citation from fishbase
links <- function(x) paste0("<a href=http://fishbase.ca/references/FBRefSummary.php?id=",MaturityData$LengthMatRef[x]," target='_blank' >", references(codes = MaturityData$LengthMatRef[x], fields = "ShortCitation" )[1], "</a>")

# Convert reference number to a clickable link
MaturityData$LengthMatRef <- mapply(links, 1:nrow(MaturityData))

# Reorder columns and delete fluff
MaturityData <- MaturityData[,c("Species","comname", "frenchname", "Sex","LengthMatMin","Lm", "Type1", "Country","Locality","LengthMatRef")]

# Rename columns
colnames(MaturityData) <- c("ScientificName", "CommonName","FrenchName", "Sex", "Minimum length at first maturity", "Mean length at first maturity (Lm)", "Measurement type", "Country", "Locality", "Reference")

# Convert length from cm to mm
MaturityData$"Minimum length at first maturity" <- MaturityData$"Minimum length at first maturity" * 10
MaturityData$"Mean length at first maturity (Lm)" <- MaturityData$"Mean length at first maturity (Lm)" * 10


# Import data derived from Coker et al., 2001 and other manually added data
OtherLengthAtMaturity <- read_csv("~/RMarkdownWebsite/apps/lengthmaturity/OtherLengthAtMaturityData.csv")

# Replace NAs in sex with "Unsexed"
OtherLengthAtMaturity$Sex[is.na(OtherLengthAtMaturity$Sex)] <- "Unsexed"

# Make links clickable
OtherLengthAtMaturity$Reference <- paste0("<a href=",OtherLengthAtMaturity$URL," target='_blank' >", OtherLengthAtMaturity$Reference,"</a>")

# Delete URL column
OtherLengthAtMaturity$URL <- NULL

# Add two data frames together 
MaturityData <- rbind(MaturityData, OtherLengthAtMaturity)


# Apply uppercase function
MaturityData$Sex <- toTitleCase(MaturityData$Sex)
MaturityData$Locality <- toTitleCase(MaturityData$Locality)
MaturityData$CommonName <- toTitleCase(MaturityData$CommonName)


# Get current date for archive and name file
CurrentDate <- Sys.Date()
csvFileName <- paste("~/RMarkdownWebsite/apps/lengthmaturity/ArchivedData/MaturityData_",CurrentDate,".csv",sep=";") 

# Use if statement to make sure the dataframe isn't empty
if(nrow(MaturityData) > 300){
  # Write CSV and make it available for length at maturity app
  write_csv(MaturityData, path = "~/RMarkdownWebsite/apps/lengthmaturity/MaturityData.csv")
  # Write to french app folder
  write_csv(MaturityData, path = "~/RMarkdownWebsite/FR/apps/longueurmaturité/MaturityData.csv")
  # Write CSV and place it in the archive
  write_csv(MaturityData, path = csvFileName)
}
