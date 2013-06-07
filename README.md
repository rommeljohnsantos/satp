# satp

The aim of `satp` is to document the cleaning, organization, exploration, and
analysis of data on violent incidents reported in the English language Pakistan
press, as documented by the South Asia Terrorism Portal. The original data can 
be found at http://www.satp.org/satporgtp/countries/pakistan/database/index.html

## Code Sections

Data Acquisition and Cleaning:

* `satp_clean.R` downloads the data from the SATP website and standardizes the 
format of that data to make it easier to pull out location and even information. It
then pulls information about province and district from each 
event and then deconflicts information across events.

Event Classification:

* `satp_classify.R` pulls information about instigators of attacks (Pakistani
military, militants, unknown instigator, drones) and casualties (security
personnel or non-personnel) to classify incidents. This code is extremely messy and 
will soon be replaced with a more efficient method for classifying events.

