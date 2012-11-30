# satp

The aim of `satp` is to document the cleaning, organization, exploration, and
analysis of data on violent incidents reported in the English language Pakistan
press, as documented by the South Asia Terrorism Portal. The original data can 
be found at http://www.satp.org/satporgtp/countries/pakistan/database/index.html

## Code Sections

Data Acquisition and Cleaning:

* `satp_clean.R` downloads the data from the SATP website and standardizes the 
format of that data to make it easier to pull out location and even information.

Geolocation:
  
* `satp_geolocate.R` pulls information about province and district from each 
event and then deconflicts information across events.

Event Classification:

* `satp_classify.R` pulls information about instigators of attacks (Pakistani
military, militants, unknown instigator, drones) and casualties (security
personnel or non-personnel) to classify incidents.

Parameter Definition and Selection

* `satp_select.R` examines different lags and hyperparameters at different 
temporal and geographic scales to determine the appropriate settings for the 
eventual models.

Model Building and Analysis

* `satp_model.R` models the data and compiles the analytic findings.

Presentation

* `satp_present.R` organizes the analytic findings into graphs and tables.