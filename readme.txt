Requirements:
------------
R-portable version 2.2.1
GoogleChromePortable version 2.3.6
Download & locate both files in ~UserDir/SedSS2/ folder.

About:

I. Name
------------
Sediment fingerprinting tool, version 2.0.(Sed-Sat V.2.0.)

II. Layout
------------
GoogleChromePortable	Google chrome portable folder	
R-Portable		R V-3.5.3 portable version
SShiny			R shily files, executable modules
RUN.bat			Main executable bat file-->
runShinyApp.R		R executable file-->shiny application


III. Design
------------
A. Program design

1. Style
Code is written by a team member. Naming conventions of pre-existing code was changed to match the team's preferred style for member functions.

2. Main app
- Execution
Run.bat executable --> runShinyApp.R source --> shinyApp (SShiny) --> App.R

- Description of RUN.bat
Included RUN.bat file used to point R - 3.5.3 portable and Google Chrome Portable folders. Both folders required (1) To run application written in .R code format; 
(2) To launch browser and application interface;

- Description of SShiny folder content
The SShiny folder includes example data set in (ExampleDatasets) folder and executable .R modules in SShiny folder.
Modules are used to split application major frames for simplicity purposes.
App main modules and purpose:
1_upload_module - serves as an input
2_datacheck_module - serves as a data quality assurance and data manipulation before statistical analysis
3_normalize_module - serves as a data quality assurance for statistical purposes
4_outliers_module - serves as a data quality assurance module for model purposes
5_correction_module - serves as a data manipulation module for statistical purposes
6_bracket_module - serves as a data quality assurance module for model purposes
7_dfa_module - serves as a module for statistical analysis of data discrimination
8_mixing_module - serves as an executable module for modelling purposes 
 

