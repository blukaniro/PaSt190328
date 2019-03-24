# PaSt

The example for paniscle structure analysis with ImageJ and R

## About:  
This is the bundle of code for panicle structure analysis in Tajima et al. (2019) Analysis of rice panicle structure using image analysis technique.(in Japanese) in the 247th Meeting of the Crop Science Society of Japan. University of Tsukuba. 28/Mar/2019.

## Requirement:  
- ImageJ  
- R  

## Usage:  
1. Create "Panicle" folder and "Grain" folder in your "Home" folder (the folder names are important).  
2. Place "Example.jpg" in "Panicle" folder.
3. "Panicle.ijm" installs to ImageJ (Plugins > Macros > Install...).  
4. Run "Panicle" (Plugins > Macros > Panicle).  
5. Check "Grain" folder (There are 139 images in the folder).
6. "Grain.ijm" installs to ImageJ (Plugins > Macros > Install...).  
7. Run "Grain" (Plugins > Macros > Grain).  
8. Save Log (Log.txt) in your "Home" folder.  
9. Run "Analysis.r" in R. (ex. Copy and paste the code in "Analysis.r" to R console).  
10. Check your "Home" folder. (There is "Fig.pdf")  
