;NAME:        langmuir_iv.pro
;AUTHOR:      Timothy DeHaas
;DATE:        March 31, 2014
;DESCRIPTION: This program is a function whose purpose is to find the temperature,
;             density, and plasma potential of a given plasma. This is done through
;             the theory of the Langmuir probe, which hypothesizes the current
;             response of a "collector" placed in a plasma at a set voltage. The 
;             exponential region of this I-V curve is given to be consistent with
;             a maxwellian distribution, hence we can find the temperature. The
;             ion saturation region is assumed to be proportional to the density.
;             The plasma potential is calculated from the intersection between the
;             electron saturation region and the expoential region. 0
;
;SYNTAX:      RESULT = LANGMUIR_IV( VRAMP, VRESPONSE )
;             RESULT    = OutPut - FLOAT VECTOR - 3 elements corresponding to:
;                         0 - Temperature (eV)
;                         1 - Density (cm^-3)
;                         2 - Plasma Potential (V)
;             VRamp     = Input - Float Vector - Voltage placed on langmuir probe
;             VResponse = Input - Float Vector - Voltage meaused across a resistor
;                         as a response to VRamp.


FUNCTION LANGMUIR_IV, VRAMP, VRESPONSE 
   ;This program is a function. This means that, when the program is called from the command line 
   ;(i.e. "IDL> result = langmuir_iv()" ), the program will run and output to the "result" variable. 
   ;Langmuir_IV" is the name of the program. 'VRamp" and "VResponse" are inputs that the programs 
   ;needs in order to run.
 
 
 
;GLOBAL VARIABLES (Constants that will be used in the calculation of the Temp,Den,Vp)
   Resistance = 11.0  ;The langmuir probe is placed at a certain voltage. As a response, current is
                       ;is collected by the probe. However, the computer can only measure voltage. 
                       ;Therefore, what is measure is a voltage across a resistor. This variable 
                       ;"Resistance" is the resistance (in ohms) of that resistor.  

   Area = 0.31 ;It is expected that the current collected by the langmuir probe (or rather
                   ;the ion saturation current) is proportional to the area of the probe. Hence
                   ;we have a variable which contains the area of the probe to help us find the
                   ;density of the plasma. The area should be in units of cm^2.
                     
   Gain_VRamp = 1.0      ;Sometimes, the computer can only measure a certain range or resolution 
   Gain_VResponse = 1.0  ;of voltages (For the LAPD, the range is +/- 2.5 Volts). Therefore, in
                         ;order to get a usable signal for our analysis, we must increase or 
                         ;descrease the strength of our measured signals before they are plugged
                         ;into the computer. These "Gain" Variables represent the quantities by 
                         ;which our input variables will be multiplied in order to correct for 
                         ;any decrease or increase of signal strength that we did.
     
   Mu = 40.0 ;In this lab, we make plasma with different nobel gasses. This variable represents
             ;the atomic weight of the ions in our plasma. We need this variable in the calculation
             ;of the density. Since we use the ion saturation current to calculate the density
             ;we need to know the speed of sound in the plasma. This is a function of the atomic
             ;weight of the ions. For He, Mu = 4.0. For Ar, Mu = 40.0



;PRACTICAL VARIABLES 
;These variables below will change depending on what your data looks like. In this way, they
;are called "practical," as they have nothing to do with the physical aspects of the plasma.
;They are simply here so the the computer program can easily do its job.

   npix_VRamp     = n_elements(VRamp)      ;This calculates the number of elements (pixels) in VRamp
   npix_VResponse = n_elements(VResponse)  ;This calculates the number of elements (pixels) in VResponse
         
   ROI_START  = 400        ;These two statements determine the Region of interest, or 
   ROI_END    = 800        ;the region where we will extract Temperature information 
                                           ;of the plasma. These two numbers are general; they will 
                                           ;change depending on what our data looks like.
 
   Smoothing  = npix_VRamp/20.0            ;Our data can be noisy. This variable indicates the number
                                           ;number of pixels we want to smooth our data by before we
                                           ;extract information from it. This gets rid of some of the
                                           ;noise in the data. Be careful, every time you perform an
                                           ;operation on the data, information is lost.
 


;ERROR CHECKING
 ;In every program you write, it is important to do some error checking. In this case, We will
 ;make sure that the number of elements in both of the inputs is the same. Note: There are many
 ;different things that we can error check that are not included in this error checking section.
   IF (npix_VRamp NE npix_VResponse) THEN BEGIN
    ;The "IF" statement above can be interpreted as "If the number of pixels in VRamp is NOT EQUAL
    ;TO (NE) the number of pixels in VReponse, then run everything under this IF statement."
      PRINT, "ERROR: langmuir_iv.pro"              ;Prints these words to the screen in order
      PRINT, "(npix_VRamp NE npix_VResponse)"      ;to let you know that an error has occured.
      RETURN, "ERROR!!!!!!!!!!!!!!!!"              ;Ends the program.
   ENDIF ;This tells the program that everything below this statement is not included in our "IF"
         ;statement above. Every IF statement needs and ENDIF.



;PROCEDURE
 ;Smoothing the two inputs and correct for the gain.
   Smoothed_VRamp     =  Gain_VRamp*SMOOTH(VRamp, Smoothing)
   Smoothed_VResponse = -Gain_VResponse*SMOOTH(VResponse, Smoothing)/Resistance                                            

 ;Truncate the two input to Region of Interest  
   RAMP   = Smoothed_VRamp[ROI_START:ROI_END]      
   CURVE  = Smoothed_VResponse[ROI_START:ROI_END]  
    ;The two inputs are vectors. A vector is a string of of numbers put together on a line. 
    ;To access a specific portion of a vector (or any type of array), you need to use the
    ;brackets ("[" and "]"). In this case we are given a vector of data, but we are concerened 
    ;with a certain region of interest. So we select the data starting from "ROI_START" to
    ;"ROI_END" and storing that information into two variables called "Ramp" and "Curve."   

 ;Calculate Ion Saturation Current
    ISAT = Mean(Smoothed_VResponse[0:ROI_START]) ;Take and average of the Voltage Response when the
                                                 ;Langmuir probe is most negative.
    

                                       
 ;FINDING THE TERMPERATURE
 ;The temperature is the inverse of the Derivative of the Log of our region of interest.
 ;This section will take the Log of the Region of interest. Then it will fit a line to 
 ;this region. The inverse of the slope should be the temperature. 
   Alog_Curve = ALOG(CURVE-ISAT) ;Take the Natural Log of Curve variable
   Temp_Coeff = POLY_FIT(RAMP, Alog_Curve, 1) ;This function fits a line to our data.
   Temperature = 1.0/Temp_Coeff[1]
  
   
   
 ;FINDING THE PLASMA POTENTIAL
 ;Fit a line to the electron region (everything after ROI_END)
   PP_Coeff = POLY_FIT(Smoothed_VRamp[ROI_END:npix_VRamp-1], Smoothed_VResponse[ROI_END:npix_VResponse-1],1)
   PP_FUNCTION   = PP_Coeff[0]+PP_Coeff[1]*Smoothed_VRamp  ;Create the fitted line for plotting
 
 ;Go back to creating the temperature fit function so we can find the intersection between the
 ;Plasma potential function and the temperature function.
   Temp_FUNCTION    = EXP(Temp_Coeff[0]+Temp_Coeff[1]*Smoothed_VRamp) ;Temperature function
      Diff_Fit_Real = Mean(Curve-Temp_Function[ROI_START:ROI_END])
   Temp_FUNCTION    = Temp_FUNCTION+Diff_Fit_Real

 ;Now, we find the minimum between the two function
   Diff_Temp_PP = ABS(Temp_FUNCTION[ROI_START:npix_VRamp-1] - PP_FUNCTION[ROI_START:npix_VRamp-1])
   WhrMin = WHERE(Diff_Temp_PP EQ Min(Diff_Temp_PP))
   PLASMA_POTENTIAL = Smoothed_VRamp[ROI_START+WhrMin[0]]
 
 
 
 ;FINDING THE DENSITY
   Cs = 9.79e5*SQRT(Temperature/Mu)      ;This is the speed of "sound" in the Plasma (cm/s)
   Density = ABS(ISAT)/(Area*Cs*1.6e-19) ;This method calculates the density useing the ion saturation 
                                         ;current. The Density can also be estimated by the electron current. 
 
  
  
 ;PLOT THE I-V CURVE                                         
   Device, Decompose=0    ;This line tells the computer that you would like to plot in color
   LoadCT, 39             ;This loads a preset color table.
    
  ;Below is created a few variable that will be used in the plotting
   VRampTitle     = "Voltage On Langmuir Probe (Volts)"
   VResponseTitle = "Reponse Current (Amps)"
   PlotTitle      = "I-V Curve of Langmuir Probe"
   VoltageRange   = [Min(Smoothed_VRamp) , Max(Smoothed_VRamp)]
   CurrentRange   = [Min(Smoothed_VResponse)-0.1*ABS(Min(Smoothed_VResponse)),$
                     Max(Smoothed_VResponse)+0.1*ABS(Max(Smoothed_VResponse))]

  ;The Statement below is the plot procedure. It Plots the I-V Curve with the smoothed data.
  ;By using the "XTitle, YTitle, Title" keywords, we can label the plot and the axes. The
  ;"XRange, YRange" keywords indicate the region that will be plotted on the screen.
   PLOT, Smoothed_VRamp, Smoothed_VResponse, XTitle = VRampTitle, YTitle = VResponseTitle, $ 
         Title = PlotTitle, XRange = VoltageRange, YRange = CurrentRange, /XStyle, /YStyle

  ;We can also plot another graph on top of the one we just made. In this case, we will plot 
  ;two lines that indicate the regions of interest that we have selected in the "PRACTICAL
  ;VARIABLES" Section
   OPLOT, [Smoothed_VRamp[ROI_START],Smoothed_VRamp[ROI_START]], CurrentRange
   OPLOT, [Smoothed_VRamp[ROI_END],Smoothed_VRamp[ROI_END]],    CurrentRange
  ;Overplot, the Fitted Temperature (exponential) function and the fitted plasma potential
  ;line in the rightmost region. The color table is indexed from 0 to 255. To plot in color
  ;use the "color" keyword with a number between 0 and 255. 
   OPLOT, Smoothed_VRamp, Temp_FUNCTION, COLOR = 100
   OPLOT, Smoothed_VRamp, PP_FUNCTION,   COLOR = 200



;FINAL VARAIBLE TO RETURN
   PLASMA_PARAMETERS = FLTARR(3)           ;Create a variable (a vector of 3 numbers)
   PLASMA_PARAMETERS[0] = Temperature      
   PLASMA_PARAMETERS[1] = Density
   PLASMA_PARAMETERS[2] = Plasma_Potential

Print, "Temperature (eV): ", Temperature
Print, "Density (cm^-3): ", Density
Print, "Plasma Potential (Volts): ", Plasma_Potential   
RETURN, PLASMA_PARAMETERS
END