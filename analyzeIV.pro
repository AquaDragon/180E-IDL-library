;NAME:        analyzeIV.pro
;AUTHOR:      Walter Gekelman / swjtang 
;DATE:        02 Nov 2018
;DESCRIPTION: A routine to calculate temperature, plasma potential and density
;             from the IV curve.

FUNCTION analyzeIV, ramp, current, flag=flag

; This is a flag to control curve plotting. Set to not zero to enable the plots.
IF NOT KEYWORD_SET(flag) THEN flag=0

resistance = 10.2                    ; resistance connected to probe [Ohms]
area = 2.0*!PI*(2.54*0.125/2)^2      ; total probe area [cm^2] for 2-sided 1/8-inch disc
bottom = 1.6e-19*area
npoints = n_elements(current)        ; number of points on the dataset

;; Check if the probe attenuates the signal (1:10 or 1:100) and if the scope is set to correct it?
;; If the scope does not automatically correct the gain, we can manually correct the gain here.
ramp_gain = 1.0
curve_gain= 1.0

;;;; --------------------------------------------------------------------------
;; We can smooth our data to get rid of high frequency noise. Smoothing in IDL uses a box-car ...
;; ... averaging method where it takes the 0th to Nth points, averages it and sets it to the N/2th ...
;; ... value. It then repeats this for the 1st to (N+1)th point and so on until it reaches the last ...
;; ... point. As a result of this process, the first and last N/2th points will not be smooth.  ...
;; ... We have to get rid of these values when analyze our data.
smoothing=FIX(npoints/50.)          ; first specify a smoothing range (our default is 2%)

; Define our region of interest. Minimally, we want to discard the unsmoothed points.
st = smoothing                      ; starting index (we discard a bit more than N/2)
ed = n_elements(ramp)-1-smoothing   ; end index

ramp1 = ramp_gain*SMOOTH(ramp[*],smoothing) ; first smooth the data
ramp1 = ramp1[st:ed]                        ; then get rid of the unsmoothed points

curve = REFORM(current)           ; get rid of extra index
isat = mean(curve[st:st+100])     ; an estimate of the ion saturation current
curve = curve-isat                ; zero the ion saturation current
curve1 = curve_gain*SMOOTH(curve,smoothing)/resistance  ; find true current value (assume input is -I)
curve1 = curve1[st:ed]            ; gets rid of noise after smoothing
ncurve= n_elements(curve1)
factor = max(curve1)-min(curve1)  ;; Normalize the current to a max value of 1. This prevents ...
curve1 = curve1/factor            ;; ... 'etest' from diverging if the current values are too large

;; We want to define certain points on the IV curve to help us analyze it. Note this this is ...
;; ... all done on the current curve (I as a function of index). We can perform a one-to-one ...
;; ... map later in the program using the voltage curve (V as a function of index), so that ...
;; ... we can eventually get I as a function of V later on.
ymax = max(curve1)                ; have to take the min/max again after normalization
ymin = min(curve1)
y25 = 0.25*(ymax-ymin) +ymin      ; 25th percentile y-value
y75 = 0.75*(ymax-ymin) +ymin      ; 75th percentile y-value
wx25 = value_locate(curve1,y25)   ; find the x-coordinate of y25, y75
wx75 = value_locate(curve1,y75)

; Checks if curve1 input was a positive or negative current. If it is positive (i.e. +I), then we need ...
; ... to flip so it is negative (+I -> -I).
IF wx25 GT wx75 THEN BEGIN
  print, '!!! [analyzeIV] Positive current input detected. Flipping current input (+I -> -I).
  dummy=wx25 & wx25=wx75 & wx75=dummy   ;; here we exchange the two variables with the help ... 
  dummy=ymax & ymax=ymin & ymin=dummy   ;; ... of a dummy variable
  dummy=y25  & y25=y75   & y75=dummy
  curve1=-curve1
ENDIF

;;;; --------------------------------------------------------------------------
;; Now estimate the transition region with a quadratic polynomial fit. We use the region between ...
;; ... the 25th and 75th percentile values. All these are arbitrary and have no physical significance.
bins = wx75-wx25
xarray = findgen(bins)
yarray = curve1[wx25:wx75-1]
straight = poly_fit(xarray,yarray,2)                        ; 2nd order (quadratic) polynomial fit
pline = straight[2]*xarray^2+straight[1]*xarray+straight[0] ; quadratic fit to transition region

; Estimate the slope of the exponential region
fold = y25/(exp(1)^3)                     ; find the y-value of one folding length below y25
efold = value_locate(curve1,fold)         ; ... and determine its x-coordinate
ebins = wx25-efold                        ; the width of the region we expect to be exponential
etest = y25/(2*straight[2]*xarray[0]+straight[1]) ; guess the temperature of the exponential slope
halfw = etest+wx25
IF halfw GE n_elements(ramp1) THEN halfw = n_elements(ramp1)-10  ; prevents high temp or mistake
vals = ebins-findgen(ebins)        ;; reverse the indices since the exponential region ...
expo = y25*exp(-(vals)/etest)      ;; ... decays from right to left

xpts = findgen(ebins)+wx25-ebins
FOR i = 0, 1000 DO BEGIN
  ; Sum the difference between the data and our theoretical fit to obtain an error residue
    err = int_tabulated(xpts,(expo-curve1[efold:wx25]))
  ; If the error is small, stop iterating
    IF abs(err) LT .03 THEN GOTO, outtahere
  ;; Otherwise, correct for 'etest' using our residue. Note that the error is positive when ...
  ;; ... expo>curve1 and negative when expo<curve1. We can perform a direct subtraction of ...
  ;; ... the error residue from our test parameter because it will necessarily converge ...
  ;; ... after a few iterations.
    etest= etest-err                 
    expo = y25*exp(-(vals)/etest)   ;; Resulting curve with new test parameter
ENDFOR
outtahere:

; Now we plot our results to see how we did.
IF flag NE 0 THEN BEGIN
  device, decomposed=0                 ; set the windows to display color
  loadct, 65, /silent                  ; loads a color table
  plot, ramp1, curve1*factor, charsize=2, thick=4, xtitle='Voltage [V]', ytitle='Current [A]'
  loadct, 34, /silent
  oplot, ramp1[xpts], expo*factor, color=75, thick=2 ; (blue)
  oplot, ramp1[xarray+wx25], pline*factor, color=500, thick=2 ; (red) see how we did
ENDIF

;;;; --------------------------------------------------------------------------
; Now estimate the esat region. Use the first 20% of the data from the right
nline = ncurve/5                           ; # points estimated to be in the esat region
xarray2 = findgen(nline)+(ncurve-nline-1)  ; the x-indices of the points in 'nline'
esdata = curve1[ncurve-nline:ncurve-1]     ; values of the points in 'nline'
result = poly_fit(xarray2,esdata,1)        ; find fit parameters
  
; Extrapolate a straight line from the esat region
xarray3 = findgen(ncurve)                  ; we intend to extrapolate all the way to the start
bigline = xarray3*result[1]+result[0]      ; extrapolated best fit line
IF flag NE 0 THEN oplot, ramp1[xarray3], bigline*factor, color=110, thick=1 ; (green)

; Using a quadratic function extrapolate a staight line from wx25 to estimate plasma potential
xarray4 = findgen(ncurve-wx25)      ; we extrapolate it from wx25 so the indices need to be shifted
bigpig = xarray4^2*straight[2]+xarray4*straight[1]+straight[0]   ; extrapolated best fit line
IF flag NE 0 THEN oplot, ramp1[xarray4+wx25], bigpig*factor, color=220, thick=2 ; (orange)

; Locate the x-position of the plasma potential. There are many ways to find the intersection ...
; ... between two curves and this is one variation.
kf=0                                    ; initiate a search index starting from zero
residue=1000                            ; set residue to a large number
FOR k=0, ncurve-wx25-1 DO BEGIN
  ;; Note that bigline begins at the start of the IV curve while bigpig starts from wx25 (i.e. their ...
  ;; ... indices are different). We are only interested in bigline after wx25. 
  diff=ABS(bigline[k+wx25]-bigpig[k])   ;; 
  IF diff LT residue THEN BEGIN
    residue = diff                      ; search for the minimum
  ENDIF ELSE BEGIN
    kf = k+wx25                         ; kf is now the index on the IV curve
    GOTO, found
  ENDELSE
ENDFOR
found: 

;;;; --------------------------------------------------------------------------
  curve1 = curve1*factor                ; restore the actual value of curve1 for calculations

IF flag NE 0 THEN BEGIN
  loadct, 65, /silent                   ; check positions of wx25 and kf
  plots, ramp1[wx25], curve1[wx25], psym=1, symsize=3 
  plots, ramp1[kf], curve1[kf], psym=4, symsize=3
ENDIF
  
  Vp = ramp1[kf]                        ; plasma potential
  Te = 2.0*(ramp1[halfw]-ramp1[wx25])
      ;; recall halfw = etest+wx25
      ;; 'etest' is the temperature found from the current curve plotted as a function of index ...
      ;; ... instead of voltage. Thus, we need to convert 'etest' to be in terms of voltage.    ...
      ;; ... We use ramp1 to get the desired range in voltage. (choose wx25 as an arbitrary point)
  vthe = 4.19e7*sqrt(Te)                ; electron thermal velocity
  esat = curve1[kf]                     ; electron saturation current
  ;esat = mean(curve1[kf:0.9*ncurve])
  density = esat/(bottom*vthe)          ; electron density

  print, 'Te      = ', Te, ' [eV]'
  print, 'Vpot    = ', Vp, ' [V]'
  print, 'density = ', density, ' [cm^-3]'

  res = fltarr(3)     ; create an empty floating point array to store our results
  res[0] = Te
  res[1] = Vp
  res[2] = density

  RETURN, res
END