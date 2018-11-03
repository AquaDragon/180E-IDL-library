;NAME:        feedIV.pro
;AUTHOR:      Walter Gekelman
;DATE:        Jan 2015
;DESCRIPTION: - read in langmuir curves, send data to analyzeIV
; 			  - This will read data in from 41 x positions
;			  - must restore a data set for example, 'LangXY.sav'

PRO FEEDIV
ix = 41 			; number of x positions
dx = 10./41 		; cm step
time= 8501 			; number of time steps in the data
smo = time/80. 		; # points to smooth data over
Ter = fltarr(ix) 	; array to keep electron temp
Vpr = fltarr(ix) 	; fltarr means array of floating point numbers
den = fltarr(ix)
params= fltarr(3)

FOR i = 0, ix-1 DO BEGIN
	curve =smooth(current[*,i], smo) 	; smooth does a running boxcar average
	ramp  =smooth(voltage[*,i], smo) 	; probe voltage array
	params = analyzeIV(ramp, curve) 		; does the curve analyses
	den[i] = params[0]
	Ter[i] = params[1]
	Vpr[i] = params[2]
ENDFOR
; end of loop for analysis at every spatial position

xvals = fltarr(41) 					; x locations of data
xvals = dx*findgen(ix)
xvals = xvals-0.5*max(xvals)
yvals = xvals
plot, xvals, den, charsize=2, thick=2 	; plot density verses x

save, filename='Pressure_line.sav', Ter, Vpr, den, xvals, yvals ; save results

; IDL save files are written in Binary and are highly compressed
; To get the data back as arrays simply write 'restore, filename'
END