;NAME:        quick_movie.pro
;AUTHOR:      Walter Gekelman
;DATE:        Jan 2015
;DESCRIPTION: this will play a movie of your probe data

; first restore your .sav file --->
PRO QUICK_MOVIE

device, decomposed=0
absemag=sqrt(ex^2+ey^2+ez^2) ;
;eperp = sqrt(ex^2+ey^2)
device, decomposed=0
nx = 25 	; z value
ny = 25 	; x's
x = zpos
y = xpos
loadct,13
window, 0
loadct,3
table=1
ts = 90
te = 495
data = absemag
titl = string(50)
titl= "f 110Mhz , 2ns/step"

FOR i= ts,te DO BEGIN ;change sample num as necessary
	print, i
	shade_surf, data[i,*,*],x,y, shades=bytscl(smooth(data[i,*,*],2)), ax=90, az=0, $
		charsize=2, yrange=[-12.0,12.0], /ystyle, xtitle='z(cm)', ytitle='x(cm)', $
		thick=2, /noerase
	xyouts, 12.0, 12.0, titl, charsize=3
	wait, 0.05
ENDFOR

END