;NAME:        save_tseries.pro
;AUTHOR:      Walter Gekelman
;DATE:        Jan 2015
;DESCRIPTION: Saves shade_surf plots of data from 2D data runs.
; Input data has dimensions (ntime,nz,ntheta) and comes from one channel of
; the bdot probe data.

; First you have to save a bunch of image files and then download the file imagej
; (available for free) to make a movie from the files.
; This site lists the instructions for making movies in ImageJ.
; http://www.cmiss.org/cmgui/wiki/MakingMoviesWithImageJ/

FUNCTION SAVE_TSERIES, data
t_start = 0 	; pick a time index to start saving shade_surf plots of bdot data
t_stop = 100 	; pick a time index to stop saving shade_surf plots of bdot data
z = insert your z values ; values for the z-axis
theta = insert your theta values ; values for the theta axis
count = 1000 ; makes sure all the files are loaded in order
; set various plot settings
!P.thick=4 & !X.thick=4 & !Y.thick=4 & !P.charthick=4 & !P.charsize=1.3

device, decomposed=0
loadct, 10 ; my favorite color bar
FOR it = t_start, t_stop DO BEGIN
	print, it
	name = 'movie_fri_' + string(count + it,'(I4)')+ '.tiff' ;create the file name
	b = data[it,*,*]

	;;; The following makes and saves the image
	wset,0
	window, retain=2
	shade_surf, b, z, theta, xtitle='z (cm)', ytitle='theta (degrees)', $
		shades=bytscl(b,min=min(data),max=max(data)), ax=90, az=0
	image=tvrd(true=1)
	write_image, name, 'tiff', image
	wait, 0.5
ENDFOR
END