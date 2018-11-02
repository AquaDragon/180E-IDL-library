;NAME:        lightwave.pro
;AUTHOR:      Walter Gekelman  
;DATE:        Jan 2015
;DESCRIPTION: Visualization of light waves emitting from a point source.

PRO lightwave

device, decomposed=0
loadct,3
window, 0, xsize=1200, ysize=1000
;
nx = 101
ii = complex(0,1)
nz = 101
nang = 101  ; # angles to sum over (arbirtary)
n = 1.   ; index of refraction in air
c = 3.0e10  ; cm/s
f = 2.0e9  ; frequency
w = (2.0*!pi*f)   ; ang freq
kr = n*w/c  ; real part of k
kim = .01   ; Can make it imaginary for damping
theta_max = !pi/2.   ; largest angle
dtheta = 2.0*theta_max/float(nang)
;
angles = fltarr(nang)
angles = (findgen(nang)*dtheta)
angles(*) = angles(*)-max(angles)/2.
;
light = complexarr(nz,nx)
k= complex(kr,KIM)  ; keep it complex for damping
;
xpos = findgen(nx)-float(nx-1)/2.
zpos = 1.0*findgen(nz)
;
for iang = 0, nang-1 do begin;
  for iz = 0,nz-1 do begin
    for ix = 0,nx-1 do begin
      R = sqrt(xpos(ix)^2+zpos(iz)^2)
       phi = atan(xpos(ix),zpos(iz))
       k_dot_r = k*R*cos(phi-angles(iang))
       light(iz,ix) = 1.0*exp(ii*(k_dot_r)) + light(iz,ix)    ; this is complex
;
  endfor  ; ix
endfor  ;  iz
phistr=string(iang)
phistr = 'angle  number = '+ phistr
phistr=strcompress(phistr)  ; get rid of spaces
; shade_surf, real_part(light), zpos, xpos, shades=bytscl(real_part(light)) $
; 	, ax=90, az=0, charsize=2
;xyouts , 20.,40.,phistr,charsize=3  ; put up title
;wait, .05   ; sec

 endfor   ; angle
;
;
 shade_surf, real_part(light), zpos, xpos, shades=bytscl(real_part(light)) $ 
 	, ax=60, az=20, charsize=2, xrange=[0,max(zpos)], /xstyle $
 	, yrange=[min(xpos),max(xpos)], /ystyle
end  