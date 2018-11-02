;NAME:        c_correlate_fft.pro
;AUTHOR:      Walter Gekelman
;DATE:        12 Jan 2007
;DESCRIPTION: Cross-correlation of two data arrays involving FFT
;SYNTAX:      Result = C_CORRELATE_FFT(x, y, nlag)
;INFO:        Result = 1D array of cross-correlation
;             x,y    = 1D array of data
;             nlag   = An integer specfying the maximum lag time to perform the cross-correlation.

FUNCTION C_CORRELATE_FFT, x, y, nlag 
    on_error, 2
    nt = n_elements(x)
    if nt ne n_elements(y) then begin
        print, 'Input arrays must be the same size.'
        return, 1
    endif

    x_pad = fltarr(nt + nlag/2)
    x_pad[0:nt-1] = x - mean(x)
    y_pad = fltarr(nt + nlag/2)
    y_pad[0:nt-1] = y - mean(y)
    corr = shift(float(fft(conj(fft(x_pad)) * fft(y_pad),/inverse)), nlag/2) $
             / sqrt(total(x_pad^2)*total(y_pad^2)) * (nt + nlag / 2) 
    return, corr;[0:nlag-1]
end