Pro Linslit, slit, flux, cubic = cub

;+
; NAME:
;		LINSLIT
; VERSION:
;		6.4
; PURPOSE:
;		Calculates opening correction for slit linearization.
; CATEGORY:
;		X_ray specific.
; CALLING SEQUENCE:
;		LINSLIT, SLIT, FLUX, [/CUBIC]
; INPUTS:
;	SLIT
;		A vector of slit-opening values.  Should have at least 2 elements (at
;		least 4 when /CUBIC is set).
;	FLUX
;		A vector of flux values corresponding to the slit openings.  Must be
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	CUBIC
;		Switch.  When set, the data is fitted to a cubic polynomial, instead
;		of a linear one in the default mode.
; OUTPUTS:
;		None other than screen output.  The output value is the slit opening
;		offset, i.e. the difference (True_value - Measured_value).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Fits the data and calculates the intercept with the SLIT = 0 axis.
; MODIFICATION HISTORY:
;		Created 30-OCT-2007 by Mati Meron.
;-

	on_error, 1

	if keyword_set(cub) then begin
		cof = Linfit_mm(slit,flux,ord=3)
		ccof = [1,-1,1,-1]*cof
		off = Root('Poleval',cof[0]/cof[1]*[-3,3],par=[1,-1,1,-1]*cof,stat=sta)
	endif else begin
		sta = 1
		cof = Linfit_mm(slit,flux)
		if cof[1] ne 0 then off = cof[0]/cof[1] else sta = 0
	endelse

	print
	if sta then print, off, form ='("	Opening offset is ",f8.4)' $
	else message, 'Bad data, cannot calculate!'

	return
end