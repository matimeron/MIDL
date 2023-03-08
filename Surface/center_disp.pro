Function Center_disp, ang, loc, lerr, reference = ref, direction = dir, $
	radians = rad, show = sho, error = err, chisq = chi

;+
; NAME:
;		CENTER_DISP
; VERSION:
;		4.3
; PURPOSE:
;		Finding the displacement of a center of rotation from geometrical center
; CATEGORY:
;		Surface spectrometer software.
; CALLING SEQUENCE:
;		Result = CENTER_DISP( ANG, LOC [, LERR] [,keywords])
; INPUTS:
;	ANG
;		A vector of angle values (assumed degrees, unless /RADIANS is set).
;	LOC
;		A vector of locations, i.e. transverse offsets of the observed center.
;		Must be same length as ANG.
; OPTIONAL INPUT PARAMETERS:
;	LERR
;		Location value errors.  If given must be either a scalar (meaning same
;		error for all locations) or a vector of same length as ANG.
; KEYWORD PARAMETERS:
;	REFERENCE
;		A reference value for ANG, optional.  If given, working angles are
;		obtaining by referring ANG to REFERENCE.
;	DIRECTION
;		Specifies whether the angles are measured in mathematical positive or
;		negative direction.  May be given either as number (in which case only
;		the sign of the number matters) or as a character string ("pos" or
;		"neg").  If not given, assumed positive.
;	/RADIANS
;		Switch.  If set, all angles are in radians.
;	/SHOW
;		Switch.  If set, plot of the data and the fit is displayed.
;	ERROR
;		Optional output, see below.
;	CHISQ
;		Optional output, see below.
; OUTPUTS:
;		Returns a 3 element vector containing (in order) the x-offset of the
;		observation line from the CoR (center of rotation) and the x and y
;		offsets of the center of the ball relative to the CoR.
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		A 3-element vector containing the rms errors of the parameters returned
;		in the output.
;	CHISQ
;		The chi-squared value for the fit.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		ANGLE, LOC and LERR (if given) must be of same length and the length
;		must be at least 3.
; PROCEDURE:
;		Straightforward least squares fit.  Calls CAST, DEFAULT, ISNUM,
;		LINFIT_MM, SIGN and STRMATCH_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 20-FEB-2003 by Mati Meron.
;-

	on_error, 1
	dposib = ['pos','neg']

	npo = n_elements(ang)
	if n_elements(loc) ne npo then message, 'Size mismatch!'
	if npo lt 3 then message, 'At least 3 elements required!'
	wlerr = Default(lerr,1.,/dtyp)
	ref = Default(ref,0.,/dtyp)
	if Isnum(dir) then wdir = Sign(dir) else wdir = -Strmatch_mm(dir,dposib,3)
	if wdir eq 0 then wdir = 1

	wang = Cast((ang - ref),4)
	wang = wdir*(wang - wang[0]) + wang[0]
	if not keyword_set(rad) then wang = !dtor*wang
	wloc = Cast(loc,4)

	res = Linfit_mm(wang,wloc,1./wlerr,base=['','cos','sin'],resid=chi,err=err)

	if keyword_set(sho) then begin
		if (!d.flags and 256)/256 then begin
			if !d.window eq (-1) then wset
			wshow
		endif
		fit = res[0] + res[1]*cos(wang) + res[2]*sin(wang)
		sor = sort(ang)
		plot, ang[sor], loc[sor]
		oplot, ang[sor], fit[sor], thick = 2
	endif

	return, res*[-1,1,1]
end