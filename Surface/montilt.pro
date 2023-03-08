Pro Montilt, ang, radians = rad, inch = inch

;+
; NAME:
;		MONTILT
; VERSION:
;		4.3
; PURPOSE:
;		Calculates tilt parameters for the surface 3-circle spectrometer.
; CATEGORY:
;		Surface spectrometer specific.
; CALLING SEQUENCE:
;		MONTILT, ANG [, keywords]
; INPUTS:
;	ANG
;		Vertical deviation (alpha) angle, in degrees unless /RADIANS is set.  A
;		vector of values can also be given.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/RADIANS
;		Switch.  Specifies that ANG is given in Radians.  Default is degrees.
;	/INCH
;		Switch.  Specifies that the motions parameters are given in inches.
;		Default is milimeters.
; OUTPUTS:
;		Prints out a table of the required corrections (for the upstream and
;		downstream jacks and the Mono-y stage).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, calls STREQ and TABULATE from MIDL.
; MODIFICATION HISTORY:
;		Created 30-JUN-2002 by Mati Meron.
;-

	hb = 249.2502
	h  = 439.3184

	if keyword_set(inch) then begin
		lun = ' (inch)'
		conv = 1./25.4
	endif else begin
		lun = ' (mm)  '
		conv = 1.
	endelse

	if keyword_set(rad) then begin
		hang = ang/2.
		aun = ' (rad.)'
	endif else begin
		hang = !dtor*ang/2.
		aun = ' (deg.)'
	endelse

	dym = 2*sin(hang)*(h*sin(hang) + hb*cos(hang))
	dyp = 2*sin(hang)*(h*sin(hang) - hb*cos(hang))
	dz = -2*sin(hang)*(h*cos(hang) - hb*sin(hang))

	tit = 'Required relative moves for 3-circle alpha correction'
	head = [' Angle', ' Upstream Z', ' Downstream Z', $
			' Mono - Y'] + [aun,lun,lun,lun]
	form = replicate('f9.5',4)
	if Streq(aun,' (rad.)') then form[0] = 'f9.6'

	print
	Tabulate, ang, conv*dym, conv*dyp, conv*dz, tit=tit, head=head, form=form
	print

	return
end