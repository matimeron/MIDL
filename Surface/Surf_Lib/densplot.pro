Pro Densplot, z, d, scale = sca, no_tail = ntl, _extra = _e

;+
; NAME:
;		DENSPLOT
; VERSION:
;		4.6
; PURPOSE:
;		Plotting "box density" functions.
; CATEGORY:
;		Display, specialized.
; CALLING SEQUENCE:
;		DENSPLOT, Z, D [, SCALE = SCA]
; INPUTS:
;	Z
;		Numeric vector, a set of the Z values where the D input changes..
;	D
;		Numeric vector, a set of D values corresponding to Z.  The
;		value D[i] is the constant density in the interval (D[i-1], D[i])
;
;		Note 1:	Z can also be given as a [2,*] array.  In this case the
;				first column is taken as Z and the second as D.  D should not
;				be provided separately in such case.
;		Note 2:	Z and D, when provided separately, must have same length.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SCALE
;		A value used to "round off" the horizontal display scale (rounded to an
;		integer multiple of SCALE).  Default value is 20.
;	/NO_TAIL
;		Switch.  Normally DENSPLOT assumes that D = 1 for all Z values beyond
;		the last provided, and draws the plot with this "high Z" tail.  Setting
;		NO_TAIL switches off this behavior (and causes SCALE to be ignored).
;	_EXTRA
;		A formal keyword used to pass plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		Non other than graphic output.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Converts the provided set of values into a "box plot", with the final D
;		value being always 1.  Calls DEFAULT and SPLIT_XY from MIDL.
; MODIFICATION HISTORY:
;		Created 10-FEB-2004 by Mati Meron.
;-

	on_error, 1
	sca = Default(sca,20.,/dtyp)

	n = Split_xy(z,d,x=wz,y=wd)
	if n gt 0 then begin
		ind = 2*lindgen(n)
		pz = (pd = fltarr(2*n+1))
		pz[ind] = (pz[ind+1] = wz)
		pd[ind] = (pd[ind+1] = wd)
		pd = shift(pd,-1)
		if keyword_set(ntl) then begin
			pz = pz[0:2*(n-1)]
			pd = pd[0:2*(n-1)]
		endif else begin
			pz[2*n] = sca*(floor(pz[2*n-1]/sca) + 1)
			pd[2*n-1:2*n] = 1
		endelse
		dmin = min(pd,max=dmax)
		plot, pz, pd, yran = [floor(dmin),floor(dmax)+1], thi=2, _extra = _e
	endif else message, 'No data!'

	return
end