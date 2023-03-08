Function Dth, alp, bet, qxy = qxy, lam = lam, ene = ene, dth_alp = dta, $
	radians = rad
;+
; NAME:
;		DTH
; VERSION:
;		4.9
; PURPOSE:
;		Calculating the value(s) of Dth (Detector Theta).
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = DTH ( ALP [, BET], keywords)
; INPUTS:
;	ALP
;		Scalar, the angle ALPHA, in degrees (unless RADIANS is set).
; OPTIONAL INPUT PARAMETERS:
;	BET
;		Scalar or vector, the angle BETA, in degrees (unless RADIANS is set).
;		If not given, defaults to same value as ALPHA.
;		If QXY or DTH_ALP (see below) is given as a vector, BET must be a
;		scalar.
; KEYWORD PARAMETERS:
;	QXY
;		The value(s) QXY in units of inverse Angstrem.  Can be scalar or vector.
;		If BET is given as a vector, QXY must be a scalar.
;	LAM
;		Scalar, the value of the wavelength in Angstrem.
;	ENE
;		Scalar, the value of the energy in keV.
;
;		Note:	If QXY is given, one and only one of LAM and ENE must be
;				provided.  Alternatively, if DTH_ALP (see below) is given, LAM
;				and ENE need not to be provided (and if provided, are ignored).
;	DTH_ALP
;		The angle DTH obtained when BETA = ALPHA.
;
;		Note:	One and only one of QXY and DTH_ALP has to be provided.
;	/RADIANS
;		Switch.  If set, all input angles are assumed to be given in radians.
;		Default is degrees.
; OUTPUTS:
;		Returns the Dth value(s) corresponding to the input value(s).  The
;		result is in the same format as QXY/DTH_ALP or BET.  The result is in
;		degrees unless /RADIANS is set.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The angles must be all in degrees or all in radians (with /RADIANS set).
;		No mixing.
; PROCEDURE:
;		Straightforward, from definition.  Two possible modes of operation, one
;		which accepts multiple values of QXY and a single value of BETA, the
;		second is the other way around.  Calls DEFAULT, ONE_OF and FPU_FIX
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-AUG-2003 by Mati Meron.
;		Upgraded and modified 15-SEP-2004 by Mati Meron.
;-

	on_error, 1
	conv = 12.398424

	if keyword_set(rad) then begin
		mult = 1.
		unit = ' radians'
	endif else begin
		mult = !dtor
		unit = ' degrees'
	endelse
	if n_elements(alp) eq 1 then ralp = (mult*alp)[0] $
	else message, 'Alpha must be a scalar!'
	rbet = mult*Default(bet,alp)
	sfac = sin((ralp+rbet)/2)*sin((ralp-rbet)/2)

	opt = One_of(qxy,dta,val=val)
	if n_elements(val) gt 1 and n_elements(bet) gt 1 $
	then message, 'Either Beta or Qxy must be a scalar!'
	if opt eq 0 then begin
		lopt = One_of(lam,ene,val=lval)
		if n_elements(lval) ne 1 then message, $
		'Energy/vavelength must be a scalar!'
		if lopt eq 0 then wlam = lval else wlam = conv/lval
		k = 2*!pi/wlam
		qsc = val/(2*k)
	endif else qsc = cos(ralp)*sin(mult*val/2)

	bmin = acos((cos(ralp) + 2*qsc) < 1 )
	bmax = acos((cos(ralp) - 2*qsc) > (-1))
	dum = where((abs(rbet) lt bmin) or (abs(rbet) gt bmax), ndum)
	if ndum gt 0 then message, 'Allowed range for |BETA| is ' + $
	strcompress('[' + strjoin(string([bmin,bmax]/mult),',') + ']' + unit + '!')

	res = 2/mult*asin(sqrt(((qsc^2-sfac^2)>0)/(cos(ralp)*cos(rbet))))

	return, FPU_fix(res)
end