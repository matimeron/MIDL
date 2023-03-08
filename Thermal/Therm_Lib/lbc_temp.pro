Function LBC_temp, d, t, blen, eps, nterms = ntr, precision= prc, net = net, $
	reset = rst

;+
; NAME:
;		LBC_TEMP
; VERSION:
;		0.9
; PURPOSE:
;		Calculates the form-factor for linear, back cooled geometry.
; CATEGORY:
;		Thermal calculations.
; CALLING SEQUENCE:
;		Result = LBC_TEMP( D, T [, BLEN, EPS] [keywords])
; INPUTS:
;	D
;		Power load width.
;	T
;		Plate thickness.
;	BLEN
;		The Biot length (defined as thermal_conductivity/film_coefficient).
;		Setting BLEN to 1 is equivalent to giving D and T in Biot length units)
;		Not providing BLEN at all has same effect.
; OPTIONAL INPUT PARAMETERS:
;	EPS
;		Precision parameter, used internally in numerical calculations.
;		Defaults to the last value saved in the common block TERM_STUFF (see
;		below) or, if that value hasn't been defined, defaults to machine
;		precision.
; KEYWORD PARAMETERS:
;	NTERMS:
;		The number of terms to use in the thermal series.  If not provided,
;		NTERMS is set automatically, in conjunction with PRECISION.
;	PRECISION
;		Character input, sets calculation precision.  Allowed values are
;		'low, 'medium' and 'high' (only first two characters are needed,
;		case doesn't matter).  Default is 'medium'
;	/NET
;		Switch, if set the form-factor is calculated for front-back, instead
;		of the default front-coolant.
;	/RESET
;		Switch, forces recalculation of the thermal series.  For testing
;		purposes only.
; OUTPUTS:
;		Returns the value of the thermal form-factor for linear, back-cooled
;		geometry.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		TERM_STUFF.  Includes current values of of EPS, LAM, NTR and the
;		thermal series (see routine BC_ROOTS for details).  Also includes info
;		for side cooled geometry.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Sums a series resulting from exact calculation.  Calls BC_ROOTS, CAST,
;		DEFAULT, SERIES_SUM, STRMATCH_MM, TOLER and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-FEB-2000 by Mati Meron.
;-

	common term_stuff, sc_ex, sc_ntr, sc_rt, $
	bc_ex, bc_eps, bc_lam, bc_ntr, bc_rt

	on_error, 1
	prepos = ['low','medium','high']
	prval = Strmatch_mm(prc,prepos,2)

	if prval lt 0 then prval = 1
	ntrdef = ceil((3 + prval)*2.*t/d) > 3
	ntr = Default(ntr,ntrdef,/dtyp) > 0

	wblen = Default(blen,1.,lo=4)
	typ = Type(d) > Type(t) > Type(wblen)
	teps = 2*Toler(type=typ)
	weps = Default(eps,Default(bc_eps,teps)) > teps
	lam = t/wblen

	if Default(bc_ex,0) eq 1 then $
	if ntr gt bc_ntr or weps lt bc_eps or lam ne bc_lam then bc_ex = 0

	if Default(bc_ex,0) eq 0 or keyword_set(rst) then begin
		bc_rt = BC_roots(lam,ntr,weps)
		bc_lam = lam
		bc_ntr = ntr
		bc_eps = weps
		bc_ex = 1
	endif

	netfl = keyword_set(net)
	if t gt 0 then begin
		asod = d*bc_rt/(2*t)
		ser =exp(-asod)/(bc_rt^2*(1+sin(2*bc_rt)/(2*bc_rt)))
		res = (1 + bc_lam)/(2*bc_lam)
		if netfl then begin
			ser = (1 - cos(bc_rt))*ser
			res = 0.5d
		endif
		tem = total(ser)
		if abs(ser[bc_ntr-1]) gt weps*tem then begin
			ttem = Series_sum(ser,stat=stat)
			if stat eq 1 then res = res - ttem else res = res - tem
		endif else res = res - tem
		res = 4*t*res/d
	endif else if netfl then res = 0 else res = 2*wblen/d

	return, Cast(res,4,typ,/fix)
end
