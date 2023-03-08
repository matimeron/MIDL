Function BMG_vpow, erange = era, estep = est, psiran = pra, psistep = pst, $
	def= def, ering= ern, rgam= rgm, current = cur, bfield= bfd, ecrit= ecr, $
	absorber = abs, thickness = thi, distance = dst, verco = vco, pack = pck

;+
; NAME:
;		BMG_VPOW
; VERSION:
;		8.714
; PURPOSE:
;		Calculates the vertical profile of a synchrotron bending magnet beam.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = BMG_SPEC( ERANGE=ERA [ESTEP=EST], PSIRAN=PRA [,PSISTEP=PST] $
;				[,keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	ERAN
; 		The top of the desired energy range, in keV.  Bottom is always 0.
; 	ESTEP
; 		The energy step used in the evaluation, in keV.  Default is 1 keV.
; 	PSIRAN
; 		The Psi (vertical) angle range used, in mr.  Working range is
; 		[-PSIRAN, PSIRAN]
; 	PSISTEP
; 		ThePsi step used in the evaluation, in mr.  Default is 0.001 mr (i.e
; 		1 microradian).
;	/DEF
;		Switch.  If set and some inputs are missing, they're replaced by
;		defaults from !BLPAR.
;	ERING
;		The electron energy (in GeV) of the source.	|	One and only one of
;	RGAM											|	these two is needed.
;		The relativistic gamma of the source.		|
;	CURRENT
;		Source electron current, in Amp.  If DEF is set, the default is the
;		machine current, else no default.
;	BFIELD
;		Bending field, in Tesla.	|
;	ECRIT							|	One and only one of these two is needed.
;		Critical energy, in keV.	|
;	ABSORBER
;		Chemical symbol of absorber element.  If given, the result will contain
;		the vertical distribution of absorbed power (requires the value for
;		THICKNESS (see below) to be given as well).  If not given, the
;		distribution of full power will be evaluated.
;	DISTANCE
;		The distance to the point of evaluation, in m.  If given, the result is
;		evaluated as power per mm^2, else (the default)it is evaluated as power
;		per mr^2.
;	VERCO
;		Optional output, see below.
;	/PACK
;		Switch.  If set, the vertical coordinate values and the result are
;		packed together and returned as a [2,N] array.  Else, the result is a
;		vector of values.
; OUTPUTS:
;		By default returns the vertical power density, in units of W/mr^2 or,
;		if DISTANCE is given, in units of W/mm^2.  The result is a vector of
;		values.
;		If PACK is set, the result is a 2 column array, containing the vertical
;		coordinates and the power density values, respectively.
; OPTIONAL OUTPUT PARAMETERS:
;	VERCO
;		Returns the vertical coordinate values corresponding to the power
;		density values in the result.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Evaluates the power density values at each energy following standard
;		synchrotron radiation formalism, then sums over energies.  Calls
;		ABS_COEFF, BL_DEFAULTS, and H_FUN from SRUFF_LIB.  Calls CALCTYPE, CAST,
;		CURRENT, DEFAULT, HOW_MANY, ISNUM, MAKE_GRID and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUL-2019 by Mati Meron, through surgery on BMG_SPEC.
;-

	on_error, 1

	amult = 1d-3
	emult = 1d6
	typ = Calctype(era,pra,0.)

	wdef = Default(def,1,/dtyp)
	dum = How_many(fir=ern,sec=rgm,thi=cur,fou=bfd,whi=whi)
	if wdef then BL_defaults, energy=ern, gamma=rgm, current=cur, bm_field=bfd

	if not Isnum(ecr) then begin
		if wdef $
		then ecr = 3*rgm^2*bfd*!srcon.ee*!srcon.scal/(2*!srcon.alp*!srcon.be) $
		else message, 'Either critical energy or magnetic field is needed!'
	endif else ecr = Cast(ecr,typ)

	est = Default(est,1.,/dtyp)
	e = Make_grid([0,era],est,/step)

	pst = Default(pst,0.001,/dtyp)
	psi = Make_grid([0,pra],pst,/step)
	psi = [-reverse(psi),psi[1:*]]
	res = 0*psi

	pcur = cur/amult
	esc = e/ecr
	psc = rgm*amult*psi
	nesc = n_elements(esc)
	if Type(abs) eq 7 then afac = 1 - exp(-0.1*thi*Abs_coeff(e>1,elem=abs)) $
	else afac = replicate(1,nesc)

	for i = 0, nesc-1 do res = res + afac[i]*H_fun(psc,esc[i])
	res = 3/!dpi^2*pcur*!srcon.alp*rgm^2*amult^2*est*res

	if keyword_set(dst) then begin
		res = res/dst^2
		vco = psi*dst
	endif else vco = psi

	if keyword_set(pck) then res = transpose([[vco],[res]])

	return, Cast(res,typ,typ,/fix)
end