Function BMG_spec, e, def= def, rgam= rgm, ering= ern, ecrit= ecr, bfield= bfd,$
	current = cur, tet_range = tet, psi_range = psi, power = pow, band = ban, $
	vsig = vsi

;+
; NAME:
;		BMG_SPEC
; VERSION:
;		6.0
; PURPOSE:
;		Calculates the energy spectrum of a bending magnet synchrotron source.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = BMG_SPEC( E {RGAM=RGM or ERING=ERN} {ECRIT=ECR or BFIELD=BFD} $
;						 [,keywords])
; INPUTS:
;	E
;		Numeric scalar or vector, energy in keV units.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/DEF
;		Switch.  If set and some inputs are missing, they're replaced by
;		defaults from !BLPAR.
;	RGAM
;		The relativistic Gamma of the source.		|	One and only one of
;	ERING											|	these two is needed.
;		The electron energy (in GeV) of the source.	|
;	ECRIT
;		Critical energy, in keV.	|
;	BFIELD							|	One and only one of these two is needed.
;		Bending field, in Tesla.	|
;	CURRENT
;		Source electron current, in Amp.  Default is 1 Amp.
;	TET_RANGE
;		The Theta (horizontal) angular extent of the source, in miliradians.
;		Default is 1 miliradian.
;	PSI_RANGE
;		The PSI (vertical) angular extent of the source, in miliradians.
;		Default is unlimited.  PSI_RANGE may be a scalar or a vector of same
;		length as E.
;	/POWER
;		Switch.  If set, the power spectrum in Watt/keV is output.  The default
;		output is in photons/keV.
;	BAND
;		Numeric value.  If given and /POWER is not set, the output is in
;		photons/kev per bandwidth, with the bandwidth value given by BAND.
;	VSIG
;		Optional output, see below.
; OUTPUTS:
;		By default returns the photon spectrum, for the energy values provided,
;		in units of photons/kev or, if BAND is given, in photons/kev/BAND.
;		If the keyword /POWER is set, the power spectrum in Watts/kev is
;		returned.
;
;		The output is of same length and form as the input E.
; OPTIONAL OUTPUT PARAMETERS:
;	VSIG
;		Returns the equivalent SIGMA value corresponding to the vertical
;		distribution.  Note that if PSI_RANGE is given, only the truncated
;		distribution, within PSI_RANGE, is considered for the SIGMA value.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Integrates the bending magnet distribution function over angles, for
;		all energy values provided, using H_INT, and scales the result
;		appropriately.  Calls CALCTYPE, CAST, DEFAULT, ISNUM, ONE_OF and TOLER,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JAN-2005 by Mati Meron.
;		Modified 1-AUG-2005 by Mati Meron.  Added keyword /DEF.
;		Modified 20-JAN-2007 by Mati Meron.  Internal changes.
;		Renamed 30-JAN-2007 by Mati Meron.  Name changed from the original
;		BM_SPEC, for consistency with new naming scheme.
;-

	on_error, 1

	amult = 1d-3
	emult = 1d6
	eps = Toler()
	typ = Calctype(e,0.)

	defl = keyword_set(def)
	misfl = 0

	case One_of(rgm,ern) of
		-1	:	begin
					if defl then misfl = 1 else message, $
					'Either gamma or ring energy is needed!'
				end
		0	:	wrgm = Cast(rgm,typ)
		1	:	wrgm = emult*ern/(!srcon.ee*!srcon.scal)
	endcase

	case One_of(ecr,bfd) of
		-1	:	begin
					if defl then misfl = 1 else message, $
					'Either critical energy or magnetic field is needed!'
				end
		0	:	wecr = Cast(ecr,typ)
		1	:	wecr=3*wrgm^2*bfd*!srcon.ee*!srcon.scal/(2*!srcon.alp*!srcon.be)
	endcase

	if n_elements(cur) eq 0 then begin
		if defl then misfl = 1 else pcur = 1.
	endif else pcur = cur

	if misfl then begin
		BL_defaults, gamma = wrgm, bm_field = bfd, cur = pcur
		wecr=3*wrgm^2*bfd*!srcon.ee*!srcon.scal/(2*!srcon.alp*!srcon.be)
	endif
	pcur = pcur/amult
	esc = e/wecr
	nesc = n_elements(esc)

	vsifl = arg_present(vsi)
	wtet = amult*Default(tet,1)
	if Isnum(psi) then begin
		ilim = wrgm*amult*psi/2
		hint = H_int(esc,ilim,prec=eps)
		if vsifl then sint = H_int(esc,ilim,pow=2,prec=eps)
	endif else begin
		hint = H_int(esc,prec=eps)
		if vsifl then sint = H_int(esc,pow=2,prec=eps)
	endelse

	res = 3/!dpi^2*pcur*wtet*!srcon.alp*wrgm*hint
	if not keyword_set(pow) then begin
		res = !srcon.scal*res
		if Isnum(ban) then res = ban*res else res = res/e
	endif
	if vsifl then vsi = hint^2/(wrgm*amult*sqrt(4*!dpi)*sint)

	return, Cast(res,typ,typ,/fix)
end