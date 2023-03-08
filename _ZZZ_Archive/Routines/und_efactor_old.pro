Function UND_efactor_old, ene, ehr, emx, harmonic = har, nper = npr, period = per, $
	und_length = udl, scu = scu, def = def, scale = scl

;+
; NAME:
;		UND_EFACTOR
; VERSION:
;		8.442
; PURPOSE:
;		Evaluates the energy profile of a synchrotron undulator beam, for fixed
;		harmonic energy.
; CATEGORY:
;		Synchrotron calculations.
; CALLING SEQUENCE:
;		Result = UND_EFACTOR( ENE, keywords)
; INPUTS:
;	ENE														|	ENE and EHR
;		Beam energy in KeV, scalar or vector.				|	cannot both
;	EHR														|	be vectors.
;		On axis harmonic energy in keV.  Scalar or vector.	|
; OPTIONAL INPUT PARAMETERS:
;	EMX
;		Scalar, maximal energy for the given harmonic.  Optional, if not
;		provided will be evaluated internally.
; KEYWORD PARAMETERS:
;	HARMONIC
;		Integer type scalar, the harmonic number.  Mandatory.
;	NPER
;		Integer type scalar, the number of undulator periods.  Mandatory.
;	PERIOD
;		The length of the undulator period.  If the value is less then 1, it is
;		assumed to be in meters, else in milimeters.  If not given, it is
;		evaluated from device length (see UND_LENGTH, below) and NPER.
;	UND_LENGTH
;		Undulator length, in m.  If not given, and DEF is set, default value is
;		read from !BLPAR.
;	/SCU
;		Switch.  If set, the evaluation is for a superconducting undulator.
;	/DEF
;		Switch.  If set, values of some missing parameters are read from
;		!BLPAR.
;	/SCALE
;		Switch.  If set, the result is scaled by the ratio of brightnesses
;		at ENE and EHR.
;
;		Note:	If ENE is a scalar, SCALE is set by default (may be disabled
;				by setting SCALE = 0).  If ENE is a vector, SCALE is unset by
;				default.
; OUTPUTS:
;		Returns the flux at the given energy(s) relative to the flux at the
;		harmonic energy.  The result is approximately valid for flux integrated
;		over a sufficiently large aperture.  The format of the result is same as
;		the format of the ENE or EHR input (depending which of them is a vector)
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		ENE must be nonnegative and EHARM must be within the range allowed for
;		the given harmonic.
; PROCEDURE:
;		Calculates based on approximation derived from the basic undulator
;		formalism.  Calls BL_DEFAULTS, ID_CONV and JSD_FUN.  Calls CALCTYPE,
;		CAST, DEFAULT, ISNUM, NULLIFY, SININT_MM and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JAN-2013 by Mati Meron.
;		Modified 20-JUL-2013 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2015 by Mati Meron.  Added keyword SCU for
;		superconducting undulator option.
;		Modified 10-NOV-2015 by Mati Meron.  Significant rewrite.  Added input
;		EMX and keywords PERIOD and SCALE.  Removed keywords RING_ENERGY and
;		RGAM which served no useful purpose.
;-

	on_error, 1

	typ = Calctype(0.,ene,ehr,def=4)
	if typ eq 4 then wpi = !pi else wpi = !dpi
	if (min(ene) < min(ehr)) lt 0 then message, 'Energy must be non-negative!'
	udfl = Isnum(udl)
	if keyword_set(def) then BL_Defaults, dev=udl, scu=scu

	wene = Cast(ene,typ)
	wehr = Cast(ehr,typ)
	scfl = Default(scl,1) ne 0
	mwhi = 1
	chdim = ((size(ene))[0] eq 0) + 2*((size(ehr))[0] eq 0)
	if chdim eq 2 then begin
		scfl = keyword_set(scl)
		mwhi = 0
	endif else if chdim eq 0 then message,'At least one input must be a scalar!'

	if n_elements(har) eq 1 then whar = long(har) $
	else message, 'Single harmonic value needed!'
	if n_elements(npr) eq 1 then wnpr = long(npr) $
	else message, 'Single # periods value needed!'
	case n_elements(per) of
		0	:	wper = floor(2d3*udl/wnpr)/2d3
		1	:	if per gt 1 then wper = 1d-3*per else wper = 1d*per
		else:	message, 'Single or no value for period needed!'
	endcase
	if n_elements(emx) eq 0 then ID_conv,per=1d3*wper,k=0.1d,emax=emx,scu=scu $
	else emx = Cast(emx,5)
	elim = whar*emx

	if (max(wehr) > max(wene)) le elim then begin
		ukh = sqrt(2*(elim/(wehr > Toler()) - 1))
		wh = ukh/(1 + ukh^2/2)
		uh = ukh*wh/4
		uke = sqrt(2*(elim/(wene > Toler()) - 1))
		we = uke/(1 + uke^2/2)
		ue = uke*we/4
	endif else message, $
	string(elim,form='("	max energy for this harmonic is ",f6.2," keV")')

	eps = 2*(wene/wehr - 1)
	res = 0.*eps + 1
	dum = where(eps ne 0,ndum)
	if ndum gt 0 then begin
		farg = wpi*wnpr*whar*eps[dum]
		res[dum] = 1 - 2/wpi*(Sinint_mm(farg) - (1 - cos(farg))/farg)
	endif
	dum = where (eps lt 0, ndum)
	if ndum gt 0 then begin
		if mwhi then begin
			dume = 0
			dumh = dum
		endif else begin
			dume = dum
			dumh = 0
		endelse
		sarg = whar^2*(2*uh[dumh] + 1)*eps[dum]
		res[dum] = res[dum]*exp(eps[dum]+sarg)*beseli(sarg,0)
		if scfl then res[dum] = res[dum]*$
		(wh[dumh]*Jsd_fun(uh[dumh],whar,/neg)/$
		(we[dume]*Jsd_fun(ue[dume],whar,/neg)))^2
	endif
	Nullify, udfl, fir=udl

	return, Cast(res,typ,typ,/fix)
end