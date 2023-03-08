Function UND_Beamsize_old, ene, rsize = rsg, asize = asg, $
	def = def, scu = scu, und_length = udl, distance = dst, $
	correct=crr, harmonic=har, period=per, nper=npr, ring_energy=ren, rgam=rgm,$
	horizontal = hor, vertical = ver, fwhm = fwhm, $
	ang_size = asz, ursig = ursg, uasig = uasg

;+
; NAME:
;		UND_BEAMSIZE
; VERSION:
;		8.45
; PURPOSE:
;		Evaluates a synchrotron undulator beam size.
; CATEGORY:
;		Synchrotron calculations.
; CALLING SEQUENCE:
;		Result = UND_BEAMSIZE( ENE, keywords)
; INPUTS:
;	ENE
;		Beam energy in KeV, scalar or vector.  Default is 10keV.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RSIZE
;		Beam spatial sigma, in mm.  Can be given as a 2-element vector, in
;		[horizontal, vertical] order or as a scalar.  If not given, default
;		values can be read from the global parameter !BLPAR.
;	ASIZE
;		Same as RSIZE, for angular sigma, value(s) in mr.
;	/DEF
;		Switch.  If set, values of some missing parameters are read from !BLPAR.
;	/SCU
;		Switch.  If set, the evaluation is for a superconducting undulator.
;		Only relevant in conjunction with DEF or CORRECT.
;	UND_LENGTH
;		Undulator length, in m.  Mandatory, unless DEF is set.
;	DISTANCE
;		Distance from source, in m.  If not given, it is taken as 0.
;	/CORRECT
;		Switch.  If set, higher order corrections to radiative spatial and
;		angular width are calculated.  This requires providing the values of
;		HARMONIC, PERIOD or NPER and RING_ENERGY or RGAM (see below).
;	HARMONIC
;		Integer type scalar, the harmonic number.  Mandatory when CORRECT is set
;		not needed otherwise.  No default.
;	PERIOD														| One and only
;		The length of the undulator period.  If value <1,		| one of these
;		assumed in meters, else in mm.  No default.				| two needs to
;	NPER														| be provided
;		Integer type scalar, the number of undulator periods.	| when CORRECT
;		No default.												| is set
;	RING_ENERGY									|	At most one of these two may
;		Synchrotron energy, in GeV.   			|	be given.  If none is and
;	RGAM										|	DEF is set, RGAM value
;		The relativistic Gamma of the source.	|	is taken from !BLPAR.
;
;		Note:	If any of HARMONIC, PERIOD, NPER, RING_ENERGY or RGAM will be
;		provided while CORRECT is not set, no error will occur but the provided
;		values will be ignored, as they won't be needed.
;	/HORIZONTAL											|
;		Switch.  If set, only the horizontal dimension 	|	These two keywords
;		values are returned.							|	are mutually
;	/VERTICAL											|	exclusive.
;		Switch.  If set, only the horizontal dimension 	|
;		values are returned.							|
;	/FWHM
;		Switch.  If set, FWHM values are returned.  Default is to return sigma
;		values.
;	ANG_SIZE
;		Optional output, see below.
;	URSIG
;		Optional output, see below.
;	UASIG
;		Optional output, see below.
; OUTPUTS:
;		Returns the beam size (sigma or fwhm) in mm corresponding to the
;		provided parameters, as a [2,N] array, where each row contains the two
;		sizes in [horizontal,vertical] order and N is the number of ENE values
;		(if ENE is a scalar, the result is plain 2-element vector). If one of
;		the keywords HORIZONTAL or VERTICAL is set, only the corresponding
;		values are returned as an N-element vector (or scalar for scalar ENE
;		input).
; OPTIONAL OUTPUT PARAMETERS:
;	ANG_SIZE
;		Returns the angular beam size, in mr, same format as the standard
;		output.
;	URSIG
;		Returns the spatial radiative beam size, im mm, always as sigma values.
;		Same format as the standard output.
;	UASIG
;		Same as URSIG, for angular radiative beam size in mr.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward calculation, including radiative broadening.
;		Calls BL_DEFAULTS and ID_CONV.  Calls CAST, DEFAULT, HOW_MANY, ISNUM,
;		NULLIFY, ONE_OF, TOLER and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-JAN-2013 by Mati Meron as an upgrade of the routine BEAM_SIZE.
;		Modified 20-JUL-2013 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2015 by Mati Meron.  Added keyword SCU for
;		superconducting undulator option.
;		Modified 10-JAN-2016 by Mati Meron.  Added keyword PERIOD.  Some
;		internal changes.
;-

	on_error, 1
	eps = Toler()
	mult = 1d3

	ene = Default(ene,10.)
	if min(ene) lt 0 then message, 'Energy must be non-negative!'
	etyp = Type(ene)
	nen = n_elements(ene)
	wene = ene > eps

	wha = How_many(fir=udl,sec=per,thi=npr,whi=whi)
	if keyword_set(def) then BL_Defaults, dev=udl, rad=rsg, ang=asg, scu=scu $
	else if not Isnum(udl) then message, 'Missing UND_LENGTH!'

	case n_elements(rsg) of
		1	:	wrsg = [rsg,rsg]
		2	:	wrsg = rsg
		else:	message, 'RSIG has to have either 1 or 2 elements!'
	endcase
	case n_elements(asg) of
		1	:	wasg = [asg,asg]
		2	:	wasg = asg
		else:	message, 'ASIG has to have either 1 or 2 elements!'
	endcase
	wrsg = transpose([[replicate(wrsg[0],nen)],[replicate(wrsg[1],nen)]])
	wasg = transpose([[replicate(wasg[0],nen)],[replicate(wasg[1],nen)]])

	if keyword_set(crr) then begin
		if n_elements(har) eq 1 then whar = long(har) $
		else message, 'Single HARMONIC value needed!'
		if whi ge 2 and whi le 5 then begin
			if whi/2 then begin
				if per lt 1 then wper = mult*per else wper = 1d*per
				wnpr = floor(mult*udl/wper)
			endif else begin
				if n_elements(npr) eq 1 then begin
					wnpr = npr
					wper = floor(2*mult*udl/npr)/2.
				endif else message, 'Single PERIOD or NPER value needed!'
			endelse
		endif else message, 'PERIOD or NPER value needed, not both!'
		BL_Defaults, min_gap=mgp, ene=ren, gamm=rgm, scu=scu
		ID_conv, gap=mgp, period=wper, emax=emx, rgam=rgm, scu=scu
		emx = whar*emx
		if max(ene) le emx*(1+ 4*eps) then begin
			uk = sqrt(2*((emx/wene- 1)>0))
			u = uk^2/(4 + 2*uk^2)
		endif else message, $
		string(emx,form='("	Max energy for this harmonic is ",f8.4," keV")')
		cfac = transpose([[2*whar^2*(1. + 2*u)],[dblarr(nen)]])
		cfac = (1. + cfac)/(whar*wnpr)
	endif else cfac = dblarr(2,nen)
	cfac = 1 + cfac

	lam = 1d-10*!srcon.conv/wene
	lam = transpose([[lam],[lam]])
	ursg = mult*sqrt(2*lam*udl*cfac)/(4*!pi)
	uasg = mult*sqrt(lam/(2*udl*cfac))
	rsz = sqrt(wrsg^2 + ursg^2)
	asz = sqrt(wasg^2 + uasg^2)
	if Isnum(dst) then rsz = sqrt(rsz^2 + dst^2*asz^2)

	whi = One_of(hor,ver)
	if whi ge 0 then begin
		rsz = rsz[whi,*]
		asz = asz[whi,*]
		if nen eq 1 then begin
			rsz = rsz[0]
			asz = asz[0]
		endif
	endif

	if keyword_set(fwhm) then begin
		mult = sqrt(alog(256))
		rsz = mult*rsz
		asz = mult*asz
	endif

	Nullify, whi, fir=udl
	asz = Cast(asz,4,etyp)
	ursg = Cast(ursg,4,etyp)
	uasg = Cast(uasg,4,etyp)
	return, Cast(rsz,4,etyp,/fix)
end