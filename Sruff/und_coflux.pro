Function UND_coflux, ene, period= per, harm= har, quiet= qui, bandwidth= ban,$
	def= def, scu= scu, nper= npr, und_length= udl, min_gap= mgp, $
	ring_energy=ren, rgam=rgm, current=cur, radial_size=rsg, angular_size=asg,$
	correct=crr, aperture=ape, distance=dst, full=ful, fraction=frc,length=len,$
	_extra = _e

;+
; NAME:
;		UND_COFLUX
; VERSION:
;		8.72
;		Calculates the coherent flux of a synchrotron undulator source.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = UND_COFLUX( ENE, PERIOD = PER [,keywords])
; INPUTS:
;	ENE
;		Numeric scalar or vector, photon energy in keV units.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	PERIOD
;		The length of the undulator period.  If the value is less then 1, it is
;		assumed to be in meters, else in milimeters.  Mandatory, no defaults.
;	HARM
;		Integer type, The undulator harmonic number.  Only odd numbers are
;		accepted.  Default is 1.
;	/QUIET
;		Switch.  If set, the result for energy values out of range for HARM is
;		zero.  The default for such case is Nan.
;	BANDWIDTH
;		The bandwidth used in calculation.  Default is 1e-3.
;	/DEF
;		Switch.  Set by default, can be disabled by explicitly setting DEF = 0.
;		When DEF is set and some inputs are missing, they're replaced by
;		defaults from !BLPAR.
;	/SCU
;		Switch.  If set, the evaluation is for a superconducting undulator.
;	NPER
;		Number of periods.  If not given, calculated	|	At most one of
;		from period and UND_LENGTH.						| 	these two may
;	UND_LENGTH											|	be given.
;		The length of the undulator in meters.  If		|
;		not given calculated from PERIOD and NPER or	|
;		(if NPER is also absent but /DEF is set) 		|
;		replaced by the default value in !BLPAR.
;	MIN_GAP
;		Minimal undulator gap, in mm.  If not given, but /DEF is set, the
;		default value from !BLPAR is used.
;	RING_ENERGY									|	At most one of these two may
;		Synchrotron energy, in GeV.   			|	be given.  If none is and
;	RGAM										|	/DEF is set value is taken
;		The relativistic Gamma of the source.	|	from !BLPAR.
;	CURRENT
;		Source electron current, in Amp.  If not given and /DEF is set, value
;		is taken from !BLPAR.
;	RADIAL_SIZE
;		Source spatial size in the format of [horizontal_sigma,vertical_sigma].
;		If not given and /DEF is set, values are taken from !BLPAR.
;	ANGULAR_SIZE
;		Source angular size in the format of [horizontal_sigma,vertical_sigma].
;		If not given and /DEF is set, values are taken from !BLPAR.
;	/CORRECT
;		Switch.  If set, higher order corrections to radiative spatial and
;		angular width are calculated.  This has a very small effect, except for
;		very high harmonics.
;	APERTURE
;		The aperture for flux calculation, given as a 2-element vector in
;		[x-size,y-size] format, in mm.  If not given and /DEF is set, the
;		standard aperture from !BLPAR is used.
;	DISTANCE
;		The distance of the aperture (see above) from source, in meters.  If not
;		given and /DEF is set, the standard distance from !BLPAR is used.
;	/FULL
;		Switch. If set, unlimited aperture is assumed.  Both APERTURE and
;		DISTANCE are ignored in this case.
;	FRACTION
;		Optional output, see below.
;	LENGTH
;		Optional output, see below.
;	_EXTRA
;		A formal keyword for transfering keywords to embedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Returns the coherent flux of the undulator, for the energy values
;		provided (it is assumed that, for each energy, the undulator is tuned
;		to this energy, in the harmonic given by HARM).  The output is of same
;		length and form as the input E. The units are photons/sec.
;
;		Note:	If some of the energy values provided are outside the allowed
;				range for the given harmonic, the corresponding results are NaN,
;				unless QUIET is set.
; OPTIONAL OUTPUT PARAMETERS:
;	FRACTION
;		The coherent fraction, i.e. the ratio(s) of the fundamental emittance,
;		wavelength/(4*Pi), and the emittance of the beam.
;		For scalar energy input returns a 2-element vector, containing (in
;		order) horizontal and vertical coherent fraction.  For vector energy
;		input returns [2,N] array where N is the number of energy vealues.  Each
;		row of the array contains horizontal and vertical fraction for the
;		corresponding energy.
;	LENGHT
;		Returns horizontal and vertical coherence lengths.  Format is same as
;		for FRACTION.
;		Same as HOR_FRAC, for the vertical direction.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The energy must be no higher than the saturation energy for the
;		undulator, at the given harmonic.
; PROCEDURE:
;		Calculates using a gaussian approximation of the distribution, as well
;		as gaussian representation of the aperture.  Calls BL_DEFAULTS,
;		UND_BEAMSIZE and UND_FLUX.  Calls CALCTYPE, CAST and DEFAULT, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JUN-2018 by Mati Meron.
;		Modified 30-MAY-2019 by Mati Meron.  Internal changes.
;		Modified 20-DEC-2020 by Mati Meron.  Internal changes.
;-

	on_error, 1
	mult = 1d-6

	typ = Calctype(ene,0.)
	nen = n_elements(ene)
	wdef = Default(def,1,/dtyp)
	lam = 1d-10*!srcon.conv/transpose(reform(ene))
	wlam = [lam,lam]
	rsig = UND_beamsize(ene, rsize=rsg, asize=asg, def=wdef, scu=scu, $
	und_length=udl, ring_energy=ren, rgam=rgm, ang_size=asig,_extra=_e)
	emit = mult*rsig*asig

	if wdef then Bl_defaults, ape=ape, dist=dst
	drsig = UND_beamsize(ene, rsize=rsg, asize=asg, def=wdef, scu=scu, $
	und_length=udl, ring_energy=ren, rgam=rgm, dist=dst,_extra=_e)
	if not keyword_set(ful) then begin
		if n_elements(ape) ne 2 then message, 'Aperture needs 2 elements!'
		wape=[transpose(replicate(ape[0],nen)),transpose(replicate(ape[1],nen))]
		qfc = wape/sqrt(wape^2 + 2*!dpi*drsig^2)
	endif else qfc = replicate(1,2,nen)

	wemit = sqrt(qfc^2*emit^2 + (1-qfc^2)*(wlam/(4*!dpi))^2)
	frc = wlam/(4*!dpi*wemit)
	res = product(frc*qfc,1)* $
	UND_flux(ene, period= per, harm= har, quiet= qui, bandwidth= ban,def= wdef,$
	scu= scu, nper= npr, und_len= udl, min_gap= mgp, ring_ene= ren, rgam= rgm, $
	current=cur, radial_size=rsg, angular_size=asg, correct=crr,/full,_extra=_e)
	frc = Cast(frc,typ,typ)
	len = Cast(frc*qfc*drsig,typ,typ)

	return, Cast(reform(res),typ,typ,/fix)
end