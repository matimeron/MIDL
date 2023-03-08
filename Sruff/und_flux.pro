Function UND_flux, ene, period = per, harm = har, quiet= qui, bandwidth= ban, $
	def= def, scu= scu, nper= npr, und_length= udl, min_gap= mgp, $
	ring_energy=ren, rgam=rgm, current=cur, radial_size=rsg, angular_size=asg,$
	correct=crr, optimize=opt, aperture=ape, distance=dst, full=ful, pack=pac, $
	set_energy = sen, _extra = _e

;+
; NAME:
;		UND_FLUX
; VERSION:
;		8.72
;		Calculates the flux of a synchrotron undulator source.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = UND_FLUX( ENE, PERIOD = PER [,keywords])
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
;	/OPTIMIZE
;		Switch.  If set, the flux is calculated assuming the undulator is tuned
;		not to ENE but to the energy yielding highest flux at ENE (which energy
;		will be slightly higher then ENE).
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
;	/PACK
;		Switch.  If set, the results are packed together with the energy input
;		into a [2,N] array which can be displayed using SCAN_SHOW.  Note that:
;			1)	PACK is disabled for scalar energy input.
;			2)	When PACK is set all the NaN results (if present) are eliminated
;				from the output, together with their corresponding energy values
;	SET_ENERGY
;		Optional output, see below.
;	_EXTRA
;		A formal keyword for transfering keywords to embedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Returns the flux of the undulator, for the energy values provided (it is
;		assumed that, for each energy, the undulator is tuned to this energy, in
;		the harmonic given by HARM.  However, this assumption is changed when
;		OPTIMIZED is set), within the given aperture and bandwidth. The output
;		is of same length and form as the input E (however, see PACK above.  The
;		units are photons/sec.
;
;		Note:	If some of the energy values provided are outside the allowed
;				range for the given harmonic, the corresponding results are NaN,
;				unless QUIET is set.
; OPTIONAL OUTPUT PARAMETERS:
;	SET_ENERGY
;		If /OPTIMIZED is set, returns the energy (or energies) to which the
;		undulator needs to be set to yield the optimized flux at ENE.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The energy must be no higher than the saturation energy for the
;		undulator, at the given harmonic.
; PROCEDURE:
;		Calculates using a gaussian approximation of the distribution,
;		correcting for finite source size (spatial and angular).  Calls
;		BL_DEFAULTS, ID_CONV, UND_BRIGHTNESS, UND_BEAMSIZE and UND_ECORR.
;		Calls CALCTYPE, CAST, DEFAULT, ERRORF_MM, ISNUM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JAN-2013 by Mati Meron, by splitting away part of the old
;		version of UND_BRILLIANCE.
;		Modified 20_JUL-2013 by Mati Meron.  Added keywords OPTIMIZED and
;		SET_ENERGY.
;		Modified 30-SEP-2015 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2015 by Mati Meron.  Added keyword SCU for
;		superconducting undulator option.
;		Modified 10-NOV-2015 by Mati Meron.  Internal changes.  Added keyword
;		PACK.
;		Modified 10-JAN-2016 by Mati Meron.  Internal changes.  Added keyword
;		QUIET.
;		Modified 20-JAN-2018 by Mati Meron.  Internal changes.  Added keyword
;		MIN_GAP.
;		Modified 30-MAY-2018 by Mati Meron.  Added keyword FULL.
;		Modified 5-JUN-2018 by Mati Meron.  Internal changes.
;		Modified 15-NOV-2019 by Mati Meron.  Bug fix.
;		Modified 20-DEC-2020 by Mati Meron.  Internal changes.
;-

	on_error, 1
	eps = Toler()
	mult = 1d3

	dum = How_many(fir=npr,sec=udl,thi=ren,fou=rgm,whi=whi)
	typ = Calctype(ene,0.)
	wdef = Default(def,1,/dtyp)
	if wdef then BL_defaults, dev= udl, min_gap= mgp, ape= ape, dist=dst,scu=scu
	if per lt 1 then wper = mult*per else wper = 1d*per
	whar = Default(har,1,/dtyp)
	if not Isnum(npr) then begin
		if wdef then wnpr = floor(mult*udl/wper) $
		else message, 'Missing NPER and DEF not set!'
	endif else wnpr = npr

	quifl = keyword_set(qui)
	nen = n_elements(ene)
	wene = Cast(ene,typ)
	res = make_array(nen,typ=typ)
	pacfl = keyword_set(pac)
	ID_conv, per= wper, gap= mgp, ene= emin, emax= emax, scu= scu, _extra= _e
	if ene[0] lt whar*emin then wene[0] = wene[0]*(1+ 4*eps)
	if wene[-1] gt whar*emax then wene[-1] = ene[-1]*(1- 4*eps) 
	good = $
	where(wene ge whar*emin and wene le whar*emax,ngood,comp=bad,ncomp=nbad)
	if nbad gt 0 then if quifl then res[bad] = 0 else res[bad] = !values.f_nan

	if ngood gt 0 then begin
		wene = wene[good]
		wres = res[good]
		if keyword_set(opt) then begin
			nwen = n_elements(wene)
			uene = (mfac = dblarr(nwen))
			for i = 0, nwen-1 do begin
				uene[i] = Und_ecorr(wene[i],emax,har=whar,nper=wnpr,per=wper,$
				scu=scu,def=wdef,fac=fac,_extra=_e)
				mfac[i] = fac
			endfor
			sen = uene
		endif else begin
			uene = wene
			sen = !null
			mfac = 1.
		endelse

		bri= UND_brightness(uene,per=wper,har=whar,ban=ban,def=wdef,$
		und=udl,scu=scu,ring_ene=ren,rgam=rgm,curr=cur,rad=rsg,ang=asg,$
		corr=crr,_extra=_e)
		wrsg = UND_beamsize(uene,rsize=rsg,asize=asg,und=udl,def=wdef,corr=crr,$
		har=whar,per=wper,scu=scu,rgam=rgm,ang=wasg,dist=dst,_extra=_e)*sqrt(8d)
		wres = mfac*bri*2*!dpi*product(wasg,1)
		if not keyword_set(ful) $
		then wres = wres*Errorf_mm(ape[0]/wrsg[0,*])*Errorf_mm(ape[1]/wrsg[1,*])
		wres = Cast(wres,4,typ,/fix)
		Nullify, whi, fir=npr,sec=udl,thi=ren,fou=rgm

		if pacfl then begin
			if ngood eq 1 then begin
				res[good] = wres
				message, 'Not enough points, cannot pack.', /con
			endif else res = transpose([[wene],[wres]])
		endif else res[good] = wres
	endif else if not quifl then $
	message, 'No energy values within admissible range', /con

	if (size(ene))[0] eq 0 and not pacfl then res = res[0]

	return, res
end