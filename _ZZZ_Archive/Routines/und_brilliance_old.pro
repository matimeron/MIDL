Function UND_brilliance_old, ene, period = per, harm = har, bandwidth = ban, $
	def= def, nper= npr, und_length= udl, scu= scu, ring_energy= ren, rgam=rgm,$
	current = cur, radial_size = rsg, angular_size = asg, correct = crr

;+
; NAME:
;		UND_BRILLIANCE
; VERSION:
;		8.44
; PURPOSE:
;		Calculates the on-axis brilliance of a synchrotron undulator source.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = UND_BRILLIANCE( ENE, PERIOD = PER [,keywords])
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
;	BANDWIDTH
;		The bandwidth used in calculation.  Default is 1e-3.
;	/DEF
;		Switch.  If set and some inputs are missing, they're replaced by
;		defaults from !BLPAR.
;	NPER
;		Number of periods.  If not given, calculated	|	At most one of
;		from period and UND_LENGTH.						| 	these two may
;	UND_LENGTH											|	be given.
;		The length of the undulator in meters.  If		|
;		not given calculated from PERIOD and NPER or	|
;		(if NPER is also absent but /DEF is set) 		|
;		replaced by the default value in !BLPAR.
;	/SCU
;		Switch.  If set, the evaluation is for a superconducting undulator.
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
; OUTPUTS:
;		Returns the on-axis brilliance of the undulator, for the energy values
;		provided (it is assumed that, for each energy, the undulator is tuned
;		to this energy, in the harmonic given by HARM).  The output is of same
;		length and form as the input E.  The units are photons/sec/mm^2/mr^2/bw,
;		where bw is the bandwidth being used.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The energy must be no higher than the saturation energy for the
;		undulator, at the given harmonic.
; PROCEDURE:
;		Calculates from the mathematical form of the on-axis distribution,
;		correcting for finite source size (spatial and angular).  Calls
;		UND_BRIGHTNESS and UND_BEAMSIZE.  Calls CAST and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 30-DEC-2006 by Mati Meron as UN_BRILLIANCE.
;		Modified 1-APR-2007 by Mati Meron.  Added keyword /DEF and renamed
;		routine to UND_BRILLIANCE, for consistency with new naming scheme.
;		Modified 20-AUG-2012 by Mati Meron.  Added keywords FLUX, APERTURE and
;		DISTANCE.
;		Completely rewritten 5-JAN-2013 by Mati Meron.  All the number crunching
;		has been delegated to the new routines UND_BEAMSIZE and UND_BRIGHTNESS.
;		Flux calculation has been split away to a separate routine, UND_FLUX.
;		Modified 25-OCT-2015 by Mati Meron.  Added keyword SCU for
;		superconducting undulator option.
;-

	on_error, 1

	bri = UND_brightness(ene,per=per,harm=har,ban=ban,def=def,nper=npr,und=udl,$
		scu=scu,ring_ene=ren,rgam=rgm,curr=cur,radial=rsg,angular=asg,corr=crr)
	wrsg = UND_beamsize(ene,rsize=rsg,asize=asg,und=udl,def=def,corr=crr, $
		har=har,nper=npr,scu=scu,rgam=rgm)
	res = bri/(2*!dpi*product(wrsg,1))

	return, Cast(res,4,Type(ene),/fix)
end