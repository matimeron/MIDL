Function UND_brightness_old, ene, period = per, harm = har, bandwidth = ban, $
	def= def, nper= npr, und_length= udl, scu= scu, ring_energy= ren, rgam=rgm,$
	current = cur, radial_size = rsg, angular_size = asg, correct = crr

;+
; NAME:
;		UND_BRIGHTNESS
; VERSION:
;		8.442
; PURPOSE:
;		Calculates the on-axis brightness of a synchrotron undulator source.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = UND_BRIGHTNESS( ENE, PERIOD = PER [,keywords])
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
;		Returns the on-axis brightness of the undulator, for the energy values
;		provided (it is assumed that, for each energy, the undulator is tuned
;		to this energy, in the harmonic given by HARM).  The output is of same
;		length and form as the input E.  The units are photons/sec/mr^2/bw,
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
;		BL_DEFAULTS, ID_CONV, JSD_FUN and UND_BEAMSIZE.  Calls CAST, DEFAULT,
;		FLTROUND, HOW_MANY, NULLIFY, ONE_OF and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JAN-2013 by Mati Meron based on the old version of 
;		UND_BRILLIANCE.
;		Modified 20-JUL-2013 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2015 by Mati Meron.  Added keyword SCU for
;		superconducting undulator option.
;		Modified 10-NOV-2015 by Mati Meron.  Internal changes.
;-

	on_error, 1
	mult = 1d3

	har = Default(har,1,/dtyp)
	ban = Default(ban,1d-3,/dtyp)
	if not (har mod 2) then message, 'Odd harmonics only accepted!'
	dum = How_many(fir=npr,sec=udl,whi=whi)
	if n_elements(per) eq 1 then begin
		if per gt 1 then wper = per/mult else wper = 1d*per
		case One_of(npr,udl) of
			-1	:
			0	:	udl = Fltround(wper*npr,dig=2)
			1	:	udl = 1.*udl
		endcase
		if keyword_set(def) then BL_Defaults, $
		dev= udl, ene= ren, gamm= rgm, curr= cur, scu= scu
		npr = Default(npr,fix(udl/wper),/dtyp)

		ID_conv, per = mult*wper, k = 0.1d, emax = emx
		emx = har*emx
		if max(ene) gt emx then message,'Energy out of range for this harmonic!'
		w = 0.*ene
		u = 0.5 + w
		dum = where(ene gt 0, ndum)
		if ndum gt 0 then begin
			uk = sqrt(2*(emx/ene[dum] - 1))
			w[dum] = uk/(1 + uk^2/2)
			u[dum] = uk*w/4
		endif

		wrsg = UND_Beamsize(ene,rsize=rsg,asize=asg,und=udl,def=def,corr=crr, $
			har=har,nper=npr,scu=scu,rgam=rgm,ang=wasg,uasig=uasg)

		res = !srcon.alp*!srcon.scal*cur*ban/mult* $
		(npr*rgm*har*w*Jsd_fun(u,har,/neg))^2*product(uasg,1)/product(wasg,1)
		Nullify, whi, fir=npr,sec=udl
	endif else message, 'Missing Period length!'

	return, Cast(res,4,Type(ene),/fix)
end