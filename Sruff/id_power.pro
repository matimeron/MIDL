Function ID_power, gap= gap, period= per, k= kvl, energy= ene, scu= scu, $
	und_length= udl, ring_energy= ren, rgam= rgm, current= cur, $
	aperture= ape, distance= dst, total= tot, first= fir, fflux= flx, $
	peak_density = ppd, _extra= _e

;+
; NAME:
;		ID_POWER
; VERSION:
;		8.45
; PURPOSE:
;		Quick approximate calculation of an insertion device power.
; CATEGORY:
;		SR specific.
; CALLING SEQUENCE:
;		Result = ID_POWER, keywords
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GAP											|
;		ID gap, in mm.							|
;	PERIOD										|
;		ID period, in mm.						|	2 and only 2 of these 4
;	K											|	parameters must be provided.
;		The K value of the ID.					|
;	ENERGY										|
;		First harmonic energy, in keV.			|
;	/SCU
;		Switch.  If set, superconducting insertion device field is used.
;	UND_LENGTH
;		Insertion device length, in meters.  Optional, if not provided, default
;		value from !BLPAR is used.
;	RING_ENERGY										|	At most one of these two
;		Synchrotron energy, in GeV.   				|	may be given.  If none 
;	RGAM											|	is, values are taken
;		The relativistic Gamma of the source.		|	from !BLPAR.
;	CURRENT
;		Ring current in Amp.  Optional, if not provided, default value from
;		!BLPAR is used.
;	APERTURE
;		Beam aperture at location DISTANCE (see below), given as a 2-element
;		vector, in milimeters.  If not provided, default values from !BLPAR
;		are used.
;	DISTANCE
;		Distance of beam aperture from source, in meters.  If not provided,
;		default value from !BLPAR is used.
;	/TOTAL
;		Switch.  If set, total power (no aperture) is calculated.
;	/FIRST
;		Switch.  If set, only the power in the first harmonic (through the
;		aperture, or total if /TOTAL is set) is returned.
;	/FFLUX
;		Switch.  If set, the first harmonic photon flux through the apperture
;		or, optionally (when /TOTAL is set), with no aperture, is returned
;		instead of power.  Active only in conjunction with /FIRST.
;	PEAK_DENSITY
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to embedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the power (all harmonics) delivered or the first harmonic
;		flux, through an aperture or, optionally, with no aperture.
; OPTIONAL OUTPUT PARAMETERS:
;	PEAK_DENSITY
;		Returns the peak power density, in units of Watt/mrad^2.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		As mentioned above, when one of the inputs is a scalar and the other an
;		arrray, the scalar is reformated as an array.  Also, on return all
;		parameters are of type no lower than 4 (float).
; RESTRICTIONS:
;		1)	The inputs GAP, PERIOD, K and ENERGY may be scalars or (same length)
;			vectors.  All other inputs must be scalars (except for APERTURE
;			which take s a 2-element vector).
;		2)	Inputs parameters (from among GAP, PERIOD, K, ENERGY) which will,
;			directly or indirectly, result in outputs out of range for the
;			given device, will yield a NaN and the routine will return an error
;			message.
; PROCEDURE:
;		Uses standard undulator power and flux results plus a gaussian
;		approximation to the KJK function.  Not as accurate as a full KJK
;		evaluation (see function KJK_FUN) but much faster.  Uses BL_DEFAULTS,
;		ID_CONV and KJK_SIGS from SRUFF_LIB.  Calls CALCTYPE, CAST, DEFAULT,
;		ERRORF_MM and POLEVAL, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-NOV-2005 by Mati Meron.
;		Modified 25-MAY-2006 by Mati Meron.  Added keyword /FIRST.
;		Modified 25-OCT-2015 by Mati Meron.  Added superconducting undulator
;		option, using the keyword SCU.  Also added vector input(s) capability.
;		Modified 25-JAN-2016 by Mati Meron.  Added keywords UND_LENGTH and
;		PEAK_DENSITY.  Replaced keyword NPER with UND_LENGTH.
;-

	on_error, 1
	mult = 1d3

	ID_conv, gap= gap, period= per, k= kvl, ene= ene, emax= emx, scu= scu, $
	num= num, _extra = _e
	BL_defaults, synch= syn, ene= ren, gam= rgm, cur= cur, ape= ape, dist= dst,$
	dev = udl, min = mgap, scu = scu
	dum = where(mgap gt gap, ndm)
	if ndm gt 0 then message, /con, $
	'Gap size(s) below the minimal possible ' + string(mgap,form='(f0.2,"mm!")')
	typ = Calctype(gap)

	npr = Default(npr,floor(mult*udl/per))

	res =  !dpi/3*!srcon.alp*npr*kvl^2*emx*(mult*cur)
	pmult= 21*rgm^2/(16*!dpi)*Poleval(kvl^2,[16,28,24,7]/7d)/(1d + kvl^2)^(7./2)
	ppd = Cast(1d-6*pmult*res,typ,typ)
	if keyword_set(fir) then begin
		res = res/(1 + kvl^2/2)^2
		if keyword_set(flx) then res = !srcon.scal/ene*res
	endif

	if not keyword_set(tot) then begin
		smult = mult*dst/rgm
		sigs = make_array(2,num,typ=typ)
		afac = make_array(num,typ=typ)
		for i = 0, num-1 do begin
			sigs[*,i] = smult*KJK_sigs(kvl[i],/cor)
			afac[i] = product(Errorf_mm(ape/(sqrt(8)*sigs[*,i])))
		endfor
		res = afac*res
	endif

	if num eq 1 then begin
		res = res[0]
		ppd = ppd[0]
	endif

	return, Cast(res,typ,typ,/fix)
end