Function ID_power_old, gap=gap, period=per, k=kvl, energy=ene, nper=npr, scu= scu,$
	synch= syn, ring_energy= ren, current= cur, aperture= ape, distance= dst, $
	total = tot, first = fir, fflux = flx, _extra = _e

;+
; NAME:
;		ID_POWER
; VERSION:
;		8.44
; PURPOSE:
;		Quick approximate calculation of an insertion device power.
; CATEGORY:
;		SR specific.
; CALLING SEQUENCE:
;		Result = ID_CONV, keywords
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
;		First harmonic energy, in keV.
;	NPER
;		Number of periods in the insertion device.  If not provided, the number
;		is estimated internally from the period's length and the standard APS
;		ID length (2.4 m).
;	/SCU
;		Switch.  If set, supeconducting insertion device field is used.		|
;	SYNCH
;		Allows to enter character value, a synchrotron name.  Currently only
;		'APS' is accepted.
;	RING_ENERGY
;		Storage ring energy, in GeV.  Optional, if not provided, the APS value
;		(7 GeV) is used.
;	CURRENT
;		Ring current in Amp.  Optional, if not provided, the APS standard value
;		(0.1 A) is used.
;	APERTURE
;		Beam aperture at location DISTANCE (see below), given as a 2-element
;		vector, in milimeters.  If not provided, standard APS aperture of
;		[3.,2.] mm (rectangular equivalent) is used.
;	DISTANCE
;		Distance of beam aperture from source, in meters.  If not provided, the
;		standard APS distance of 25 m is used.
;	/TOTAL
;		Switch.  If set, total power (no aperture) is calculated.
;	/FIRST
;		Switch.  If set, only the power in the first harmonic (through the
;		aperture, or total if /TOTAL is set) is returned.
;	/FFLUX
;		Switch.  If set, the first harmonic photon flux through the apperture
;		or, optionally (when /TOTAL is set), with no aperture, is returned
;		instead of power.  Active only in conjunction with /FIRST.
;	_EXTRA
;		A formal keyword used to pass keywords to embedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the power (all harmonics) delivered or the first harmonic
;		flux, through an aperture or, optionally, with no aperture.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		As mentioned above, when one of the inputs is a scalar and the other an
;		arrray, the scalar is reformated as an array.  Also, on return all
;		parameters are of type no lower than 4 (float).
; RESTRICTIONS:
;		1)	Only scalar inputs accepted.
;		2)	Inputs parameters (from among GAP, PERIOD, K, ENERGY) which will,
;			directly or indirectly result in radiation energy out of range for
;			the given device, will yield a NaN and the routine will return an
;			error message.
; PROCEDURE:
;		Uses standard undulator power and flux results plus a gaussian
;		approximation to the KJK function.  Not as accurate as a full KJK
;		evaluation (see function KJK_FUN) but much faster.  Uses BL_DEFAULTS,
;		ID_CONV and KJK_SIGS from SRUFF_LIB.  Calls CALCTYPE, CAST, DEFAULT and
;		ERRORF_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 15-NOV-2005 by Mati Meron.
;		Modified 25-MAY-2006 by Mati Meron.  Added keyword /FIRST.
;		Modified 25-OCT-2015 by Mati Meron.  Added superconductung undulator 
;		option, using the keyword SCU.  Also added vector input(s) capability. 
;-

	on_error, 1
	mult = 1e3

	ID_conv, gap= gap, period= per, k= kvl, ene= ene, emax= emx, scu= scu, $
	num= num, _extra = _e
	BL_defaults, synch= syn, ene= ren, gam= rgm, cur= cur, ape= ape, dist= dst,$
	dev = dlen, min = mgap, scu = scu
	dum = where(mgap gt gap, ndm)
	if ndm gt 0 then message, /con, $
	'Gap size(s) below the minimal possible ' + string(mgap,form='(f0.2,"mm!")')
	typ = Calctype(gap)

	npr = Default(npr,floor(mult*dlen/per))
	
	res =  !pi/3*!srcon.alp*npr*kvl^2*emx*(mult*cur)
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
	if num eq 1 then res = res[0]

	return, Cast(res,typ,typ,/fix)
end