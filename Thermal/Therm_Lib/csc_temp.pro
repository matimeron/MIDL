Function CSC_temp, alp, del, surfload = sul, nterms = ntr

;+
; NAME:
;		CSC_TEMP
; VERSION:
;		0.9
; PURPOSE:
;		Calculates the form-factor for circular, side-cooled geometry.
; CATEGORY:
;		Thermal calculations.
; CALLING SEQUENCE:
;		Result = CSC_TEMP( ALP, DEL [ keywords])
; INPUTS:
;	ALP
;		The ratio: power_load_diameter/plate_diameter.
;	DEL
;		The ratio: thickness/plate_diameter.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/SURFLOAD
;		Switch.  If set, the calculation is for surface power load.  The
;		default is volume (uniform) power load.
;	NTERMS
;		The number of terms to use in the summation of the series.  Currently
;		the default is set on 31 due to a bug in the IDL Bessel-function
;		routines on ALPHA/VMS.  Will be upgraded in the future.
; OUTPUTS:
;		Returns the value of the thermal form factor for circular, side-cooled
;		geometry.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		TERM_STUFF.  Includes current values of NTR and of the Bessel J_0
;		roots.  Also incudes info for back-cooled geometry.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Sums a series resulting from exact calculation.  Calls BESSEL_ROOTS, as
;		well as CAST, DEFAULT and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 1-FEB-2000 by Mati Meron.
;-

	common term_stuff, sc_ex, sc_ntr, sc_rt, $
		bc_ex, bc_eps, bc_lam, bc_ntr, bc_rt

	on_error, 1
	ntrdef = 31l
	ntr = Default(ntr,ntrdef,/dtyp)

	typ = Type(alp) > Type(del)
	if Type(sc_ex) eq 0 or Default(sc_ntr,ntr) ne ntr then begin
		sc_rt = Bessel_roots(0,ntr)
		sc_ntr = ntr
		sc_ex = 1
	endif

	res = alog(1./alp) + 0.5
	if keyword_set(sul) then begin
		if alp eq 0 then aterm= 0.5 else aterm= beselj(alp*sc_rt,1)/(alp*sc_rt)
		if del eq 0 then dterm= 0 else dterm= 2*del*sc_rt/tanh(2*del*sc_rt) - 1
		res = res + 4*total(aterm*dterm/(sc_rt*beselj(sc_rt,1))^2)
	endif

	return, Cast(alp*res/del,4,typ,/fix)
end