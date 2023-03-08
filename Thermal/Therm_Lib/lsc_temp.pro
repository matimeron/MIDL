Function LSC_temp, alp, del, surfload = sul, nterms = ntr

;+
; NAME:
;	LSC_TEMP
; VERSION:
;	0.9
; PURPOSE:
;	Calculates the form-factor for linear, side-cooled geometry.
; CATEGORY:
;	Thermal calculations.
; CALLING SEQUENCE:
;	Result = LSC_TEMP( ALP, DEL [ keywords])
; INPUTS:
;    ALP
;	The ratio: power_load_width/plate_width.
;    DEL
;	The ratio: thickness/plate_width.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    /SURFLOAD
;	Switch.  If set, the calculation is for surface power load.  The
;	default is volume (uniform) power load.
;    NTERMS
;	The number of terms to use in the summation of the series.  Will get
;	automated in the future.
; OUTPUTS:
;	Returns the value of the thermal form factor for linear, side-cooled
;	geometry.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TERM_STUFF.  Includes current values of NTR and of the Bessel J_0
;	roots.  Also incudes info for back-cooled geometry.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Sums a series resulting from exact calculation.  Calls CAST, DEFAULT
;	and TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 1-FEB-2000 by Mati Meron.
;-

    common term_stuff, sc_ex, sc_ntr, sc_rt, $
        bc_ex, bc_eps, bc_lam, bc_ntr, bc_rt

    on_error, 1
    ntrdef = 50l
    ntr = Default(ntr,ntrdef,/dtyp)

    typ = Type(alp) > Type(del)
    res = 1 - 0.5*alp
    if keyword_set(sul) then begin
	nser = !dpi/2*(1 + 2*dindgen(ntr))
	if alp eq 0 then aterm = 1. else aterm = sin(alp*nser)/(alp*nser)
	if del eq 0 then dterm = 0 else dterm = 2*del*nser/tanh(2*del*nser) - 1
	res = res + 2*total(aterm*dterm/nser^2)
    endif

    return, Cast(res/(2*del),4,typ,/fix)
end
