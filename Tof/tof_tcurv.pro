Pro Tof_tcurv, npoints, eymax, edex, edey

;+
; NAME:
;	TOF_TCURV
; PURPOSE:
;	Generates the spline coefficients needed to approximate the optimal
;	Y(1) versus Y(2) curve for a TOF device, both without and with 
;	position and velocity spreads.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	TOF_TCURV, NPOINTS, EYMAX [, optional input parameters]
; INPUTS:
;    NPOINTS
;	Integer, the number of points to be used to generate the splines.
;    EYMAX
;	Maximal value of Y(2).
; OPTIONAL INPUT PARAMETERS:
;    EDEX
;	Value of DEX (normalized position spread).  Default is the value from
;	the common block TOF_VARS.
;    EDEY
;	Value of DEY (normalized velocity spread).  Default is the value from
;	the common block TOF_VARS.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	None.  Results are passed through the common block TOF.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS and TOF_EXT.
; SIDE EFFECTS:
;	Both common blocks may change during the call.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses ROOT (from MIDL) in conjunction with TOF_MINT to find the Y(1)
;	values corresponding to the preset Y(2), then uses SPLIN_COEFFS (from
;	MIDL) to establish the coefficient table.  Also calls DEFAULT and 
;	SPLIN_EVAL from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready
    common Tof_ext, ysmax, nspl, yspl, ntspl, tyspl

    on_error, 1
    if ns lt 4 then message, 'Small TOF, routine not applicable!'

    dex = Default(edex,dex)
    dey = Default(edey,dey)
    ysmax = 1.*eymax
    ysec = ysmax*(Make_grid([0.,1.], npoints + 1, fun = yfir))^2
    nd = ns - flag
    ramax = (2.*x(1)/(x(nd) - x(nd-1)))^(1./3.)
    for i = 1, npoints do begin
	y(2) = ysec(i)
	st = 0
	teps = eps
	while not st and eps ge 1e-3*teps do begin
	    yfir(i) = Root('Tof_mint', [0, y(nd)*ramax] + 2*eps*[1,-1],stat=st)
	    if not st then eps = 0.1*eps
	endwhile
	eps = teps
    endfor
    yspl = Splin_coeffs(ysec,yfir)
    nspl = (size(yspl))(1)

    if dey gt 0 then begin
	for i = 1, nspl - 1 do begin
	    y(1:2) = yspl(i,[1,0])
	    der = Splin_eval(y(2), yspl, deriv = 1)
	    st = 0
	    teps = eps
	    while not st and eps ge 1e-3*teps do begin
		brac = [0,(y(2) - ysec(i-1) - eps)/der] < 1
		t = Root('Tof_mint', brac, par=der, stat=st)
		if not st then eps = 0.1*eps
	    endwhile
	    eps = teps
	    yfir(i) = yspl(i,1) + t
	    ysec(i) = yspl(i,0) - der*t
	endfor
	tyspl = Splin_coeffs(ysec,yfir)
	ntspl = (size(tyspl))(1)
    endif else begin
	tyspl = yspl
	ntspl = nspl
    endelse

    splready = 1
    return
end
