Pro Tof3_special, npoints, xrang

;+
; NAME:
;	TOF3_SPECIAL
; PURPOSE:
;	Calculates potential Y(1), appropriate for optimal timing and 
;	focus/image point on target.
; CATEGORY:
;	TOF3 specific.
; CALLING SEQUENCE:
;	TOF3_SPECIAL, NPOINTS [, XRANG]
; INPUTS:
;    NPOINTS
;	Number of points in plot.
; OPTIONAL INPUT PARAMETERS:
;    XRANG
;	Range of X(1).  Default is [0,1]
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	None
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	XRANG should be contained in [0,1]
; PROCEDURE:
;	Creates an one-dimensional grid of points X(1).  For each point 
;	calculates X(2) using the optimal timing condition, then calculates 
;	Y(1) using ROOT (from MIDL) with TOF3_OPCURV.  Also calls CAST, DEFAULT
;	and PLOT_VER2 from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready

    on_error, 1
    if n_elements(ns) eq 0 then Tof_comm, nseg = 3 $
    else if ns ne 3 then message, 'Not a 3 element TOF!'

    xrang = 2*eps > Cast(Default(xrang,[0.,1.]),4) < (1 - 2*eps)
    xsc = Make_grid(xrang, npoints + 1, fun = xf)
    xi = xf
    yf = xf
    yi = xf
    for i = 0, npoints do begin
	x(1) = xsc(i)
        s = (4*(1./x(1) - 1))^(1./3.)
	ylims = 2./[sqrt(1 + s^3) - 1, s]
	if ylims(0) gt ylims(1) then ylims = reverse(ylims)
	ylims = ylims*[1 + eps, 1 - eps]
	xeps = (3*x(1) - 1)/2
	yf(i) = Root('Tof3_opcurv',ylims, .01*eps, /rel, par = 0, stat = st)
	if st eq 0 then begin
	    xf(i) = x(1) + (xeps^2)/3
	    yf(i) = 1 + xeps + 11./12.*xeps^3
	endif else xf(i) = x(2)
	yi(i) = Root('Tof3_opcurv',ylims, .01*eps, /rel, par = 1, stat = st)
	if st eq 0 then begin
	    xi(i) = x(1) + (xeps^2)/5
	    yi(i) = 1 + xeps + 49./60.*xeps^3
	endif else xi(i) = x(2)
    endfor

    Plvar_keep, act = 'sav'
    !p.font = -1
    !p.charthick = 3
    !x.thick = 4
    !y.thick = 4
    !x.charsize = 1.5
    !y.charsize = 1.5
    Plot_ver2, xsc, [[xf - xsc],[xi - xsc]], [[yf],[yi]], ytyp = [0,1], $
    xtitle = 'X!I1', ytitle = ['X!I2!N - X!I1', 'Y!I1'], thick = 3, $
    linestyle = [0,2], gap = 1
    Plvar_keep, act = 'res'

    return
end
