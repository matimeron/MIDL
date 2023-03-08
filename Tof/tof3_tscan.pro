Pro Tof3_tscan, npoints, xrang, superimp = sup, relative = rel

;+
; NAME:
;	TOF3_TSCAN
; PURPOSE:
;	Generates a contour plot of time dispertion in TOF3.  Also, if the 
;	keyword SUPERIMP is set, calculates the optimal focusing and imaging 
;	curves and superimposes them on the contour plot.
; CATEGORY:
;	TOF3 specific.
; CALLING SEQUENCE:
;	TOF3_TSCAN, NPOINTS [, XRANG]
; INPUTS:
;    NPOINTS
;	Number of points in the plot.
; OPTIONAL INPUT PARAMETERS:
;    XRANG
;	Range of X(1).  Default is [0,1]
; KEYWORD PARAMETERS:
;    SUPERIMP
;	Switch.  If set, the routine calculates the optimal focusing and 
;	imaging X1 versus X2 curves and superimposes them on the contour plot.
; OUTPUTS:
;	None
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	XRANG must be in the contained in [0,1].
; PROCEDURE:
;	Generates a 2-dimensional grid of points for X(1), X(2).  For each grid 
;	point finds the optimal Y(1) using ROOT (from MIDL) with TOF3_MINT, 
;	then finds the reduced timing dispertion value for each X(1), X(2),
;	using TOF_DT.  Uses also TOF_COMM, as well as CAST, DEFAULT and 
;	MAKE_GRID from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready

    on_error, 1
    if n_elements(ns) eq 0 then Tof_comm, nseg = 3 $
    else if ns ne 3 then message, 'Not a 3 element TOF!'

    xrang = 2*eps > Cast(Default(xrang,[0.,1.]),4) < (1 - 2*eps)
    xvals = Make_grid([[xrang],[xrang]], npoints + 1, fun = tarr)

    rfl = keyword_set(rel)
    for i = 0, npoints do begin
	s = (4*(1./xvals(0,i,i) - 1))^(1./3.)
	if rfl then tarr(i,i) = 1. else tarr(i,i) = 2.*s/(s^2 + 4.)
	for j = i + 1, npoints do begin
	    x(1:2) = xvals(*,i,j)
	    ylims = 2./[sqrt(1 + s^3) - 1, s]
	    if ylims(0) gt ylims(1) then ylims = reverse(ylims)
	    ylims = ylims*[1 + eps, 1 - eps]
	    y(1) = Root('Tof3_mint', ylims, status = st)
	    if not st then begin
		if j eq npoints then y(1) = 2./(sqrt(1 + s^3) - 1) $
		else y(1) = 1. + (3.*x(1) - 1)*(1 + 3.*(x(2) - x(1))/4)/2
	    endif
	    tarr(i,j) = Tof_dt(1)
	    if rfl then begin
		xfac = (2*x(1)/(1. - x(2)))^(1./3.)
		tarr(i,j) = (xfac + 1/xfac)*tarr(i,j)
	    endif
	endfor
    endfor

    if rfl then levarr = findgen(11) else levarr = 0.05*(1 + 3*findgen(7))
    contour, tarr, reform(xvals(0,*,0)),reform(xvals(1,0,*)),$
    levels = levarr, c_annotation = string(levarr,format='(f6.3)'), $
    xstyle = 1, ystyle = 1

    if keyword_set(sup) then begin
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
	    yf(i) = Root('Tof3_opcurv',ylims, .01*eps, /rel, par = 0, stat= st)
	    if st eq 0 then begin
		xf(i) = x(1) + (xeps^2)/3
		yf(i) = 1 + xeps + 11./12.*xeps^3
	    endif else xf(i) = x(2)
	    yi(i) = Root('Tof3_opcurv',ylims, .01*eps, /rel, par = 1, stat= st)
	    if st eq 0 then begin
		xi(i) = x(1) + (xeps^2)/5
		yi(i) = 1 + xeps + 49./60.*xeps^3
	    endif else xi(i) = x(2)
	endfor

	oplot, xsc, xf, thick = 4
	oplot, xsc, xi, thick = 4, linestyle = 2
    endif

    return
end
