Pro Tof_tscan, npoints, yfmax

;+
; NAME:
;	TOF_TSCAN
; PURPOSE:
;	Generates a contour plot of the timing dispertion function, as a
;	function of Y(1), Y(2), ans superimposes on it a plot of the optimal
;	Y(2) versus Y(1) curve.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	TOF_TSCAN, NPOINTS [, YFMAX]
; INPUTS:
;    NPOINTS
;	Number of points along each dimension.
; OPTIONAL INPUT PARAMETERS:
;    YFMAX
;	Maximal value of Y(1).  Default is 1.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS and TOF_EXT.
; SIDE EFFECTS:
;	The common blocks are updated.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Finds the time dispertion function using calls to TOF_DT.  Also uses 
;	DEFAULT, MAKE_GRID and SPLIN_EVAL from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready
    common Tof_ext, ysmax, nspl, yspl, ntspl, tyspl

    on_error, 1
    if ns lt 4 then message, 'Small TOF, routine not applicable!'
    if not splready then message, 'Run TOF_TCURV first, to create splines!'

    yfmax = Default(yfmax,1.)
    yvals = Make_grid([[0., yfmax],[0., ysmax]], npoints + 1, fun = tarr)

    for i = 1, npoints do begin
	for j = 1, npoints do begin
	    y(1:2) = yvals(*,i,j)
	    if dey eq 0 then tarr(i,j)= Tof_dt(1) else tarr(i,j)= Tof_dt(2)/dey
	endfor
    endfor

    levarr = .1/y(ns-1)*(1 + findgen(10))
    dum = check_math(0,1)
    contour, tarr(1:*,1:*), reform(yvals(0,1:*,0)),reform(yvals(1,0,1:*)),$
    levels = levarr, c_annotation = string(levarr,format='(f4.1)')
    dum = check_math(0,0)
    ys = reform(yvals(1,0,*))
    yf = Splin_eval(ys,yspl)
    oplot, yf, ys, thick = 2, linestyle = 2
    yf = Splin_eval(ys,tyspl)
    oplot, yf, ys, thick = 2

    return
end
