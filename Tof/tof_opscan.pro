Pro Tof_opscan, npoints, yfmax, xev

;+
; NAME:
;	TOF_OPSCAN
; PURPOSE:
;	Generates a contour plot of the focus and image elements of the TOF
;	optical matrix as a function of Y(1), Y(2), ans superimposes on it a 
;	plot of the optimal Y(2) versus Y(1) curve.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	TOF_OPSCAN, NPOINTS [, YFMAX, XEV]
; INPUTS:
;    NPOINTS
;	Number of points along each dimension.
; OPTIONAL INPUT PARAMETERS:
;    YFMAX
;	Maximal value of Y(1).  Default is 1.
;    XEV
;	The X value at which the optical matrix is evaluated.  Default is 1.
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
;	Finds the optical elements values using calls to TOF_OPT.  Also uses 
;	DEFAULT, MAKE_GRID and SPLIN_EVAL from MIDL, and TOF_SEARCH to 
;	identify the optimal operating points.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready
    common Tof_ext, ysmax, nspl, yspl, ntspl, tyspl

    on_error, 1
    if ns lt 4 then message, 'Small TOF, routine not applicable!'
    if not splready then message, 'Run TOF_TCURV first, to create splines!'

    yfmax = Default(yfmax,1.)
    xev = Default(xev,1.)
    yvals = Make_grid([[0., yfmax],[0., ysmax]], npoints + 1, fun = focarr)
    imarr = focarr

    for i = 1, npoints do begin
	for j = 1, npoints do begin
	    y(1:2) = yvals(*,i,j)
	    dum = Tof_opt(xev, /new)
	    focarr(i,j) = dum(0,0)
	    imarr(i,j) = dum(1,0)
	endfor
    endfor

    Plvar_keep, act = 'sav'
    !p.font = -1
    !p.charthick = 3
    !x.thick = 4
    !y.thick = 4
    !x.charsize = 1.5
    !y.charsize = 1.5

    levarr = [0.]
    dum = check_math(0,1)
    contour, focarr(1:*,1:*), reform(yvals(0,1:*,0)),reform(yvals(1,0,1:*)), $
    /follow, levels = levarr, c_linestyle = 1, c_thick = 3, c_labels = 0, $
    xtitle = 'Y!I1', ytitle = 'Y!I2'
    contour, imarr(1:*,1:*), reform(yvals(0,1:*,0)),reform(yvals(1,0,1:*)), $
    /follow, levels = levarr, /noerase, c_linestyle= 2, c_thick= 3,c_labels= 0
    dum = check_math(0,0)
    ys = reform(yvals(1,0,*))
    yf = Splin_eval(ys,yspl)
    oplot, yf, ys, thick = 2
    yf = Splin_eval(ys,tyspl)
    oplot, yf, ys, thick = 2

    usersym, sqrt(.5)*[[1,1],[-1,1],[-1,-1],[1,-1]], /fill
    dum = Tof_search(0)
    if n_elements(dum) gt 1 then plots, dum, psym = 8
    usersym, [[1,0],[0,1],[-1,0],[0,-1]], /fill
    dum = Tof_search(1)
    if n_elements(dum) gt 1 then plots, dum, psym = 8

    Plvar_keep, act = 'res'

    return
end
