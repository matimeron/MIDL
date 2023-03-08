Pro Tof_ratio, min, normalize = norm

;+
; NAME:
;	TOF_RATIO
; PURPOSE:
;	Calculates and displayes the normalized time dispertion of a TOF as a
;	function of Y(1).
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	TOF_RATIO [, MIN]
; INPUTS:
;	All inputs are optional.
; OPTIONAL INPUT PARAMETERS:
;    MIN
;	The minimal value of Y(1) in the display.  Default is 0.
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
;	Straightforward.  Uses calls to TOF_DT.  Also calls DEFAULT from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready
    common Tof_ext, ysmax, nspl, yspl, ntspl, tyspl

    if ns lt 4 then message, 'Small TOF, routine not applicable!'
    if not splready then message, 'Run TOF_TCURV first, to create splines!'

    min = Default(min,eps)

    disp = fltarr(nspl)
    for i = 1, nspl - 1 do begin
	y(1:2) = yspl(i,[1,0])
	disp(i) = Tof_dt(1)
    endfor

    if dey gt 0 then begin
	tdisp = fltarr(ntspl)
	for i = 1, ntspl - 1 do begin
	    y(1:2) = tyspl(i,[1,0])
	    tdisp(i) = Tof_dt(2)/dey
	endfor
    endif else tdisp = disp

    nd = ns - flag
    xfac = (2*x(1)/(x(nd) - x(nd-1)))^(1./3.)
    mfac = y(nd)*(xfac + 1/xfac)
    if keyword_set(norm) then nfac = mfac else nfac = 1.

    dum = where(yspl(*,1) ge min)
    tdum = where(tyspl(*,1) ge min)

    Plvar_keep, act = 'sav'

    !p.font = -1
    !p.charthick = 3
    !x.thick = 4
    !y.thick = 4
    !x.charsize = 1.5
    !y.charsize = 1.5
    plot, yspl(dum,1), nfac*disp(dum), thick = 2, linestyle = 2, $
    xtitle = 'Y!I1', ytitle = 'D'
    oplot, tyspl(tdum,1), nfac*tdisp(tdum), thick = 2
    if not keyword_set(norm) then $
    plots, tyspl([tdum(0),ntspl-1],1), [1,1]/mfac, linestyle = 2, thick = 1.5

    Plvar_keep, act = 'res'

    return
end
