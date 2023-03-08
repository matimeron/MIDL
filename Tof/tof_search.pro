Function Tof_search, optel, xev

;+
; NAME:
;	TOF_SEARCH
; PURPOSE:
;	Identifies points where the optimal timing curve crosses the focal and
;	image curves.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	Result = TOF_SEARCH( OPTEL [, XEV])
; INPUTS:
;    OPTEL
;	Optical element identifier.  0 for focus, 1 for image.
; OPTIONAL INPUT PARAMETERS:
;    XEV
;	The X value where the optical matrix is evaluated.  Default is 1.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns a (2,N) array  where N is the number of crossing points found.
;	Each line of the array contains the Y(1), Y(2) values corresponding to
;	the crossing.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS and TOF.
; SIDE EFFECTS:
;	The common blocks are updated.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Identifies the Y(2) values, from TYSPL, between which crossings occur,
;	then uses ROOT (from MIDL), with TOF_WPOINT to find the exact 
;	locations.  Also calls TOF_OPT, and DEFAULT and SPLIN_EVAL from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready
    common Tof_ext, ysmax, nspl, yspl, ntspl, tyspl


    on_error, 1
    if ns lt 4 then message, 'Small TOF, routine not applicable!'
    if not splready then message, 'Run TOF_TCURV first, to create splines!'

    xev = Default(xev,1.)
    tem = 0
    el = 0
    for i = 1, ntspl - 1 do begin
	y(1:2) = tyspl(i,[1,0])
	nel = (Tof_opt(xev, /new))(optel)
	if i gt 1 and el*nel le 0 then tem = [tem,i]
	el = nel
    endfor

    nres = n_elements(tem) - 1
    if nres gt 0 then begin
	tem = tem(1:*)
	res = fltarr(2,nres)
	for j = 0, nres - 1 do begin
	    ysres = Root('Tof_wpoint',tyspl(tem(j)-1:tem(j),0),par=[optel,xev])
	    res(*,j) = [Splin_eval(ysres,tyspl), ysres]
	endfor
    endif else res = 0

    return, res
end
