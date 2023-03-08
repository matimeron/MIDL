Pro Tof_view, step

;+
; NAME:
;	TOF_VIEW
; PURPOSE:
;	Displayes the focus and image elements of the optical matrix as a 
;	function of X.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	TOF_VIEW [, STEP]
; INPUTS:
;	All inputs are optional.
; OPTIONAL INPUT PARAMETERS:
;    STEP
;	Size of X step to use when creating the plot.  Default is 0.01
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS	
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Uses TOF_OPT.  Also uses DEFAULT, NINT and PLOTOT
;	from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready

    on_error, 1

    step = Default(step,0.01)
    xview = x(0)
    for i = 1, ns do begin
	if y(i) ne y(i-1) then begin
	    ntem = 5 > Nint((x(i) - x(i-1))/step)
	    xview = [xview, x(i-1) + (x(i) - x(i-1))/ntem*(1 + findgen(ntem))]
	endif else xview = [xview,x(i)]
    endfor

    nview = n_elements(xview)
    yview = fltarr(2,nview)
    Tof_comm, /opt
    for i = 0, nview - 1 do yview(*,i) = (Tof_opt(xview(i)))(*,0)

    Plvar_keep, act = 'sav'
    !p.font = -1
    !p.charthick = 3
    !x.thick = 4
    !y.thick = 4
    !x.charsize = 1.5
    !y.charsize = 1.5

    Plotot, xview, yview, thick = 3, linestyle = [0,2], xtitle = 'X', $
    ytitle = 'F!I11!N , F!I12'
    plots,[[0,0],[1,0]], thick = 1.5
    for i = 1, ns - 1 do plots, x(i)*[1,1], !y.crange, linestyle =1, thick= 2

    Plvar_keep, act = 'res'

    return
end
