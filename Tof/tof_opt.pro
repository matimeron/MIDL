Function Tof_opt, xv, newmat = new

;+
; NAME:
;	TOF_OPT
; PURPOSE:
;	Evaluates the TOF optical matrix at the provided location.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	Result = TOF_OPT ([ XV [, /NEW ] ])
; INPUTS:
;	All inputs are optional.
; OPTIONAL INPUT PARAMETERS:
;    XV
;	Evaluation point.
; KEYWORD PARAMETERS:
;    NEWMAT
;	Reinitializes the global optical matrix.
; OUTPUTS:
;	Returns the optical matrix at XV.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	XV must be in the [0,1] range.
; PROCEDURE:
;	Straightforward.  Uses DEFAULT from MIDL.  Optionally calls TOF_OPT.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready

    on_error, 1
    xv = Default(xv,1.)
    if xv lt 0 or xv gt 1 then message, 'X value out of range!'
    if keyword_set(new) then Tof_comm, /opt

    dum = where(x eq xv, iseq)
    if iseq ne 0 then res = opmat(*,*,dum(0)) $
    else begin
	dum = where(x gt xv)
	k = dum(0)
	yv = sqrt(((x(k) - xv)*y(k-1)^2 + (xv - x(k-1))*y(k)^2)/(x(k) - x(k-1)))
	alpv = 2.*(xv - x(k-1))/(yv + y(k-1))
	res = opmat(*,*,k-1)#[[1., alpv],[0., 1.]]
    endelse

    return, res
end
