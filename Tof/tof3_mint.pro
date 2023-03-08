Function Tof3_mint, ty

;+
; NAME:
;	TOF3_MINT
; PURPOSE:
;	called through ROOT to optimize the TOF3 time dispertion.
; CATEGORY:
;	TOF3 specific.
; CALLING SEQUENCE:
;	Result = TOF3_MINT(TY)
; INPUTS:
;    TY
;	Numerical, value of Y(1).
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns value proportional to the derivative of the TOF3 time 
;	dispertion with respect to Y(1).
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS
; SIDE EFFECTS:
;	The value of Y(1) in the common block TOF_VARS is replaced by TY.
;	ALPH and BETH are recalculated accordingly.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses calls to Tof_dt to calculate either DTX (if DEY = 0) or the
;	numerical derivative of the full timing dispertion.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready

    on_error, 1
    if ns ne 3 then message, 'Not a 3 element TOF!'

    if dey eq 0 then begin
	y(1) = ty
	res = Tof_dt(0)
    endif else begin
	y(1) = ty - eps
	vall = Tof_dt(2)
	y(1) = ty + eps
	valh = Tof_dt(2)
	y(1) = ty
	Tof_comm
	res = (valh - vall)/(2*eps)
    endelse

    return, res
end
