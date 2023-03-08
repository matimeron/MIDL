Function Tof_mint, ty, der

;+
; NAME:
;	TOF_MINT
; PURPOSE:
;	called through ROOT to optimize the TOF time dispertion.
; CATEGORY:
;	TOF specific, #segments >= 4.
; CALLING SEQUENCE:
;	Result = TOF_MINT(TY [, DER])
; INPUTS:
;    TY
;	Numerical, value of Y(1) if DER not provided, else search parameter.
; OPTIONAL INPUT PARAMETERS:
;    DER
;	Numerical, the value of the derivative of Y(1) with respect to Y(2).
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns value proportional to the gradient of the TOF time 
;	dispertion.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS.
; SIDE EFFECTS:
;	The value of Y(1) in the common block TOF_VARS is replaced by TY.
;	ALPH and BETH are recalculated accordingly.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses calls to TOF_DT to calculate either DTX (if DEY = 0) or the
;	numerical derivative of the full timing dispertion.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready

    on_error, 1
    if ns lt 4 then message, 'Small TOF, routine not applicable!'

    if n_elements(der) eq 0 then begin
	y(1) = ty
	res = Tof_dt(0)
    endif else begin
	keep = y
	grad = [1., -der]
	y(1:2) = keep(1:2) + grad*(ty - eps)
	vall = Tof_dt(2)
	y(1:2) = keep(1:2) + grad*(ty + eps)
	valh = Tof_dt(2)
	y = keep
	Tof_comm
	res = (valh - vall)/(2*eps)
    endelse

    return, res
end
