Function Tof_dt, mode

;+
; NAME:
;	TOF_DT
; PURPOSE:
;	Calculates relative time dispertion in TOF.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	Result = TOF_DT ( [MODE] )
; INPUTS:
;	Only optional inputs exist.
; OPTIONAL INPUT PARAMETERS:
;    MODE
;	Integer value, sets calculation mode.  Possible values are:
;	    0 -	calculates only X dependent part.  Default mode.
;	    1 - calculates only 1 dependent part.
;	    2 - calculates full time dispertion.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the value of the time dispertion normalized to the full time.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Calls TOF_COMM and TOF_DTX.  Uses DEFAULT from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready

    on_error, 1
    Tof_comm
    mode = fix(Default(mode,0))

    case mode of
	0    :	tres = Tof_dtx()/x(1)
	1    :	tres = 1./y(1)
	2    :	tres = sqrt((dex*Tof_dtx()/x(1))^2 + (dey/y(1))^2)
	else :	message, 'Unknown TOF_DT mode!'
    endcase

    return, tres*alph(1)/total(alph)
end
