Function OID_conv_fun, x, par, scu = scu, gap = gap, _extra = _e

;+
; NAME:
;		OID_CONV_FUN
; VERSION:
;		8.44
; PURPOSE:
;		An internal function to be used by ROOT for OID_CONV purposes.
; CATEGORY:
;		Mathematical, SR specific
; CALLING SEQUENCE:
;		Result = OID_CONV_FUN( X, PAR)
; INPUTS:
;	X
;		Scalar or vector.  Represents magnetic field value.
;	PAR
;		A 3 element vector, including:
;			0	:	Calculation option number.
;			1-2	:	Calculation parameters.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/SCU
;		Switch.  If set, the calculation is for superconducting (NbTi) magnets.
;	GAP
;		Scalar, the gap value, in mm, for the superconducting magnets.  If SCU
;		is set and GAP is not provided, it defaults to the value in !BLPAR.SGAP.
;		If SCU is not set, GAP has no effect.
; OUTPUTS:
;		Returns calculation result, details depend on case.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straighforward.  Calls OID_FIELD.
; MODIFICATION HISTORY:
;		Created 20-SEP-2005 by Mati Meron.
;		Modified 25-OCT-2015 by Mati Meron to accomodate superconducting
;		undulators.  Added keywords SCU and GAP.
;		Obsoleted and renamed OID_CONV_FUN 15-JUL-2020 by Mati Meron.
;-

	on_error, 1

	tem = OID_field(x,/inv,/qui,scu=scu,gap=gap,_extra=_e)
	case par[0] of
		10	:	res = tem - par[1]*x
		34	:	res = tem^3 - par[1]*(tem^2 + (par[2]*x)^2/2)
		else:	message, 'Option code ' + string(fix(par[0],form='(i3)')) + $
				'not acceptable!'
	endcase

	return, res
end