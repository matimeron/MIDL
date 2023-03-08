Function ID_conv_fun, x, par, gap = gap, scu = scu, _extra = _e

;+
; NAME:
;		ID_CONV_FUN
; VERSION:
;		8.44
; PURPOSE:
;		An internal function to be used by ROOT for ID_CONV purposes.
; CATEGORY:
;		Mathematical, SR specific
; CALLING SEQUENCE:
;		Result = ID_CONV_FUN( X, PAR)
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
;	GAP
;		Scalar, the gap value, in mm.
;	/SCU
;		Switch.  If set, the calculation is for superconducting (NbTi) magnets.
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
;		Straighforward.  Calls BGP_CONV.
; MODIFICATION HISTORY:
;		Created 20-SEP-2005 by Mati Meron.
;		Modified 25-OCT-2015 by Mati Meron to accomodate superconducting
;		undulators.  Added keywords SCU and GAP.
;		Modified 15-JUL-2020 by Mati Meron, to adapt to new field calculation
;		scheme.
;-

	on_error, 1

	y = x*BGP_conv(per=x,gap=gap,scu=scu,_extra=_e)
	case par[0] of
		10	:	res = y - par[1]
		34	:	res = par[2]*x*(1 + par[1]*y^2) - 1
		else:	message, 'Option code ' + string(fix(par[0],form='(i3)')) + $
				'not acceptable!'
	endcase

	return, res
end