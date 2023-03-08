Function OBGP_conv_fun, x, par

;+
; NAME:
;		OBGP_CONV_FUN
; VERSION:
;		8.44
; PURPOSE:
;		An internal function to be used by ROOT for OBGP_CONV purposes.
; CATEGORY:
;		Mathematical, Superconducting undulator specific.
; CALLING SEQUENCE:
;		Result = OBGP_CONV_FUN( X, PAR)
; INPUTS:
;	X
;		Scalar or vector.  Represents either undulator period or gap.
;	PAR
;		A 3 element vector, including:
;			0	:	Calculation option number, either 3 or 5 (see BGP_CONV)
;			1	:	Magnetic field, in Tesla.
;			2	:	For option 3, gap, for option 5, period.  Both in mm.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the "third parameter", which is period for option 3 and gap for
;		option 5.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Evaluates the difference between assigned and calculated field, calling
;		OSCU_FIELD.
; MODIFICATION HISTORY:
;		Created 25-OCT-2015 by Mati Meron.
;		Obsoleted and renamed OBGP_CONV_FUN 15-JUL-2020 by Mati Meron.
;-

on_error, 1

	res = 0*x
	case par[0] of
		3	:	res = OSCU_field(par[2]/x,par[2],field=par[1])
		5	:	for i = 0, n_elements(x)-1 do res[i] = $
				OSCU_field(x[i]/par[2],x[i],field=par[1])
		else:	message, 'Option code ' + string(par[0],form='(i0)') + $
					' is not acceptable!'
	endcase

	return, res
end