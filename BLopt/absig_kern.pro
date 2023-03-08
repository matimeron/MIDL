Function Absig_kern, x, mud = mud, n = n, wire = wir, edge = edg, _extra = _e

;+
; NAME:
;		ABSIG_KERN
; VERSION:
;		8.72
; PURPOSE:
;		Integration Kernel for edge and wirebroadening calculations.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = ABSIG_KERN ( X, MUD = MUD, N = N [, keywords])
; INPUTS:
; 	X
; 		Numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	MUD
;		Numeric scalar
;	N
;		Integer scalar.
;	/WIRE												|
;		Switch, specifies "wire" calculation option.	| One and only one of
;	/EDGE												| these two must be set.
;		Switch, specifies "edge" calculation option.	|
;	_EXTRA
;		A formal keyword used to pass keywords from calling routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the funcion required as kernel of the integration.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None other than the restrictions specified for inputs.
; PROCEDURE:
;		Straightforward. Calls CALCTYPE, CAST, DEFAULT, FPU_FIX and ONE_OF, from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 30-DEC-2020 by Mati Meron, based on previous WIRE_KERN.
;-

	on_error, 1

	typ = Calctype(x,0.)

	wmud = Default(mud,Cast(0.,typ,typ),/dtyp)
	wn = Default(n,0,/dtyp) > 0
	whi = One_of(wir,edg)
	Case whi of
		0	:	res = exp(-wmud*x)*(1-x^2)^(wn/2.)
		1	:	res = exp(-wmud*x)*(1-x)^wn
		else:	message, 'Either WIRE or EDGE must be specified!'
	endcase

	return, FPU_fix(res)
end