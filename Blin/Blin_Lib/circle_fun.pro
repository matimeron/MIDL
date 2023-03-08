Function Circle_fun, p, x, y, grad = grd, hess = hes

;+
; NAME:
;		CIRCLE_FUN
; VERSION:
;		7.09
; PURPOSE:
;		Evaluates the circle function and its derivatives.  Primary purpose is
;		to serve as evaluation function for fitting, CIRCLE_FIT.
; CATEGORY:
;		Mathematical, geometrical fitting.
; CALLING SEQUENCE:
;		Result = CIRCLE_FUN( P [, X, Y] [, keywords])
; INPUTS:
;	P
;		Numeric vector, containing the circle parameters, in order:
;		[Radius, X_center, Y_center]
;	X
;		Numeric vector, the X coordinates of the points being evaluated.
;	Y
;		Numeric vector, the Y coordinates of the points being evaluated.
;
;	Note 1 :	The P input is mandatory.  X and Y are mandatory if the common
;				block M_CIRC_KEEP (see below) has not been initialized, optional
;				otherwise.  If the common block has been initialized and X, Y
;				are not provided, they are supplied from the common block.  If
;				X, Y are provided, the common block is reinitialized.
;	Note 2 :	If and when X and Y are provided, they must be of same length.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GRAD
;		Optional output, see below.
;	HESS
;		Optional output, see below.
; OUTPUTS:
;		Returns the evaluated function F = R^2 - (X - X_cen)^2 - (Y - Y_cen)^2.
;		The dimension of the result is Nx (the length of X and Y)
; OPTIONAL OUTPUT PARAMETERS:
;	GRAD
;		Returns the gradient of the result as a 2D matrix of dimension [Nx,Np]
;	HESS
;		Returns the Hessian (second derivative) of the result as a 3D array
;		of dimension [Nx,Np,Np]
; COMMON BLOCKS:
;		M_CIRC_KEEP.  Contains the following:
;
;		TYP	:	Data type code, the highest of the types of P, X, Y and no
;				lower than 4.
;		NXY :	The length of X (and Y).
;		WX :	The last X vector.
;		WY :	The last Y vector.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		As mentioned in Notes 1-2, above.
; PROCEDURE:
;		Evaluates the circle function and its derivatives.  Calls CALCTYPE,
;		CAST and ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2009 by Mati Meron.
;-

	common m_circ_keep, typ, nxy, wx, wy
	on_error, 1

	case (n_params() < 3) of
		1	:	if not Isnum(typ) then message, 'Not initialized!'
		3	:	begin
					if n_elements(p) ne 3 then message, '3 parameters needed!'
					typ = Calctype(0.,p,x,y)
					nxy = n_elements(x)
					if n_elements(y) ne nxy then message, $
					'X-Y dimensional mismatch!'
					wx = x
					wy = y
				end
		 else:	message, 'Wrong number of inputs!'
	endcase

	if arg_present(grd) then $
	grd = Cast(2*[[replicate(p[0],nxy)],[wx-p[1]],[wy-p[2]]],typ,typ)
	if arg_present(hes) then begin
		hes = make_array(nxy,3,3,typ=typ)
		hes[*,0,0] = 2
		hes[*,1,1] = (hes[*,2,2] = -2)
	endif

	return, Cast(p[0]^2 - (wx-p[1])^2 - (wy-p[2])^2, typ, typ, /fix)
end