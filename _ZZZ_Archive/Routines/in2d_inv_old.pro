Function In2D_inv_old, xval = xvl, yval = yvl, fval = fvl, multi = mul, $
	x_sor = xs, y_sor = ys, z_sor = zs, ind = ind, check = chk, last = las

;+
; NAME:
;		IN2D_INV
; VERSION:
;		8.7
; PURPOSE:
;		Inverse 2D interpolation on rectangular grids.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result= IN2D_INV( XVAL = XVL or YVAL = YVL, FVAL = FVL [keywords])
; INPUTS:
; 		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	XVAL											|
;		Numeric scalar, interpolation X-value.		|	One and only one of
;	YVAL											|	these must be given.
;		Numeric scalar, interpolation Y-value.		|
;	FVAL
;		Numeric scalar, the desired interpolation value.
;	MULTI
;		Switch.  In standard operation a unique result (for X or Y) yielding the
;		required FVAL is sought, and if multiple results are possible, the
;		function returns !NULL.  With MULTI set, multiple results can be
;		returned.
;	X_SOR
;		Partial or complete base coordinates for the interpolation.
;	Y_SOR
;		Ditto.
;	Z_SOR
;		Ditto.
;
;		The base coordinates, X, Y and Z (where Z is taken as Z(X,Y)) can be
;		provided as either a single input or 3 separate inputs, as follows:
;
;		1)	A single 3D array, with first dimension >= 3.  In this case the
;			[0,*,*] subarray will be used as the X base coordinates, [1,*,*]
;			as Y and [2,*,*] as Z.  The input can be provided through either
;			X_SOR, Y_SOR or Z_SOR, but only one at a time.
;
;		2)	3 separate inputs for base X, Y and Z can be provided through the
;			3 keywords above.  In this case they all have to be 2D arrays of
;			same size and format.
;	IND
;		Integer scalar.  If X(Y Z)_SOR is a 3D array, IND specifies the index of
;		the Z data within the array.  In other words, X (Y Z)[IND,*,*] is the
;		actual Z data.  The default value for IND is 2.
;	/CHECK
;		Switch.  If set, the source array is checked for rectangularity.
;	/LAST
;		Switch.  If set, the base coordinates from the last calculation are
;		reused.  A time saver.
; OUTPUTS:
;		Returns the X-value (for a given YVAL) or Y-value (for a given XVAL)
;		yielding FVAL when interpolating from the base coordinates.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		INTER_INFO.  Contains the interpolation data from the last call.  See
;		IN2D_COM for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		1)	The base coordinates, when provided, must form a rectangular grid.
; 			See the routine IN2D_COM for details.
; 		2)	The inputs, XVAL or YVAL must be within the X or Y range defined by
; 			the base coordinates.
; PROCEDURE:
;		Using the interpolation routine INTER_2D.
;		For XVAL given, solves the equation INTER_2D(XVAL,res) - FVAL = 0.
;		For YVAL given, solves the equation INTER_2D(res,YVAL) - FVAL = 0.
;		If no solution is found, returns !NULL.  If multiple solutions are found
;		returns !NULL, unless MULTI is set, in which case all the solutions are
;		returned. Calls DEFAULT, IN2D_COM, IN2D_FUN, ONE_OF and ROOT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUL-2018 by Mati Meron.
;-

	common inter_info, exs, nxs, nys, lxs, lys, dx, dy, a, bx, by, cxy
	on_error, 1

	if not keyword_set(las) then $
		In2D_com, x_sor = xs, y_sor = ys, z_sor = zs, ind = ind, check = chk $
	else if Default(exs,0) eq 0 then message, '"Last" not defined!'
	xran = [min(lxs,max=max),max]
	yran = [min(lys,max=max),max]
	if keyword_set(mul) then mlt = -1 else mlt = 1

	whi = One_of(xvl,yvl,val=con)
	case whi of
		0	:	begin
					if con ge xran[0] and con le xran[1] then $
					res = $
					Root('in2d_fun',yran,xconst=con,zval=fvl,stat=sta,mult=mlt)$
					else message, 'XVAL out of available range!'
					if not min(sta) then res = !null
				end
		1	:	begin
					if con ge yran[0] and con le yran[1] then $
					res = $
					Root('in2d_fun',xran,yconst=con,zval=fvl,stat=sta,mult=mlt)$
					else message, 'YVAL out of available range!'
					if not min(sta) then res = !null
				end
		else:	message, 'Missing input(s)!'
	endcase

	return, res
end