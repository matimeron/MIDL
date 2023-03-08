Function In2D_inv, xval = xvl, yval = yvl, fval = fvl, multi = mul, $
	x_sor= xs, y_sor= ys, z_sor= zs, ind= ind, check= chk, last= las, _extra= _e

;+
; NAME:
;		IN2D_INV
; VERSION:
;		8.72
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
;		Numeric scalar or vector, the desired interpolation value(s).
;	/MULTI
;		Switch.  In standard operation a unique result (for X or Y) yielding the
;		required FVAL is sought, and if multiple results are possible, the
;		function returns !NULL.  With MULTI set, multiple results can be
;		returned.
;
;		Note: MULTI cannot be set when the FVAL input is not a scalar.
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
;	_EXTRA
;		Formal keyword used to transfer values to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the X-value(s) (for a given YVAL) or Y-value(s)(for a given 
;		XVAL) yielding FVAL(s) when interpolating from the base coordinates.
;
;		If no appropriate values can be found within the base coordinates,
;		returns !NAN.
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
;		If no solution is found, returns !NAN.  If multiple solutions are found,
;		returns !NAN, unless MULTI is set, in which case all the solutions are
;		returned. Calls CALCTYPE, DEFAULT, IN2D_COM, IN2D_FUN, ONE_OF and
;		WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUL-2018 by Mati Meron.
;		Modified 20-OCT-2019 by Mati Meron.  Added multiple FVAL capacity.
;		Modofied 10-JUL-2020 by Mati Meron.  Internal changes.
;-

	common inter_info, exs, nxs, nys, lxs, lys, dx, dy, a, bx, by, cxy
	on_error, 1

	if not keyword_set(las) then $
		In2D_com, x_sor = xs, y_sor = ys, z_sor = zs, ind = ind, check = chk $
	else if Default(exs,0) eq 0 then message, '"Last" not defined!'
	xran = [min(lxs,max=max),max]
	yran = [min(lys,max=max),max]
	typ = Calctype(xvl,yfl,fvl,def=4)
	if typ le 4 then wnan = !values.f_nan else wnan = !values.d_nan

	nvl = n_elements(fvl)
	if nvl gt 0 then res = 0.*fvl else message, 'Missing FVAL input!'
	if keyword_set(mul) then begin
		if nvl eq 1 then mlt = -1 else message, $
		'Multiple roots only allowed with single function value!'
	endif else mlt = 1
	efl = ((Wherinstruct('ext',_e))[0] ge 0)
	if efl then begin
		eran = [0.5,1.5]
		xran = eran*xran
		yran = eran*yran
	endif

	whi = One_of(xvl,yvl,val=con)
	case whi of
		0	:	begin
					for i = 0, nvl-1 do begin
						if (con ge xran[0] and con le xran[1]) or efl then begin
							tres = Root('in2d_fun',yran,$
							xconst=con,zval=fvl[i],stat=sta,mult=mlt,_extra=_e)
							if not min(sta) then tres = wnan
						endif else tres = wnan
						if nvl eq 1 then res = tres else res[i] = tres
					endfor
				end
		1	:	begin
					for i = 0, nvl-1 do begin
						if (con ge yran[0] and con le yran[1]) or efl then begin
							tres = Root('in2d_fun',xran,$
							yconst=con,zval=fvl[i],stat=sta,mult=mlt,_extra=_e)
							if not min(sta) then tres = wnan
						endif else tres = wnan
						if nvl eq 1 then res = tres else res[i] = tres
					endfor
				end
		else:	message, 'Missing input(s)!'
	endcase

	if n_elements(res) eq 1 then res = res[0]
	return, res
end