Function Inter_2D_old, x, y, x_sor = xs, y_sor = ys, z_sor = zs, ind = ind, $
	check = chk, vec_to_arr = vta, extrapolate = ext, pack = pac, last = las

;+
; NAME:
;		INTER_2D
; VERSION:
;		8.212
; PURPOSE:
;		2D interpolation on rectangular grids.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result= INTER_2D( X [, Y], X_SOR= XS [,Y_SOR= YS, Z_SOR= ZS] [keywords])
; INPUTS:
;	X
;		Result coordinates.  Numeric, mandatory.
;	Y
;		Result coordinates.  Numeric, mandatory or optional (depends on X).
;
;		The following options are possible, for X and Y:
;
;		1)	X is a 3D array, with the first dimension >= 2.  In this case
;			X[0,*,*] is used for the X-coordinates of the result and X[1,*,*]
;			is used for the Y-coordinates.  Y should not be given in this case.
;
;		2)	Both X and Y are given as numeric inputs of arbitrary but *same*
;			format.  In this case X is used for the X-coordinates and Y for Y.
;
;		3)	If the keyword VEC_TO_ARR (see below) is set and both X and Y are
;			1D vectors (not necessarily of same length) then 2D X and Y
;			coordinate sets are generated internally from the X and Y inputs.
;
;	Any other combination will result in error message.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
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
;	/VEC_TO_ARR
;		Switch.  If set, the X and Y inputs can be vectors of different length
;		and they are converted to 2D X and Y arrays internally.
;	/EXTRAPOLATE
;		Switch.  If set, extrapolation, i.e. calculation for points beyond the
;		bounds of the base region, is allowed.  By default it is forbidden (for
;		good reasons).
;	/PACK
;		Switch.  If set and if the result is 2D, a 3D result is generated with
;		Result[0,*,*] containing the X coordinates, Result[1,*,*] the Y and
;		Result[2,*,*] the interpolated Z values.
;	/LAST
;		Switch.  If set, the base coordinates from the last calculation are
;		reused.  A time saver.
; OUTPUTS:
;		Returns the interpolated (from the base inputs) Z-values corresponding
;		to the inputs X, Y.  For alternative form, see PACK.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		INTER_INFO.  Contains the interpolation data from the last call.
;		the last triangulation.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Note:	The base coordinates (from X_SOR, Y_SOR ...must form a 
;				rectangular grid, i.e.
;					X[*,j] independent of j
;					Y[i,*] independent of i.
;
;				No limitation on the interpolation coordinates X, Y, other that
;				they must be within the range defined by the base coordinates
;				(unless /EXTRAPOLATE is set).
;		
; PROCEDURE:
;		Second order minimal surface (hyperbolic) interpolation.  INTER_2D is
;		less general than the combination of the IDL functions TRIGRID and
;		TRIANGULATE, as it works only with rectangular coordinates.  On the
;		other hands it is much faster, for large arrays.  Calls ARREQ, CALCTYPE,
;		CAST, DEFAULT, DIF, FPU_FIX, GRID_CHECK, HOW_MANY and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-MAR-2009 by Mati Meron.
;		Modified 15-JUN-2009 by Mati Meron.  Added keyword IND.
;		Modified 25-JUL-2009 by Mati Meron.  Internal changes.
;		Modified 1-JUN-2013 by Mati Meron.  Internal changes.
;		Modified 25-SEP-2013 by Mati Meron.  Streamlined.  Removed a 
;		requirement of uniform spacing for the base X and Y coordinates.
;-

	common inter_info, exs, nxs, nys, lxs, lys, dx, dy, a, bx, by, cxy
	on_error, 1

	noex = 1 - keyword_set(ext)
	case n_params() of
		1	:	begin
					wxy = reform(x)
					siz = size(wxy)
					if siz[0] eq 3 and siz[1] ge 2 then begin
						wx = reform(wxy[0,*,*])
						wy = reform(wxy[1,*,*])
					endif else message, 'Bad XYZ input!'
				end
		2	:	begin
					wx = reform(x)
					wy = reform(y)
					sizx = size(wx)
					sizy = size(wy)
					if keyword_set(vta) then begin
						if sizx[0] eq 1 and sizy[0] eq 1 then begin
							lx = n_elements(wx)
							ly = n_elements(wy)
							wx = wx#replicate(1.,ly)
							wy = replicate(1.,lx)#wy
						endif else message, 'Unacceptable input!'
					endif else if not Arreq(wx,wy,/nov) then $
					message, 'Incommeasurable inputs!'
				end
		else:	message, 'Wrong number of inputs!
	endcase
	typ = Calctype(wx,wy,0.)
	eps = 4*Toler(type=typ)

	if not keyword_set(las) then begin
		zind = Default(ind,2,/dtyp) > 2
		wha = How_many(fir=xs,sec=ys,thi=zs,whi=whi)
		case whi of
			1	:	begin
						sizs = size(xs)
						if sizs[0] eq 3 and sizs[1] ge 3 then begin
							wxs = Cast(reform(xs[0,*,*]),typ)
							wys = Cast(reform(xs[1,*,*]),typ)
							wzs = Cast(reform(xs[zind,*,*]),typ)
						endif else message, 'bad source input!'
					end
			2	:	begin
						sizs = size(ys)
						if sizs[0] eq 3 and sizs[1] ge 3 then begin
							wxs = Cast(reform(ys[0,*,*]),typ)
							wys = Cast(reform(ys[1,*,*]),typ)
							wzs = Cast(reform(ys[zind,*,*]),typ)
						endif else message, 'bad source input!'
					end
			4	:	begin
						sizs = size(zs)
						if sizs[0] eq 3 and sizs[1] ge 3 then begin
							wxs = Cast(reform(zs[0,*,*]),typ)
							wys = Cast(reform(zs[1,*,*]),typ)
							wzs = Cast(reform(zs[zind,*,*]),typ)
						endif else message, 'bad source input!'
					end
			7:		begin
						if Arreq(xs,ys,/nov) and Arreq(ys,zs,/nov) then begin
							if (size[zs])[0] eq 2 then begin
								wxs = Cast(xs,typ)
								wys = Cast(ys,typ)
								wzs = Cast(zs,typ)
							endif else message, 'Source must be 2D!'
						endif else message, 'Incommeasurable source components!'
					end
			else:	message, 'Wrong number of source inputs!'
		endcase
		if keyword_set(chk) then if not Grid_check(wxs,wys) $
		then message, 'Not a rectangular grid!'

		sizs = size(wzs)
		nxs = sizs[1]
		nys = sizs[2]

		lxs = reform(wxs[*,0])
		dx = (Dif(lxs))[1:*]/2
		if dx[0] lt 0 then begin
			lxs = reverse(lxs)
			wzs = reverse(wzs,1)
		endif

		lys = reform(wys[0,*])
		dy = (Dif(lys))[1:*]/2
		if dy[0] lt 0 then begin
			lys = reverse(lys)
			wzs = reverse(wzs,2)
		endif

		szs = wzs/4
		z0 = szs[0:nxs-2,0:nys-2]
		z1 = szs[1:nxs-1,0:nys-2]
		z2 = szs[1:nxs-1,1:nys-1]
		z3 = szs[0:nxs-2,1:nys-1]
		a = z0 + z1 + z2 + z3
		bx = -z0 + z1 + z2 - z3
		by = -z0 - z1 + z2 + z3
		cxy = z0 - z1 + z2 - z3

		exs = 1
	endif else if Default(exs,0) eq 0 then message, '"Last" not defined!'

	epsx = eps*max(abs(lxs))
	ix = value_locate(lxs,wx)
	dum = where(ix eq -1,ndum)
	if ndum gt 0 then ix[dum] = value_locate(lxs,wx[dum]+epsx)
	dum = where(ix ge (nxs-1),ndum)
	if ndum gt 0 then ix[dum] = value_locate(lxs,wx[dum]-epsx)
	if noex then begin
		dum = where(ix lt 0 or ix ge (nxs-1), ndum)
		if ndum gt 0 then message, 'Points out of interpolation range!
	endif else ix = 0 > ix < (nxs-2)
	fx = (wx - lxs[ix])/dx[ix] - 1

	epsy = eps*max(abs(lys))
	iy = value_locate(lys,wy)
	dum = where(iy eq -1,ndum)
	if ndum gt 0 then iy[dum] = value_locate(lys,wy[dum]+epsy)
	dum = where(iy ge (nys-1),ndum)
	if ndum gt 0 then iy[dum] = value_locate(lys,wy[dum]-epsy)
	if noex then begin
		dum = where(iy lt 0 or iy ge (nys-1), ndum)
		if ndum gt 0 then message, 'Points out of interpolation range!
	endif else iy = 0 > iy < (nys-2)
	fy = (wy - lys[iy])/dy[iy] - 1

	tres = a[ix,iy] + bx[ix,iy]*fx + by[ix,iy]*fy + cxy[ix,iy]*fx*fy
	if keyword_set(pac) then begin
		siz = size(reform(tres))
		if siz[0] eq 2 then begin
			res = make_array(dim=[3,siz[1:2]],typ=typ)
			res[0,*,*] = wx
			res[1,*,*] = wy
			res[2,*,*] = tres
		endif else message, 'Cannot pack!'
	endif else res = Cast(tres,typ,typ)

	return, FPU_fix(res)
end