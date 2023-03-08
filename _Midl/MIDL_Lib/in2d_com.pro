Pro In2D_com, x_sor = xs, y_sor = ys, z_sor = zs, ind = ind, check = chk

;+
; NAME:
;		IN2D_COM
; VERSION:
;		8.7
; PURPOSE:
;		Grid generation for 2D interpolation.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		IN2D_COM, X_SOR= XS [,Y_SOR= YS, Z_SOR= ZS] [keywords])
; INPUTS:
;		None.
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
;			as Y and [2,*,*] as Z (however, note keyword IND, below).  The input
;			can be provided through either X_SOR, Y_SOR or Z_SOR, but only one
;			at a time.
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
; OUTPUTS:
;		Indirect outputs only, through the common block (see below).
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		INTER_INFO.  Contains the following:
;			EXS	-	Scalar flag, set to 1 once the common block has been created
;			NXS	-	Number of points along the X-dimension of the base array.
;			NYS	-	Number of points along the X-dimension of the base array.
;			LXS	-	Vector of length NXS, containing the X coordinates present.
;			LYS	-	Vector of length NYS, containing the Y coordinates present.
;			DX	-	Vector of the differences of the coordinates in LXS.
;			DY	-	Vector of the differences of the coordinates in LYS.
;			A. BX, BY, CXY	-	2D arrays of dimensions [NXS - 1, NYS - 1], used
;								in the intepolation.  Details can be found in
;								the "2D Interpolation on a Rectangular Grid"
;								write-up.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Note:	The base coordinates (from X_SOR, Y_SOR ...must form a
;				rectangular grid, i.e.
;					X[*,j] independent of j.
;					Y[i,*] independent of i.
; PROCEDURE:
;		Generates the arrays using the algorithm from "2D Interpolation on a
;		Rectangular Grid".  See there for details.  Calls ARREQ, CALCTYPE, CAST,
;		DEFAULT, DIF, GRID_CHECK and HOW_MANY, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JUL-2018 by Mati Meron, through a surgery on INTER_2D.
;-

	common inter_info, exs, nxs, nys, lxs, lys, dx, dy, a, bx, by, cxy
	on_error, 1

	typ = Calctype(xs,ys,zs,0.,def=4)
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
		7	:	begin
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

	return
end