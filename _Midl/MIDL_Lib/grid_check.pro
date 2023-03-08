Function Grid_check, x, y, threshold = tre, details = det

;+
; NAME:
;		GRID_CHECK
; VERSION:
;		8.212
; PURPOSE:
;		Checks for deviation of a grid from rectangularity.
; CATEGORY:
;		Array Manipulation.
; CALLING SEQUENCE:
;		Result = GRID_CHECK( X, Y)
; INPUTS:
;	X
;		Grid coordinates.  Numeric, mandatory.
;	Y
;		Grid coordinates.  Numeric, mandatory or optional (depends on X).
;
;	The following options are possible, for X and Y:
;
;		1)	X is a 3D array, with the first dimension >= 2.  In this case
;			X[0,*,*] is used for the X-coordinates of the grid and X[1,*,*] is
;			used for the Y-coordinates.
;
;		2)	Both X and Y are given as numeric 2D arrays.  In this case X is
;			used for the X-coordinates and Y for Y.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	THRESHOLD
;		Accepts a scalar value to serve as the determination threshold.  Grid
;		is accepted as rectangular if the maximal relative deviations from 
;		rectangularity do not exceed THRESHOLD.  Default (and minimal) value is 
;		4*TOLER() (see there) for the appropriate data type.
;	DETAILS
;		Optional output, see below.
; OUTPUTS:
;	Returns 1 if both values in DETAILS(see below) are <= THRESHOLD, else 
;	returns 0.
; OPTIONAL OUTPUT PARAMETERS:
;	DETAILS
;		Returns a 2 element vector containing the maximal relative deviations of
;		the X and Y coordinates, respectively, from a perfect rectangular grid.	
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Both X and Y need to be valid 2D arrays, with same dimensions.  Also,
;		both dimensions have to be > 1.
; PROCEDURE:
;		Straightforward.  Calls ARREQ, CALCTYPE, DEFAULT, DIF and TOLER, from 
;		MIDL.
; MODIFICATION HISTORY:
;		Created 25-SEP-2013 by Mati Meron.
;-

	on_error, 1

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
					if not Arreq(wx,wy,/nov) then $
					message, 'Incommeasurable inputs!'
				end
		else:	message, 'Wrong number of inputs!
	endcase
	dim = size(wx,/dim)
	if min(dim) lt 2 then message, 'Not a grid!'

	typ = Calctype(wx,wy,0.)
	eps = 4*Toler(type=typ)
	wtre = Default(tre,eps,/dtyp)
	
	tx = reform(wx[*,0])
	cx = tx#replicate(1.,dim[1])
	dx = Dif(tx,/lin)#replicate(1.,dim[1])
	ty = reform(wy[0,*])
	cy = replicate(1.,dim[0])#ty
	dy = replicate(1.,dim[0])#Dif(ty,/lin)

	det = [max(abs((wx-cx)/dx)),max(abs((wy-cy)/dy))] 
	
	return, min(det le wtre)
end