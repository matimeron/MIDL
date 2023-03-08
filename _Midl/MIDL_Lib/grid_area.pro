Function Grid_area, x, y

;+
; NAME:
;		GRID_AREA
; VERSION:
;		8.212
; PURPOSE:
;		Calculates the areas of all all elements of an XY grid.
; CATEGORY:
;		Array Manipulation.
; CALLING SEQUENCE:
;		Result = GRID_AREA( X, Y)
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
;		None.
; OUTPUTS:
;		Returns an array containing the areas of all the grid elements.  For X
;		and Y of dimensions [M, N] the return array has dimensions [M-1, N-1], 
;		where term [i,j] contains the area of the quadrilateral defined by the
;		vertices [i,j], [i+1,j], [i+1,j+1] and [i,j+1].
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Both X and Y need to be valid 2D arrays, with same dimensions.  Also,
;		both dimensions have to be > 1.
; PROCEDURE:
;		Straightforward, same as in SHAPE_AREA.  Calls ARREQ from MIDL.
; MODIFICATION HISTORY:
;		Created 30-APR-2013 by Mati Meron.
;		Modified 25-SEP-2013 by Mati Meron.  Streamlined.
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

	res = 0.5*((wx[1:-1,0:-2]- wx[0:-2,1:-1])*(wy[1:-1,1:-1]- wy[0:-2,0:-2])- $
			   (wx[1:-1,1:-1]- wx[0:-2,0:-2])*(wy[1:-1,0:-2]- wy[0:-2,1:-1]))
	
	return, res
end