Function Sphereg, npo, dim, size = siz, center = cnt, radius = rad, $
	outside= out, count = con

;+
; NAME:
;		SPHEREG
; VERSION:
;		3.3
; PURPOSE:
;		Selects the inside (and, optionally, outside) of a circular or annular 
;		region of an array.
; CATEGORY:
;		Array processing.
; CALLING SEQUENCE:
;		Result = SPHEREG([NPO] [,DIM] [SIZE = SIZ], RADIUS = RAD [,keywords])
; INPUTS:
;	NPO
;		Scalar or vector.  Not needed if SIZ is provided, mandatory otherwise.
;		If NPO is a scalar and DIM (see below) is not provided then SPHEREG 
;		assumes a square NPOxNPO array.  If DIM is provided then an array with
;		DIM dimensions and NPO points along each dimension is assumed.  Finally
;		if NPO is a vector then a NPO[0]xNPO[1]x... array is assumed and DIM 
;		(if provided) is ignored.
; OPTIONAL INPUT PARAMETERS:
;	DIM
;		Number of dimensions in the array.  Matters only if NPO is scalar and 
;		SIZE is not provided.
; KEYWORD PARAMETERS:
;	SIZE
;		Accepts input in the form of the output of the IDL SIZE command and 
;		uses it to form the array.  Mandatory, unless NPO is provided.  If both
;		SIZE and NPO are provided, NPO is ignored.
;	CENTER
;		Optional.  Location of the center of the selection sphere.  If given
;		must be a vector with one entry for each dimension of the array.  If
;		not provided, defaults to the middle of each dimension.
;	RADIUS
;		Mandatory.  The radius of the selection sphere.  may be given as scalar 
;		or as a 2D vector.  In the second case the annular region between
;		RADIUS_min and RADIUS_max is selected.
;	OUTSIDE
;		Optional output, see below.
;	COUNT
;		Optional output, see below.
; OUTPUTS:
;		Returns the indices of the array locations which fulfill the 
;		appropriate "inside" condition.  If no such indices exist, returns -1.
; OPTIONAL OUTPUT PARAMETERS:
;	OUTSIDE
;		The name of the variable to receive to complement of the indices set
;		returned by the function (i.e. indices corresponding to the outside of
;		the spherical or annular region).
;	COUNT
;		The name of the variable to receive the number of points fulfilling the
;		"inside" condition.  Same as the keyword COUNT in the WHERE function.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Generates an array of same dimension as the original,
;		containing the sums of the squares of the coordinates (relative to the
;		center) and compares with the provided radius (radii).  Calls DEFAULT
;		and MAKE_GRID from MIDL.
; MODIFICATION HISTORY:
;		Created 4-OCT-1999 by Mati Meron.
;-

	on_error, 1
	ndmax = 6

	if n_elements(siz) eq 0 then begin
		ntem = n_elements(npo)
		case ntem of
			0	:	message, 'Missing size information!'
			1	:	begin
						ndim = Default(dim,2l,/dtyp)
						dims = replicate(npo,ndim)
					end
			else:	begin
						ndim = ntem
						dims = npo
					end
		endcase
	endif else begin
		ndim = siz(0)
		dims = siz(1:ndim)
	endelse
	if ndim gt ndmax then message, $
	'At most' + string(ndmax,form='(i2)') + ' dimensions allowed!'

	cnt = Default(cnt,0.5*(dims-1),/dtyp)
	rsq = [min(([rad,0.])[0:1]^2,max=max),max]
	gvals = transpose([[0*dims],[dims-1]])
	grid = Make_grid(gvals,dims,fun= sqv)
	for i = 0, ndim - 1 do sqv = sqv + (grid[i,*,*,*,*,*,*] - cnt[i])^2

	bin = sqv ge rsq[0] and sqv le rsq[1]
	out = where(bin - 1b)

	return, where(bin, con)
end