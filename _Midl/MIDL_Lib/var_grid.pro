Function Var_grid, bounds, npoints, stepsize = stp, atmost = atm, length = len

;+
; NAME:
;		VAR_GRID
; VERSION:
;		8.41
; PURPOSE:
;		Generates a 1D grid with variable spacing.
; CATEGORY:
;		Array manipulation.
; CALLING SEQUENCE:
;		Result = VAR_GRID ( BOUNDS, NPOINTS [, keywords])
; INPUTS:
;	BOUNDS
;		Numeric vector, length >= 2.  Each consecutive entry represents a 
;		segment boundary.  Bounds must be in ascending or descending order.
;	NPOINTS
;		Numeric vector containing the number of points within each segment.
;		Length must be 1 less than the length of BOUNDS, or 1, in which case
;		same number of points is assigned to all the segments.
; KEYWORD PARAMETERS:
;	/STEPSIZE
;		Switch.  Specifies that the entries in NPOINTS are to be treated as
;		step sizes (see the routine MAKE_GRID).
;	/ATMOST
;		Switch.  Specifies that step sides can only be rounded down (see 
;		the routine MAKE_GRID)
;	DIMVEC
;		Optional output, see below.
;	FUNARR
;		Optional output, see below.
; OUTPUTS:
;		Returns an array with the dimensions (NDIM,NPOINTS(0),...) where NDIM
;		is the number of dimensions of the grid, such that Result(*,i0,i1,...)
;		is a vector containing the cartesian coordinates of the point at
;		location (i0,i1,...) within the grid.  The output type is FLOAT (or
;		DOUBLE if SPAN is of type DOUBLE).
; OPTIONAL OUTPUT PARAMETERS:
;	LENGTH
;		The name of the variable to receive the number of points in the result.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		See restriction on BOUNDS, above.  Also the standard restrictions of
;		MAKE_GRID.
; PROCEDURE:
;		Straightforward.  Uses ARREQ, FLTROUND, MAKE_GRID and SORPURGE,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 1-MAR-2015 by Mati Meron.
;-

	on_error, 1

	n = n_elements(bounds)
	if n ge 2 then begin
		nn = n_elements(npoints)
		if nn eq 1 then wnpo = replicate(npoints,n-1) $
		else if nn eq n-1 then wnpo = npoints $
		else message, 'Input mismatch!' 

		okfl = 0
		s = sort(bounds)
		if Arreq(s,lindgen(n)) then okfl = 1 $
		else if Arreq(s,reverse(lindgen(n))) then okfl = -1
		if okfl then begin
			res = []
			for i = 0, n-2 do begin
				next = Make_grid(bounds[i:i+1],wnpo[i],step=stp,atmost=atm)
				res = [res,next]
			endfor
		endif else message, 'Improper boundaries!'
		res = Fltround(res,dig=7)
		ss = Sorpurge(res,net=len)
		res = res[ss]
		if okfl eq -1 then res = reverse(res)		
	endif else message, 'Boundaries need at least 2 points!'

	return, res
end