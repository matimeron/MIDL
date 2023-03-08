Function Img_reform, dat, tol, xvals = xvl, yvals = yvl, sort = sor

;+
; NAME:
;		IMG_REFORM
; VERSION:
;		8.31
; PURPOSE:
;		Reformats an image, changing the X and/or Y coordinates.
; CATEGORY:
;		2D data processing
; CALLING SEQUENCE:
;		Result = IMG_REFORM( DAT [, XVALS = XVL] [, YVALS = YVL])
; INPUTS:
;	DAT
;		Standard data representation 3D array with dimensions [4,M,N].
;		Dimensions [3,M,N] (no errors) also acceptable.
; OPTIONAL INPUT PARAMETERS:
;	TOL
;		Tolerance value, specifying by how much the new coordinates may exceed
;		the original range and still be considered (note that in such case 
;		they'll be corrected to lie within the original range).  Default is 0.
; KEYWORD PARAMETERS:
;	XVALS
;		Vector to be used as the new X coordinates.  Doesn't have to be 
;		equispaced.  If not given, the original X coordinates will be used.
;
;		Note:	Only the values of XVALS within the range covered by the 
;				original X coordinates will be used (however, see TOL above). At
;				least 2 such values must be present, else an error will result.
;	YVALS
;		Same as XVALS, for the Y coordinates.
;
;		Note:	If neither XVALS nor YVALS are given, the original data will
;				be returned.
;	/SORT
;		Switch.  If set, the XVALS and/or YVALS inputs, if defined, are sorted
;		in ascending order prior to being used.
; OUTPUTS:
;		Returns the reformated data, in [4,M',N'] (or [3,M',N']) format.
;		If XVALS and YVALS are not provided, returns the original data.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Both XVALS and YVALS, when given, must have at least 2 values within 
;		the original X and Y ranges, respectively.
; PROCEDURE:
;		Generates the new data from the original one using interpolation.  
;		Calls DEFAULT, FPU_FIX, HOW_MANY, INTER_2D and SORPURGE, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2013 by Mati Meron.
;		Modified 25-OCT-2013 by Mati Meron.  Added input TOL.
;		Modified 10-JUN-2014 by Mati Meron.  Internal changes.
;-

	on_error, 1

	siz = size(dat)
	if siz[0] eq 3 then begin
		case (siz[1] < 4) of
			3	:	erfl = 0
			4	:	erfl = 1
			else:	message, 'Bad data format!'
		endcase
		xs = reform(dat[0,*,0])
		xran = [min(xs,max=xmax),xmax]
		ys = reform(dat[1,0,*])
		yran = [min(ys,max=ymax),ymax]

		if How_many(fir=xvl,sec=yvl,whi=whi) gt 0 then begin
			tol = Default(tol,0.)
			sfl = keyword_set(sor)
			if whi and 1 then begin
				if sfl then wxvl = xvl[Sorpurge(xvl)] else wxvl = xvl
				dum = where(wxvl ge xran[0]-tol and wxvl le xran[1]+tol, ndum)
				if ndum gt 1 then xr = wxvl[dum] $
				else message, 'Not enough X-points to proceed!'
				xr = xran[0] > xr < xran[1]
			endif else xr = xs
			if whi/2 and 1 then begin
				if sfl then wyvl = yvl[Sorpurge(yvl)] else wyvl = yvl
				dum = where(wyvl ge yran[0]-tol and wyvl le yran[1]+tol, ndum)
				if ndum gt 1 then yr = wyvl[dum] $
				else message, 'Not enough Y-points to proceed!'
				yr = yran[0] > yr < yran[1]
			endif else yr = ys
			res = fltarr(3+erfl,n_elements(xr),n_elements(yr))
			res[0:2,*,*] = Inter_2d(xr,yr,/vec,z_sor=dat,/pack)
			if erfl then res[3,*,*] = Inter_2d(xr,yr,/vec,z_sor=dat,ind=3)
		endif else res = dat
	endif else message, 'Bad input!'

	return, FPU_fix(res)
end