Function CRL_opt, energy= ene, lens= len, size= siz, serror= ser, show = sho, $
	minsiz= mis, rerror = rer, _extra = _e

;+
; NAME:
;		CRL_OPT
; VERSION:
;		8.72
; PURPOSE:
;		Calculates optimal number of lenses, or optimal energy, for focusing.
; CATEGORY:
;		CRL optics.
; CALLING SEQUENCE:
;		Result = CRL_OPT( {ENERGY = ENE or LENS = LEN}, SIZE = SIZ
;		 		[,SERROR = SER] [, more keywords])
; INPUTS:
; 		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	ENERGY										|
; 		Vector, length 3 or more.				|	One and only one of these
; 	LENS										|	two must be provided.
; 		Vector, length 3 or more.				|
; 	SIZE
; 		Beam size, vector, same length as ENERGY of LENS.
; 	SERROR
; 		The error values of SIZE, same length as SIZE.
; 	/SHOW
; 		Switch.  If set, a plot of the data and the fit is displayed.
;	MINSIZE
;		Optional output, see below.
;	RERROR
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.  Provides access to PAREXT.
; OUTPUTS:
;		Returns the location of the extremum of the function represented by Y.
; OPTIONAL OUTPUT PARAMETERS:
;	MINSIZE
;		Returns the estimated value of the function at the extremum.
;	RERROR
;		Returns the estimated error of the return value.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Fits the input X, Y values to a parabola and find the fit's minimum.
;		Calls PAREXT from CRL_LIB.  Calls CODIMS, ISNUM, MAKE_GRID, ONE_OF,
;		POLEVAL and SORPURGE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-FEB-2019 by Mati Meron.
;		Modified 20-MAR-2020 by MAti Meron.  Added keyword SHOW.
;-

	on_error, 1

	if One_of(ene,len,val=elen) ge 0 then begin
		if Codims(elen,siz,ser,/same,dim=dim) then begin
			if Isnum(ser) then fer = 2*siz*ser else fer = []
			res = Parext(elen,siz^2,fer,ext=misq,error=rer,coe=coe)
			mis = sqrt(misq)
			if keyword_set(sho) then begin
				x = [elen,Make_grid([min(elen,max=max),max],16*dim+1)]
				x = x[Sorpurge(x)]
				y = sqrt(Poleval(x,coe))
				plot, x, y, /nodata, _extra = _e
				oplot, x, y, thi=2, col=!pcol.green
				oplot, elen, siz, psym=7, symsize=1.4, thi=2, col=!pcol.blue
				oplot, [res], [mis], psym=8, symsize=1.5, thi=2, col=!pcol.red
			endif

		endif else message, 'Inconsistent inputs!'
	endif else message, 'Missing Energy/Lenses input!'

	return, res
end