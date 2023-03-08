Function Img_lc, id_0, id_1, id_2, id_3, id_4, id_5, id_6, id_7, $
	coeffs = cof, dcoeffs = dcof, extrapolate = ext

;+
; NAME:
;		IMG_LC
; VERSION:
;		7.1
; PURPOSE:
;		Generates a linear combination of 2D image data.
; CATEGORY:
;		2D data processing.
; CALLING SEQUENCE:
;		Result = IMG_LC(ID_0, ...  [, keywords])
; INPUTS:
;	ID_0, ID_1, .. ID_7
;		2D image data sets.  Each input must be either a [3,M,N] array or a
;		[4,M,N] array, where the first "slice", [0,*,*], represents the X
;		coordinates of the data, the second slice represents the Y coordinates,
;		the third slice is the data itself and the fourth slice (if present)
;		represents the data errors.  Mixing of the two types of inputs is not
;		allowed.  The maximal number of data sets accepted is 8 (can be
;		increased in the future if needed.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COEFFS
;		Numeric vector, the coefficients of the linear combination.  The number
;		of entries in COEFFS *must* agree with the number of images provided.
;	DCOEFFS
;		Numeric vector, optional.  The statistical errors of the coefficients
;		from COEFFS.  If given, must be same length as COEFFS.  DCOEFFS has no
;		impact on the Y values of the combination but it influences the errors.
;	/EXTRAPOLATE
;		Switch.  If set, the resulting linear combination is formed within the
;		smallest rectangle overlapping all the inputs, extrapolating the data
;		as needed.  By default, the linear combination is formed within the 
;		largest rectangle fully overlapped by all the inputs (so no 
;		extrapolation is needed).
; OUTPUTS:
;		Returns the linear combination of the inputs, in same format as the 
;		inputs (see above).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Unless /EXTRAPOLATE is set, all the data sets have to have a common 
;		area.
; PROCEDURE:
;		Linear combination with interpolation to assure all data sets share
;		same coordinates.  Calls FPU_FIX, INTER_2D, MAKE_GRID and NPARDEF, 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created  1-OCT-2009 by Mati Meron.
;-

	on_error, 1
	nmax = 8

	nco = n_elements(cof)
	if nco gt 0 then begin
		ndco = n_elements(dcof)
		if ndco eq 0 then dcfl = 0 $
		else if ndco eq nco then dcfl = 1 $
		else message, 'Coeffs and errors mismatch!'
	endif else message, 'Coefficients missing!'

	n = Npardef(id_0, id_1, id_2, id_3, id_4, id_5, id_6, id_7, whi=j)
	if n gt 0 then begin
		if n eq nco then begin
			idnam = strcompress('id_' + sindgen(nmax),/remove)
			dims = lonarr(3,n)
			xlim = (ylim = fltarr(2,n))
			dx = (dy = fltarr(n))

			for i = 0, n-1 do begin
				dum = execute('tem = ' + idnam[j[i]])
				stem = size(tem)
				if stem[0] eq 3 and stem[1] ge 3 then dims[*,i] = stem[1:3] $
				else message, 'Input # ' + string(j[i],form='(i0)') + ' is bad!'
				cmin = min(tem[0,*,0],max=cmax)
				xlim[*,i] = [cmin,cmax]
				dx[i] = (cmax - cmin)/(dims[1,i] - 1)
				cmin = min(tem[1,0,*],max=cmax)
				ylim[*,i] = [cmin,cmax]
				dy[i] = (cmax - cmin)/(dims[2,i] - 1)
			endfor
			dxy = [min(dx),min(dy)]

			pmin = min(dims[0,*],max=pmax)
			if pmin eq pmax then begin
				case (pmin < 4) of
					3	:	erfl = 0
					4	:	erfl = 1
					else:	message, 'Bad data format!'
				endcase
			endif else message, 'Inconsistent data format!'

			if keyword_set(ext) then begin
				xglim = [min(xlim[0,*]),max(xlim[1,*])]
				yglim = [min(ylim[0,*]),max(ylim[1,*])]
			endif else begin
				xglim = [max(xlim[0,*]),min(xlim[1,*])]
				yglim = [max(ylim[0,*]),min(ylim[1,*])]
				if xglim[0] ge xglim[1] or yglim[0] ge yglim[1] $
				then message, "No overlap region, can't proceed!"
			endelse

			xgl = Make_grid(xglim,dxy[0],/step,dim=xgn)
			ygl = Make_grid(yglim,dxy[1],/step,dim=ygn)
			res = fltarr(3+erfl,xgn,ygn)
			res[0,*,*] = xgl#replicate(1.,ygn)
			res[1,*,*] = replicate(1.,xgn)#ygl

			for i = 0, n-1 do begin
				dum = execute('tem = ' + idnam[j[i]])
				dat = Inter_2d(res[0,*,*],res[1,*,*],z_sor=tem,ext=ext)
				res[2,*,*] = res[2,*,*] + cof[i]*dat
				if erfl then begin
					err= Inter_2d(res[0,*,*],res[1,*,*],z_sor=tem,ext=ext,ind=3)
					serr = (cof[i]*err)^2
					if dcfl then serr = serr + (dcof[i]*dat)^2
					res[3,*,*] = res[3,*,*] + serr
				endif
			endfor
			if erfl then res[3,*,*] = sqrt(res[3,*,*])
		endif else message, 'Data - coefficients mismatch!'
	endif else message, 'Missing data!'

	return, FPU_fix(res)
end