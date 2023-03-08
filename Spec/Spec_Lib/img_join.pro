Function Img_join, id_0, id_1, id_2, id_3, id_4, id_5, id_6, id_7, $
	horizontal= hor, vertical= ver, transition = trn, boundary = bnd, $
	factor = mfac, _extra = _e

;+
; NAME:
;		IMG_JOIN
; VERSION:
;		8.21
; PURPOSE:
;		Joins 2D image data, matching amplitudes in the overlap areas.
; CATEGORY:
;		2D data processing.
; CALLING SEQUENCE:
;		Result = IMG_JOIN(ID_0, ...  [, keywords])
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
;	/HORIZONTAL											|	One and only one of
;		Switch.  If set, the images are patched in the	|	these two may be
;		horizontal (X) direction.						|	given.  If none is,
;	/VERTICAL											|	the default option
;		Switch.  If set, the images are patched in the	|	is "horizontal".
;		vertical (Y) direction.
;	/TRANSITION
;		Switch.  If set, a smoother transition between the patch regions is
;		provided, at the cost of some growth of relative errors.
;	/BOUNDARY
;		Switch, obsolete, maintained only for backward compatibility.
;	FACTOR
;		Optional output, see below.
;	_EXTRA
;		A formal key, used to transfer additional keywords to SCAN_JOIN and
;		routines imbedded therein.  Especially, the SCAN_JOIN keywords /FORCE
;		and /RELAXED can be used.
; OUTPUTS:
;		Returns an output of the same format as the inputs (see above), covering
;		the largest rectangle overlapped by all the input data sets.
; OPTIONAL OUTPUT PARAMETERS:
;	FACTOR
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scan #i is multiplied, relative to scan #0.  The 0-th value is
;		always 1.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1)  Each data set must have some overlap with the next one, unless
;			/FORCE is set.
;		2)	All data sets must have a non-null common transverse area, meaning,
;			in case of horizontal patching, a common Y-range, and in case of
;			vertical patching, a common X-range.
; PROCEDURE:
;		Amplitude matching over overlap areas, using interpolation to assure a 
;		rectangular coordinate grid for the final result.  Calls SCAN_JOIN and 
;		SCAN_ORDER.  Calls FPU_FIX, INTER_2D, MAKE_GRID, NPARDEF, ONE_OF and 
;		TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created  1-APR-2008 by Mati Meron.
;		Modified 30-JUL-2009 by Mati Meron.  Internal change, speeded up by
;		using INTER_2D instead of generic interpolation.
;		Modified 5-APR-2013 by Mati Meron.  Internal changes.
;		Modified 25-JUL-2013 by Mati Meron.  Internal changes.
;-

	on_error, 1

	nmax = 8
	del = Toler()
	eps = (machar()).xmin
	vfl = One_of(hor,ver) > 0

	n = Npardef(id_0, id_1, id_2, id_3, id_4, id_5, id_6, id_7, whi=j)
	if n gt 0 then begin
		idnam = strcompress('id_' + sindgen(nmax),/remove)
		wdnam = strcompress('wd_' + sindgen(n),/remove)
		odnam = strcompress('od_' + sindgen(n),/remove)
		dims = lonarr(3,n)
		xlim = (ylim = fltarr(2,n))
		dx = (dy = fltarr(n))
		mfac = replicate(1.,n)

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

		if vfl then begin
			xglim = [max(xlim[0,*]),min(xlim[1,*])]
			if xglim[0] ge xglim[1] then message, 'Unpatchable data!'
			yglim = [min(ylim[0,*]),max(ylim[1,*])]
			xgl = Make_grid(xglim,dxy[0],/step,/atmost,dim=xgn)
			ygl = Make_grid(yglim,dxy[1],/step,/atmost,dim=ygn)
			for i = 0, n-1 do begin
				dum = execute('tem = ' + idnam[j[i]])
				yloc = where(ygl ge ylim[0,i] and ygl le ylim[1,i],yln)
				ttem = fltarr(3+erfl,xgn,yln)
				ttem[0:2,*,*] = Inter_2d(xgl,ygl[yloc],/vec,z_sor=tem,/pack)
				if erfl then ttem[3,*,*] = $
				Inter_2d(xgl,ygl[yloc],/vec,z_sor=tem,ind=3) > 0
				otem = fltarr(3,yln)
				otem[0,*] = ygl[yloc]
				otem[1,*] = total(reform(ttem[2,*,*]),1)
				if erfl then otem[2,*] = sqrt(total(reform(ttem[3,*,*])^2,1)) $
				else otem[2,*] = sqrt(otem[1,*])
				dum = execute(wdnam[i] + ' = ttem')
				dum = execute(odnam[i] + ' = otem')
			endfor

			dum = execute('k = Scan_order(' + strjoin(odnam,',') + ')')
			dum = execute('res = ' + wdnam[k[0]])
			dum = execute('pres = ' + odnam[k[0]])
			for i = 1, n-1 do begin
				fir = res
				dum = execute('sec = ' + wdnam[k[i]])
				lo = pres
				dum = execute('hi = ' + odnam[k[i]])
				nlo = (size(lo))[2]
				nhi = (size(hi))[2]
				pres = Scan_join(lo,hi,fac=ffac,efac=efac,over=nov,_extra=_e)
				nov = nov[1]
				mfac[i] = ffac[1]
				if erfl then sec[3,*,*] = $
					sqrt((efac[1]*sec[2,*,*])^2 + (ffac[1]*sec[3,*,*])^2)
				sec[2,*,*] = ffac[1]*sec[2,*,*]
				nto = (size(pres))[2]
				nnov = nlo + nhi - nto
				if nov ne nnov then message, 'Accounting problems!'
				res = fltarr(3+erfl,xgn,nto)
				res[*,*,0:nlo-1] = fir
				if nto gt nlo then res[*,*,nlo:nto-1] = sec[*,*,nov:nhi-1]
				if nov gt 0 then begin
					p = (where(lo[0,*] ge (hi[0,0] - Toler()*abs(hi[0,0]))))[0]
					if erfl then begin
						ulo=abs(fir[2,*,p:p+nov-1]/(fir[3,*,p:p+nov-1]^2>del^2))
						uhi = abs(sec[2,*,0:nov-1]/(sec[3,*,0:nov-1]^2 > del^2))
					endif else ulo = (uhi = 0.5)
					if keyword_set(trn) then begin
						thi = replicate(1.,xgn)#(1 + findgen(nov))
						tlo = reverse(thi,2)
						ulo = tlo*ulo
						uhi = thi*uhi
					endif
					wlo = ulo/((ulo + uhi) > del)
					whi = uhi/((ulo + uhi) > del)
					res[2,*,p:p+nov-1] = $
					wlo*fir[2,*,p:p+nov-1] + whi*sec[2,*,0:nov-1]
					if erfl then res[3,*,p:p+nov-1] = $
					sqrt((wlo*fir[3,*,p:p+nov-1])^2 + (whi*sec[3,*,0:nov-1])^2)
				endif
			endfor
		endif else begin
			xglim = [min(xlim[0,*]),max(xlim[1,*])]
			yglim = [max(ylim[0,*]),min(ylim[1,*])]
			if yglim[0] ge yglim[1] then message, 'Unpatchable data!'
			xgl = Make_grid(xglim,dxy[0],/step,/atmost,dim=xgn)
			ygl = Make_grid(yglim,dxy[1],/step,/atmost,dim=ygn)
			for i = 0, n-1 do begin
				dum = execute('tem = ' + idnam[j[i]])
				xloc = where(xgl ge xlim[0,i] and xgl le xlim[1,i],xln)
				ttem = fltarr(3+erfl,xln,ygn)
				ttem[0:2,*,*] = Inter_2d(xgl[xloc],ygl,/vec,z_sor=tem,/pack)
				if erfl then ttem[3,*,*]= $
				Inter_2d(xgl[xloc],ygl,/vec,z_sor=tem,ind=3) > 0
				otem = fltarr(3,xln)
				otem[0,*] = xgl[xloc]
				otem[1,*] = total(reform(ttem[2,*,*]),2)
				if erfl then otem[2,*] = sqrt(total(reform(ttem[3,*,*])^2,2)) $
				else otem[2,*] = sqrt(otem[1,*])
				dum = execute(wdnam[i] + ' = ttem')
				dum = execute(odnam[i] + ' = otem')
			endfor

			dum = execute('k = Scan_order(' + strjoin(odnam,',') + ')')
			dum = execute('res = ' + wdnam[k[0]])
			dum = execute('pres = ' + odnam[k[0]])
			for i = 1, n-1 do begin
				fir = res
				dum = execute('sec = ' + wdnam[k[i]])
				lo = pres
				dum = execute('hi = ' + odnam[k[i]])
				nlo = (size(lo))[2]
				nhi = (size(hi))[2]
				pres = Scan_join(lo,hi,fac=ffac,efac=efac,over=nov,_extra=_e)
				nov = nov[1]
				mfac[i] = ffac[1]
				if erfl then sec[3,*,*] = $
					sqrt((efac[1]*sec[2,*,*])^2 + (ffac[1]*sec[3,*,*])^2)
				sec[2,*,*] = ffac[1]*sec[2,*,*]
				nto = (size(pres))[2]
				nnov = nlo + nhi - nto
				if nov ne nnov then message, 'Accounting problems!'
				res = fltarr(3+erfl,nto,ygn)
				res[*,0:nlo-1,*] = fir
				if nto gt nlo then res[*,nlo:nto-1,*] = sec[*,nov:nhi-1,*]
				if nov gt 0 then begin
					p = (where(lo[0,*] ge (hi[0,0] - Toler()*abs(hi[0,0]))))[0]
					if erfl then begin
						ulo=abs(fir[2,p:p+nov-1,*]/(fir[3,p:p+nov-1,*]^2>del^2))
						uhi = abs(sec[2,0:nov-1,*]/(sec[3,0:nov-1,*]^2 > del^2))
					endif else ulo = (uhi = 0.5)
					if keyword_set(trn) then begin
						thi = (1+findgen(nov))#replicate(1.,ygn)
						tlo = reverse(thi,1)
						ulo = tlo*ulo
						uhi = thi*uhi
					endif
					wlo = ulo/((ulo + uhi) > del)
					whi = uhi/((ulo + uhi) > del)
					res[2,p:p+nov-1,*] = $
					wlo*fir[2,p:p+nov-1,*] + whi*sec[2,0:nov-1,*]
					if erfl then res[3,p:p+nov-1,*] = $
					sqrt((wlo*fir[3,p:p+nov-1,*])^2 + (whi*sec[3,0:nov-1,*])^2)
				endif
			endfor
		endelse
	endif else message, 'Missing Data!

	return, FPU_fix(res)
end